package edu.ucsd.snippy

import edu.ucsd.snippy.ast._
import edu.ucsd.snippy.enumeration._
import edu.ucsd.snippy.predicates._
import edu.ucsd.snippy.scoopy.ScopeSpecification
//import edu.ucsd.snippy.scoopy.ScopeSpecification
import edu.ucsd.snippy.solution._
import edu.ucsd.snippy.utils._
import edu.ucsd.snippy.vocab._
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.JsonParser

import scala.collection.mutable

class SynthesisTask(
	// Problem definition8
	val parameters: List[(String, ast.Types.Value)],
	val outputVariables: Set[String],
	val vocab     : VocabFactory,
	val contexts: List[Map[String, Any]],
	val predicate: Predicate,

	// Synthesizer state
	val oeManager : OEValuesManager,
	val enumerator: SolutionEnumerator)
{
	override def toString: String =
	{
		s"\tparameters: $parameters\n" +
			"\tvocab: [...]\n" +
			s"\tcontexts: $contexts" +
			s"\tpredicate: $predicate"
	}
}

object SynthesisTask
{
	type Context = Map[String, Any]
	val reserved_names: Set[String] =
		Set("time", "#", "$", "lineno", "prev_lineno", "next_lineno", "__run_py__")

	def fromString(jsonString: String, simAssign: Boolean = false): SynthesisTask = {
		val input = JsonParser.parse(jsonString).asInstanceOf[JObject].values
		val outputVarNames: Set[String] = input("varNames").asInstanceOf[List[String]].toSet
		val envs: List[Map[String, Any]] = input("envs").asInstanceOf[List[Map[String, Any]]]
		val previousEnvMap: Map[Int, Map[String, Any]] = input("previousEnvs").asInstanceOf[Map[String, Map[String, Any]]].map(tup => tup._1.toInt -> tup._2)

		// First, build a tuple of (prevEnv, env) for all the envs
		val allEnvs: List[(Option[Map[String, Any]], Map[String, Any])] = organizeEnvs(envs, previousEnvMap);
		val justEnvs = allEnvs.map(_._2)

		var contexts: List[Context] = allEnvs.map {
			case (Some(prevEnv), env) =>
				env.filter(entry => !outputVarNames.contains(entry._1)) ++
					outputVarNames.filter(prevEnv.contains).collect(varName => varName -> prevEnv(varName)).toMap
			case (None, env) => env.filter(entry => !outputVarNames.contains(entry._1))
		}

		val oeManager = new InputsValuesManager
		val additionalLiterals = getStringLiterals(justEnvs, outputVarNames.toList)

		val predicate: Predicate = outputVarNames.toList match {
			case single :: Nil => Predicate.getPredicate(single, justEnvs, oeManager, new Contexts(contexts))
			case multiple =>
				val (newContexts, pred) = this.mulitvariablePredicate(multiple, contexts, justEnvs)
				contexts = newContexts
				pred
		}

		val parameters = contexts.flatMap(_.keys)
			.toSet[String]
			.map(varName => varName -> Utils.getTypeOfAll(contexts.map(ex => ex.get(varName)).filter(_.isDefined).map(_.get)))
			.filter(!_._2.equals(Types.Unknown))
			.toList
		val vocab: VocabFactory = VocabFactory(parameters, additionalLiterals)

		val enumerator: SolutionEnumerator = predicate match {
			case pred: MultilineMultivariablePredicate if simAssign =>
				new ConditionalSingleEnumMultivarSimultaneousSolutionEnumerator(pred, parameters, additionalLiterals)
			case pred: MultilineMultivariablePredicate =>
				new ConditionalSingleEnumMultivarSolutionEnumerator(pred, parameters, additionalLiterals)
			case pred: SingleVariablePredicate =>
				val bank = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
				val mini = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
				val enumerator = new enumeration.ProbEnumerator(vocab, oeManager, contexts, false, 0, bank, mini, 100)
				new ConditionalSingleEnumSingleVarSolutionEnumerator(enumerator, pred.varName, pred.retType, pred.values, contexts)
			case _ =>
				val enumerator = new BasicEnumerator(vocab, oeManager, contexts)
				new BasicSolutionEnumerator(predicate, enumerator)
		}

		new SynthesisTask(
			parameters,
			outputVarNames,
			vocab,
			contexts,
			predicate,
			oeManager,
			enumerator)
	}


	def getContextsFromExamples(examples: List[Map[String, Any]], outVarNames: Set[String]): List[Context] = {
		examples.map {
			env => env.filter(entry => !outVarNames.contains(entry._1))
		}
	}

	/**
	 * This function is used to create a SynthesisTask from a ScopeSpecification
	 * @param spec - the ScopeSpecification
	 * @param requiredASTs asts that must be in the solution
	 * @param knownAssignments known assignments to variables. its maps from the branch name to to required assignments
	 * @param simAssign
	 * @return SynthesisTask
	 */
	def fromSpec(spec:ScopeSpecification, requiredASTs:List[ASTNode]=List(), knownAssignments:Map[String, ASTNode], simAssign: Boolean = false): SynthesisTask={
		val outputVarNames: Set[String] = spec.outputVarNames
		val (previousEnvMap, envs):(Map[Int, Map[String, Any]], List[Map[String, Any]]) = spec.getPrevEnvsAndEnvs()


		val allEnvs: List[(Option[Map[String, Any]], Map[String, Any])] = organizeEnvs(envs, previousEnvMap);
		val justEnvs = allEnvs.map(_._2)

		var contexts: List[Context] = allEnvs.map {
			case (Some(prevEnv), env) =>
				env.filter(entry => !outputVarNames.contains(entry._1)) ++
					outputVarNames.filter(prevEnv.contains).collect(varName => varName -> prevEnv(varName)).toMap
			case (None, env) => env.filter(entry => !outputVarNames.contains(entry._1))
		}



		val oeManager = new RequiresValuesManager
		val additionalLiterals = getStringLiterals(justEnvs, outputVarNames.toList)

		val predicate: Predicate = outputVarNames.toList match {
			case single :: Nil => Predicate.getPredicate(single, justEnvs, oeManager, new Contexts(contexts))
			case multiple =>
				val (newContexts, pred) = this.mulitvariablePredicate(multiple, contexts, justEnvs)
				contexts = newContexts
				pred
		}

		val parameters = contexts.flatMap(_.keys)
			.toSet[String]
			.map(varName => varName -> Utils.getTypeOfAll(contexts.map(ex => ex.get(varName)).filter(_.isDefined).map(_.get)))
			.filter(!_._2.equals(Types.Unknown))
			.toList
		val requiredVocabMakers = requiredASTs.zipWithIndex.map((x)=> new RequiredVocabMaker(x._1, List(), x._2, new Contexts(contexts))) // makes the contexts good in singleVar. in multi var it will be done inside the node

		val vocab: VocabFactory = VocabFactory(parameters, additionalLiterals,  requiredVocabMakers)


		val enumerator: SolutionEnumerator = predicate match {
			case pred: MultilineMultivariablePredicate if simAssign =>
				new ConditionalSingleEnumMultivarSimultaneousSolutionEnumerator(pred, parameters, additionalLiterals, spec.getPartitionFunc)
			case pred: MultilineMultivariablePredicate =>
				if(requiredASTs.nonEmpty){
					new ConditionalSingleEnumMultivarSolutionEnumeratorInnerVars(pred, parameters, additionalLiterals, spec.getPartitionFunc, knownAssignments, requiredASTs)
				}
				else{
					new ConditionalSingleEnumMultivarSolutionEnumerator(pred, parameters, additionalLiterals, spec.getPartitionFunc, knownAssignments, requiredASTs)
				}
			case pred: SingleVariablePredicate =>
				val bank = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
				val mini = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
				val enumerator = new enumeration.ProbEnumerator(vocab, oeManager, contexts, false, 0, bank, mini, 100)
				new ConditionalSingleEnumSingleVarSolutionEnumerator(enumerator, pred.varName, pred.retType, pred.values, contexts, spec.getPartitionFunc, requiredVocabMakers.length)
		}

		new SynthesisTask(
			parameters,
			outputVarNames,
			vocab,
			contexts,
			predicate,
			oeManager,
			enumerator)
	}
	//build a tuple of (prevEnv, env) for all the envs
	def organizeEnvs(envs: List[Map[String, Any]], previousEnvMap:Map[Int, Map[String, Any]]): List[(Option[Map[String, Any]], Map[String, Any])] ={
		envs.map(env => {
			val time = env("time").asInstanceOf[BigInt].toInt
			if (previousEnvMap.contains(time)) {
				Some(previousEnvMap(time)) -> env
			} else {
				// We need to use the time + iter to see if we can find the previous env in the
				// other envs

				val iterStr =if (env.contains("#"))  env("#").asInstanceOf[String] else "";
				if (iterStr.isEmpty) {
					// Not a loop, and no prev env, so no luck :(
					None -> env
				} else {
					// Find the nearest entry with the iter one less than this one
					val iter = iterStr.toInt
					val prevEnv = envs
						.filter(env => env("time").asInstanceOf[BigInt].toInt < time)
						.filter(env => env.contains("#") && env("#").asInstanceOf[String].toInt == iter - 1)
						.lastOption
					prevEnv -> env
				}
			}
		})
			.map(tup => tup._1.map(cleanupInputs) -> cleanupInputs(tup._2))
	}

	def mulitvariablePredicate(
		outputVarNames: List[String],
		inputContexts: List[Context],
		outputEnvs: List[Map[String, Any]]): (List[Context], MultilineMultivariablePredicate) = {
		var contexts = inputContexts


		val dupVarNames = outputVarNames.filter(name=>name.contains("else") || name.contains("then")).map(_.replace("_then", "").replace("_else", "")).toSet
		val ActualVarName = (outputVarNames.filter(name=> !name.contains("else") && !name.contains("then")) ++ dupVarNames).toSet
		// We can support multiline assignments, so let's build the graph
		// We start with enumerating all the possible environments
		val environments_ = this.enumerateEnvs(outputVarNames, contexts, outputEnvs)
		val environments = environments_.filter(tup => tup._1.count(name => name.contains("_then") || name.contains("_else")) < 2) //no mre than 2 of the same var TODO:what if there is two _then vars. rs_then t_then
		// enviornments.head := Same as "contexts", the environment with all old values -> The starting node
		// environment.last := processedEnvs, the environment with all the new values -> The end node

		// We need to add all but the above to the evaluation context, both to preserve completeness under OE, and
		// because the MultilineMultivariablePredicate uses them to determine when Synthesis is complete.
		// We also need to keep track of their indices in the final context, since the context is a flatmap of them,
		// while the predicate needs to be able to extract only the relevant values from synthesized ASTNodes.

		// We can actually just overwrite `context` here, since the original context is represented by the
		// first graph node.

		val nodes = environments
			.map { case (preAssigned, envs) =>
				contexts = contexts ++ envs
				(envs, Range(contexts.length - envs.length, contexts.length).toList, preAssigned)
			}
			.map { case (env, indices, preAssigned) => new predicates.Node(env, Nil, indices, false, preAssigned.toList) }

		// We never evaluate in the final env, so can we pop that from context.
		contexts = contexts.dropRight(1)

		// Set the final node
		//nodes.last.isEnd = true
		nodes.foreach(x=> if(x.nodesVars.length==ActualVarName.size) {x.isEnd=true})

		// Note: There's a dependency here between the order of nodes, and the order of the `environments`, which
		// we use below to find the nodes that should have a variable in-between.

		// Now we need to create the edges and set them in the nodes
		environments.zipWithIndex.foreach{ case ((thisVars, _), thisIdx) =>
			val nodeEdges: List[predicates.Edge] = Range(thisIdx + 1, environments.length).map(thatIdx => {
				val thatVars = environments(thatIdx)._1
				if (thatVars.size > thisVars.size && thisVars.forall(thatVars.contains)) {
					// We need to create an edge between the two
					val newVars = thatVars -- thisVars
					val parent = nodes(thisIdx)
					val child = nodes(thatIdx)

					if (newVars.size == 1) {
						val values = outputEnvs.flatMap(map => map.filter(_._1 == newVars.head).values)
						Some(SingleEdge(None, newVars.head, Utils.getTypeOfAll(values), parent, child))
					} else {
						Some(MultiEdge(
							mutable.Map.from(newVars.map(_ -> None)),
							newVars.map(varName => varName -> Utils.getTypeOfAll(outputEnvs.flatMap(map => map.filter(_._1 == varName).values))).toMap,
							parent,
							child))
					}
				} else {
					// There is no edge between these nodes
					None
				}
			})
				.filter(_.isDefined)
				.map(_.get)
				.toList

			nodes(thisIdx).edges = nodeEdges
		}

		(contexts, new MultilineMultivariablePredicate(nodes.head, new Contexts(contexts)))
	}

	def cleanupInputs(input: Map[String, Any]): Map[String, Any] =
	{
		val parser = new InputParser
		input
			.filter(v => !reserved_names.contains(v._1))
			// TODO Is there a cleaner way to do this?
			.filter(_._2.isInstanceOf[String])
			.map(variable => parser.parse(variable._2.asInstanceOf[String]) match {
				case None =>
					DebugPrints.eprintln(s"Input not recognized: $variable")
					(variable._1, null) ;
				case Some("__BOT__")=> (variable._1, None)
				case Some(v) =>
					(variable._1, v)
			})
			.filter(v => v._2 != null)
	}

	private def enumerateEnvs(
		outputVariables: List[String],
		startingEnvs: List[Map[String, Any]],
		endingEnvs: List[Map[String, Any]]): List[(Set[String], List[Map[String, Any]])] = {
		outputVariables match {
			case last :: Nil => List(
				(Set(), startingEnvs),
				(Set(last), startingEnvs.zip(endingEnvs).map(envs => envs._1 + (last -> envs._2(last)))))
			case first :: rest =>
				enumerateEnvs(rest, startingEnvs, endingEnvs) ++
				enumerateEnvs(
					rest,
					startingEnvs.zip(endingEnvs).map(envs => envs._1 + (first -> envs._2(first))),
					endingEnvs)
					.map(tup => (tup._1 + first, tup._2))
		}
	}

	def getStringLiterals(
		startingContexts: List[Map[String, Any]],
		outputNames: List[String]): Set[String] =
	{
		outputNames
			.filter(name => if(startingContexts.forall(_.contains(name))) startingContexts.map(_(name)).exists(Types.typeof(_) == Types.String) else false)
			.flatMap(outputName =>
			{
				startingContexts.flatMap {
						ex =>
							val stringInputs = ex
								.filter { case (name, value) => !outputNames.contains(name) && Types.typeof(value) == Types.String }
								.map(_._2.asInstanceOf[String])
							ex(outputName)
								.asInstanceOf[String]
								.filter(char => stringInputs.forall(inputVal => !inputVal.contains(char.toLower) && !inputVal.contains(char.toUpper)))
								.map(_.toString)
								.toSet
					}
			})
		.toSet
	}
}
