package edu.ucsd.snippy.scoopy

import edu.ucsd.snippy.enumeration.OEValuesManager
import edu.ucsd.snippy.predicates.Predicate
import edu.ucsd.snippy.solution.SolutionEnumerator
import edu.ucsd.snippy.vocab.VocabFactory
import edu.ucsd.snippy.{SynthesisTask, ast}

class ScopesSynthesisTask(
	 // Problem definition8
	 override val parameters: List[(String, ast.Types.Value)],
	 override val outputVariables: List[String],
	 override val vocab: VocabFactory,
	 override val contexts: List[Map[String, Any]],
	 override val predicate: Predicate,

	 // Synthesizer state
	 override val oeManager: OEValuesManager,
	 override val enumerator: SolutionEnumerator) extends
		SynthesisTask(parameters, outputVariables, vocab, contexts, predicate, oeManager, enumerator){


}

object ScopesSynthesisTask {

//	def fromString(jsonString: String, simAssign: Boolean = false): ScopesSynthesisTask ={
//		val input = JsonParser.parse(jsonString).asInstanceOf[JObject].values
//		val outputVarNames: List[String] = input("varNames").asInstanceOf[List[String]]
//		val envs: Map[String,List[Map[String, Any]]] = input("envs").asInstanceOf[Map[String,List[Map[String, Any]]]];//mpa from scope id to list of environments
//
//
//
//
//		// First, build a tuple of (prevEnv, env) for all the envs
//		val allEnvs: List[(Option[Map[String, Any]], Map[String, Any])] = SynthesisTask.organizeEnvs(envs.get("0").get, Map[Int, Map[String, Any]]());
//		val justEnvs = allEnvs.map(_._2);
//
//		var contexts: List[Context] = allEnvs.map {
//			case (Some(prevEnv), env) =>
//				env.filter(entry => !outputVarNames.contains(entry._1)) ++
//					outputVarNames.filter(prevEnv.contains).collect(varName => varName -> prevEnv(varName)).toMap
//			case (None, env) => env.filter(entry => !outputVarNames.contains(entry._1))
//		}
//
//		val oeManager = new RequiresValuesManager
//		val additionalLiterals = getStringLiterals(justEnvs, outputVarNames)
//
//		val predicate: Predicate = outputVarNames match {
//			case single :: Nil => Predicate.getPredicate(single, justEnvs, oeManager)
//			case multiple =>
//				val (newContexts, pred) = SynthesisTask.mulitvariablePredicate(multiple, contexts, justEnvs)
//				contexts = newContexts
//				pred
//		}
//
//		val parameters = contexts.flatMap(_.keys)
//			.toSet[String]
//			.map(varName => varName -> Utils.getTypeOfAll(contexts.map(ex => ex.get(varName)).filter(_.isDefined).map(_.get)))
//			.filter(!_._2.equals(Types.Unknown))
//			.toList
//
//		//val requiredMakers = getRetains(Scopeable: string)
//
////		val requiredMaker: RequiredVocabMaker();
////		val vocab: VocabFactory = VocabFactory(parameters, additionalLiterals, List(requiredMaker))
////
////		val enumerator: SolutionEnumerator = predicate match {
////			case pred: MultilineMultivariablePredicate if simAssign =>
////				new ConditionalSingleEnumMultivarSimultaneousSolutionEnumerator(pred, parameters, additionalLiterals)
////			case pred: MultilineMultivariablePredicate =>
////				new ConditionalSingleEnumMultivarSolutionEnumerator(pred, parameters, additionalLiterals)
////			case pred: SingleVariablePredicate =>
////				val bank = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
////				val mini = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
////				val enumerator = new enumeration.ProbEnumerator(vocab, oeManager, contexts, false, 0, bank, mini, 100)
////				new ConditionalSingleEnumSingleVarSolutionEnumerator(enumerator, pred.varName, pred.retType, pred.values, contexts)
////			case _ =>
////				val enumerator = new BasicEnumerator(vocab, oeManager, contexts)
////				new BasicSolutionEnumerator(predicate, enumerator)
////		}
//
//
//	}



}





