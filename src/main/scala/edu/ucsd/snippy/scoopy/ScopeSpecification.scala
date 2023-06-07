package edu.ucsd.snippy.scoopy

import edu.ucsd.snippy.Snippy.synthesizeAST
import edu.ucsd.snippy.SynthesisTask.Context
import edu.ucsd.snippy.ast.Types.Types
import edu.ucsd.snippy.ast._
import edu.ucsd.snippy.enumeration.Contexts
import edu.ucsd.snippy.utils.Utils
import edu.ucsd.snippy.utils.Utils.getBinaryPartitions
import edu.ucsd.snippy.vocab.{BasicVocabMaker, VocabMaker}
import edu.ucsd.snippy.{PythonParser, SynthesisTask}
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.JsonParser

import scala.collection.mutable

class ScopeSpecification(val scopeExamples: List[Map[String, Any]],
						 val partition: (List[Int], List[Int]),
						 val requiredVocabMakers: List[VocabMaker],
						 val optionalVocabMakers: List[VocabMaker],
						 val outputVarNames: List[String]=List(),
						 val auxVariables:List[String] = List(),
						 val scopeType:String){

	def getPartitionFunc(): List[Any] => List[(Set[Int], Set[Int])] = {
		val pastPartition = (this.partition._1.toSet, this.partition._2.toSet)
		(indices: List[Any]) => {
			getBinaryPartitions(indices).filter(part =>
				(pastPartition._1.subsetOf(part._1) && pastPartition._2.subsetOf(part._1)) || // both in the first
					(pastPartition._1.subsetOf(part._2) && pastPartition._1.subsetOf(part._2)) || //both in the second
					(pastPartition._1.subsetOf(part._1) && pastPartition._2.subsetOf(part._2)) || // first in the first, second in the second
					(pastPartition._1.subsetOf(part._2) && pastPartition._2.subsetOf(part._1))) // first in the second, second in the first
		}
	}
}


object ScopeSpecification {
	var required = 0

	def ASTVarsVisitor(root: ASTNode, goodVars: List[String] = List(), hole: ASTNode, contexts: Contexts): ASTNode = {
		val newAst = root match {
			case v: VariableNode[_] => if (!goodVars.contains(v.name)) hole else v
			case _ => root.updateChildren(root.children.map(child => ASTVarsVisitor(child, goodVars, hole, contexts).updateValues(contexts)).toSeq, List())
		}
		newAst
	}

	def ASTVarsCounter(root: ASTNode, goodVars: List[String] = List()): Int = {
		root match {
			case v: VariableNode[_] => if (!goodVars.contains(v.name)) 1 else 0
			case _ => root.children.map(child => ASTVarsCounter(child, goodVars)).sum
		}
	}

	def ASTGetVarType(root: ASTNode, goodVars: List[String] = List(), typesMap:mutable.Map[String,Types]): List[Types] = {
		root match {
			case v: VariableNode[_] => if (!goodVars.contains(v.name)) List(typesMap(v.name)) else List()
			case _ => root.children.flatMap(child => ASTGetVarType(child, goodVars, typesMap)).toList
		}
	}

	def ASTGetGoodVars(root: ASTNode): List[String] = {
		root match {
			case v: ListCompNode[_] => List(v.varName)
			case _: VariableNode[_] => List()
			case _: LiteralNode[_] => List()
			case _ => root.children.map(child => ASTGetGoodVars(child)).flatten.toList
		}
	}

	def assign(code: String, contexts: List[Context], inputVars:List[String]): ScopeSpecification = {
		val typesMap_ = contexts.flatMap(_.keys)
			.toSet[String]
			.map(varName => varName -> Utils.getTypeOfAll(contexts.map(ex => ex.get(varName)).filter(_.isDefined).map(_.get)))
			.filter(!_._2.equals(Types.Unknown))
			.toMap
		val typesMap = collection.mutable.Map(typesMap_.toSeq: _*)

		val varName = code.split("=")(0).trim
		val assignment = s"'${code.split("=").last.trim}"
		val parser = new PythonParser(contexts)
		val ast = parser.parse(assignment)
		ast.manuallyInserted = true
		//TODO: what if the code has variable in it?

		val goodVars = ASTGetGoodVars(ast) ++ inputVars

		val vocabMaker = new BasicVocabMaker {
			override val returnType: Types = ast.nodeType

			override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode = children.length match{
				case 0 => ast.updateValues(new Contexts(contexts))
				case 1 => ASTVarsVisitor(ast, goodVars, children.head, new Contexts(contexts))
				case _ => throw new Exception("ScopeSpecification: unexpected number of children")

			}

			override val arity: Int = ASTVarsCounter(ast, goodVars)
			override val childTypes: List[Types] = ASTGetVarType(ast, goodVars, typesMap)
			override val nodeType: Class[_ <: ASTNode] = ast.getClass
			override val head: String = "scoopy"
		}
		new ScopeSpecification(List(), Tuple2(List(), List()), List(), List(vocabMaker), auxVariables = List(varName),scopeType = "assign")
	}

	def cond(scopeSpecification1: ScopeSpecification, scopeSpecification2: ScopeSpecification): ScopeSpecification ={
		val jointExamples = scopeSpecification1.scopeExamples ++ scopeSpecification2.scopeExamples;
		val partition = Tuple2(Range(0, scopeSpecification1.scopeExamples.size).toList,
			Range(scopeSpecification1.scopeExamples.size, jointExamples.size).toList);
		val requiredVocabMakers = scopeSpecification1.requiredVocabMakers ++ scopeSpecification2.requiredVocabMakers;
		val optionalVocabMakers = scopeSpecification1.optionalVocabMakers ++ scopeSpecification2.optionalVocabMakers;
		val auxVariables = scopeSpecification1.auxVariables ++ scopeSpecification2.auxVariables;
		new ScopeSpecification(jointExamples, partition, requiredVocabMakers, optionalVocabMakers, auxVariables = auxVariables, scopeType = "cond")
	}

	def concat(scopeSpecifications: List[ScopeSpecification]): ScopeSpecification ={
		val jointExamples = List();
		val partition = Tuple2(List(), List());
		val requiredVocabMakers = scopeSpecifications.flatMap(_.requiredVocabMakers);
		val optionalVocabMakers = scopeSpecifications.flatMap(_.optionalVocabMakers);
		val auxVariables = scopeSpecifications.flatMap(_.auxVariables);
		new ScopeSpecification(jointExamples, partition, requiredVocabMakers, optionalVocabMakers, auxVariables = auxVariables, scopeType = "concat")
	}

	def scope(scopeSpecification: ScopeSpecification, examples:List[Map[String, Any]], outputVarNames:List[String], synth:Boolean=true): ScopeSpecification ={
		val jointExamples = scopeSpecification.scopeExamples.filter(map=>outputVarNames.forall(name=>map.keys.exists(key=>key==name))) ++ examples;
		val partition = scopeSpecification.partition;
		val requiredVocabMakers = scopeSpecification.requiredVocabMakers;
		val optionalVocabMakers = scopeSpecification.optionalVocabMakers;
		val auxVariables = scopeSpecification.auxVariables.filter(!outputVarNames.contains(_));
		val inputVars:List[String] = examples.map(_.keys).flatten.filter(!outputVarNames.contains(_));
		val tmpScope = new ScopeSpecification(jointExamples, partition, List(), List(), outputVarNames, auxVariables, scopeType ="scopeAux");
		if(synth) {
			println("inner synth")
			val typesMap_ = examples.flatMap(_.keys)
				.toSet[String]
				.map(varName => varName -> Utils.getTypeOfAll(examples.map(ex => ex.get(varName)).filter(_.isDefined).map(_.get)))
				.filter(!_._2.equals(Types.Unknown))
				.toMap
			val typesMap = collection.mutable.Map(typesMap_.toSeq: _*)
			val synthTask = SynthesisTask.fromSpec(tmpScope)
			val solution = synthesizeAST(synthTask, 5);
			if (solution.isDefined) {
				val ast = solution.get
				val newReqMakers = ast match {
					case _:SemiCondNode =>{
						List()
					}
					case _=>{
						val goodVars = ASTGetGoodVars(ast) ++ inputVars
						val vocabMaker = new BasicVocabMaker {
							override val returnType: Types = ast.nodeType
							val vec = List.fill(required + 1)(false).updated(required, true)

							override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode = children.length match {
								case 0 => val x = ast.updateValues(new Contexts(contexts))
									x.updateChildren(x.children.toSeq, vec)
									x
								case 1 => val x = ASTVarsVisitor(ast, goodVars, children.head, new Contexts(contexts))
									x.updateChildren(x.children.toSeq, vec)
									x
								case _ => throw new Exception("ScopeSpecification: unexpected number of children")

							}

							override val arity: Int = ASTVarsCounter(ast, goodVars)
							override val childTypes: List[Types] = ASTGetVarType(ast, goodVars, typesMap)
							override val nodeType: Class[_ <: ASTNode] = ast.getClass
							override val head: String = "scoopy"
						}
						required = required + 1
						List(vocabMaker)
					}
				}

				val newRequiredVocabMakers = requiredVocabMakers ++ newReqMakers

				return new ScopeSpecification(jointExamples, partition, newRequiredVocabMakers, optionalVocabMakers, outputVarNames, auxVariables, scopeType = "scope with synth")
			}
			else{
				// synth failed but let say we fond a solution
				new ScopeSpecification(jointExamples, partition, requiredVocabMakers, optionalVocabMakers, outputVarNames, scopeType = "scope without synth")
			}
		}

		new ScopeSpecification(jointExamples, partition, requiredVocabMakers, optionalVocabMakers, outputVarNames, scopeType = "scope without synth")
	}

	def fromString(jsonString: String): ScopeSpecification ={
		val input = JsonParser.parse(jsonString).asInstanceOf[JObject].values
		val scopesTree: Map[String, Any] = input("scopeTree").asInstanceOf[Map[String, Any]]
		val scopeInfoString  = input("scopes").asInstanceOf[Map[String, Map[String, Any]]]
		val scopeInfo:Map[String, ParsedComment] = scopeInfoString map {case (k,v) => k -> ParsedComment.fromMap(v)}
		val rootId = scopesTree.keys.head
		val inputVars = scopeInfo(rootId).inputVarNames()
		val spec = this.fromTree(scopesTree, scopeInfo,inputVars, false)

		return spec
	}

	def fromTree(scopesTree: Map[String, Any], scopesInfo:Map[String, ParsedComment], inputVars:List[String], synth:Boolean=true): ScopeSpecification ={
		val roots = scopesTree.keys.toList
		var holes:List[String]=List()
		val rootsSpecs  = roots map(id => {
			val currentScope = scopesInfo(id)
			val sons = scopesTree(id).asInstanceOf[Map[String, Any]]
			val sonsIds = sons.values.toList.map(_.asInstanceOf[Map[String, Any]].keys.toList).flatten
			sonsIds.zipWithIndex.foreach(x => sonsIds.slice(0,x._2).foreach(y=>scopesInfo(y).getOutputVarNames().foreach(out=>scopesInfo(x._1).siblingsVars.appended(out))))

			val sonsSpecs: Map[String, ScopeSpecification] = sons map {
				case("NoBranch", sonTree) => "NoBranch" ->this.fromTree(sonTree.asInstanceOf[Map[String, Any]], scopesInfo,currentScope.inputVarNames(), !(sons("NoBranch").asInstanceOf[Map[String, Any]].size==1 && scopesInfo(id).getAssigns().length==0)) // no branch and no other assigns, i.e scope in scope
				case (sonBranch, sonTree) => sonBranch -> this.fromTree(sonTree.asInstanceOf[Map[String, Any]], scopesInfo, currentScope.inputVarNames())
			}
			val thenScope = sonsSpecs.get("T")
			val elseScope = sonsSpecs.get("F")
			val neitherScope = sonsSpecs.get("NoBranch")
			var condScope:Option[ScopeSpecification] = Option.empty;
			val assignsOutputs = currentScope.getAssigns().map(_.split("=")(0).replace("'","").trim)
			holes ++= assignsOutputs
			val assignmentsScope = if(currentScope.getAssigns().length!=0) {
				if(currentScope.getAssigns().length==1 ) {Option(ScopeSpecification.assign(currentScope.getAssigns().head, currentScope.getContext(),inputVars))}
				else{Option(ScopeSpecification.concat(currentScope.getAssigns().map(assign=>ScopeSpecification.assign(assign, currentScope.getContext(),inputVars++assignsOutputs))))}
			} else Option(new ScopeSpecification(List(), Tuple2(List(), List()), List(), List(),scopeType = "empty"))
			if (elseScope.isDefined || thenScope.isDefined){
				val oneBranch = if (thenScope.isDefined) thenScope.get else new ScopeSpecification(List(), Tuple2(List(), List()), List(), List(),scopeType = "empty")
				val secondBranch = if (elseScope.isDefined) elseScope.get else new ScopeSpecification(List(), Tuple2(List(), List()), List(), List(), scopeType = "empty")
				 condScope = Option(ScopeSpecification.cond(oneBranch, secondBranch))
			}
			var inside:ScopeSpecification =null
			// if more than one scope is defined we need to concat them
			if ((List(condScope, assignmentsScope, neitherScope).filter(_.isDefined).length > 1)
				&&((assignmentsScope.isDefined && assignmentsScope.get.scopeType=="assign") || !assignmentsScope.isDefined)) {
					inside = ScopeSpecification.concat(List(condScope, assignmentsScope, neitherScope).filter(_.isDefined).map(_.get))

			}else {
				 inside = List(condScope, assignmentsScope, neitherScope).filter(_.isDefined).head.get
			}
			ScopeSpecification.scope(inside, currentScope.getExamples(), currentScope.getOutputVarNames(), synth)

		})

		var resultScope:ScopeSpecification=null;
		if(rootsSpecs.length == 1)
			resultScope = rootsSpecs.head
		else
			resultScope = ScopeSpecification.concat(rootsSpecs)
		resultScope

	}

}
