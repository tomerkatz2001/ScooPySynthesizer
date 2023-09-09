package edu.ucsd.snippy.scoopy

import edu.ucsd.snippy.Snippy.synthesize
import edu.ucsd.snippy.SynthesisTask
import edu.ucsd.snippy.ast.Types.Types
import edu.ucsd.snippy.ast._
import edu.ucsd.snippy.enumeration.Contexts
import edu.ucsd.snippy.predicates.Predicate
import edu.ucsd.snippy.utils.Utils.getBinaryPartitions
import edu.ucsd.snippy.utils._
import edu.ucsd.snippy.vocab.RequiredVocabMaker
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.JsonParser

import scala.collection.mutable




class ScopeSpecification(private val scopeExamples: List[Map[String, Any]],
						 //val contexts: Contexts,
						 val partition: (List[Int], List[Int]),
						 val required: List[(List[ASTNode], Map[String, ASTNode])=>SynthesisTask],
						 val outputVarNames: Set[String],
						 val scopeType:String,
						 val appliedOperators: List[String]){


	def getPartitionFunc(): List[Any] => List[(Set[Int], Set[Int])] = {
		val pastPartition = (this.partition._1.toSet, this.partition._2.toSet)
		(indices: List[Any]) => {
			getBinaryPartitions(indices).filter(part =>
				(pastPartition._1.subsetOf(part._1) && pastPartition._2.subsetOf(part._1)) || // both in the first
					(pastPartition._1.subsetOf(part._2) && pastPartition._2.subsetOf(part._2)) || //both in the second
					(pastPartition._1.subsetOf(part._1) && pastPartition._2.subsetOf(part._2)) || // first in the first, second in the second
					(pastPartition._1.subsetOf(part._2) && pastPartition._2.subsetOf(part._1))) // first in the second, second in the first
		}
	}

	def getPrevEnvsAndEnvs()	 ={
		var time:BigInt = 0;
		var prevEnvs:Map[Int, Map[String, Any]] = Map();
		def splitEnv(env:Map[String, Any]) = {
			val previousVars = env.keys.filter(k => k.contains("_in")).toList
			var preEnv = Map[String, Any]()
			if(previousVars.nonEmpty){
				preEnv = env.filter(v => !(previousVars.map(k => k.replace("_in", ""))).contains(v._1)).
					map(v => (v._1.replace("_in", ""), v._2))
			}
			val nextEnv = env.filter(v => !previousVars.contains(v._1))
			time = time + 2
			if(previousVars.nonEmpty) {
				prevEnvs = prevEnvs ++ Map(time.toInt -> preEnv);
			}
			List(preEnv ++ Map("time" -> (time - 1)), nextEnv ++ Map("time" -> time))
		}
		val envs = this.scopeExamples.flatMap(splitEnv).filter(env => env("time").asInstanceOf[BigInt] % 2 == 0) // only the real examples
		(prevEnvs, envs)
	}
	def covertToASTList(assignment :Assignment, contexts: Contexts): List[ASTNode] = {
			assignment match {
					case SingleAssignment(_, value) => List(value)
					case BasicMultivariableAssignment(_, values) => values
					case MultilineMultivariableAssignment(assignments) => assignments.flatMap(ass=>covertToASTList(ass, contexts))
					case ConditionalAssignment(condition, ifTrue, ifFalse) => {
						val ifTrueList = covertToASTList(ifTrue, contexts)
						val ifFalseList = covertToASTList(ifFalse, contexts)
						assert(ifTrueList.length == 1)
						if (condition.exampleValues.forall(_.get)){
							ifTrueList
						}
						else if(condition.exampleValues.forall(!_.get)){
							ifFalseList
						}
						else {
							ifTrueList.head match {
								case _: IntNode =>IntCondNode(condition, ifTrueList.head.asInstanceOf[IntNode], ifFalseList.head.asInstanceOf[IntNode], contexts) :: Nil;
								case _: StringNode => StringCondNode(condition, ifTrueList.head.asInstanceOf[StringNode], ifFalseList.head.asInstanceOf[StringNode], contexts) :: Nil;
								case _: DoubleNode => DoubleCondNode(condition,ifTrueList.head.asInstanceOf[DoubleNode], ifFalseList.head.asInstanceOf[DoubleNode], contexts) :: Nil;
								case _: IntListNode => IntListCondNode(condition, ifTrueList.head.asInstanceOf[IntListNode], ifFalseList.head.asInstanceOf[IntListNode], contexts) :: Nil;
								case _ => println("didnt find type");ifTrueList.head::Nil// error
							}

						}
					}
					case _ => throw new Exception("Unknown assignment type")
				}

	};

	def extractAstOf(v: String, assignment: Assignment, contexts: Contexts): ASTNode = {
		val vars = assignment.getAssignedVars().toList
		val index = vars.indexOf(v)
		assert(index >= 0)
		covertToASTList(assignment, contexts)(index)
	}
	def solve(timeout:Int = 10) ={
		assert(this.scopeType == "scope")
		val topLevelVarNames = this.outputVarNames
		var requiredVocabMakers: List[RequiredVocabMaker] = List()
		var innerSpecifications: mutable.Map[String, Predicate] = mutable.Map();
		var requiredAssignments: mutable.Map[String, ASTNode] = mutable.Map();
		var seenASTs: List[ASTNode] = List()
		var sol: (Option[String], Int, Int, Option[Assignment]) = (None, 0, 0, None)
		for ((req, i) <- this.required.zipWithIndex.reverse) {
			val synthesisTask = req.apply(List(), Map())
			sol = synthesize(synthesisTask, timeout);
			assert(synthesisTask.outputVariables.forall(v=> !requiredAssignments.contains(v))) // we dont require top level veriables from inner scopes
			requiredAssignments ++= synthesisTask.outputVariables.diff(topLevelVarNames).map(v => v -> extractAstOf(v, sol._4.get, new Contexts(synthesisTask.contexts))).toList
			innerSpecifications ++= synthesisTask.outputVariables.diff(topLevelVarNames).map(v => v -> synthesisTask.predicate).toList
			//println("found solution: " + sol._4.get.code(true))
			val solutionAssignment: Option[Assignment] = sol._4
			//requiredVarNames = solutionAssignment.get.getAssignedVars() + requiredVarNames;
			 covertToASTList(solutionAssignment.get, new Contexts(synthesisTask.contexts)).filter(ast => !seenASTs.contains(ast)).zipWithIndex.map { case (ast, i) => {
				seenASTs = ast :: seenASTs
				new RequiredVocabMaker(ast, topLevelVarNames.toList, requiredVocabMakers.length + i, new Contexts(synthesisTask.contexts))
			}
			};

		}
		val innerVars = innerSpecifications.keys.toList
		val newVars = (this.outputVarNames.toList ++ innerVars).toSet
		val extendedExamples = this.scopeExamples.map(env => env ++ innerVars.map(v => v -> "'__BOT__'").toMap)
		val finalTask = SynthesisTask.fromSpec(new ScopeSpecification(extendedExamples, this.partition, this.required, newVars, this.scopeType, this.appliedOperators), seenASTs, requiredAssignments.toMap)
		sol = synthesize(finalTask, timeout);
		if (sol._4.nonEmpty) {
			sol._4.get.addExamples(innerSpecifications.map(tup => (tup._1, tup._2.envs)).toMap)
		}
		sol //._4.get.code(disablePostProcess=true);
	}
}


object ScopeSpecification {
	//var required = 0

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

	def assign(code: String): ScopeSpecification = {
		val varNames = code.split("=")(0).replace("\'","").split(",").map(_.trim).toSet
		new ScopeSpecification(List(), Tuple2(List(), List()), List(),varNames,scopeType = "assign", List("assign"))
	}

	def cond(scopeSpecification1: ScopeSpecification, scopeSpecification2: ScopeSpecification): ScopeSpecification ={
		val jointExamples = scopeSpecification1.scopeExamples ++ scopeSpecification2.scopeExamples;
//		val jointContexts = new Contexts(scopeSpecification1.contexts.contexts ++ scopeSpecification2.contexts.contexts)
		val partition = Tuple2(Range(0, scopeSpecification1.scopeExamples.size).toList,
			Range(scopeSpecification1.scopeExamples.size, jointExamples.size).toList);
		val required = scopeSpecification1.required ++ scopeSpecification2.required;
		val varNames = scopeSpecification1.outputVarNames ++ scopeSpecification2.outputVarNames;
		var appliedOperators = "cond" :: (scopeSpecification1.appliedOperators ++ scopeSpecification2.appliedOperators)
		new ScopeSpecification(jointExamples, partition, required, varNames, scopeType = "cond", appliedOperators)
	}

	def concat(scopeSpecifications: List[ScopeSpecification]): ScopeSpecification ={
		val jointExamples = List();
//		val jointContexts:Contexts = new Contexts(List())// no examples so no need of contexts
		val partition = Tuple2(List(), List());
		val fixed_scopeSpecifications = scopeSpecifications.map((spec)=>if (spec.scopeExamples.size > 0) toScopeable(spec) else spec);
		val jointOutputVarNames = fixed_scopeSpecifications.map(_.outputVarNames).flatten.toSet;
		val required = fixed_scopeSpecifications.flatMap(_.required);
		val appliedOperators = "concat" :: (scopeSpecifications.map(_.appliedOperators).reduce((a,b)=> a++b));
		new ScopeSpecification(jointExamples, partition, required, jointOutputVarNames, scopeType = "concat", appliedOperators)
	}

	def scope(specification: ScopeSpecification, examples:List[Map[String, Any]], outputVarNames: Set[String]):ScopeSpecification ={
		val jointExamples = specification.scopeExamples ++ examples; //TODO: check if the examples are disjoint
//		val contexts = getContextsFromExamples(jointExamples, outputVarNames);
		//val jointOutputVarNames = (specification.outputVarNames ++ outputVarNames).filter(jointExamples.head.contains);
		val appliedOperators = "scope" :: specification.appliedOperators;
		new ScopeSpecification(jointExamples, specification.partition, specification.required, outputVarNames, scopeType = "scope", appliedOperators)
	}

	def toScopeable(specification: ScopeSpecification): ScopeSpecification ={
		val required = (requiredASTs:List[ASTNode], requiredAssignments:Map[String, ASTNode])=>SynthesisTask.fromSpec(specification, requiredASTs, requiredAssignments);
		val appliedOperators = "scopeable" :: specification.appliedOperators;
		new ScopeSpecification(List(), (List(), List()), required::specification.required, specification.outputVarNames, scopeType = "scopeable", appliedOperators)
	}

	class ScopeTree(val scopeInfo: ParsedComment, val falseSons:List[ScopeTree], val trueSons:List[ScopeTree], val nonBranchSons:List[ScopeTree] ){
		val isLeaf = falseSons.isEmpty && trueSons.isEmpty && nonBranchSons.isEmpty;
	}
	object ScopeTree{

		def getSons(bodyJson: Map[String, Any], scopeExamplesMap:Map[String, ParsedComment]):(List[ScopeTree],List[ScopeTree],List[ScopeTree])={
			val falseSon = apply(bodyJson.get("F").asInstanceOf[Option[Map[String, Any]]], scopeExamplesMap);
			val trueSon = apply(bodyJson.get("T").asInstanceOf[Option[Map[String, Any]]], scopeExamplesMap);
			val nonBranchSon = apply(bodyJson.get("NB").asInstanceOf[Option[Map[String, Any]]], scopeExamplesMap);
			(falseSon,trueSon,nonBranchSon);
		}
		def apply(treeJson: Option[Map[String, Any]], scopeExamplesMap:Map[String, ParsedComment]):List[ScopeTree]={
			if (treeJson.isEmpty || treeJson.get.isEmpty) {
				return List();
			}
			treeJson.get.map {
				case ("-1", _) => new ScopeTree(new ParsedComment("-1", List(),List("tmp"), List("tmp = something")), List(), List(), List())
				case (num:String, json) => {
					val (falseSon,trueSon,nonBranchSon) = getSons(json.asInstanceOf[Map[String, Any]], scopeExamplesMap);
					new ScopeTree(scopeExamplesMap(num),falseSon,trueSon,nonBranchSon);
				}
			}.toList
		}
	}
	def fromString(jsonString: String): ScopeSpecification ={
		val input = JsonParser.parse(jsonString).asInstanceOf[JObject].values
		val scopesTree: Map[String, Any] = input("scopesTree").asInstanceOf[Map[String, Any]]
		val scopeInfoString  = input("scopes").asInstanceOf[Map[String, Map[String, Any]]]
		val scopeInfo:Map[String, ParsedComment] = scopeInfoString map {case (k,v) => k -> ParsedComment(v)}
		val realTree = ScopeTree(Some(scopesTree), scopeInfo);


		assert(realTree.length == 1)
		val spec = this.fromTree(realTree(0))

		spec
	}

	def fromTree(scopesTree: ScopeTree): ScopeSpecification = {
		var innerScope: Option[ScopeSpecification] = None;
		if(scopesTree.isLeaf){
			val assignSpec = ScopeSpecification.assign(scopesTree.scopeInfo.assignments.head)
			innerScope = Some(assignSpec);
		}
		val trueSpecs = scopesTree.trueSons.map(fromTree);
		val falseSpecs = scopesTree.falseSons.map(fromTree);
		val nonBranchSpecs = scopesTree.nonBranchSons.map(fromTree);
		if(trueSpecs.nonEmpty && falseSpecs.nonEmpty && nonBranchSpecs.isEmpty){ // if - else
			var innerTrueSpec = trueSpecs(0);
			var innerFalseSpec = falseSpecs(0);
			if (trueSpecs.length > 1) {
				//TODO: make all scope spec to scpoabel before concating
				innerTrueSpec = ScopeSpecification.concat(trueSpecs);
			}
			if(falseSpecs.length > 1){
				innerFalseSpec = ScopeSpecification.concat(falseSpecs);
			}
			innerScope = Some(ScopeSpecification.cond(innerTrueSpec, innerFalseSpec))
		}
		if(trueSpecs.nonEmpty && falseSpecs.isEmpty && nonBranchSpecs.isEmpty) { // only if
			var innerTrueSpec = trueSpecs(0);
			if (trueSpecs.length > 1) {
				//TODO: make all scope spec to scpoabel before concating
				innerTrueSpec = ScopeSpecification.concat(trueSpecs);
			}
			val variables = innerTrueSpec.outputVarNames.mkString(",")
			innerScope = Some(ScopeSpecification.cond(innerTrueSpec, ScopeSpecification.assign(s"${variables} = dont care")))
		}
		if (trueSpecs.isEmpty && falseSpecs.isEmpty && nonBranchSpecs.nonEmpty) { // normal concat
			var innerNonBranchSpec = nonBranchSpecs(0);
			if (nonBranchSpecs.length > 1) {
				innerNonBranchSpec = ScopeSpecification.concat(nonBranchSpecs);
			}
			innerScope = Some(innerNonBranchSpec)
		}
		if(trueSpecs.nonEmpty && falseSpecs.isEmpty && nonBranchSpecs.nonEmpty) { // concat of if and something else
			var innerTrueSpec = trueSpecs(0);
			var innerNonBranchSpec = nonBranchSpecs(0);
			if (trueSpecs.length > 1) {
				innerTrueSpec = ScopeSpecification.concat(trueSpecs);
			}
			if (nonBranchSpecs.length > 1) {
				innerNonBranchSpec = ScopeSpecification.concat(nonBranchSpecs);
			}
			innerScope = Some(ScopeSpecification.concat(List(innerTrueSpec, innerNonBranchSpec)))
		}
		if (trueSpecs.nonEmpty && falseSpecs.nonEmpty && nonBranchSpecs.nonEmpty){ // cond and cat
			var innerTrueSpec = trueSpecs(0);
			var innerFalseSpec = falseSpecs(0);
			if (trueSpecs.length > 1) {
				innerTrueSpec = ScopeSpecification.concat(trueSpecs);
			}
			if (falseSpecs.length > 1) {
				innerFalseSpec = ScopeSpecification.concat(falseSpecs);
			}
			val condSpec = ScopeSpecification.cond(innerTrueSpec, innerFalseSpec)
			var innerNonBranchSpec = nonBranchSpecs(0);
			if (nonBranchSpecs.length > 1) {
				innerNonBranchSpec = ScopeSpecification.concat(nonBranchSpecs);
			}
			innerScope = Some(ScopeSpecification.concat(List(condSpec, innerNonBranchSpec)))
		}
		if(scopesTree.scopeInfo.commentId == "-1"){ // this is assign without scope
			return innerScope.get;
		}
		ScopeSpecification.scope(innerScope.get, scopesTree.scopeInfo.rawCommentExamples,scopesTree.scopeInfo.outputVarNames.toSet)
	}

}
