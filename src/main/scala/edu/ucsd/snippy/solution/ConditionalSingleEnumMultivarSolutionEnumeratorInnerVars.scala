package edu.ucsd.snippy.solution

import edu.ucsd.snippy.ast.Types.{Types, typeof}
import edu.ucsd.snippy.ast._
import edu.ucsd.snippy.enumeration.{Contexts, Enumerator, InputsValuesManager, ProbEnumerator}
import edu.ucsd.snippy.predicates.MultilineMultivariablePredicate
import edu.ucsd.snippy.utils.Utils.{falseForIndices, filterByIndices, getBinaryPartitions, trueForIndices}
import edu.ucsd.snippy.utils._
import edu.ucsd.snippy.vocab.{RequiredVocabMaker, VocabFactory}

import scala.collection.convert.ImplicitConversions.`collection asJava`
import scala.collection.mutable

class ConditionalSingleEnumMultivarSolutionEnumeratorInnerVars(
			 predicate: MultilineMultivariablePredicate,
			 variables: List[(String, Types)],
			 literals: Iterable[String],
			 partitionFunction: List[Any] => List[(Set[Int], Set[Int])],
			 knownVarAssignments: Map[String, ASTNode],
			 requiredASTs: List[ASTNode]) extends SolutionEnumerator
{
	val partitions: List[(Set[Int], Set[Int])] = partitionFunction(predicate.graphStart.state.indices.toList)
	print("number of partitions: " + partitions.size+";")
	val conditionals: List[CondStore] = this.partitions.map(part => {
		val rs = new CondStore
		if (part._2.isEmpty) {
			rs.cond = Some(BoolLiteral(value = true, part._1.size))
		}
		rs
	})
	val graph: MultiValueNode = MultiValueNode.convert(predicate.graphStart, partitions, variables, literals,knownVarsAssignments= knownVarAssignments, requiredASTs=requiredASTs)
	var solution: Option[Assignment] = None

	// Setup the conditional enum listener
	graph.onStep = {
		case program: BoolNode if program.exampleValues.forall(_.isDefined) =>
			val values: List[Boolean] = program.exampleValues.map(_.get)
			for ((store, index) <- this.conditionals.zipWithIndex.filter(_._1.cond.isEmpty)) {
				val (thenIndices, elseIndices) = this.partitions(index)
				if (trueForIndices(values, thenIndices) && falseForIndices(values, elseIndices)) {
					if (program.usesVariables) {
						store.cond = Some(program)
					} else {
						graph.enum.oeManager.remove(program)
					}
				} else if (trueForIndices(values, elseIndices) && falseForIndices(values, thenIndices)) {
					if (program.usesVariables) {
						store.cond = Some(NegateBool(program))
					} else {
						graph.enum.oeManager.remove(program)
					}
				}
			}
		case _ => ()
	}

	def this(predicate: MultilineMultivariablePredicate,
	         variables: List[(String, Types)],
	         literals: Iterable[String]) {
		this(predicate, variables, literals, (indices:List[Any]) => getBinaryPartitions(indices),Map(), Nil)
	}
	def containsAllReqs(Assignments: List[Assignment], reqLen: Int): Boolean ={
		return true
		val vectors = Assignments.flatMap(_.programs).map(_.requireBits)
		val reqVec:List[Boolean] = vectors.fold(List())((v1:List[Boolean], v2:List[Boolean])=> v1.zipAll(v2, false, false).map(x=>x._1 || x._2))
		//println(reqVec.forall(p => p) && reqVec.length == reqLen)
		reqVec.forall(p => p) && reqVec.length == reqLen
	}
	def step(): Unit = {
		if (this.graph.step()) { // if graph has changed
			this.graph.computeShortestPaths()
			if (this.solution.isEmpty) {
				val paths = for ((condStore, index) <- this.conditionals.zipWithIndex; if condStore.cond.isDefined) yield {
					val weight = if (graph.distancesToEnd(index).thenPath._1 == Int.MaxValue || graph.distancesToEnd(index).elsePath._1 == Int.MaxValue) Int.MaxValue
					else if (index == 0) 0
					else graph.distancesToEnd(index).thenPath._1 + graph.distancesToEnd(index).elsePath._1 + condStore.cond.get.terms
					(condStore,weight,index)
				}
				val (condStore, _, index) = paths.minBy(_._2)

					graph.traverse(index) match {
						case Some((thenAssignments, elseAssignments)) =>
							if(containsAllReqs(thenAssignments ++ elseAssignments, requiredASTs.length)) {
								this.solution = Some(ConditionalAssignment(
									condStore.cond.get,
									MultilineMultivariableAssignment(thenAssignments),
									MultilineMultivariableAssignment(elseAssignments)))
							}
						case _ => ()
					}
			}

		}
	}

	override def programsSeen: Int = this.graph.programsSeen
}


case class MultiValueEdge(
				   parent: MultiValueNode,
				   child: MultiValueNode,
				   variables: Map[Variable, List[CondProgStore]]) {
	override def toString: String = s"Edge: {${variables.map((v => v._1.name)).toString()} from ${parent.edges.length}"
}




object MultiValueNode {
	def createProgStore(
		prevEnvs: (List[Map[String, Any]], List[Map[String, Any]]),
		envs: (List[Map[String, Any]], List[Map[String, Any]]),
		variable: Variable,
		partition: (Set[Int], Set[Int]),
		knownAssignments:Map[String, ASTNode]): CondProgStore = {
		val fromThenValues = envs._1.map(_(variable.name))
		val fromElseValues = envs._2.map(_(variable.name))
		val rs = CondProgStore(
			new ProgStore(partition._1, filterByIndices(fromThenValues, partition._1)),
			new ProgStore(partition._2, filterByIndices(fromElseValues, partition._2)))

		val knownVars = knownAssignments.map({case (v,node)=>(v,node.nodeType)})
		if (knownVars.keys.contains(variable.name)) {
			val defaultNode = VariableNode.nodeFromType(variable.name, knownVars(variable.name),List())
			rs.thenCase.program = if(knownAssignments.contains(variable.name)) Some(knownAssignments(variable.name)) else defaultNode
			rs.elseCase.program = defaultNode
			return rs
		}

		// If the else case is empty, we can set it to any program, and it will be removed in post-
		// processing
		if (partition._2.isEmpty) {
			rs.elseCase.program = Some(BoolLiteral(value = true, partition._1.size))
		}

		// If the variable doesn't change between envs, assign it to itself so we can trivially
		// remove the assignment in post.

		val prevValuesThen = prevEnvs._1.map(_.get(variable.name))
		val thenValues = filterByIndices(prevValuesThen, partition._1)
		if (thenValues.forall(_.isDefined) &&
			thenValues.map(_.get) == rs.thenCase.values) {
			rs.thenCase.program = VariableNode.nodeFromType(variable.name,variable.typ,envs._1)
		}

		val prevValuesElse = prevEnvs._2.map(_.get(variable.name))
		val elseValues = filterByIndices(prevValuesElse, partition._2)
		if (elseValues.forall(_.isDefined) && elseValues.map(_.get) == rs.elseCase.values) {
			rs.elseCase.program = VariableNode.nodeFromType(variable.name,variable.typ,envs._2)
		}

		// If we have more than one example, with all values constants, we should trivially assign
		// to a constant
		if (rs.thenCase.program.isEmpty) {
			rs.thenCase.program = Utils.synthesizeLiteralOption(variable.typ, rs.thenCase.values)
		}

		if (rs.elseCase.program.isEmpty) {
			rs.elseCase.program = Utils.synthesizeLiteralOption(variable.typ, rs.elseCase.values)
		}

		rs
	}

	def convert(
	  parent: edu.ucsd.snippy.predicates.Node,
	  partitionIndices: List[(Set[Int], Set[Int])],
	  variables: List[(String, Types)],
	  literals: Iterable[String],
	  seen: mutable.Map[edu.ucsd.snippy.predicates.Node, MultiValueNode] = mutable.Map.empty,
	  knownVarsAssignments: Map[String, ASTNode],
	  requiredASTs: List[ASTNode] = Nil): MultiValueNode={

		val n = convertAux(parent, partitionIndices, variables, literals, seen, knownVarsAssignments, requiredASTs)
		replaceNones(n, knownVarsAssignments)
		n
	}

	def replaceNones(n:MultiValueNode, knownVarsAssignments: Map[String, ASTNode]): Unit = {
		var l = n::Nil
		while (l.nonEmpty){
			val node = l.head
			l = l.tail ::: node.edges.map(_.child)

			node.edges.foreach(edge => if (edge.variables.size == 1 && edge.child.edges.nonEmpty) {
				val oldVars = edge.child.state._1.flatten.filter(x => x._2 != None).map(x => x._1).toSet // _1 symmetric to _2
				for (v <- knownVarsAssignments.keys) {
					if (edge.variables.map(_._1.name).contains(v)) {
						val thenProgram = knownVarsAssignments(v).updateValues(new Contexts(node.state._1))
						val thenVals =thenProgram.exampleValues.map(x => if (x.nonEmpty) x.get else x)
						val elseProgram = VariableNode.nodeFromType(v, knownVarsAssignments(v).nodeType, node.state._2).get.updateValues(new Contexts(node.state._2))
						val elseVals = elseProgram.exampleValues.map(x => if (x.nonEmpty) x.get else x)
						val newTup = (edge.child.state._1.zipWithIndex.map(x => x._1.updated(v, thenVals(x._2))), edge.child.state._2.zipWithIndex.map(x => x._1.updated(v, elseVals(x._2))))
						edge.child.state = newTup

						for (i <- node.distancesToEnd.indices) {
							edge.variables.foreach { case (_, stores) =>
								stores(i).thenCase.program = Some(thenProgram)
								stores(i).elseCase.program = Some(elseProgram)
								if (filterByIndices(thenVals,stores(i).thenCase.indices).contains(None)) {
									node.distancesToEnd.update(i, node.distancesToEnd(i).copy(thenPath = (Int.MaxValue, Some(edge))))
								}
								if (filterByIndices(elseVals, stores(i).elseCase.indices).contains(None)) {
									node.distancesToEnd.update(i, node.distancesToEnd(i).copy(elsePath = (Int.MaxValue, Some(edge))))
								}
							}
						}
					}
				}
				val useableVars = knownVarsAssignments.keys.filter(x => !oldVars.contains(x))
				val newVocab = edge.child.enum.vocab.addVars(edge.child.state._1.head.filter(x => useableVars.contains(x._1) && x._2 != None).map(x => (x._1, typeof(x._2))).toList)
				edge.child.enum = edge.child.enum.copy(newVocab, edge.child.enum.oeManager, edge.child.state._1)



			})
		}
	}
	def convertAux (
		parent: edu.ucsd.snippy.predicates.Node,
		partitionIndices: List[(Set[Int], Set[Int])],
		variables: List[(String, Types)],
		literals: Iterable[String],
		seen: mutable.Map[edu.ucsd.snippy.predicates.Node,MultiValueNode] = mutable.Map.empty,
		knownVarsAssignments: Map[String, ASTNode],
		requiredASTs:List[ASTNode] = Nil): MultiValueNode = {
		if (seen.contains(parent)) return seen(parent)
		val assignedVars = parent.nodesVars;
		val requiredVocabMakers = requiredASTs.zipWithIndex.map((x)=> new RequiredVocabMaker(x._1, List("count", "rs"), x._2, new Contexts(parent.state))) // makes the contexts good in singleVar. in multi var it will be done inside the node

		val enumerator = new ProbEnumerator(
			VocabFactory(variables, literals, requiredVocabMakers),
			new InputsValuesManager,
			parent.state,
			false,
			0,
			mutable.Map[Int, mutable.ArrayBuffer[ASTNode]](),
			mutable.Map[Int, mutable.ArrayBuffer[ASTNode]](),
			100)
		val n: MultiValueNode = MultiValueNode(enumerator, (parent.state, parent.state), Nil, partitionIndices, parent.nodesVars.toSet)
		val edges = parent.edges
			.map{e =>
				e -> convertAux(e.child, partitionIndices, variables, literals, seen,knownVarsAssignments, requiredASTs)
			}
			.map {
				case (edu.ucsd.snippy.predicates.SingleEdge(_, variable, outputType, _, _), child) =>
					val newVariable = Variable(variable, outputType)
					val stores = partitionIndices.map(indices => createProgStore((parent.state, parent.state), (child.state._1, child.state._1), newVariable, indices, knownVarsAssignments))
					MultiValueEdge(n, child, List(newVariable -> stores).toMap)
				case (edu.ucsd.snippy.predicates.MultiEdge(_, outputTypes, _, _), child) =>
					val variables = outputTypes.map {
						case (variable, outputType) =>
							val newVariable = Variable(variable, outputType)
							val stores = partitionIndices.map(idxs => createProgStore((parent.state, parent.state), (child.state._1, child.state._1), newVariable, idxs, knownVarsAssignments))
							newVariable -> stores
					}
					MultiValueEdge(n, child, variables)
			}

		n.edges = edges


		if (n.edges.isEmpty) {
			knownVarsAssignments.keys.foreach(fixedVarName => {
				n.state._1.foreach(env => env.updated(fixedVarName, Some(None)));
				n.state._2.foreach(env => env.updated(fixedVarName, Some(None)))
			});
			for (i <- n.distancesToEnd.indices) n.distancesToEnd.update(i,MultiValueDistancePaths((0,None),(0,None)))
		}
		seen += parent -> n
		n
	}
}



case class MultiValueDistancePaths(thenPath: (Int,Option[MultiValueEdge]),elsePath: (Int,Option[MultiValueEdge]))
case class MultiValueNode(
	var enum: Enumerator,
	var state: (List[Map[String, Any]],List[Map[String, Any]]), // first element is state after then, second is state after else
	var edges: List[MultiValueEdge],
	partitionIndices: List[(Set[Int], Set[Int])],
	val nodeVars:Set[String],
	var onStep: ASTNode => Unit = _ => ()) {

	var seen = false
	private def reset_seen(): Unit = {
		seen = false
		for (edge <- edges)
			edge.child.reset_seen()
	}
	def step(): Boolean = {
		reset_seen()
		do_step()
	}
	var done = false
	private def do_step(): Boolean = {
		if (seen)
			false
		else {
			seen = true
			var graphChanged = false

		if (!done &&  this.enum.hasNext) {
			val program = this.enum.next()


//			print(program.code+" .... "+this.toString+" .... "+program.exampleValues + "\n")

			this.onStep(program)
			val thenProgram = program // the enumerator has the then state already
			val elseProgram = program.updateValues(new Contexts(this.state._2))
			for (edge <- this.edges) {
				for ((variable, stores) <- edge.variables) {
					if (variable.typ == program.nodeType) {
						for (store <- stores) {
							if (store.thenCase.program.isEmpty) {
								val programValues = filterByIndices(thenProgram.exampleValues, store.thenCase.indices)
								if (programValues.zip(store.thenCase.values).forall(Utils.programConnects)) {
									if (thenProgram.usesVariables) {
										store.thenCase.program = Some(thenProgram)
										graphChanged = true
									} else {
										this.enum.oeManager.remove(thenProgram)
									}
								}
							}

							if (store.elseCase.program.isEmpty) {
								val programValues = filterByIndices(elseProgram.exampleValues, store.elseCase.indices)
								if (programValues.zip(store.elseCase.values).forall(Utils.programConnects)) {
									if (elseProgram.usesVariables) {
										store.elseCase.program = Some(elseProgram)
										graphChanged = true
									} else {
										this.enum.oeManager.remove(elseProgram)
									}
								}
							}
						}
					}
				}
				graphChanged |= edge.child.do_step()
			}
			if (edges.forall(edge => edge.variables.forall(v => v._2.forall(store => store.isComplete)))) {
				done = true
			}
		}
		else if (done ) { //don't run this enumerator but still run children.
			for (edge <- this.edges) {
				graphChanged |= edge.child.do_step()
			}
		}

		graphChanged
		}
	}

	def blocked():Boolean={ // if some variable is on None, the father Node is not yet finished
		state._1.exists(env => env.exists(tup => tup._2 == None)) || state._2.exists(env => env.exists(tup => tup._2 == None))
	}
	val distancesToEnd: Array[MultiValueDistancePaths] = Array.fill(this.partitionIndices.length)(MultiValueDistancePaths((Int.MaxValue,None),(Int.MaxValue,None)))
	def computeShortestPaths(): Unit = {
		reset_seen()
		do_computeShortest()
	}
	def do_computeShortest(): Unit = {
		if (seen) return
		for (edge <- this.edges) {
			edge.child.do_computeShortest()
			for (i <- distancesToEnd.indices) {
				//then case
				if (edge.variables.map(_._2(i)).forall(store => store.thenCase.program.isDefined && !store.thenCase.program.get.exampleValues.contains(None)) && edge.child.distancesToEnd(i).thenPath._1 < Int.MaxValue) {
					val distanceOnThenEdge = edge.child.distancesToEnd(i).thenPath._1 + edge.variables.map { case (_, stores) =>
						if (stores(i).thenCase.program.get.exampleValues.contains(None))  Int.MaxValue else  stores(i).thenCase.program.get.terms - stores(i).thenCase.program.get.requireBits.count(_== true) * 4 // 4 for no reason
					}.sum
					if (distanceOnThenEdge < distancesToEnd(i).thenPath._1 || (distanceOnThenEdge == distancesToEnd(i).thenPath._1 && distancesToEnd(i).thenPath._2.exists(e => edge.variables.size < e.variables.size))) {
						distancesToEnd.update(i,distancesToEnd(i).copy(thenPath = (distanceOnThenEdge,Some(edge))))
					}
				}
				//else case
				if (edge.variables.map(_._2(i)).forall(store => store.elseCase.program.isDefined && !store.elseCase.program.get.exampleValues.contains(None)) && edge.child.distancesToEnd(i).elsePath._1 < Int.MaxValue) {
					val distanceOnElseEdge = edge.child.distancesToEnd(i).elsePath._1 + edge.variables.map{ case (_,stores) =>
						if (stores(i).elseCase.program.get.exampleValues.contains(None))  Int.MaxValue else stores(i).elseCase.program.get.terms - stores(i).elseCase.program.get.requireBits.count(_== true) * 4 // 4 for no reason
					}.sum

					if (distanceOnElseEdge < distancesToEnd(i).elsePath._1 || (distanceOnElseEdge == distancesToEnd(i).elsePath._1 && distancesToEnd(i).elsePath._2.exists(e => edge.variables.size < e.variables.size))) {
						distancesToEnd.update(i,distancesToEnd(i).copy(elsePath = (distanceOnElseEdge,Some(edge))))
					}
				}
			}
		}
		seen = true
	}

	def traverse(partitionIndex: Int): Option[(List[Assignment], List[Assignment])] = {
		if (this.distancesToEnd(partitionIndex).thenPath._2.isEmpty || this.distancesToEnd(partitionIndex).elsePath._2.isEmpty) {
			None
		} else {
			val thenAssigns = traverse(partitionIndex,thenBranch = true)
			val elseAssigns = traverse(partitionIndex,thenBranch = false)
			for (t <- thenAssigns; e <- elseAssigns) yield (t,e)
		}
	}
	def traverse(partitionIndex: Int, thenBranch: Boolean): Option[List[Assignment]] = {
		if (edges.isEmpty) {
			Some(Nil)
		} else {
			val path = if (thenBranch) this.distancesToEnd(partitionIndex).thenPath else this.distancesToEnd(partitionIndex).elsePath
			path._2.foreach { edge =>
					//if (edge.variables.map(_._2(partitionIndex)).forall(_.isComplete))
					edge.child.traverse(partitionIndex, thenBranch) match {
						case None => ()
						case Some(assign) =>
							val newAssign = if (edge.variables.size == 1) {
								val (variable, store) = edge.variables.head
								if (thenBranch)
									SingleAssignment(variable.name, store(partitionIndex).thenCase.program.get)
								else
									SingleAssignment(variable.name, store(partitionIndex).elseCase.program.get)
							} else {
								val ordered = edge.variables.map(tup => tup._1 -> tup._2(partitionIndex)).toList
								val names = ordered.map(_._1.name)
								if (thenBranch)
									BasicMultivariableAssignment(names, ordered.map(_._2.thenCase.program.get))
								else
									BasicMultivariableAssignment(names, ordered.map(_._2.elseCase.program.get))
							}
							return Some(newAssign :: assign)
					}
			}
			None
		}
	}

	def programsSeen: Int = {
		//This is not a map on purpose. :(
		val seen = mutable.ArrayBuffer[(MultiValueNode,Int)]()
		get_progCount(seen)
		seen.map(_._2).sum
	}
	private def get_progCount(seen: mutable.ArrayBuffer[(MultiValueNode, Int)]): Unit = {
		if (!seen.exists(_._1 == this)) {
			seen += (this -> this.enum.programsSeen)

			for (edge <- edges) {
				edge.child.get_progCount(seen)
			}
		}
	}

	def do_print(nodeLabel: MultiValueNode => String, edgeLabel: MultiValueEdge => String): Unit = {
		if (seen) return
		println(s"${System.identityHashCode(this).toString} [label=${'"' + nodeLabel(this) + '"'}]")
		for (edge <- edges) {
			edge.child.do_print(nodeLabel,edgeLabel)
			println(s"${System.identityHashCode(this)} -> ${System.identityHashCode(edge.child)} [label=${'"' + edgeLabel(edge) + '"'}]")
		}
		seen = true
	}

	def printGraph(nodeLabel: MultiValueNode => String, edgeLabel: MultiValueEdge => String): Unit = {
		reset_seen()
		println("digraph G {")
		do_print(nodeLabel,edgeLabel)
		println("}")
	}

	override def toString: String = s"Node - {$nodeVars}"
}
