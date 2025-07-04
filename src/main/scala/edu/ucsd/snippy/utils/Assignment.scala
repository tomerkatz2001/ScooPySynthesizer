package edu.ucsd.snippy.utils
import edu.ucsd.snippy.PostProcessor
import edu.ucsd.snippy.ast.{ASTNode, BoolNode, NegateBool, VariableNode}

sealed abstract class Assignment {
	def code(disablePostProcess:Boolean = false): String
	def getAssignedVars(): Set[String]

	def addExamples(examples: Map[String, List[Map[String, Any]]]) ={
		this.innerSpecs = Some(examples)
	}
	var innerSpecs: Option[Map[String, List[Map[String, Any]]]] = None
	val scopePrefix = "#! Start of specification scope:"
	val ScopeSuffix = "#! End of specification scope"
	val programs: List[ASTNode];

	def asPythonString(value: Any):String={
		value match {
			case value: Number => value.toString
			case value: String => s"'$value'"
			case value: List[_]=> s"[${value.map(asPythonString).mkString(",")}]"
			case value: Set[_] => s"{${value.map(asPythonString).mkString(",")}}"
			case value => value.toString
		}
	}
	def exampleToString(example: Map[String, Any], index:Int): String = {
		val assignedVars= this.getAssignedVars()
		val inputs = example.filter {
			case (name, value) => !name.contains("_in") || assignedVars.contains(name.replace("_in",""))
		}.map {
			case (name, value) if(!assignedVars.contains(name))=> f"$name = ${asPythonString(value)}"
			case _ => ""
		}.filter(x=>x!="").mkString(", ")
		val outputs = example.map {
			case (name, value) if(assignedVars.contains(name))=> f"$name = ${asPythonString(value)}"
			case _ => ""
		}.filter(x=>x!="").mkString(", ")
		f"#! ${index}) $inputs => $outputs"

	}



}

case class SingleAssignment(name: String, val program: ASTNode) extends Assignment {
	override val programs = List(program)
	def code(disablePostProcess:Boolean = false): String = {
		var rs = f"$name = ${PostProcessor.clean(program).code}"
		val selfAssign = s"$name = $name + "
		if (rs.startsWith(selfAssign)) {
			rs = rs.replace(selfAssign, s"$name += ")
		}

		val relevantExamples: Option[List[Map[String, Any]]]= if(innerSpecs.nonEmpty) innerSpecs.get.get(name) else None;
		if(relevantExamples.nonEmpty){
			val examplesWrap = relevantExamples.get.zipWithIndex.map {
				case (example, i) => this.exampleToString(example, i + 1)
			}.mkString("\n")
			scopePrefix + "\n" + examplesWrap +"\n"+ rs + "\n" + ScopeSuffix
		}
		else {
			rs
		}
	}

	override def getAssignedVars(): Set[String] = Set(name)

}

case class BasicMultivariableAssignment(names: List[String], override val programs: List[ASTNode]) extends Assignment
{
	override def code(disablePostProcess:Boolean = false): String = {
		// See if we can break it up into multiple lines
		val singleLine = names.zipWithIndex.exists {
			case (name, i) => programs.zipWithIndex.exists {
				case (program, j) => i != j && program.includes(name)
			}
		}
		if (singleLine) {
			val lhs = names.mkString(", ")
			val rhs = programs.map(pred => PostProcessor.clean(pred).code).mkString(", ")
			f"$lhs = $rhs"
		} else {
			names.zip(programs).map {
				case (name, program) => {
					val rs = f"$name = ${PostProcessor.clean(program).code}"
					val relevantExamples: Option[List[Map[String, Any]]]= if(innerSpecs.nonEmpty) innerSpecs.get.get(name) else None;
					if (relevantExamples.nonEmpty) {
						val examplesWrap = relevantExamples.get.zipWithIndex.map {
							case (example, i) => this.exampleToString(example, i + 1)
						}.mkString("\n")
						scopePrefix + "\n" + examplesWrap +"\n"+  rs + "\n" + ScopeSuffix
					}
					else {
						rs
					}
				}
			}.mkString("\n")
		}
	}

	override  def getAssignedVars(): Set[String] = names.toSet
}

case class MultilineMultivariableAssignment(assignments: List[Assignment]) extends Assignment
{
	override val programs = assignments.flatMap(_.programs)
	override def code(disablePostProcess:Boolean = false): String = {
		assignments.map(_.code()).mkString("\n")
	}

	override def getAssignedVars(): Set[String] = assignments.flatMap(_.getAssignedVars()).toSet

	override def addExamples(innerSpecs: Map[String, List[Map[String, Any]]]): Unit = {
		assignments.foreach(_.addExamples(innerSpecs))
	}
}

case class ConditionalAssignment(var cond: BoolNode, var thenCase: Assignment, var elseCase: Assignment) extends Assignment
{
	override val programs = thenCase.programs ++ elseCase.programs
	override def addExamples(innerSpecs: Map[String, List[Map[String, Any]]]): Unit = {
		thenCase.addExamples(innerSpecs)
		elseCase.addExamples(innerSpecs)
	}

	// PostProcessor handles double negation
	PostProcessor.clean(cond) match {
		case NegateBool(inner, l) =>
			// Flip the if-condition
			cond = inner
			val tmp = elseCase
			elseCase = thenCase
			thenCase = tmp
		case cleanCond =>
			cond = cleanCond.asInstanceOf[BoolNode]
	}

	override def code(disablePostProcess:Boolean = false): String = {
		// Cleanup!

		// First check if the condition is all true or false
		if (cond.exampleValues.forall(_.get)) {
			this.flatten(thenCase).filter(deadCodeFilter).map(_.code()).mkString("\n")
		} else if (cond.exampleValues.forall(!_.get)) {
			this.flatten(elseCase).filter(deadCodeFilter).map(_.code()).mkString("\n")
		} else {
			// TODO This cleanup code is (a) very ugly and (b) not well tested

			//everything here, run to a fixpoint:
			var preStrs, thenStrs,elseStrs,postStrs: List[String] = Nil
			var preCondition: List[Assignment] = List()
			var thenCode: List[Assignment] = this.flatten(thenCase) //can this change too somehow? Does it need to go in the loop?
			var elseCode: List[Assignment] = this.flatten(elseCase)
			var postCondition: List[Assignment] = List()

				while (preStrs != preCondition.map(_.code()) || thenStrs != thenCode.map(_.code()) || elseStrs != elseCode.map(_.code()) || postStrs != postCondition.map(_.code())) {
					//changed, update the prev state:
					preStrs = preCondition.map(_.code())
					thenStrs = thenCode.map(_.code())
					elseStrs = elseCode.map(_.code())
					postStrs = postCondition.map(_.code())
					// Now take out common prefixes,...
					if(!disablePostProcess) {
						while (thenCode.nonEmpty && elseCode.nonEmpty && thenCode.head.code() == elseCode.head.code()) {
						preCondition = preCondition :+ thenCode.head
						thenCode = thenCode.tail
						elseCode = elseCode.tail
						}
					}

					// ...Lines sandwiched b/w other assignments,...
					var i = 0
					while (i < thenCode.length) {
						// TODO This can/should? be part of the loop above. But this whole code is hacky
						//  spaghetti, so ¯\_('')_/¯
						thenCode(i) match {
							case SingleAssignment(name, thenProgram) =>
								elseCode.zipWithIndex.find(a => a._1.isInstanceOf[SingleAssignment] && a._1.asInstanceOf[SingleAssignment].name == name) match {
									case Some((SingleAssignment(_, elseProgram), j)) if elseProgram.code == thenProgram.code =>
										val thenVarsAssignedBefore = this.varsAssigned(thenCode.slice(0, i))
										val elseVarsAssignedBefore = this.varsAssigned(elseCode.slice(0, j))
										val thenVarsAssignedAfter = this.varsAssigned(thenCode.slice(i + 1, thenCode.length))
										val elseVarsAssignedAfter = this.varsAssigned(elseCode.slice(j + 1, elseCode.length))

										if (!(thenVarsAssignedBefore.exists(thenProgram.includes) ||
											elseVarsAssignedBefore.exists(elseProgram.includes)) &&
											!(anyInclude(thenCode.slice(0, i), name) ||
												anyInclude(elseCode.slice(0, j), name))) {
											preCondition = preCondition :+ thenCode(i)
											thenCode = thenCode.slice(0, i) ++ thenCode.slice(i + 1, thenCode.length)
											elseCode = elseCode.slice(0, j) ++ elseCode.slice(j + 1, elseCode.length)
										}
										else if (!(thenVarsAssignedAfter.exists(thenProgram.includes) ||
											elseVarsAssignedAfter.exists(elseProgram.includes)) &&
											!(this.anyInclude(thenCode.slice(i + 1, thenCode.length), name) ||
												this.anyInclude(elseCode.slice(j + 1, elseCode.length), name))) {
											postCondition = postCondition :+ thenCode(i)
											thenCode = thenCode.slice(0, i) ++ thenCode.slice(i + 1, thenCode.length)
											elseCode = elseCode.slice(0, j) ++ elseCode.slice(j + 1, elseCode.length)
										} else {
											i += 1
										}
									case _ => i += 1
								}
							case _ => i += 1
						}
					}

					// ...and postfixes.
					while (thenCode.nonEmpty && elseCode.nonEmpty && thenCode.last.code() == elseCode.last.code()) {
						postCondition = thenCode.last :: postCondition
						thenCode = thenCode.slice(0, thenCode.length - 1)
						elseCode = elseCode.slice(0, elseCode.length - 1)
					}

					preCondition = preCondition.filter(deadCodeFilter)
					thenCode = thenCode.filter(deadCodeFilter)
					elseCode = elseCode.filter(deadCodeFilter)
					postCondition = postCondition.filter(deadCodeFilter)

				} //end while changed

			val preCondString = preCondition.map(_.code()).mkString("\n")
			val postCondString = postCondition.map(_.code()).mkString("\n")

			val condString: String = (thenCode, elseCode) match {
				case (Nil, Nil) => ""
				case (thenCode, Nil) =>
					f"if ${PostProcessor.clean(cond).code}:\n" +
						"\t" + thenCode.map(_.code()).mkString("\n\t")
				case (Nil, elseCode) =>
					f"if ${PostProcessor.clean(NegateBool(cond)).code}:\n" +
						"\t" + elseCode.map(_.code()).mkString("\n\t")
				case (thenCode, elseCode) =>
					f"if ${PostProcessor.clean(cond).code}:\n" +
						"\t" + thenCode.flatMap(_.code().split("\n")).mkString("\n\t") +
						"\nelse:\n" +
						"\t" + elseCode.flatMap(_.code().split("\n")).mkString("\n\t")
			}
			(preCondString, condString, postCondString) match {
				case ("", "", "") => "None"
				case ("", rs, "") => rs
				case (rs, "", "") => rs
				case (pre, cond, "") => pre + "\n" + cond
				case ("", "", post) => post
				case ("", rs, post) => rs + "\n" + post
				case (rs, "", post) => rs + "\n" + post
				case (pre, cond, post) => pre + "\n" + cond + "\n" + post
			}

		}
	}

	def anyInclude(assigns: List[Assignment], varName: String): Boolean = assigns match {
		case Nil => false
		case SingleAssignment(_, program) :: rest => anyInclude(rest, varName) || program.includes(varName)
		case BasicMultivariableAssignment(_, programs) :: rest => anyInclude(rest, varName) || programs.exists(_.includes(varName))
		case MultilineMultivariableAssignment(as) :: rest => anyInclude(as ++ rest, varName)
		case ConditionalAssignment(_, thenCase, elseCase) :: rest => anyInclude(thenCase :: elseCase :: rest, varName)
	}

	def varsAssigned(lst: List[Assignment]): Set[String] =
		lst.foldRight(Set[String]()) {
			case (SingleAssignment(name, _), soFar) => soFar + name
			case (BasicMultivariableAssignment(names, _), soFar) => soFar ++ names
			case (MultilineMultivariableAssignment(assigns), soFar) => soFar ++ varsAssigned(assigns)
			case (ConditionalAssignment(_, thenCase, elseCase), soFar) => soFar ++ varsAssigned(thenCase :: elseCase :: Nil)
		}

	def flatten(a: Assignment): List[Assignment] = a match {
		case a: MultilineMultivariableAssignment => a.assignments
		case a => a :: Nil
	}

	def deadCodeFilter(assign: Assignment): Boolean = assign match {
		case SingleAssignment(name, v: VariableNode[_]) => name != v.name
		case _ => true
	}

	override def getAssignedVars(): Set[String] = this.varsAssigned(List(this.thenCase)) | this.varsAssigned(List(this.elseCase))


}