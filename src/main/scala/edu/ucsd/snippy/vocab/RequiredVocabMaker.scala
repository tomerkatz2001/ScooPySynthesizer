package edu.ucsd.snippy.vocab

import edu.ucsd.snippy.ast.Types.Types
import edu.ucsd.snippy.ast._
import edu.ucsd.snippy.enumeration.Contexts




class RequiredVocabMaker(program: ASTNode, assignedVars: List[String], idx:Int, contexts: Contexts) extends BasicVocabMaker {

	val (progWithHoles, replacedASTs)  = ASTHolesMakerVisitor(program, assignedVars, contexts);
	override val head: String = "required"
	override val arity: Int = replacedASTs.length
	override val returnType: Types = program.nodeType
	override val childTypes: List[Types] = replacedASTs.map(_.nodeType)
	override val nodeType: Class[_ <: ASTNode] = program.getClass


	def ASTHolesMakerVisitor(root: ASTNode, assignedVars: List[String] = List(), contexts: Contexts): (ASTNode, List[ASTNode]) = {
		val (newAST, replacedASTs) = root match {
			case v: IntVariable => if (!assignedVars.contains(v.name)) (IntHoleNode(v.name, v.contexts), List(v)) else (v, List())
			case v: StringVariable => if (!assignedVars.contains(v.name)) (StringHoleNode(v.name, v.contexts), List(v)) else (v, List())
			case v: BoolVariable => if (!assignedVars.contains(v.name)) (BoolHoleNode(v.name, v.contexts), List(v)) else (v, List())
			case v: DoubleVariable => if (!assignedVars.contains(v.name)) (DoubleHoleNode(v.name, v.contexts), List(v)) else (v, List())
			case v: ListVariable[_] => if (!assignedVars.contains(v.name)) (ListHoleNode(v.name, v.contexts), List(v)) else (v, List())
			case v: MapVariable[_,_] => if (!assignedVars.contains(v.name)) (MapHoleNode(v.name, v.contexts), List(v)) else (v, List())
			case v: SetVariable[_] => if (!assignedVars.contains(v.name)) (SetHoleNode(v.name, v.contexts), List(v)) else (v, List())
			case _ => {
				var newChildren: List[ASTNode] = List();
				var ChildrenHoles: List[ASTNode] = List();
				for(child <- root.children) {
					val (newChild, newHoles) = ASTHolesMakerVisitor(child, assignedVars, contexts)
					newChildren = newChild.updateValues(contexts) :: newChildren
					ChildrenHoles = newHoles ++ ChildrenHoles;
				}
				(root.updateChildren(newChildren.reverse.toSeq, List()), ChildrenHoles)
			}
		}
		(newAST, replacedASTs)
	}

	override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode = {
		val vec = List.fill(idx + 1)(false).updated(idx, true)

		var newProgram: ASTNode = progWithHoles
		if (children.isEmpty)
			newProgram = newProgram.updateChildren(newProgram.children.toSeq, vec)


		for (child <- children) {
			val childVec = child.requireBits
			val newVec =  vec.zipAll(childVec,false, false).map{case (a,b) => a || b}
			newProgram.updateValues(new Contexts(contexts))
			newProgram = ASTVisitor(newProgram,child, newVec)
			//println("newProgram: " + newProgram.code)
		}
		newProgram.updateValues(new Contexts(contexts))
		newProgram
	}

	// I hope that because this is the same tree visiting pattern as the ASTHolesMakerVisitor, the first hole found will be the correct one
	def ASTVisitor(root: ASTNode, replacement: ASTNode, reqVec:List[Boolean]): ASTNode = {
		root match {
			case _ : HoleNode[_] => replacement
			case _ => root.updateChildren(root.children.map(child => ASTVisitor(child, replacement, reqVec)).toSeq, reqVec)
		}
	}


}
