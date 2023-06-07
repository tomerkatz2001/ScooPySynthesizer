package edu.ucsd.snippy.vocab

import edu.ucsd.snippy.ast.{ASTNode, VariableNode}
import edu.ucsd.snippy.ast.Types.Types
import edu.ucsd.snippy.enumeration.Contexts




class RequiredVocabMaker(program: ASTNode, nodesToHoles: List[ASTNode], id:Int) extends BasicVocabMaker {

	override val head: String = ""
	override val arity: Int = nodesToHoles.length
	override val returnType: Types = program.nodeType
	override val childTypes: List[Types] = nodesToHoles.map(_.nodeType)
	override val nodeType: Class[_ <: ASTNode] = program.getClass

	def ASTVarsVisitor(root: ASTNode, goodVars: List[String] = List(), hole: ASTNode, contexts: Contexts): ASTNode = {
		val newAst = root match {
			case v: VariableNode[_] => if (!goodVars.contains(v.name)) hole else v
			case _ => root.updateChildren(root.children.map(child => ASTVarsVisitor(child, goodVars, hole, contexts).updateValues(contexts)).toSeq, List())
		}
		newAst
	}

	override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode = {
		val vec = List.fill(id + 1)(false).updated(id, true)


		var newProgram: ASTNode = program
		if (children.isEmpty)
			newProgram = newProgram.updateChildren(newProgram.children.toSeq, vec)

		for (i <- children.indices) {
			newProgram = ASTVisitor(newProgram, nodesToHoles(i), children(i), vec)
		}
		newProgram.updateValues(new Contexts(contexts))
		newProgram
	}

	def ASTVisitor(root: ASTNode, nodeToReplace: ASTNode, replacement: ASTNode, reqVec:List[Boolean]): ASTNode = {
		if (root == nodeToReplace) replacement
		else root.updateChildren(root.children.map(child => ASTVisitor(child, nodeToReplace, replacement, reqVec)).toSeq, reqVec)
	}


}
