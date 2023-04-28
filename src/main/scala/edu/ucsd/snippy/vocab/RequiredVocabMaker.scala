package edu.ucsd.snippy.vocab

import edu.ucsd.snippy.ast.ASTNode
import edu.ucsd.snippy.ast.Types.Types




class RequiredVocabMaker(program: ASTNode, nodesToHoles: List[ASTNode]) extends BasicVocabMaker {

	override val head: String = ""
	override val arity: Int = nodesToHoles.length
	override val returnType: Types = program.nodeType
	override val childTypes: List[Types] = nodesToHoles.map(_.nodeType)
	override val nodeType: Class[_ <: ASTNode] = program.getClass

	override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode = {
		var newProgram: ASTNode = program.asInstanceOf[ASTNode]
		for (i <- children.indices) {
			newProgram = ASTVisitor(newProgram, nodesToHoles(i), children(i))
		}
		newProgram
	}



	def ASTVisitor(root: ASTNode, nodeToReplace: ASTNode, replacement: ASTNode): ASTNode = {
		if (root == nodeToReplace) replacement
		else root.updateChildren(root.children.map(child => ASTVisitor(child, nodeToReplace, replacement)).toSeq)
	}


}
