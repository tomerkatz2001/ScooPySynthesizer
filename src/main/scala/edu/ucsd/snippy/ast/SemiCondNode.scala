package edu.ucsd.snippy.ast

import edu.ucsd.snippy.ast.Types.Types
import edu.ucsd.snippy.enumeration.Contexts

case class SemiCondNode(cond: BoolNode, thenCase: ASTNode, elseCase: ASTNode) extends ASTNode {

	def make(cond: BoolNode, thenCase: ASTNode, elseCase: ASTNode): SemiCondNode = {
		SemiCondNode(cond, thenCase, elseCase)
	}

	def doOp(l: Any, r: Any): Option[Boolean] = None

	override val _values: List[Option[Any]] = List();

	def includes(varName: String): Boolean = cond.includes(varName) || thenCase.includes(varName) || elseCase.includes(varName)

	override val nodeType: Types = Types.Unknown
	override val code: String = "if (" + cond.code + ") " + thenCase.code + " else " + elseCase.code
	override val height: Int = 1 + Math.max(cond.height, Math.max(thenCase.height, elseCase.height))
	override val terms: Int = cond.terms + thenCase.terms + elseCase.terms
	override val children: Iterable[ASTNode] = List(cond, thenCase, elseCase)
	override val usesVariables: Boolean = cond.usesVariables || thenCase.usesVariables || elseCase.usesVariables
	override protected val parenless: Boolean = false

	override def updateValues(contexts: Contexts): ASTNode = ???
}