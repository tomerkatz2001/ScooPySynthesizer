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

abstract class HoleNode[T](contexts: List[Map[String, Any]]) extends VariableNode[T](contexts){
	override val height: Int = 0
	override val children: Iterable[ASTNode] = Iterable.empty
	override protected val parenless: Boolean = true
	override val requireBits: List[Boolean] = reqVeq
	override val terms = 1
	override lazy val usesVariables: Boolean = true;

}

case class IntHoleNode(name: String, contexts: List[Map[String, Any]], reqVeq: List[Boolean] =List()) extends HoleNode[Int](contexts) with IntNode {
	override val nodeType: Types = Types.Unknown
	lazy override val code: String = "hole"
	override val _values: List[Option[Int]] = List.fill(contexts.length)(None)
	override def updateValues(contexts: Contexts): IntHoleNode = copy(name, contexts.contexts)
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): ASTNode = copy(name, contexts, reqVeq)
}

case class BoolHoleNode(name: String, contexts: List[Map[String, Any]], reqVeq: List[Boolean] =List()) extends HoleNode[Boolean](contexts) with BoolNode {
	override val nodeType: Types = Types.Unknown
	lazy override val code: String = "hole"
	override val _values: List[Option[Boolean]] = List.fill(contexts.length)(None)
	override def updateValues(contexts: Contexts): BoolHoleNode = copy(name, contexts.contexts)
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): ASTNode = copy(name, contexts, reqVeq)
}

case class DoubleHoleNode(name: String, contexts: List[Map[String, Any]], reqVeq: List[Boolean] =List()) extends HoleNode[Double](contexts) with DoubleNode {
	override val nodeType: Types = Types.Unknown
	lazy override val code: String = "hole"
	override val _values: List[Option[Double]] = List.fill(contexts.length)(None)
	override def updateValues(contexts: Contexts): DoubleHoleNode = copy(name, contexts.contexts)
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): ASTNode = copy(name, contexts, reqVeq)
}

case class StringHoleNode(name: String, contexts: List[Map[String, Any]], reqVeq: List[Boolean] =List()) extends HoleNode[String](contexts) with StringNode {
	override val nodeType: Types = Types.Unknown
	lazy override val code: String = "hole"
	override val _values: List[Option[String]] = List.fill(contexts.length)(None)
	override def updateValues(contexts: Contexts): StringHoleNode = copy(name, contexts.contexts)
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): ASTNode = copy(name, contexts, reqVeq)
}

case class ListHoleNode(name: String, contexts: List[Map[String, Any]], reqVeq: List[Boolean] =List()) extends HoleNode[List[Any]](contexts) with ListNode[Any] {
	override lazy val nodeType: Types = Types.Unknown
	override lazy val childType: Types = Types.Unknown
	lazy override val code: String = "hole"
	override val _values: List[Option[List[Any]]] = List.fill(contexts.length)(None)
	override def updateValues(contexts: Contexts): ListHoleNode = copy(name, contexts.contexts)
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): ASTNode = copy(name, contexts, reqVeq)
}

case class MapHoleNode(name: String, contexts: List[Map[String, Any]], reqVeq: List[Boolean] =List()) extends HoleNode[Map[Any, Any]](contexts) with MapNode[Any, Any] {
	override lazy val nodeType: Types = Types.Unknown
	override lazy val keyType: Types = Types.Unknown
	override lazy val valType: Types = Types.Unknown
	lazy override val code: String = "hole"
	override val _values: List[Option[Map[Any, Any]]] = List.fill(contexts.length)(None)
	override def updateValues(contexts: Contexts): MapHoleNode = copy(name, contexts.contexts)
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): ASTNode = copy(name, contexts, reqVeq)


}

case class SetHoleNode(name: String, contexts: List[Map[String, Any]], reqVeq: List[Boolean] =List()) extends HoleNode[Set[Any]](contexts) with SetNode[Any] {
	override lazy val nodeType: Types = Types.Unknown
	override lazy val childType: Types = Types.Unknown

	lazy override val code: String = "hole"
	override val _values: List[Option[Set[Any]]] = List.fill(contexts.length)(None)
	override def updateValues(contexts: Contexts): SetHoleNode = copy(name, contexts.contexts)
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): ASTNode = copy(name, contexts, reqVeq)

}



