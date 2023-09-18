package edu.ucsd.snippy.ast

import edu.ucsd.snippy.ast.Types.Types
import edu.ucsd.snippy.enumeration.Contexts

abstract class AbsCondNode(val cond: BoolNode, val thenCase: ASTNode, val elseCase: ASTNode, val contexts: Contexts) extends ASTNode{

//	def make(cond: BoolNode, thenCase: ASTNode, elseCase: ASTNode): SemiCondNode = {
//		SemiCondNode(cond, thenCase, elseCase)
//	}



	override val _values: List[Option[Any]] = {
		val condValues = cond.exampleValues
		val thenValues = thenCase.exampleValues
		val elseValues = elseCase.exampleValues

		condValues.zip(thenValues).zip(elseValues).map {
			case ((Some(c), Some(t)), Some(e)) => Some(if (c) t else e)
			case _ => None
		}
	};

	def includes(varName: String): Boolean = cond.includes(varName) || thenCase.includes(varName) || elseCase.includes(varName)

	override val code: String = thenCase.code +" if (" + cond.code + ")" + " else " + elseCase.code
	override val height: Int = 1 + Math.max(cond.height, Math.max(thenCase.height, elseCase.height))
	override val terms: Int = cond.terms + thenCase.terms + elseCase.terms
	override val children: Iterable[ASTNode] = List(cond, thenCase, elseCase)
	override val usesVariables: Boolean = cond.usesVariables || thenCase.usesVariables || elseCase.usesVariables
	override protected val parenless: Boolean = false


}

case class IntCondNode(override val cond: BoolNode, override val thenCase: IntNode,override val elseCase: IntNode, override  val contexts: Contexts) extends AbsCondNode(cond, thenCase, elseCase, contexts) with IntNode {

	override val nodeType: Types = Types.Int
	override val _values: List[Option[Int]] = {
		val condValues = cond.exampleValues
		val thenValues = thenCase.exampleValues
		val elseValues = elseCase.exampleValues

		condValues.zip(thenValues).zip(elseValues).map {
			case ((Some(c), Some(t)), Some(e)) => Some(if (c) t else e)
			case _ => None
		}
	};
	override def updateValues(contexts: Contexts): IntNode = copy(
		cond = cond.updateValues(contexts),
		thenCase = thenCase.updateValues(contexts),
		elseCase = elseCase.updateValues(contexts),
		contexts
	)

	override def updateChildren(children: Seq[ASTNode], requiredVec: List[Boolean]): ASTNode = copy(
		cond = children.head.asInstanceOf[BoolNode],
		thenCase = children(1).asInstanceOf[IntNode],
		elseCase = children(2).asInstanceOf[IntNode],
		contexts
	)
}

case class StringCondNode(override  val cond: BoolNode, override  val thenCase: StringNode, override  val elseCase: StringNode, override  val contexts: Contexts) extends AbsCondNode(cond, thenCase, elseCase, contexts) with StringNode {

	override val nodeType: Types = Types.String
	override val _values: List[Option[String]] = {
		val condValues = cond.exampleValues
		val thenValues = thenCase.exampleValues
		val elseValues = elseCase.exampleValues

		condValues.zip(thenValues).zip(elseValues).map {
			case ((Some(c), Some(t)), Some(e)) => Some(if (c) t else e)
			case _ => None
		}
	};
	override def updateValues(contexts: Contexts): StringNode = copy(
		cond = cond.updateValues(contexts),
		thenCase = thenCase.updateValues(contexts),
		elseCase = elseCase.updateValues(contexts),
		contexts
	)

	override def updateChildren(children: Seq[ASTNode], requiredVec: List[Boolean]): ASTNode = copy(
		cond = children.head.asInstanceOf[BoolNode],
		thenCase = children(1).asInstanceOf[StringNode],
		elseCase = children(2).asInstanceOf[StringNode],
		contexts
	)
}

case class DoubleCondNode(override  val cond: BoolNode, override  val thenCase: DoubleNode,override val elseCase: DoubleNode,override val contexts: Contexts) extends AbsCondNode(cond, thenCase, elseCase, contexts) with DoubleNode {

	override val nodeType: Types = Types.Double
	override val _values: List[Option[Double]] = {
		val condValues = cond.exampleValues
		val thenValues = thenCase.exampleValues
		val elseValues = elseCase.exampleValues

		condValues.zip(thenValues).zip(elseValues).map {
			case ((Some(c), Some(t)), Some(e)) => Some(if (c) t else e)
			case _ => None
		}
	};
	override def updateValues(contexts: Contexts): DoubleNode = copy(
		cond = cond.updateValues(contexts),
		thenCase = thenCase.updateValues(contexts),
		elseCase = elseCase.updateValues(contexts),
		contexts
	)

	override def updateChildren(children: Seq[ASTNode], requiredVec: List[Boolean]): ASTNode = copy(
		cond = children.head.asInstanceOf[BoolNode],
		thenCase = children(1).asInstanceOf[DoubleNode],
		elseCase = children(2).asInstanceOf[DoubleNode],
		contexts
	)
}

case class IntListCondNode(override  val cond: BoolNode, override val thenCase: ListNode[Int], override  val elseCase: ListNode[Int], override  val contexts: Contexts) extends AbsCondNode(cond, thenCase, elseCase, contexts) with IntListNode with ListNode[Int] {

	lazy override val nodeType: Types = Types.IntList
	override val _values: List[Option[Iterable[Int]]] = {
		val condValues = cond.exampleValues
		val thenValues = thenCase.exampleValues
		val elseValues = elseCase.exampleValues

		condValues.zip(thenValues).zip(elseValues).map {
			case ((Some(c), Some(t)), Some(e)) => Some(if (c) t else e)
			case _ => None
		}
	};
	override def updateValues(contexts: Contexts): IntListNode = copy(
		cond = cond.updateValues(contexts),
		thenCase = thenCase.updateValues(contexts),
		elseCase = elseCase.updateValues(contexts),
		contexts
	)

	override def updateChildren(children: Seq[ASTNode], requiredVec: List[Boolean]): ASTNode = copy(
		cond = children.head.asInstanceOf[BoolNode],
		thenCase = children(1).asInstanceOf[ListNode[Int]],
		elseCase = children(2).asInstanceOf[ListNode[Int]],
		contexts
	)
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

case class ListHoleNode[T](name: String, contexts: List[Map[String, Any]], reqVeq: List[Boolean] =List()) extends HoleNode[Iterable[T]](contexts) with ListNode[T] {
	override lazy val nodeType: Types = Types.Unknown
	override lazy val childType: Types = Types.Unknown
	lazy override val code: String = "hole"
	override val _values: List[Option[Iterable[T]]] = List.fill(contexts.length)(None)
	override def updateValues(contexts: Contexts): ListHoleNode[T] = copy(name, contexts.contexts)
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



