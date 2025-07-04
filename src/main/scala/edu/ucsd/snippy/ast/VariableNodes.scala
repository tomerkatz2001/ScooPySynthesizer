package edu.ucsd.snippy.ast

import edu.ucsd.snippy.ast.Types.Types
import edu.ucsd.snippy.enumeration.Contexts

abstract class VariableNode[T](contexts: List[Map[String, Any]]) extends ASTNode
{
	val reqVeq : List[Boolean];
	override lazy val code: String = name
	override val height: Int = 0
	override val children: Iterable[ASTNode] = Iterable.empty
	override protected val parenless: Boolean = true
	override val requireBits: List[Boolean] = reqVeq

	val terms = 1
	val name: String

	override val _values: List[Option[T]] = contexts.map(context => context.get(name).asInstanceOf[Option[T]])
	override def exampleValues: List[Option[T]] = _values
	def includes(varName: String): Boolean = name == varName

	override lazy val usesVariables: Boolean = true

}

object VariableNode {
	def nodeFromType(name: String, retType: Types.Types, contexts: List[Map[String, Any]]): Option[ASTNode] = retType match {
		case Types.Bool => Some(BoolVariable(name, contexts))
		case Types.String => Some(StringVariable(name, contexts))
		case Types.Int => Some(IntVariable(name, contexts))
		case Types.Double => Some(DoubleVariable(name, contexts))
		case Types.IntList => Some(ListVariable[Int](name, contexts, Types.Int))
		case Types.StringList => Some(ListVariable[String](name, contexts, Types.String))
		case Types.IntIntMap => Some(MapVariable[Int,Int](name, contexts, Types.Int, Types.Int))
		case Types.IntStringMap => Some(MapVariable[Int,String](name, contexts, Types.Int, Types.String))
		case Types.StringIntMap => Some(MapVariable[String,Int](name, contexts, Types.String, Types.Int))
		case Types.StringStringMap => Some(MapVariable[String,String](name, contexts, Types.String, Types.String))
		case Types.IntSet => Some(SetVariable[Int](name, contexts, Types.Int))
		case Types.StringSet => Some(SetVariable[String](name,contexts,Types.String))
		case Types.BoolList => Some(ListVariable[Boolean](name,contexts,Types.Bool))
		case Types.DoubleList => Some(ListVariable[Double](name,contexts, Types.Double))
		case Types.DoubleSet => Some(SetVariable[Double](name, contexts, Types.Double))
		case _ => None
	}
}

case class StringVariable(name: String, contexts: List[Map[String, Any]], reqVeq: List[Boolean] = List()) extends VariableNode[String](contexts) with StringNode
{
	override def updateValues(contexts: Contexts): StringVariable = copy(name, contexts = contexts.contexts)

	override def updateChildren(children: Seq[ASTNode],reqVeq: List[Boolean] = List()): ASTNode = copy(name, contexts, reqVeq)

}

case class IntVariable(name: String, contexts: List[Map[String, Any]], reqVeq: List[Boolean] = List()) extends VariableNode[Int](contexts) with IntNode
{

	override def updateValues(contexts: Contexts): IntVariable = copy(name, contexts = contexts.contexts)
	override def updateChildren(children: Seq[ASTNode],reqVeq: List[Boolean] = List()): ASTNode = copy(name, contexts, reqVeq)

}

case class BoolVariable(name: String, contexts: List[Map[String, Any]], reqVeq: List[Boolean] = List()) extends VariableNode[Boolean](contexts) with BoolNode
{

	override def updateValues(contexts: Contexts): BoolVariable = copy(name, contexts = contexts.contexts)
	override def updateChildren(children: Seq[ASTNode],reqVeq: List[Boolean] = List()): ASTNode = copy(name, contexts, reqVeq)

}

case class DoubleVariable(name: String, contexts: List[Map[String, Any]], reqVeq: List[Boolean] = List()) extends  VariableNode[Double](contexts) with DoubleNode
{

	override def updateValues(contexts: Contexts): DoubleVariable = copy(name, contexts = contexts.contexts)
	override def updateChildren(children: Seq[ASTNode],reqVeq: List[Boolean] = List()): ASTNode = copy(name, contexts, reqVeq)

}

case class ListVariable[T](name: String, contexts: List[Map[String, Any]], childType: Types, reqVeq: List[Boolean] = List()) extends VariableNode[Iterable[T]](contexts) with ListNode[T]
{

	override def updateValues(contexts: Contexts): ListVariable[T] = copy(name, contexts = contexts.contexts)
	override def updateChildren(children: Seq[ASTNode],reqVeq: List[Boolean] = List()): ASTNode = copy(name, contexts,childType, reqVeq)

}

case class MapVariable[K, V](name: String, contexts: List[Map[String, Any]], keyType: Types, valType: Types, reqVeq: List[Boolean] = List()) extends VariableNode[Map[K, V]](contexts) with MapNode[K, V]
{

	override def updateValues(contexts: Contexts): MapVariable[K, V] = copy(name, contexts = contexts.contexts)
	override def updateChildren(children: Seq[ASTNode],reqVeq: List[Boolean] = List()): ASTNode = copy(name, contexts,keyType, valType, reqVeq)

}

case class SetVariable[T](name: String, contexts: List[Map[String, Any]], childType: Types, reqVeq: List[Boolean] = List()) extends VariableNode[Set[T]](contexts) with SetNode[T]
{

	override def updateValues(contexts: Contexts): SetNode[T] = copy(name, contexts = contexts.contexts)
	override def updateChildren(children: Seq[ASTNode],reqVeq: List[Boolean] = List()): ASTNode = copy(name, contexts,childType, reqVeq)

}