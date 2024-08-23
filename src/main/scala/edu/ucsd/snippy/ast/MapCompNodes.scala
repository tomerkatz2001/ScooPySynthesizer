package edu.ucsd.snippy.ast

import edu.ucsd.snippy.ast.Types.Types
import edu.ucsd.snippy.enumeration.Contexts

trait MapCompNode[K, V] extends MapNode[K, V]
{
	val list: IterableNode
	val key: ASTNode
	val value: ASTNode
	val varName: String

	assert(key.exampleValues.length == value.exampleValues.length, "Key and value did not match")

	override val keyType: Types = Types.childOf(list.nodeType)
	override val valType: Types = value.nodeType

	override val requireBits: List[Boolean] = list.requireBits.zipAll(key.requireBits, false, false).map(x=>x._1|| x._2 ).zipAll(value.requireBits, false, false).map(x=>x._1|| x._2 )
	override val _values: List[Option[Map[K, V]]] = {
		//		val entries: Iterable[(Option[K], Option[V])] = key
		//			.values
		//			.zip(value.values)
		//			.asInstanceOf[Iterable[(Option[K], Option[V])]]

		var idx = 0

		list.exampleValues.map {
			case None => None
			case Some(None)=> None
			case Some(value: String) =>
				// First collect the tuples matching this value
				val keys: List[Option[K]] = this.key.exampleValues.slice(idx, idx + value.length).asInstanceOf[List[Option[K]]]
				val vals: List[Option[V]] = this.value.exampleValues.slice(idx, idx + value.length).asInstanceOf[List[Option[V]]]
				idx += value.length

				if (keys.contains(None) || vals.contains(None)) {
					None
				} else {
					Some(keys.map(_.get).zip(vals.map(_.get)).toMap)
				}
			case Some(value: Iterable[_]) =>
				// First collect the tuples matching this value
				val keys: List[Option[K]] = this.key.exampleValues.slice(idx, idx + value.size).asInstanceOf[List[Option[K]]]
				val vals: List[Option[V]] = this.value.exampleValues.slice(idx, idx + value.size).asInstanceOf[List[Option[V]]]
				idx += value.size

				if (keys.contains(None) || vals.contains(None)) {
					None
				} else {
					Some(keys.map(_.get).zip(vals.map(_.get)).toMap)
				}
		}
	}
	override val height: Int = 1 + Math.max(list.height, value.height)
	override val terms: Int = 1 + list.terms + value.terms
	override val children: Iterable[ASTNode] = List(list, value)
	override protected val parenless: Boolean = true
	override val code: String = s"{${key.code}: ${value.code} for $varName in ${list.code}}"
	override def includes(varName: String): Boolean =
		varName.equals(this.varName) || list.includes(varName) || key.includes(varName) || value.includes(varName)
	override lazy val usesVariables: Boolean = list.usesVariables || key.usesVariables || value.usesVariables
}

trait FilteredMapNode[K, V] extends MapNode[K, V]
{
	val map: MapNode[K, V]
	val filter: BoolNode
	val keyName: String
	val reqVeq:List[Boolean]

	override val keyType: Types = map.keyType
	override val valType: Types = map.valType

	override val requireBits: List[Boolean] = if (reqVeq.isEmpty)map.requireBits.zipAll(filter.requireBits, false, false).map(x=>x._1 || x._2 ) else reqVeq
	override val _values: List[Option[Map[K, V]]] = filterOp(map, filter)
	override val height: Int = 1 + Math.max(map.height, filter.height)
	override val terms: Int = 1 + map.terms + filter.terms
	override val children: Iterable[ASTNode] = List(map, filter)
	override protected val parenless: Boolean = true
	override val code: String = s"{$keyName: ${map.code}[$keyName] for $keyName in ${map.code} if ${filter.code}}"
	override def includes(varName: String): Boolean =
		varName.equals(this.keyName) || map.includes(varName) || filter.includes(varName)
	override lazy val usesVariables: Boolean = map.usesVariables || filter.usesVariables
	def make(map: MapNode[K, V], filter: BoolNode, keyName: String): FilteredMapNode[K, V]

	def findKeyVarInNode(node: ASTNode): Option[VariableNode[_]] =
	{
		node match {
			case n: VariableNode[_] if n.name == keyName => Some(n)
			case n: ASTNode if n.terms > 1 => findKeyVar(n.children)
			case _ => None
		}
	}

	def findKeyVar(nodes: Iterable[ASTNode]): Option[VariableNode[_]] =
	{
		val keyVar = nodes.map(findKeyVarInNode).filter(_.isDefined)
		if (keyVar.isEmpty) {
			None
		} else {
			keyVar.head
		}
	}

	def filterOp(map: MapNode[K, V], filter: BoolNode): List[Option[Map[K, V]]] =
	{
		val keyNode: VariableNode[K] = findKeyVar(filter.children).get.asInstanceOf[VariableNode[K]]

		// Create the key map
		val filterValues: List[Option[List[Boolean]]] = splitByIterable(map.exampleValues, filter.exampleValues)
		val keyValues: List[Option[List[K]]] = splitByIterable(map.exampleValues, keyNode.exampleValues)
		val keyMap: List[Option[List[(K, Boolean)]]] =
			keyValues.zip(filterValues).map {
				case (Some(k), Some(b)) => Some(k.zip(b))
				case _ => None
			}

		map.exampleValues
			.zip(keyMap)
			.map({
				case (Some(valMap: Map[K, V]), Some(keyMap: List[(K, Boolean)])) =>
					Some(valMap.filter({
						case (k: K, _: V) => keyMap.find(_._1.equals(k)).get._2
					}))
				case _ => None
			})
	}
}

case class StringStringMapCompNode(list: StringNode, key: StringNode, value: StringNode, varName: String, reqVeq: List[Boolean] =List()) extends MapCompNode[String, String] {
	override def updateValues(contexts: Contexts): StringStringMapCompNode
	= copy(list.updateValues(contexts), key.updateValues(contexts), value.updateValues(contexts), varName)

	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): ASTNode = copy(
		children.head.asInstanceOf[StringNode],
		key, value, varName, reqVeq
	)
}

case class StringIntMapCompNode(list: StringNode, key: StringNode, value: IntNode, varName: String, reqVeq: List[Boolean] =List()) extends MapCompNode[String, Int] {
	override def updateValues(contexts: Contexts): StringIntMapCompNode
	= copy(list.updateValues(contexts), key.updateValues(contexts), value.updateValues(contexts), varName)

	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): ASTNode = copy(
		children.head.asInstanceOf[StringNode],
		key, value, varName, reqVeq
	)
}

case class StringListStringMapCompNode(list: ListNode[String], key: StringNode, value: StringNode, varName: String, reqVeq: List[Boolean] =List()) extends MapCompNode[String, String] {
	override def updateValues(contexts: Contexts): StringListStringMapCompNode
	= copy(list.updateValues(contexts), key.updateValues(contexts), value.updateValues(contexts), varName)
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): ASTNode = copy(
		children.head.asInstanceOf[ListNode[String]],
		key, value, varName, reqVeq
	)
}

case class StringListIntMapCompNode(list: ListNode[String], key: StringNode, value: IntNode, varName: String, reqVeq: List[Boolean] =List()) extends MapCompNode[String, Int] {
	override def updateValues(contexts: Contexts): StringListIntMapCompNode
	= copy(list.updateValues(contexts), key.updateValues(contexts), value.updateValues(contexts), varName)

	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): ASTNode = copy(
		children.head.asInstanceOf[ListNode[String]],
		key, value, varName, reqVeq
	)
}

case class IntStringMapCompNode(list: ListNode[Int], key: IntNode, value: StringNode, varName: String, reqVeq: List[Boolean] =List()) extends MapCompNode[Int, String] {
	override def updateValues(contexts: Contexts): IntStringMapCompNode
	= copy(list.updateValues(contexts), key.updateValues(contexts), value.updateValues(contexts), varName)

	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): ASTNode = copy(
		children.head.asInstanceOf[ListNode[Int]],
		key, value, varName, reqVeq
	)
}

case class IntIntMapCompNode(list: ListNode[Int], key: IntNode, value: IntNode, varName: String, reqVeq: List[Boolean] =List()) extends MapCompNode[Int, Int] {
	override def updateValues(contexts: Contexts): IntIntMapCompNode
	= copy(list.updateValues(contexts), key.updateValues(contexts), value.updateValues(contexts), varName)

	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): ASTNode = copy(
		children.head.asInstanceOf[ListNode[Int]],
		key, value, varName, reqVeq
	)
}

case class StringStringFilteredMapNode(map: MapNode[String, String], filter: BoolNode, keyName: String, reqVeq: List[Boolean] =List()) extends FilteredMapNode[String, String]
{
	override def make(map: MapNode[String, String], filter: BoolNode, keyName: String): FilteredMapNode[String, String] =
		StringStringFilteredMapNode(map, filter, keyName)
	override def updateValues(contexts: Contexts): StringStringFilteredMapNode
	= copy(map.updateValues(contexts), filter.updateValues(contexts), keyName)

	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): ASTNode = copy(
		children.head.asInstanceOf[MapNode[String, String]],
		children(1).asInstanceOf[BoolNode],
		keyName,
		reqVeq
	)
}

case class StringIntFilteredMapNode(map: MapNode[String, Int], filter: BoolNode, keyName: String, reqVeq: List[Boolean] =List()) extends FilteredMapNode[String, Int]
{
	override def make(map: MapNode[String, Int], filter: BoolNode, keyName: String): FilteredMapNode[String, Int] =
		StringIntFilteredMapNode(map, filter, keyName)
	override def updateValues(contexts: Contexts): StringIntFilteredMapNode
	= copy(map.updateValues(contexts), filter.updateValues(contexts), keyName)

	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): ASTNode = copy(
		children.head.asInstanceOf[MapNode[String, Int]],
		filter, keyName, reqVeq
	)
}

case class IntStringFilteredMapNode(map: MapNode[Int, String], filter: BoolNode, keyName: String, reqVeq: List[Boolean] =List()) extends FilteredMapNode[Int, String]
{
	override def make(map: MapNode[Int, String], filter: BoolNode, keyName: String): FilteredMapNode[Int, String] =
		IntStringFilteredMapNode(map, filter, keyName)
	override def updateValues(contexts: Contexts): IntStringFilteredMapNode
	= copy(map.updateValues(contexts), filter.updateValues(contexts), keyName)

	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): ASTNode = copy(
		children.head.asInstanceOf[MapNode[Int, String]],
		filter, keyName, reqVeq
	)
}

case class IntIntFilteredMapNode(map: MapNode[Int, Int], filter: BoolNode, keyName: String, reqVeq: List[Boolean] =List()) extends FilteredMapNode[Int, Int]
{
	override def make(map: MapNode[Int, Int], filter: BoolNode, keyName: String): FilteredMapNode[Int, Int] =
		IntIntFilteredMapNode(map, filter, keyName)
	override def updateValues(contexts: Contexts): IntIntFilteredMapNode
	= copy(map.updateValues(contexts), filter.updateValues(contexts), keyName)

	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): ASTNode = copy(
		children.head.asInstanceOf[MapNode[Int, Int]],
		filter, keyName, reqVeq
	)
}
