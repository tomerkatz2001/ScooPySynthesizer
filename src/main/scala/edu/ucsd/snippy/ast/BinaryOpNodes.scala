package edu.ucsd.snippy.ast

import edu.ucsd.snippy.DebugPrints
import edu.ucsd.snippy.ast.Types.Types
import edu.ucsd.snippy.enumeration.Contexts

trait BinaryOpNode[T] extends ASTNode
{
	val lhs: ASTNode
	val rhs: ASTNode
	val reqVeq: List[Boolean]

	override val height: Int = 1 + Math.max(lhs.height, rhs.height)
	override val terms: Int = 1 + lhs.terms + rhs.terms
	override val children: Iterable[ASTNode] = Iterable(lhs, rhs)
	override lazy val usesVariables: Boolean = lhs.usesVariables || rhs.usesVariables
	override protected val parenless: Boolean = false
	override val requireBits: List[Boolean] = if(reqVeq.isEmpty) lhs.requireBits.zipAll(rhs.requireBits,false,false).map(x=>x._1 || x._2 ) else reqVeq

	if (lhs.exampleValues.length != rhs.exampleValues.length)
		println(lhs.code, lhs.exampleValues, rhs.code, rhs.exampleValues)
	assert(lhs.exampleValues.length == rhs.exampleValues.length)

	def doOp(l: Any, r: Any): Option[T]
	def make(l: ASTNode, r: ASTNode): BinaryOpNode[T]

	override val _values: List[Option[T]] = lhs.exampleValues.zip(rhs.exampleValues).map {
		case (Some(left), Some(right)) => this.doOp(left, right)
		case _ => None
	}


	def includes(varName: String): Boolean = lhs.includes(varName) || rhs.includes(varName)

	protected def wrongType(l: Any, r: Any): Option[T] =
	{
		DebugPrints.eprintln(s"[${this.getClass.getSimpleName}] Wrong value types: $l $r")
		None
	}

}

case class LessThanEq(lhs: IntNode, rhs: IntNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Boolean] with BoolNode
{
	override lazy val code: String = lhs.code + " <= " + rhs.code


	override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
		case (l: Int, r: Int) => Some(l <= r)
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
		LessThanEq(l.asInstanceOf[IntNode], r.asInstanceOf[IntNode])

	override def updateValues(contexts: Contexts): LessThanEq = copy(
		lhs.updateValues(contexts),
		rhs.updateValues(contexts))

	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): LessThanEq = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[IntNode],
			children.last.asInstanceOf[IntNode],
			reqVeq)
	}

}

case class min2(lhs: ASTNode, rhs: ASTNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Int] with IntNode
{
	override lazy val code: String = "min("+lhs.code +", "+ rhs.code+")"

	override def doOp(l: Any, r: Any): Option[Int] = (l, r) match {
		case (l: Integer, r: Integer) => Some(Math.min(l, r))
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Int] =
		min2(l.asInstanceOf[ASTNode], r.asInstanceOf[ASTNode])

	override def updateValues(contexts: Contexts): min2 = copy(
		lhs.updateValues(contexts),
		rhs.updateValues(contexts))

	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): min2 = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[ASTNode],
			children.last.asInstanceOf[ASTNode],
			reqVeq)
	}

}

case class GreaterThan(lhs: IntNode, rhs: IntNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Boolean] with BoolNode
{
	override lazy val code: String = lhs.code + " > " + rhs.code

	override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
		case (l: Int, r: Int) => Some(l > r)
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
		GreaterThan(l.asInstanceOf[IntNode], r.asInstanceOf[IntNode])

	override def updateValues(contexts: Contexts): GreaterThan = copy(
		lhs.updateValues(contexts),
		rhs.updateValues(contexts))

	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): GreaterThan = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[IntNode],
			children.last.asInstanceOf[IntNode]
			,reqVeq)
	}
}

case class Equals(lhs: IntNode, rhs: IntNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Boolean] with BoolNode
{
	override lazy val code: String = lhs.code + " == " + rhs.code

	override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
		case (l: Int, r: Int) => Some(l == r)
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
		Equals(l.asInstanceOf[IntNode], r.asInstanceOf[IntNode])

	override def updateValues(contexts: Contexts): Equals = copy(lhs.updateValues(contexts), rhs.updateValues(contexts))

	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): Equals = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[IntNode],
			children.last.asInstanceOf[IntNode]
			,reqVeq)
	}
}

case class StringEquals(lhs: StringNode, rhs: StringNode,reqVeq: List[Boolean] =List()) extends BinaryOpNode[Boolean] with BoolNode
{
	override lazy val code: String = lhs.code + " == " + rhs.code

	override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
		case (l: String, r: String) => Some(l == r)
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
		StringEquals(l.asInstanceOf[StringNode], r.asInstanceOf[StringNode])

	override def updateValues(contexts: Contexts): StringEquals = copy(lhs.updateValues(contexts), rhs.updateValues(contexts))

	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): StringEquals = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[StringNode],
			children.last.asInstanceOf[StringNode],
			reqVeq)
	}
}

case class StringConcat(lhs: StringNode, rhs: StringNode,reqVeq: List[Boolean] =List()) extends BinaryOpNode[String] with StringNode
{
	override lazy val code: String = lhs.code + " + " + rhs.code

	override def doOp(l: Any, r: Any): Option[String] = (l, r) match {
		case (l: String, r: String) => Some(l + r)
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[String] =
		StringConcat(l.asInstanceOf[StringNode], r.asInstanceOf[StringNode])

	override def updateValues(contexts: Contexts): StringConcat = copy(
		lhs.updateValues(contexts),
		rhs.updateValues(contexts))

	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): StringConcat = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[StringNode],
			children.last.asInstanceOf[StringNode],
			reqVeq)
	}
}

case class MapGet(lhs: MapNode[String, Int], rhs: StringNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Int] with IntNode
{
	override protected val parenless: Boolean = true
	override lazy val code: String = lhs.parensIfNeeded + "[" + rhs.code + "]"

	override def doOp(l: Any, r: Any): Option[Int] = (l, r) match {
		case (map: Map[String, Int], key: String) => map.get(key)
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Int] =
		MapGet(l.asInstanceOf[MapNode[String, Int]], r.asInstanceOf[StringNode])

	override def updateValues(contexts: Contexts): MapGet = copy(lhs.updateValues(contexts), rhs.updateValues(contexts))

	override def updateChildren(children: Seq[ASTNode],reqVeq: List[Boolean] =List()): MapGet = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[MapNode[String, Int]],
			children.last.asInstanceOf[StringNode],
			reqVeq)
	}
}

case class StringMapGet(lhs: MapNode[String, String], rhs: StringNode,reqVeq: List[Boolean] =List()) extends BinaryOpNode[String] with StringNode
{
	override protected val parenless: Boolean = true
	override lazy val code: String = lhs.parensIfNeeded + "[" + rhs.code + "]"

	override def doOp(l: Any, r: Any): Option[String] = (l, r) match {
		case (map: Map[String, String], key: String) => map.get(key)
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[String] =
		StringMapGet(l.asInstanceOf[MapNode[String, String]], r.asInstanceOf[StringNode])

	override def updateValues(contexts: Contexts): StringMapGet = copy(lhs.updateValues(contexts), rhs.updateValues(contexts))

	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): StringMapGet = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[MapNode[String, String]],
			children.last.asInstanceOf[StringNode],
			reqVeq)
	}
}

case class IntAddition(lhs: IntNode, rhs: IntNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Int] with IntNode
{
	override lazy val code: String = lhs.code + " + " + rhs.code

	override def doOp(l: Any, r: Any): Option[Int] = (l, r) match {
		case (l: Int, r: Int) => Some(l + r)
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Int] =
		IntAddition(l.asInstanceOf[IntNode], r.asInstanceOf[IntNode])

	override def updateValues(contexts: Contexts): IntAddition = copy(lhs.updateValues(contexts), rhs.updateValues(contexts))

	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): IntAddition = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[IntNode],
			children.last.asInstanceOf[IntNode],
			reqVeq)

	}
}

case class IntMultiply(lhs: IntNode, rhs: IntNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Int] with IntNode
{
	override lazy val code: String = lhs.parensIfNeeded + " * " + rhs.parensIfNeeded

	override def doOp(l: Any, r: Any): Option[Int] = (l, r) match {
		case (l: Int, r: Int) => Some(l * r)
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Int] =
		IntMultiply(l.asInstanceOf[IntNode], r.asInstanceOf[IntNode])

	override def updateValues(contexts: Contexts): IntMultiply = copy(lhs.updateValues(contexts), rhs.updateValues(contexts))

	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): IntMultiply = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[IntNode],
			children.last.asInstanceOf[IntNode],
			reqVeq)
	}
}

case class StringMultiply(lhs: StringNode, rhs: IntNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[String] with StringNode
{
	override lazy val code: String = lhs.code + " * " + rhs.code

	override def doOp(l: Any, r: Any): Option[String] = (l, r) match {
		case (l: String, r: Int) => Some(l * r)
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[String] =
		StringMultiply(l.asInstanceOf[StringNode], r.asInstanceOf[IntNode])

	override def updateValues(contexts: Contexts): StringMultiply = copy(lhs.updateValues(contexts), rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): StringMultiply = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[StringNode],
			children.last.asInstanceOf[IntNode],
			reqVeq)
	}
}

case class IntSubtraction(lhs: IntNode, rhs: IntNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Int] with IntNode
{
	override lazy val code: String = lhs.code + " - " + rhs.code

	override def doOp(l: Any, r: Any): Option[Int] = (l, r) match {
		case (l: Int, r: Int) => Some(l - r)
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Int] =
		IntSubtraction(l.asInstanceOf[IntNode], r.asInstanceOf[IntNode])

	override def updateValues(contexts: Contexts): IntSubtraction = copy(lhs.updateValues(contexts), rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): IntSubtraction = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[IntNode],
			children.last.asInstanceOf[IntNode],
			reqVeq)
	}
}

case class IntDivision(lhs: IntNode, rhs: IntNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Int] with IntNode
{
	override lazy val code: String = lhs.parensIfNeeded + " // " + rhs.parensIfNeeded

	override def doOp(l: Any, r: Any): Option[Int] =
		(l, r) match {
			case (_: Int, 0) => None
			case (l: Int, r: Int) => Some(Math.floorDiv(l, r))
			case _ => wrongType(l, r)
		}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Int] =
		IntDivision(l.asInstanceOf[IntNode], r.asInstanceOf[IntNode])

	override def updateValues(contexts: Contexts): IntDivision = copy(lhs.updateValues(contexts), rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): IntDivision = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[IntNode],
			children.last.asInstanceOf[IntNode],
			reqVeq)
	}
}

case class Modulo(lhs: IntNode, rhs: IntNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Int] with IntNode
{
	override lazy val code: String = lhs.parensIfNeeded + " % " + rhs.parensIfNeeded

	override def doOp(l: Any, r: Any): Option[Int] =
		(l, r) match {
			case (_: Int, 0) => None
			case (l: Int, r: Int) => Some(Math.floorMod(l, r))
			case _ => wrongType(l, r)
		}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Int] =
		Modulo(l.asInstanceOf[IntNode], r.asInstanceOf[IntNode])

	override def updateValues(contexts: Contexts): Modulo = copy(
		lhs.updateValues(contexts),
		rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): Modulo = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[IntNode],
			children.last.asInstanceOf[IntNode],
			reqVeq)
	}
}

case class Find(lhs: StringNode, rhs: StringNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Int] with IntNode
{
	override protected val parenless: Boolean = true
	override lazy val code: String = lhs.parensIfNeeded + ".find(" + rhs.code + ")"

	override def doOp(l: Any, r: Any): Option[Int] = (l, r) match {
		case (l: String, r: String) => Some(l.indexOf(r))
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Int] =
		Find(l.asInstanceOf[StringNode], r.asInstanceOf[StringNode])

	override def updateValues(contexts: Contexts): Find = copy(lhs.updateValues(contexts), rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): Find = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[StringNode],
			children.last.asInstanceOf[StringNode],
			reqVeq)
	}
}

case class Contains(lhs: StringNode, rhs: StringNode, reqVeq: List[Boolean] = List()) extends BinaryOpNode[Boolean] with BoolNode
{
	override lazy val code: String = lhs.parensIfNeeded + " in " + rhs.parensIfNeeded

	override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
		case (substr: String, str: String) => Some(str.contains(substr))
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
		Contains(l.asInstanceOf[StringNode], r.asInstanceOf[StringNode])

	override def updateValues(contexts: Contexts): Contains = copy(lhs.updateValues(contexts), rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): Contains = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[StringNode],
			children.last.asInstanceOf[StringNode],
			reqVeq)
	}
}

case class StringSplit(lhs: StringNode, rhs: StringNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Iterable[String]] with StringListNode
{
	override protected val parenless: Boolean = true
	override lazy val code: String = lhs.parensIfNeeded + ".split(" + rhs.code + ")"

	override def doOp(l: Any, r: Any): Option[Iterable[String]] = (l, r) match {
		case (_, "") => None
		case (l: String, r: String) => {
			if(l == r) Some(List("", "")) // imagine how much fun was it to find that scala's split is not like python's
			else {
				try {
					Some(l.split(r).toList)
				}
				catch {
					case e: Exception => None // probobly cause r is not a valid regex
				}
			}
		}
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Iterable[String]] =
		StringSplit(l.asInstanceOf[StringNode], r.asInstanceOf[StringNode])

	override def updateValues(contexts: Contexts): StringSplit = copy(
		lhs.updateValues(contexts),
		rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): StringSplit = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[StringNode],
			children.last.asInstanceOf[StringNode],
			reqVeq)
	}
}

case class StringJoin(lhs: StringNode, rhs: ListNode[String], reqVeq: List[Boolean] =List()) extends BinaryOpNode[String] with StringNode
{
	override protected val parenless: Boolean = true
	override lazy val code: String = lhs.parensIfNeeded + ".join(" + rhs.code + ")"

	override def doOp(l: Any, r: Any): Option[String] = (l, r) match {
		case (str: String, lst: Iterable[_]) => Some(lst.mkString(str))
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[String] =
		StringJoin(l.asInstanceOf[StringNode], r.asInstanceOf[ListNode[String]])

	override def updateValues(contexts: Contexts): StringJoin = copy(lhs.updateValues(contexts), rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): StringJoin = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[StringNode],
			children.last.asInstanceOf[ListNode[String]],
			reqVeq)
	}
}

case class Count(lhs: StringNode, rhs: StringNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Int] with IntNode
{
	override protected val parenless: Boolean = true
	override lazy val code: String = lhs.parensIfNeeded + ".count(" + rhs.code + ")"

	override def doOp(l: Any, r: Any): Option[Int] = (l, r) match {
		case ("", _) => Some(0)
		case (l: String, "") => Some(l.length + 1)
		case (l: String, r: String) =>
			var count = 0
			var i = 0
			while (i != -1) {
				val nextInstance = l.indexOf(r, i)
				if (nextInstance > -1) {
					count += 1
					i = nextInstance + r.length
				}
				else {
					i = -1
				}
			}
			Some(count)
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Int] =
		Count(l.asInstanceOf[StringNode], r.asInstanceOf[StringNode])

	override def updateValues(contexts: Contexts): Count = copy(lhs.updateValues(contexts), rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): Count = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[StringNode],
			children.last.asInstanceOf[StringNode],
			reqVeq)
	}
}

case class BinarySubstring(lhs: StringNode, rhs: IntNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[String] with StringNode
{
	override protected val parenless: Boolean = true
	override lazy val code: String = lhs.parensIfNeeded + "[" + rhs.code + "]"

	override def doOp(l: Any, r: Any): Option[String] = (l, r) match {
		case (str: String, idx: Int) =>
			if (idx >= 0 && idx < str.length) Some(str(idx).toString)
			else if(idx < 0 && -idx <= str.length) Some(str(str.length+idx).toString)
			else None
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[String] =
		BinarySubstring(l.asInstanceOf[StringNode], r.asInstanceOf[IntNode])

	override def updateValues(contexts: Contexts): BinarySubstring = copy(lhs.updateValues(contexts), rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): BinarySubstring = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[StringNode],
			children.last.asInstanceOf[IntNode],
			reqVeq)
	}
}

case class StartsWith(lhs: StringNode, rhs: StringNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Boolean] with BoolNode
{
	override protected val parenless: Boolean = true
	override lazy val code: String = lhs.code + ".startswith(" + rhs.code + ")"

	override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
		case (l: String, r: String) => Some(l.startsWith(r))
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
		StartsWith(l.asInstanceOf[StringNode], r.asInstanceOf[StringNode])

	override def updateValues(contexts: Contexts): StartsWith = copy(lhs.updateValues(contexts), rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): StartsWith = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[StringNode],
			children.last.asInstanceOf[StringNode],
			reqVeq)
	}
}

case class EndsWith(lhs: StringNode, rhs: StringNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Boolean] with BoolNode
{
	override protected val parenless: Boolean = true
	override lazy val code: String = lhs.code + ".endswith(" + rhs.code + ")"

	override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
		case (l: String, r: String) => Some(l.endsWith(r))
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
		EndsWith(l.asInstanceOf[StringNode], r.asInstanceOf[StringNode])

	override def updateValues(contexts: Contexts): EndsWith = copy(lhs.updateValues(contexts), rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): EndsWith = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[StringNode],
			children.last.asInstanceOf[StringNode],
			reqVeq)
	}
}

case class StringStep(lhs: StringNode, rhs: IntNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[String] with StringNode
{
	override protected val parenless: Boolean = true
	override lazy val code: String = lhs.parensIfNeeded + "[::" + rhs.code + "]"

	override def doOp(l: Any, r: Any): Option[String] = (l, r) match {
		case (_, _: 0) => None
		case (str: String, step: Int) =>
			val rs: StringBuilder = new StringBuilder(Math.abs(str.length / step) + 1)
			var idx = if (step > 0) 0 else str.length - 1
			while (idx >= 0 && idx < str.length) {
				rs += str(idx)
				idx += step
			}
			Some(rs.toString)
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[String] =
		StringStep(l.asInstanceOf[StringNode], r.asInstanceOf[IntNode])

	override def updateValues(contexts: Contexts): StringStep = copy( lhs.updateValues(contexts), rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): StringStep = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[StringNode],
			children.last.asInstanceOf[IntNode],
			reqVeq)
	}
}

case class ListStep[T](lhs: ListNode[T], rhs: IntNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Iterable[T]] with ListNode[T]
{
	override val childType: Types = lhs.childType
	override protected val parenless: Boolean = true
	override lazy val code: String = lhs.parensIfNeeded + "[::" + rhs.code + "]"

	override def doOp(l: Any, r: Any): Option[Iterable[T]] = (l, r) match {
		case (_, _: 0) => None
		case (str: List[T], step: Int) =>
			var rs = List[T]()
			var idx = if (step > 0) 0 else str.length - 1
			while (idx >= 0 && idx < str.length) {
				rs = rs :+ str(idx)
				idx += step
			}
			Some(rs)
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Iterable[T]] =
		ListStep[T](l.asInstanceOf[ListNode[T]], r.asInstanceOf[IntNode])

	override def updateValues(contexts: Contexts): ListStep[T] =
		copy(lhs.updateValues(contexts), rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): ListStep[T] = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[ListNode[T]],
			children.last.asInstanceOf[IntNode],
			reqVeq)
	}
}

abstract sealed class ListLookup[T](lhs: ListNode[T], rhs: IntNode) extends BinaryOpNode[T]
{
	override protected val parenless: Boolean = true
	override val nodeType: Types = lhs.childType
	override val code: String = s"${lhs.parensIfNeeded}[${rhs.code}]"
	override def doOp(l: Any, r: Any): Option[T] = (l, r) match {
		case (lst: List[T], idx: Int) =>
			if (idx >= 0 && idx < lst.length) Some(lst(idx))
			else if (idx<0 && -idx <= lst.length) Some(lst(lst.length + idx)) // negatieve indexing
			else None
		case _ => wrongType(l, r)
	}
}

case class StringListLookup(lhs: ListNode[String], rhs: IntNode, reqVeq: List[Boolean] =List()) extends ListLookup[String](lhs, rhs) with StringNode
{

	override def make(l: ASTNode, r: ASTNode): ListLookup[String] =
		StringListLookup(l.asInstanceOf[ListNode[String]], r.asInstanceOf[IntNode])

	override def updateValues(contexts: Contexts): StringListLookup = copy(lhs.updateValues(contexts), rhs.updateValues(contexts))


	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): StringListLookup = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[ListNode[String]],
			children.last.asInstanceOf[IntNode],
			reqVeq)
	}
}

case class IntListLookup(lhs: ListNode[Int], rhs: IntNode, reqVeq: List[Boolean] =List()) extends ListLookup[Int](lhs, rhs) with IntNode
{

	override def make(l: ASTNode, r: ASTNode): ListLookup[Int] =
		IntListLookup(l.asInstanceOf[ListNode[Int]], r.asInstanceOf[IntNode])

	override def updateValues(contexts: Contexts): IntListLookup = copy(lhs.updateValues(contexts), rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): IntListLookup = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[ListNode[Int]],
			children.last.asInstanceOf[IntNode],
			reqVeq)
	}
}

case class DoubleListLookup(lhs: ListNode[Double], rhs: IntNode, reqVeq: List[Boolean] =List()) extends ListLookup[Double](lhs,rhs) with DoubleNode{

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Double] =
		DoubleListLookup(l.asInstanceOf[ListNode[Double]], r.asInstanceOf[IntNode])
	override def updateValues(contexts: Contexts): DoubleNode = copy(lhs.updateValues(contexts), rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): DoubleNode = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[ListNode[Double]],
			children.last.asInstanceOf[IntNode],
			reqVeq)
	}
}

case class ListContains[T, E <: ASTNode](lhs: E, rhs: ListNode[T], reqVeq: List[Boolean] =List()) extends BinaryOpNode[Boolean] with BoolNode
{

	override protected val parenless: Boolean = false
	override val code: String = s"${lhs.parensIfNeeded} in ${rhs.parensIfNeeded}"

	override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
		case (e: T, r: List[T]) => Some(r.contains(e))
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
		ListContains[T, E](l.asInstanceOf[E], r.asInstanceOf[ListNode[T]])

	override def updateValues(contexts: Contexts): ListContains[T, E] =
		copy(lhs.updateValues(contexts).asInstanceOf[E], rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): ListContains[T, E] = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[E],
			children.last.asInstanceOf[ListNode[T]],
			reqVeq)
	}
}

case class ListConcat[T](lhs: ListNode[T], rhs: ListNode[T], reqVeq: List[Boolean] =List()) extends BinaryOpNode[Iterable[T]] with ListNode[T]
{
	override val childType: Types = lhs.childType
	override val code: String = s"${lhs.parensIfNeeded} + ${rhs.parensIfNeeded}"
	override protected val parenless: Boolean = false

	override def doOp(l: Any, r: Any): Option[Iterable[T]] = (l, r) match {
		case (l: List[T], r: List[T]) => Some(l ++ r)
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Iterable[T]] =
		ListConcat[T](l.asInstanceOf[ListNode[T]], r.asInstanceOf[ListNode[T]])

	override def updateValues(contexts: Contexts): ListConcat[T] =
		copy(lhs.updateValues(contexts), rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): ListConcat[T] = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[ListNode[T]],
			children.last.asInstanceOf[ListNode[T]],
			reqVeq)
	}
}

case class ListAppend[T, E <: ASTNode](lhs: ListNode[T], rhs: E, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Iterable[T]] with ListNode[T]
{
	override val childType: Types = lhs.childType
	override protected val parenless: Boolean = false
	override val code: String = s"${lhs.parensIfNeeded} + [${rhs.code}]"

	override def doOp(l: Any, r: Any): Option[Iterable[T]] = (l, r) match {
		case (lst: List[T], elem: T) => Some(lst :+ elem)
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Iterable[T]] =
		ListAppend(l.asInstanceOf[ListNode[T]], r.asInstanceOf[E])

	override def updateValues(contexts: Contexts): ListAppend[T, E] =
		copy(lhs.updateValues(contexts), rhs.updateValues(contexts).asInstanceOf[E])
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): ListAppend[T, E] = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[ListNode[T]],
			children.last.asInstanceOf[E],
			reqVeq)
	}
}


case class SetAppend[T, E <: ASTNode](lhs: SetNode[T], rhs: E, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Set[T]] with SetNode[T]
{

	override def doOp(l: Any, r: Any): Option[Set[T]] = (l,r) match {
		case (s: Set[T], elem: T) => Some(s + elem)
		case _ => wrongType(l,r)
	}

	override def make(l: ASTNode, r: ASTNode): SetAppend[T,E] =
		SetAppend(l.asInstanceOf[SetNode[T]], r.asInstanceOf[E])

	override val childType: Types = lhs.childType

	override def updateValues(contexts: Contexts): SetNode[T] =
		copy(lhs.updateValues(contexts), rhs.updateValues(contexts).asInstanceOf[E])

	override val code: String = s"${lhs.code} || {${rhs.code}}"

	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): SetNode[T] = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[SetNode[T]],
			children.last.asInstanceOf[E],
			reqVeq)
	}
}

case class ListPrepend[T, E <: ASTNode](lhs: E, rhs: ListNode[T], reqVeq: List[Boolean] =List()) extends BinaryOpNode[Iterable[T]] with ListNode[T]
{

	override val childType: Types = rhs.childType
	override protected val parenless: Boolean = false
	override val code: String = s"[${lhs.code}] + ${rhs.parensIfNeeded}"

	override def doOp(l: Any, r: Any): Option[Iterable[T]] = (l, r) match {
		case (elem: T, lst: List[T]) => Some(elem :: lst)
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Iterable[T]] =
		ListPrepend[T, E](l.asInstanceOf[E], r.asInstanceOf[ListNode[T]])

	override def updateValues(contexts: Contexts): ListPrepend[T, E] =
		copy(lhs.updateValues(contexts).asInstanceOf[E], rhs.updateValues(contexts))

	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): ListPrepend[T, E] = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[E],
			children.last.asInstanceOf[ListNode[T]],
			reqVeq)
	}
}

case class LessThanEqDoubles(lhs: DoubleNode, rhs: DoubleNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Boolean] with BoolNode
{

	override lazy val code: String = lhs.code + " <= " + rhs.code

	override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
		case (l: Double, r: Double) => Some(l <= r)
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
		LessThanEqDoubles(l.asInstanceOf[DoubleNode], r.asInstanceOf[DoubleNode])

	override def updateValues(contexts: Contexts): LessThanEqDoubles = copy(
		lhs.updateValues(contexts),
		rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): LessThanEqDoubles = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[DoubleNode],
			children.last.asInstanceOf[DoubleNode],
			reqVeq)
	}
}

case class GreaterThanDoubles(lhs: DoubleNode, rhs: DoubleNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Boolean] with BoolNode
{
	override lazy val code: String = lhs.code + " > " + rhs.code

	override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
		case (l: Double, r: Double) => Some(l > r)
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
		GreaterThanDoubles(l.asInstanceOf[DoubleNode], r.asInstanceOf[DoubleNode])

	override def updateValues(contexts: Contexts): GreaterThanDoubles = copy(
		lhs.updateValues(contexts),
		rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): GreaterThanDoubles = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[DoubleNode],
			children.last.asInstanceOf[DoubleNode],
			reqVeq)
	}
}

case class LessThanEqDoubleInt(lhs: DoubleNode, rhs: IntNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Boolean] with BoolNode
{
	override lazy val code: String = lhs.code + " <= " + rhs.code

	override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
		case (l: Double, r: Int) => Some(l <= r)
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
		LessThanEqDoubleInt(l.asInstanceOf[DoubleNode], r.asInstanceOf[IntNode])

	override def updateValues(contexts: Contexts): LessThanEqDoubleInt = copy(
		lhs.updateValues(contexts),
		rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): LessThanEqDoubleInt = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[DoubleNode],
			children.last.asInstanceOf[IntNode],
			reqVeq)
	}
}

case class GreaterThanDoubleInt(lhs: DoubleNode, rhs: IntNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Boolean] with BoolNode
{
	override lazy val code: String = lhs.code + " > " + rhs.code

	override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
		case (l: Double, r: Int) => Some(l > r)
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
		GreaterThanDoubleInt(l.asInstanceOf[DoubleNode], r.asInstanceOf[IntNode])

	override def updateValues(contexts: Contexts): GreaterThanDoubleInt = copy(
		lhs.updateValues(contexts),
		rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): GreaterThanDoubleInt = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[DoubleNode],
			children.last.asInstanceOf[IntNode],
			reqVeq)
	}
}
case class LessThanEqIntDouble(lhs: IntNode, rhs: DoubleNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Boolean] with BoolNode
{
	override lazy val code: String = lhs.code + " <= " + rhs.code

	override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
		case (l: Int, r: Double) => Some(l <= r)
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
		LessThanEqIntDouble(l.asInstanceOf[IntNode], r.asInstanceOf[DoubleNode])

	override def updateValues(contexts: Contexts): LessThanEqIntDouble = copy(
		lhs.updateValues(contexts),
		rhs.updateValues(contexts))

	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): LessThanEqIntDouble = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[IntNode],
			children.last.asInstanceOf[DoubleNode],
			reqVeq)
	}
}

case class GreaterThanIntDouble(lhs: IntNode, rhs: DoubleNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Boolean] with BoolNode
{
	override lazy val code: String = lhs.code + " > " + rhs.code

	override def doOp(l: Any, r: Any): Option[Boolean] = (l, r) match {
		case (l: Int, r: Double) => Some(l > r)
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
		GreaterThanIntDouble(l.asInstanceOf[IntNode], r.asInstanceOf[DoubleNode])

	override def updateValues(contexts: Contexts): GreaterThanIntDouble = copy(
		lhs.updateValues(contexts),
		rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): GreaterThanIntDouble = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[IntNode],
			children.last.asInstanceOf[DoubleNode],
			reqVeq)
	}
}

case class DoublesAddition(lhs: DoubleNode, rhs: DoubleNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Double] with DoubleNode
{
	override lazy val code: String = lhs.code + " + " + rhs.code

	override def doOp(l: Any, r: Any): Option[Double] = (l, r) match {
		case (l: Double, r: Double) => Some(l + r)
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Double] =
		DoublesAddition(l.asInstanceOf[DoubleNode], r.asInstanceOf[DoubleNode])

	override def updateValues(contexts: Contexts): DoublesAddition = copy(lhs.updateValues(contexts), rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): DoublesAddition = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[DoubleNode],
			children.last.asInstanceOf[DoubleNode],
			reqVeq)
	}
}

case class DoublesMultiply(lhs: DoubleNode, rhs: DoubleNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Double] with DoubleNode
{
	override lazy val code: String = lhs.parensIfNeeded + " * " + rhs.parensIfNeeded

	override def doOp(l: Any, r: Any): Option[Double] = (l, r) match {
		case (l: Double, r: Double) => Some(l * r)
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Double] =
		DoublesMultiply(l.asInstanceOf[DoubleNode], r.asInstanceOf[DoubleNode])

	override def updateValues(contexts: Contexts): DoublesMultiply = copy(lhs.updateValues(contexts), rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): DoublesMultiply = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[DoubleNode],
			children.last.asInstanceOf[DoubleNode],
			reqVeq)
	}
}

case class DoublesSubtraction(lhs: DoubleNode, rhs: DoubleNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Double] with DoubleNode
{
	override lazy val code: String = lhs.code + " - " + rhs.code

	override def doOp(l: Any, r: Any): Option[Double] = (l, r) match {
		case (l: Double, r: Double) => Some(l - r)
		case _ => wrongType(l, r)
	}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Double] =
		DoublesSubtraction(l.asInstanceOf[DoubleNode], r.asInstanceOf[DoubleNode])

	override def updateValues(contexts: Contexts): DoublesSubtraction = copy(lhs.updateValues(contexts), rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): DoublesSubtraction = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[DoubleNode],
			children.last.asInstanceOf[DoubleNode],
			reqVeq)
	}
}

case class DoublesDivision(lhs: DoubleNode, rhs: DoubleNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Double] with DoubleNode
{
	override lazy val code: String = lhs.parensIfNeeded + " / " + rhs.parensIfNeeded

	override def doOp(l: Any, r: Any): Option[Double] =
		(l, r) match {
			case (_: Double, 0) => None
			case (l: Double, r: Double) => Some(l/r)
			case _ => wrongType(l, r)
		}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Double] =
		DoublesDivision(l.asInstanceOf[DoubleNode], r.asInstanceOf[DoubleNode])

	override def updateValues(contexts: Contexts): DoublesDivision = copy(lhs.updateValues(contexts), rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): DoublesDivision = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[DoubleNode],
			children.last.asInstanceOf[DoubleNode],
			reqVeq)
	}
}

case class LAnd(lhs: BoolNode, rhs: BoolNode, reqVeq: List[Boolean] =List()) extends BinaryOpNode[Boolean] with BoolNode
{
	override val code: String = lhs.code + " and " + rhs.code

	override def doOp(l: Any, r: Any): Option[Boolean] =
		(l, r) match {
			case (l: Boolean, r: Boolean) => Some(l && r)
			case _ => wrongType(l, r)
		}

	override def make(l: ASTNode, r: ASTNode): BinaryOpNode[Boolean] =
		LAnd(l.asInstanceOf[BoolNode], r.asInstanceOf[BoolNode])

	override def updateValues(contexts: Contexts): LAnd = copy(lhs.updateValues(contexts), rhs.updateValues(contexts))
	override def updateChildren(children: Seq[ASTNode], reqVeq: List[Boolean] =List()): LAnd = {
		assert(children.length == 2)
		copy(children.head.asInstanceOf[BoolNode],
			children.last.asInstanceOf[BoolNode],
			reqVeq)
	}
}