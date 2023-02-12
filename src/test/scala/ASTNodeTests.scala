import edu.ucsd.snippy.ast._
import edu.ucsd.snippy.enumeration.Contexts
import org.junit.Assert._
import org.junit.Test
import org.scalatestplus.junit.JUnitSuite

class ASTNodeTests extends JUnitSuite
{
	@Test def stringLiteralNode(): Unit =
	{
		val literal: StringLiteral = StringLiteral("abc", 1)
		assertEquals(1, literal.exampleValues.length)
		assertEquals("abc", literal.exampleValues.head.get)
		assertEquals(Types.String, literal.nodeType)
		assertEquals("\"abc\"", literal.code)
		assertEquals(0, literal.height)
		assertEquals(1, literal.terms)
		assertTrue(literal.children.isEmpty)
	}

	@Test def stringLiteralEscaping(): Unit =
	{
		assertEquals("\"a\\tb\\r\\n\"", StringLiteral("a\tb\r\n", 1).code)
		assertEquals("\"a\\\\tb\\\\r\\\\n\"", StringLiteral("a\\tb\\r\\n", 1).code)
		assertEquals("\"a\\\"b\\\"c\"", StringLiteral("a\"b\"c", 1).code)
		assertEquals("\"\\xd83d\\xdca9\"", StringLiteral("\uD83D\uDCA9", 1).code)
	}

	@Test def intLiteralNode(): Unit =
	{
		val literal: IntLiteral = IntLiteral(42, 2)
		assertEquals(2, literal.exampleValues.length)
		assertEquals(42, literal.exampleValues.head.get)
		assertEquals(Types.Int, literal.nodeType)
		assertEquals("42", literal.code)
		assertEquals(0, literal.height)
		assertEquals(1, literal.terms)
		assertTrue(literal.children.isEmpty)
	}

	@Test def boolLiteralNode(): Unit =
	{
		var literal: BoolLiteral = BoolLiteral(value = false, 3)
		assertEquals(3, literal.exampleValues.length)
		assertEquals(false, literal.exampleValues.head.get)
		assertEquals(Types.Bool, literal.nodeType)
		assertEquals("False", literal.code)
		assertEquals(0, literal.height)
		assertEquals(1, literal.terms)
		assertTrue(literal.children.isEmpty)

		literal = BoolLiteral(value = true, 4)
		assertEquals(4, literal.exampleValues.length)
		assertEquals(true, literal.exampleValues.head.get)
		assertEquals(Types.Bool, literal.nodeType)
		assertEquals("True", literal.code)
		assertEquals(0, literal.height)
		assertEquals(1, literal.terms)
		assertTrue(literal.children.isEmpty)
	}

	@Test def doubleLiteralNode(): Unit = {
		val literal: DoubleLiteral = DoubleLiteral(84.72, 1)
		assertTrue(literal.isInstanceOf[DoubleNode])
		assertEquals(1,literal.exampleValues.length)
		//assertEquals(84.72, literal.exampleValues.head.get,0.0)
		assertEquals(84.72, literal.exampleValues.head.get)
		assertEquals(Types.Double, literal.nodeType)
		assertEquals("84.72", literal.code)
		assertEquals(0, literal.height)
		assertEquals(1, literal.terms)
		assertTrue(literal.children.isEmpty)
	}

	@Test def listOfDoublesLiteralNode(): Unit = {
		val literal: ListLiteral[Double] = ListLiteral(Types.Double, List(1.0,-1.0,0.0),1)
		assertEquals(1, literal.exampleValues.length)
		assertEquals(List(1.0,-1.0,0.0), literal.exampleValues.head.get)
		assertEquals("[1.0, -1.0, 0.0]", literal.code)
		assertEquals(0, literal.height)
		assertEquals(1, literal.terms)
		assertTrue(literal.children.isEmpty)
	}

	@Test def intToStringNode(): Unit =
	{
		val node: IntToString = IntToString(IntLiteral(83, 1))
		assertEquals(1, node.exampleValues.length)
		assertEquals("83", node.exampleValues.head.get)
		assertEquals(Types.String, node.nodeType)
		assertEquals("str(83)", node.code)
		assertEquals(1, node.height)
		assertEquals(2, node.terms)
		assertEquals(node.children.size, 1)
	}

	@Test def stringToIntNode(): Unit =
	{
		val node: StringToInt = StringToInt(StringLiteral("83", 1))
		assertEquals(1, node.exampleValues.length)
		assertEquals(83, node.exampleValues.head.get)
		assertEquals(Types.Int, node.nodeType)
		assertEquals("int(\"83\")", node.code)
		assertEquals(1, node.height)
		assertEquals(2, node.terms)
		assertEquals(node.children.size, 1)
	}

	@Test def stringLowerNode(): Unit =
	{
		var node: StringLower = StringLower(StringLiteral("aBC", 1))
		assertEquals(1, node.exampleValues.length)
		assertEquals("abc", node.exampleValues.head.get)
		assertEquals(Types.String, node.nodeType)
		assertEquals("\"aBC\".lower()", node.code)
		assertEquals(1, node.height)
		assertEquals(2, node.terms)
		assertEquals(node.children.size, 1)

		node = StringLower(StringConcat(StringLiteral("aBC", 1), StringLiteral("deF", 1)))
		assertEquals(1, node.exampleValues.length)
		assertEquals("abcdef", node.exampleValues.head.get)
		assertEquals(Types.String, node.nodeType)
		assertEquals("(\"aBC\" + \"deF\").lower()", node.code)
		assertEquals(2, node.height)
		assertEquals(4, node.terms)
		assertEquals(node.children.size, 1)
	}

	@Test def intMultiplication(): Unit =
	{
		val multiplyNumbers =
			IntMultiply(IntMultiply(IntLiteral(1, 1), IntLiteral(2, 1)),
			            IntMultiply(IntLiteral(3, 1), IntLiteral(4, 1)))
		assertEquals("(1 * 2) * (3 * 4)", multiplyNumbers.code)
		assertEquals(24, multiplyNumbers.exampleValues.head.get)
	}

	@Test def stringMultiplication(): Unit =
	{
		val multiply = StringMultiply(StringLiteral("a", 1), IntLiteral(3, 1))
		assertEquals("\"a\" * 3", multiply.code)
		assertEquals("aaa", multiply.exampleValues.head.get)
	}

	@Test def alpha(): Unit =
	{
		val isAlpha1 = IsAlpha(StringLiteral("abc", 1))
		val isAlpha2 = IsAlpha(StringLiteral("a123", 1))
		val isAlpha3 = IsAlpha(StringLiteral("a ", 1))
		val isAlpha4 = IsAlpha(StringLiteral("a%*", 1))
		assertEquals(true, isAlpha1.exampleValues.head.get)
		assertEquals(false, isAlpha2.exampleValues.head.get)
		assertEquals(false, isAlpha3.exampleValues.head.get)
		assertEquals(false, isAlpha4.exampleValues.head.get)
	}

	@Test def capitalize(): Unit =
	{
		val caps1 = Capitalize(StringLiteral("abc", 1))
		assertEquals("Abc", caps1.exampleValues.head.get)
	}

	@Test def concat(): Unit =
	{
		val code1 = ListConcat(new IntListNode
		{
			override val _values: List[Option[Iterable[Int]]] = List(-1123 :: 2 :: 1 :: Nil).map(Some(_))
			override protected val parenless: Boolean = true
			override val code: String = "[-1123, 2, 1]"
			override val height: Int = 1
			override val terms: Int = 1
			override val children: Iterable[ASTNode] = Nil

			override def includes(varName: String): Boolean = false

			override lazy val usesVariables: Boolean = false

			override def updateValues(contexts: Contexts): IntListNode = ???


		}, new IntListNode
		{
			override val _values: List[Option[Iterable[Int]]] = List(-2212 :: 3 :: 2 :: Nil).map(Some(_))
			override protected val parenless: Boolean = true
			override val code: String = "[-1123, 2, 1]"
			override val height: Int = 1
			override val terms: Int = 1
			override val children: Iterable[ASTNode] = Nil

			override def includes(varName: String): Boolean = false

			override lazy val usesVariables: Boolean = false

			override def updateValues(contexts: Contexts): IntListNode = ???
		})
		val code2 = ListConcat(new StringListNode
		{
			override val _values: List[Option[Iterable[String]]] = List("abc" :: "def" :: Nil).map(Some(_))
			override protected val parenless: Boolean = true
			override val code: String = "['abc', 'def']"
			override val height: Int = 1
			override val terms: Int = 1
			override val children: Iterable[ASTNode] = Nil

			override def includes(varName: String): Boolean = false

			override lazy val usesVariables: Boolean = false

			override def updateValues(contexts: Contexts): StringListNode = ???
		}, new StringListNode
		{
			override val _values: List[Option[Iterable[String]]] = List("ghi" :: "jkl" :: Nil).map(Some(_))
			override protected val parenless: Boolean = true
			override val code: String = "['ghi', 'jkl']"
			override val height: Int = 1
			override val terms: Int = 1
			override val children: Iterable[ASTNode] = Nil

			override def includes(varName: String): Boolean = false

			override lazy val usesVariables: Boolean = false

			override def updateValues(contexts: Contexts): StringListNode = ???
		})
		assertEquals(List(-1123, 2, 1, -2212, 3, 2), code1.exampleValues.head.get)
		assertEquals(List("abc", "def", "ghi", "jkl"), code2.exampleValues.head.get)
	}

	@Test def lookupTest(): Unit = {
		val code1 = IntListLookup(new IntListNode
		{
			override val _values: List[Option[Iterable[Int]]] = List(-1123 :: 2 :: 1 :: Nil).map(Some(_))
			override protected val parenless: Boolean = true
			override val code: String = "[-1123, 2, 1]"
			override val height: Int = 1
			override val terms: Int = 1
			override val children: Iterable[ASTNode] = Nil

			override def includes(varName: String): Boolean = false

			override lazy val usesVariables: Boolean = false

			override def updateValues(contexts: Contexts): IntListNode = ???
		}, IntLiteral(0, 1))
		val code2 = StringListLookup(new StringListNode
		{
			override val _values: List[Option[Iterable[String]]] = List("abc" :: "def" :: Nil).map(Some(_))
			override protected val parenless: Boolean = true
			override val code: String = "['abc', 'def']"
			override val height: Int = 1
			override val terms: Int = 1
			override val children: Iterable[ASTNode] = Nil

			override def includes(varName: String): Boolean = false

			override lazy val usesVariables: Boolean = false

			override def updateValues(contexts: Contexts): StringListNode = ???
		}, IntLiteral(1, 1))
		assertEquals(-1123, code1.exampleValues.head.get)
		assertEquals("def", code2.exampleValues.head.get)
	}

	@Test def containsTest(): Unit = {
		val code1 = ListContains[Int, IntNode](IntLiteral(0, 1), new IntListNode {
			override val _values: List[Option[Iterable[Int]]] = List(-1123 :: 2 :: 1 :: Nil).map(Some(_))
			override protected val parenless: Boolean = true
			override val code: String = "[-1123, 2, 1]"
			override val height: Int = 1
			override val terms: Int = 1
			override val children: Iterable[ASTNode] = Nil

			override def includes(varName: String): Boolean = false

			override lazy val usesVariables: Boolean = false

			override def updateValues(contexts: Contexts): IntListNode = ???
		})
		val code2 = ListContains[String, StringNode](StringLiteral("abc", 1), new StringListNode
		{
			override val _values: List[Option[Iterable[String]]] = List("abc" :: "def" :: Nil).map(Some(_))
			override protected val parenless: Boolean = true
			override val code: String = "['abc', 'def']"
			override val height: Int = 1
			override val terms: Int = 1
			override val children: Iterable[ASTNode] = Nil

			override def includes(varName: String): Boolean = false

			override lazy val usesVariables: Boolean = false

			override def updateValues(contexts: Contexts): StringListNode = ???
		})
		assertEquals(false, code1.exampleValues.head.get)
		assertEquals(true, code2.exampleValues.head.get)
	}

	@Test def appendTest(): Unit = {
		val code1 = ListAppend[Int, IntNode](new IntListNode
		{
			override val _values: List[Option[Iterable[Int]]] = List(-1123 :: 2 :: 1 :: Nil).map(Some(_))
			override protected val parenless: Boolean = true
			override val code: String = "[-1123, 2, 1]"
			override val height: Int = 1
			override val terms: Int = 1
			override val children: Iterable[ASTNode] = Nil

			override def includes(varName: String): Boolean = false

			override lazy val usesVariables: Boolean = false

			override def updateValues(contexts: Contexts): IntListNode = ???
		}, IntLiteral(123, 1))
		val code2 = ListAppend[String, StringNode](new StringListNode
		{
			override val _values: List[Option[Iterable[String]]] = List("abc" :: "def" :: Nil).map(Some(_))
			override protected val parenless: Boolean = true
			override val code: String = "['abc', 'def']"
			override val height: Int = 1
			override val terms: Int = 1
			override val children: Iterable[ASTNode] = Nil

			override def includes(varName: String): Boolean = false

			override lazy val usesVariables: Boolean = false

			override def updateValues(contexts: Contexts): StringListNode = ???
		}, StringLiteral("123", 1))
		assertEquals(List(-1123, 2, 1, 123), code1.exampleValues.head.get)
		assertEquals(List("abc", "def", "123"), code2.exampleValues.head.get)

	}

//	@Test def numeric(): Unit =
//	{
//		val isNumeric1 = IsNumeric(StringLiteral("abc123", 1))
//		val isNumeric2 = IsNumeric(StringLiteral("123", 1))
//		val isNumeric3 = IsNumeric(StringLiteral("123 ", 1))
//		val isNumeric4 = IsNumeric(StringLiteral("123%*", 1))
//		assertEquals(false, isNumeric1.exampleValues.head.get)
//		assertEquals("\"abc123\".isnumeric()", isNumeric1.code)
//		assertEquals(true, isNumeric2.exampleValues.head.get)
//		assertEquals(false, isNumeric3.exampleValues.head.get)
//		assertEquals(false, isNumeric4.exampleValues.head.get)
//	}

	@Test def startsWith(): Unit =
	{
		val StartsWith1 = StartsWith(StringLiteral("abc123", 1), StringLiteral("abc", 1))
		val StartsWith2 = StartsWith(StringLiteral("123", 1), StringLiteral("23", 1))
		val StartsWith3 = StartsWith(StringLiteral("start 123", 1), StringLiteral("start", 1))
		val EndsWith1 = EndsWith(StringLiteral("abc123", 1), StringLiteral("123", 1))
		val EndsWith2 = EndsWith(StringLiteral("123", 1), StringLiteral("3", 1))
		assertEquals(true, StartsWith1.exampleValues.head.get)
		assertEquals("\"abc123\".startswith(\"abc\")", StartsWith1.code)
		assertEquals(false, StartsWith2.exampleValues.head.get)
		assertEquals(true, StartsWith3.exampleValues.head.get)
		assertEquals("\"abc123\".endswith(\"123\")", EndsWith1.code)
		assertEquals(true, EndsWith1.exampleValues.head.get)
		assertEquals(true, EndsWith2.exampleValues.head.get)

	}

	@Test def maxNode(): Unit =
	{
		val node: Max = Max(new IntListNode
		{
			override val _values: List[Option[Iterable[Int]]] = List(-1123 :: 2 :: 1 :: Nil).map(Some(_))
			override protected val parenless: Boolean = true
			override val code: String = "[-1123, 2, 1]"
			override val height: Int = 1
			override val terms: Int = 1
			override val children: Iterable[ASTNode] = Nil

			override def includes(varName: String): Boolean = false

			override lazy val usesVariables: Boolean = false

			override def updateValues(contexts: Contexts): IntListNode = ???
		})
		assertEquals(1, node.exampleValues.length)
		assertEquals(2, node.exampleValues.head.get)
		assertEquals(Types.Int, node.nodeType)
		assertEquals("max([-1123, 2, 1])", node.code)
		assertEquals(2, node.height)
		assertEquals(2, node.terms)
		assertEquals(node.children.size, 1)
	}

	@Test def minNode(): Unit =
	{
		val node: Min = Min(new IntListNode
		{
			override val _values: List[Option[Iterable[Int]]] = List(-1123 :: 2 :: 1 :: Nil).map(Some(_))
			override protected val parenless: Boolean = true
			override val code: String = "[-1123, 2, 1]"
			override val height: Int = 1
			override val terms: Int = 1
			override val children: Iterable[ASTNode] = Nil

			override def includes(varName: String): Boolean = false

			override lazy val usesVariables: Boolean = false

			override def updateValues(contexts: Contexts): IntListNode = ???
		})
		assertEquals(1, node.exampleValues.length)
		assertEquals(-1123, node.exampleValues.head.get)
		assertEquals(Types.Int, node.nodeType)
		assertEquals("min([-1123, 2, 1])", node.code)
		assertEquals(2, node.height)
		assertEquals(2, node.terms)
		assertEquals(node.children.size, 1)
	}

	@Test def unarySplitNode(): Unit =
	{
		var node = UnarySplit(StringLiteral("abc", 1))
		assertEquals(1, node.exampleValues.length)
		assertEquals(List("abc"), node.exampleValues.head.get)
		assertEquals(Types.Iterable(Types.String), node.nodeType)
		assertEquals("\"abc\".split()", node.code)
		assertEquals(1, node.height)

		node = UnarySplit(StringLiteral("abc def", 1))
		assertEquals(1, node.exampleValues.length)
		assertEquals(List("abc", "def"), node.exampleValues.head.get)
		assertEquals(Types.Iterable(Types.String), node.nodeType)
		assertEquals("\"abc def\".split()", node.code)
		assertEquals(1, node.height)

		node = UnarySplit(StringLiteral("abc\tdef", 1))
		assertEquals(1, node.exampleValues.length)
		assertEquals(List("abc", "def"), node.exampleValues.head.get)
		assertEquals(Types.Iterable(Types.String), node.nodeType)
		assertEquals("\"abc\\tdef\".split()", node.code)
		assertEquals(1, node.height)

		node = UnarySplit(StringLiteral("abc\ndef", 1))
		assertEquals(1, node.exampleValues.length)
		assertEquals(List("abc", "def"), node.exampleValues.head.get)
		assertEquals(Types.Iterable(Types.String), node.nodeType)
		assertEquals("\"abc\\ndef\".split()", node.code)
		assertEquals(1, node.height)
	}

	// Binary Operations
	@Test def binarySubstringNode(): Unit =
	{
		val str: StringNode = StringLiteral("abc", 1)

		var node: BinarySubstring = BinarySubstring(str, IntLiteral(0, 1))
		assertEquals(1, node.exampleValues.length)
		assertEquals("a", node.exampleValues.head.get)
		assertEquals(Types.String, node.nodeType)
		assertEquals("\"abc\"[0]", node.code)
		assertEquals(1, node.height)
		assertEquals(3, node.terms)
		assertEquals(node.children.size, 2)

		node = BinarySubstring(str, IntLiteral(1, 1))
		assertEquals(1, node.exampleValues.length)
		assertEquals("b", node.exampleValues.head.get)
		assertEquals(Types.String, node.nodeType)
		assertEquals("\"abc\"[1]", node.code)
		assertEquals(1, node.height)
		assertEquals(3, node.terms)
		assertEquals(node.children.size, 2)

		node = BinarySubstring(str, IntLiteral(2, 1))
		assertEquals(1, node.exampleValues.length)
		assertEquals("c", node.exampleValues.head.get)
		assertEquals(Types.String, node.nodeType)
		assertEquals("\"abc\"[2]", node.code)
		assertEquals(1, node.height)
		assertEquals(3, node.terms)
		assertEquals(node.children.size, 2)

		node = BinarySubstring(str, IntLiteral(3, 1))
		assertEquals(1, node.exampleValues.length)
		assertEquals(None, node.exampleValues.head)
		assertEquals(Types.String, node.nodeType)
		assertEquals("\"abc\"[3]", node.code)
		assertEquals(1, node.height)
		assertEquals(3, node.terms)
		assertEquals(node.children.size, 2)
	}

	// Ternary Operations
	@Test def ternarySubstringNode(): Unit =
	{
		val str: StringNode = StringLiteral("abc", 1)
		var node: TernarySubstring = TernarySubstring(str, IntLiteral(0, 1), IntLiteral(3, 1))
		assertEquals(1, node.exampleValues.length)
		assertEquals("abc", node.exampleValues.head.get)
		assertEquals(Types.String, node.nodeType)
		assertEquals("\"abc\"[0:3]", node.code)
		assertEquals(1, node.height)
		assertEquals(4, node.terms)
		assertEquals(node.children.size, 3)

		// [-4, -3] -> ""
		node = TernarySubstring(
			str,
			IntLiteral(-4, 1),
			IntLiteral(-3, 1))
		assertEquals("", node.exampleValues.head.get)

		// [-4, -2] -> "a"
		node = TernarySubstring(
			str,
			IntLiteral(-4, 1),
			IntLiteral(-2, 1))
		assertEquals("a", node.exampleValues.head.get)

		// [-4, -1] -> "ab"
		node = TernarySubstring(
			str,
			IntLiteral(-4, 1),
			IntLiteral(-1, 1))
		assertEquals("ab", node.exampleValues.head.get)

		// [-4, 0]  -> ""
		node = TernarySubstring(
			str,
			IntLiteral(-4, 1),
			IntLiteral(0, 1))
		assertEquals("", node.exampleValues.head.get)

		// [-4, 1]  -> "a"
		node = TernarySubstring(
			str,
			IntLiteral(-4, 1),
			IntLiteral(1, 1))
		assertEquals("a", node.exampleValues.head.get)

		// [-4, 2]  -> "ab"
		node = TernarySubstring(
			str,
			IntLiteral(-4, 1),
			IntLiteral(2, 1))
		assertEquals("ab", node.exampleValues.head.get)

		// [-4, 3]  -> "abc"
		node = TernarySubstring(
			str,
			IntLiteral(-4, 1),
			IntLiteral(3, 1))
		assertEquals("abc", node.exampleValues.head.get)

		// [-4, 4]  -> "abc"
		node = TernarySubstring(
			str,
			IntLiteral(-4, 1),
			IntLiteral(4, 1))
		assertEquals("abc", node.exampleValues.head.get)

		// [0, -4]  -> ""
		node = TernarySubstring(
			str,
			IntLiteral(0, 1),
			IntLiteral(-4, 1))
		assertEquals("", node.exampleValues.head.get)

		// [0, -3]  -> ""
		node = TernarySubstring(
			str,
			IntLiteral(0, 1),
			IntLiteral(-3, 1))
		assertEquals("", node.exampleValues.head.get)

		// [0, -2]  -> "a"
		node = TernarySubstring(
			str,
			IntLiteral(0, 1),
			IntLiteral(-2, 1))
		assertEquals("a", node.exampleValues.head.get)

		// [0, -1]  -> "ab"
		node = TernarySubstring(
			str,
			IntLiteral(0, 1),
			IntLiteral(-1, 1))
		assertEquals("ab", node.exampleValues.head.get)

		// [0, 0]  -> ""
		node = TernarySubstring(
			str,
			IntLiteral(0, 1),
			IntLiteral(0, 1))
		assertEquals("", node.exampleValues.head.get)

		// [0, 1]  -> "a"
		node = TernarySubstring(
			str,
			IntLiteral(0, 1),
			IntLiteral(1, 1))
		assertEquals("a", node.exampleValues.head.get)

		// [0, 2]  -> "ab"
		node = TernarySubstring(
			str,
			IntLiteral(0, 1),
			IntLiteral(2, 1))
		assertEquals("ab", node.exampleValues.head.get)

		// [0, 3]  -> "abc"
		node = TernarySubstring(
			str,
			IntLiteral(0, 1),
			IntLiteral(3, 1))
		assertEquals("abc", node.exampleValues.head.get)

		// [0, 4]  -> "abc"
		node = TernarySubstring(
			str,
			IntLiteral(0, 1),
			IntLiteral(4, 1))
		assertEquals("abc", node.exampleValues.head.get)

		// [1, -4]  -> ""
		node = TernarySubstring(
			str,
			IntLiteral(1, 1),
			IntLiteral(-4, 1))
		assertEquals("", node.exampleValues.head.get)

		// [1, -3]  -> ""
		node = TernarySubstring(
			str,
			IntLiteral(1, 1),
			IntLiteral(-3, 1))
		assertEquals("", node.exampleValues.head.get)

		// [1, -2]  -> ""
		node = TernarySubstring(
			str,
			IntLiteral(1, 1),
			IntLiteral(-2, 1))
		assertEquals("", node.exampleValues.head.get)

		// [1, -1]  -> "b"
		node = TernarySubstring(
			str,
			IntLiteral(1, 1),
			IntLiteral(-1, 1))
		assertEquals("b", node.exampleValues.head.get)

		// [1, 0]  -> ""
		node = TernarySubstring(
			str,
			IntLiteral(1, 1),
			IntLiteral(0, 1))
		assertEquals("", node.exampleValues.head.get)

		// [1, 1]  -> ""
		node = TernarySubstring(
			str,
			IntLiteral(1, 1),
			IntLiteral(1, 1))
		assertEquals("", node.exampleValues.head.get)

		// [1, 2]  -> "b"
		node = TernarySubstring(
			str,
			IntLiteral(1, 1),
			IntLiteral(2, 1))
		assertEquals("b", node.exampleValues.head.get)

		// [1, 3]  -> "bc"
		node = TernarySubstring(
			str,
			IntLiteral(1, 1),
			IntLiteral(3, 1))
		assertEquals("bc", node.exampleValues.head.get)

		// [1, 4]  -> "bc"
		node = TernarySubstring(
			str,
			IntLiteral(1, 1),
			IntLiteral(4, 1))
		assertEquals("bc", node.exampleValues.head.get)


		// [3, -4]  -> ""
		// [3, -3]  -> ""
		// [3, -2]  -> ""
		// [3, -1]  -> ""
		// [3, 0]  -> ""
		// [3, 1]  -> ""
		// [3, 2]  -> ""
		// [3, 3]  -> ""
		// [3, 4]  -> ""
		for (i <- -3 to 4) {
			node = TernarySubstring(
				str,
				IntLiteral(3, 1),
				IntLiteral(i, 1))
			assertEquals("", node.exampleValues.head.get)
		}
	}

	@Test def ternarySubListNode(): Unit =
	{
		val lst: IntListNode = new IntListNode
		{
			override val _values: List[Option[Iterable[Int]]] = List(1 :: 2 :: 3 :: Nil).map(Some(_))
			override protected val parenless: Boolean = true
			override val code: String = "[-1123, 2, 1]"
			override val height: Int = 1
			override val terms: Int = 1
			override val children: Iterable[ASTNode] = Nil

			override def includes(varName: String): Boolean = false

			override lazy val usesVariables: Boolean = false

			override def updateValues(contexts: Contexts): IntListNode = ???
		}
		var node: TernarySubList[Int] = TernarySubList[Int](lst, IntLiteral(0, 1), IntLiteral(3, 1))
		assertEquals(1, node.exampleValues.length)
		assertEquals(2, node.height)
		assertEquals(4, node.terms)
		assertEquals(node.children.size, 3)

		// [-4, -3] -> ""
		node = TernarySubList[Int](
			lst,
			IntLiteral(-4, 1),
			IntLiteral(-3, 1))
		assertEquals(List(), node.exampleValues.head.get)

		// [-4, -2] -> "a"
		node = TernarySubList[Int](
			lst,
			IntLiteral(-4, 1),
			IntLiteral(-2, 1))
		assertEquals(List(1), node.exampleValues.head.get)

		// [-4, -1] -> "ab"
		node = TernarySubList[Int](
			lst,
			IntLiteral(-4, 1),
			IntLiteral(-1, 1))
		assertEquals(List(1, 2), node.exampleValues.head.get)

		// [-4, 0]  -> ""
		node = TernarySubList[Int](
			lst,
			IntLiteral(-4, 1),
			IntLiteral(0, 1))
		assertEquals(List(), node.exampleValues.head.get)

		// [-4, 1]  -> "a"
		node = TernarySubList[Int](
			lst,
			IntLiteral(-4, 1),
			IntLiteral(1, 1))
		assertEquals(List(1), node.exampleValues.head.get)

		// [-4, 2]  -> "ab"
		node = TernarySubList[Int](
			lst,
			IntLiteral(-4, 1),
			IntLiteral(2, 1))
		assertEquals(List(1, 2), node.exampleValues.head.get)

		// [-4, 3]  -> "abc"
		node = TernarySubList[Int](
			lst,
			IntLiteral(-4, 1),
			IntLiteral(3, 1))
		assertEquals(List(1, 2, 3), node.exampleValues.head.get)

		// [-4, 4]  -> "abc"
		node = TernarySubList[Int](
			lst,
			IntLiteral(-4, 1),
			IntLiteral(4, 1))
		assertEquals(List(1, 2, 3), node.exampleValues.head.get)

		// [0, -4]  -> ""
		node = TernarySubList[Int](
			lst,
			IntLiteral(0, 1),
			IntLiteral(-4, 1))
		assertEquals(List(), node.exampleValues.head.get)

		// [0, -3]  -> ""
		node = TernarySubList[Int](
			lst,
			IntLiteral(0, 1),
			IntLiteral(-3, 1))
		assertEquals(List(), node.exampleValues.head.get)

		// [0, -2]  -> "a"
		node = TernarySubList[Int](
			lst,
			IntLiteral(0, 1),
			IntLiteral(-2, 1))
		assertEquals(List(1), node.exampleValues.head.get)

		// [0, -1]  -> "ab"
		node = TernarySubList[Int](
			lst,
			IntLiteral(0, 1),
			IntLiteral(-1, 1))
		assertEquals(List(1, 2), node.exampleValues.head.get)

		// [0, 0]  -> ""
		node = TernarySubList[Int](
			lst,
			IntLiteral(0, 1),
			IntLiteral(0, 1))
		assertEquals(List(), node.exampleValues.head.get)

		// [0, 1]  -> "a"
		node = TernarySubList[Int](
			lst,
			IntLiteral(0, 1),
			IntLiteral(1, 1))
		assertEquals(List(1), node.exampleValues.head.get)

		// [0, 2]  -> "ab"
		node = TernarySubList[Int](
			lst,
			IntLiteral(0, 1),
			IntLiteral(2, 1))
		assertEquals(List(1, 2), node.exampleValues.head.get)

		// [0, 3]  -> "abc"
		node = TernarySubList[Int](
			lst,
			IntLiteral(0, 1),
			IntLiteral(3, 1))
		assertEquals(List(1, 2, 3), node.exampleValues.head.get)

		// [0, 4]  -> "abc"
		node = TernarySubList[Int](
			lst,
			IntLiteral(0, 1),
			IntLiteral(4, 1))
		assertEquals(List(1, 2, 3), node.exampleValues.head.get)

		// [1, -4]  -> ""
		node = TernarySubList[Int](
			lst,
			IntLiteral(1, 1),
			IntLiteral(-4, 1))
		assertEquals(List(), node.exampleValues.head.get)

		// [1, -3]  -> ""
		node = TernarySubList[Int](
			lst,
			IntLiteral(1, 1),
			IntLiteral(-3, 1))
		assertEquals(List(), node.exampleValues.head.get)

		// [1, -2]  -> ""
		node = TernarySubList[Int](
			lst,
			IntLiteral(1, 1),
			IntLiteral(-2, 1))
		assertEquals(List(), node.exampleValues.head.get)

		// [1, -1]  -> "b"
		node = TernarySubList[Int](
			lst,
			IntLiteral(1, 1),
			IntLiteral(-1, 1))
		assertEquals(List(2), node.exampleValues.head.get)

		// [1, 0]  -> ""
		node = TernarySubList[Int](
			lst,
			IntLiteral(1, 1),
			IntLiteral(0, 1))
		assertEquals(List(), node.exampleValues.head.get)

		// [1, 1]  -> ""
		node = TernarySubList[Int](
			lst,
			IntLiteral(1, 1),
			IntLiteral(1, 1))
		assertEquals(List(), node.exampleValues.head.get)

		// [1, 2]  -> "b"
		node = TernarySubList[Int](
			lst,
			IntLiteral(1, 1),
			IntLiteral(2, 1))
		assertEquals(List(2), node.exampleValues.head.get)

		// [1, 3]  -> "bc"
		node = TernarySubList[Int](
			lst,
			IntLiteral(1, 1),
			IntLiteral(3, 1))
		assertEquals(List(2, 3), node.exampleValues.head.get)

		// [1, 4]  -> "bc"
		node = TernarySubList[Int](
			lst,
			IntLiteral(1, 1),
			IntLiteral(4, 1))
		assertEquals(List(2, 3), node.exampleValues.head.get)


		// [3, -4]  -> ""
		// [3, -3]  -> ""
		// [3, -2]  -> ""
		// [3, -1]  -> ""
		// [3, 0]  -> ""
		// [3, 1]  -> ""
		// [3, 2]  -> ""
		// [3, 3]  -> ""
		// [3, 4]  -> ""
		for (i <- -3 to 4) {
			node = TernarySubList[Int](
				lst,
				IntLiteral(3, 1),
				IntLiteral(i, 1))
			assertEquals(List(), node.exampleValues.head.get)
		}
	}
	// Quaternary Operations
	@Test def quaternarySubstringNode(): Unit =
	{
		val str: StringNode = StringLiteral("abc", 1)
		var node: QuaternarySubstring = new QuaternarySubstring(
			str,
			IntLiteral(0, 1),
			IntLiteral(3, 1),
			IntLiteral(1, 1))
		assertEquals(1, node.exampleValues.length)
		assertEquals("abc", node.exampleValues.head.get)
		assertEquals(Types.String, node.nodeType)
		assertEquals("\"abc\"[0:3:1]", node.code)
		assertEquals(1, node.height)
		assertEquals(5, node.terms)
		assertEquals(node.children.size, 4)

		// [-4, -3] -> ""
		node = new QuaternarySubstring(
			str,
			IntLiteral(-4, 1),
			IntLiteral(-3, 1),
			IntLiteral(1, 1))
		assertEquals("", node.exampleValues.head.get)

		// [-4, -2] -> "a"
		node = new QuaternarySubstring(
			str,
			IntLiteral(-4, 1),
			IntLiteral(-2, 1),
			IntLiteral(1, 1))
		assertEquals("a", node.exampleValues.head.get)

		// [-4, -1] -> "ab"
		node = new QuaternarySubstring(
			str,
			IntLiteral(-4, 1),
			IntLiteral(-1, 1),
			IntLiteral(1, 1))
		assertEquals("ab", node.exampleValues.head.get)

		// [-4, 0]  -> ""
		node = new QuaternarySubstring(
			str,
			IntLiteral(-4, 1),
			IntLiteral(0, 1),
			IntLiteral(1, 1))
		assertEquals("", node.exampleValues.head.get)

		// [-4, 1]  -> "a"
		node = new QuaternarySubstring(
			str,
			IntLiteral(-4, 1),
			IntLiteral(1, 1),
			IntLiteral(1, 1))
		assertEquals("a", node.exampleValues.head.get)

		// [-4, 2]  -> "ab"
		node = new QuaternarySubstring(
			str,
			IntLiteral(-4, 1),
			IntLiteral(2, 1),
			IntLiteral(1, 1))
		assertEquals("ab", node.exampleValues.head.get)

		// [-4, 3]  -> "abc"
		node = new QuaternarySubstring(
			str,
			IntLiteral(-4, 1),
			IntLiteral(3, 1),
			IntLiteral(1, 1))
		assertEquals("abc", node.exampleValues.head.get)

		// [-4, 4]  -> "abc"
		node = new QuaternarySubstring(
			str,
			IntLiteral(-4, 1),
			IntLiteral(4, 1),
			IntLiteral(1, 1))
		assertEquals("abc", node.exampleValues.head.get)

		// [0, -4]  -> ""
		node = new QuaternarySubstring(
			str,
			IntLiteral(0, 1),
			IntLiteral(-4, 1),
			IntLiteral(1, 1))
		assertEquals("", node.exampleValues.head.get)

		// [0, -3]  -> ""
		node = new QuaternarySubstring(
			str,
			IntLiteral(0, 1),
			IntLiteral(-3, 1),
			IntLiteral(1, 1))
		assertEquals("", node.exampleValues.head.get)

		// [0, -2]  -> "a"
		node = new QuaternarySubstring(
			str,
			IntLiteral(0, 1),
			IntLiteral(-2, 1),
			IntLiteral(1, 1))
		assertEquals("a", node.exampleValues.head.get)

		// [0, -1]  -> "ab"
		node = new QuaternarySubstring(
			str,
			IntLiteral(0, 1),
			IntLiteral(-1, 1),
			IntLiteral(1, 1))
		assertEquals("ab", node.exampleValues.head.get)

		// [0, 0]  -> ""
		node = new QuaternarySubstring(
			str,
			IntLiteral(0, 1),
			IntLiteral(0, 1),
			IntLiteral(1, 1))
		assertEquals("", node.exampleValues.head.get)

		// [0, 1]  -> "a"
		node = new QuaternarySubstring(
			str,
			IntLiteral(0, 1),
			IntLiteral(1, 1),
			IntLiteral(1, 1))
		assertEquals("a", node.exampleValues.head.get)

		// [0, 2]  -> "ab"
		node = new QuaternarySubstring(
			str,
			IntLiteral(0, 1),
			IntLiteral(2, 1),
			IntLiteral(1, 1))
		assertEquals("ab", node.exampleValues.head.get)

		// [0, 3]  -> "abc"
		node = new QuaternarySubstring(
			str,
			IntLiteral(0, 1),
			IntLiteral(3, 1),
			IntLiteral(1, 1))
		assertEquals("abc", node.exampleValues.head.get)

		// [0, 4]  -> "abc"
		node = new QuaternarySubstring(
			str,
			IntLiteral(0, 1),
			IntLiteral(4, 1),
			IntLiteral(1, 1))
		assertEquals("abc", node.exampleValues.head.get)

		// [1, -4]  -> ""
		node = new QuaternarySubstring(
			str,
			IntLiteral(1, 1),
			IntLiteral(-4, 1),
			IntLiteral(1, 1))
		assertEquals("", node.exampleValues.head.get)

		// [1, -3]  -> ""
		node = new QuaternarySubstring(
			str,
			IntLiteral(1, 1),
			IntLiteral(-3, 1),
			IntLiteral(1, 1))
		assertEquals("", node.exampleValues.head.get)

		// [1, -2]  -> ""
		node = new QuaternarySubstring(
			str,
			IntLiteral(1, 1),
			IntLiteral(-2, 1),
			IntLiteral(1, 1))
		assertEquals("", node.exampleValues.head.get)

		// [1, -1]  -> "b"
		node = new QuaternarySubstring(
			str,
			IntLiteral(1, 1),
			IntLiteral(-1, 1),
			IntLiteral(1, 1))
		assertEquals("b", node.exampleValues.head.get)

		// [1, 0]  -> ""
		node = new QuaternarySubstring(
			str,
			IntLiteral(1, 1),
			IntLiteral(0, 1),
			IntLiteral(1, 1))
		assertEquals("", node.exampleValues.head.get)

		// [1, 1]  -> ""
		node = new QuaternarySubstring(
			str,
			IntLiteral(1, 1),
			IntLiteral(1, 1),
			IntLiteral(1, 1))
		assertEquals("", node.exampleValues.head.get)

		// [1, 2]  -> "b"
		node = new QuaternarySubstring(
			str,
			IntLiteral(1, 1),
			IntLiteral(2, 1),
			IntLiteral(1, 1))
		assertEquals("b", node.exampleValues.head.get)

		// [1, 3]  -> "bc"
		node = new QuaternarySubstring(
			str,
			IntLiteral(1, 1),
			IntLiteral(3, 1),
			IntLiteral(1, 1))
		assertEquals("bc", node.exampleValues.head.get)

		// [1, 4]  -> "bc"
		node = new QuaternarySubstring(
			str,
			IntLiteral(1, 1),
			IntLiteral(4, 1),
			IntLiteral(1, 1))
		assertEquals("bc", node.exampleValues.head.get)


		// [3, -4]  -> ""
		// [3, -3]  -> ""
		// [3, -2]  -> ""
		// [3, -1]  -> ""
		// [3, 0]  -> ""
		// [3, 1]  -> ""
		// [3, 2]  -> ""
		// [3, 3]  -> ""
		// [3, 4]  -> ""
		for (i <- -3 to 4) {
			node = new QuaternarySubstring(
				str,
				IntLiteral(3, 1),
				IntLiteral(i, 1),
				IntLiteral(1, 1))
			assertEquals("", node.exampleValues.head.get)
		}

		// s[3:-3:-1] -> 'cb'
		node = new QuaternarySubstring(
			str,
			IntLiteral(3, 1),
			IntLiteral(-3, 1),
			IntLiteral(-1, 1))
		assertEquals("cb", node.exampleValues.head.get)

		// s[3:-2:-1] -> 'c'
		node = new QuaternarySubstring(
			str,
			IntLiteral(3, 1),
			IntLiteral(-2, 1),
			IntLiteral(-1, 1))
		assertEquals("c", node.exampleValues.head.get)

		// s[3:-1:-1] -> ''
		node = new QuaternarySubstring(
			str,
			IntLiteral(3, 1),
			IntLiteral(-1, 1),
			IntLiteral(-1, 1))
		assertEquals("", node.exampleValues.head.get)

		// s[3:0:-1] -> 'cb'
		node = new QuaternarySubstring(
			str,
			IntLiteral(3, 1),
			IntLiteral(0, 1),
			IntLiteral(-1, 1))
		assertEquals("cb", node.exampleValues.head.get)

		// s[3:1:-1] -> 'c'
		node = new QuaternarySubstring(
			str,
			IntLiteral(3, 1),
			IntLiteral(1, 1),
			IntLiteral(-1, 1))
		assertEquals("c", node.exampleValues.head.get)

		// s[3:3:-1] -> ''
		// s[3:2:-1] -> ''
		// s[3:4:-1] -> ''
		for (i <- 2 to 4) {
			node = new QuaternarySubstring(
				str,
				IntLiteral(3, 1),
				IntLiteral(i, 1),
				IntLiteral(-1, 1))
			assertEquals("", node.exampleValues.head.get)
		}

		// s[2:-3:-1] -> 'cb'
		node = new QuaternarySubstring(
			str,
			IntLiteral(2, 1),
			IntLiteral(-3, 1),
			IntLiteral(-1, 1))
		assertEquals("cb", node.exampleValues.head.get)

		// s[2:-2:-1] -> 'c'
		node = new QuaternarySubstring(
			str,
			IntLiteral(2, 1),
			IntLiteral(-2, 1),
			IntLiteral(-1, 1))
		assertEquals("c", node.exampleValues.head.get)

		// s[2:-1:-1] -> ''
		node = new QuaternarySubstring(
			str,
			IntLiteral(2, 1),
			IntLiteral(-1, 1),
			IntLiteral(-1, 1))
		assertEquals("", node.exampleValues.head.get)

		// s[2:0:-1] -> 'cb'
		node = new QuaternarySubstring(
			str,
			IntLiteral(2, 1),
			IntLiteral(0, 1),
			IntLiteral(-1, 1))
		assertEquals("cb", node.exampleValues.head.get)

		// s[2:1:-1] -> 'c'
		node = new QuaternarySubstring(
			str,
			IntLiteral(2, 1),
			IntLiteral(1, 1),
			IntLiteral(-1, 1))
		assertEquals("c", node.exampleValues.head.get)

		// s[2:2:-1] -> ''
		node = new QuaternarySubstring(
			str,
			IntLiteral(2, 1),
			IntLiteral(2, 1),
			IntLiteral(-1, 1))
		assertEquals("", node.exampleValues.head.get)

		// s[2:3:-1] -> ''
		node = new QuaternarySubstring(
			str,
			IntLiteral(2, 1),
			IntLiteral(3, 1),
			IntLiteral(-1, 1))
		assertEquals("", node.exampleValues.head.get)

		// s[2:4:-1] -> ''
		node = new QuaternarySubstring(
			str,
			IntLiteral(2, 1),
			IntLiteral(4, 1),
			IntLiteral(-1, 1))
		assertEquals("", node.exampleValues.head.get)

		// s[1:-3:-1] -> 'b'
		node = new QuaternarySubstring(
			str,
			IntLiteral(1, 1),
			IntLiteral(-3, 1),
			IntLiteral(-1, 1))
		assertEquals("b", node.exampleValues.head.get)

		// s[1:-2:-1] -> ''
		node = new QuaternarySubstring(
			str,
			IntLiteral(1, 1),
			IntLiteral(-2, 1),
			IntLiteral(-1, 1))
		assertEquals("", node.exampleValues.head.get)

		// s[1:-1:-1] -> ''
		node = new QuaternarySubstring(
			str,
			IntLiteral(1, 1),
			IntLiteral(-1, 1),
			IntLiteral(-1, 1))
		assertEquals("", node.exampleValues.head.get)

		// s[1:0:-1] -> 'b'
		node = new QuaternarySubstring(
			str,
			IntLiteral(1, 1),
			IntLiteral(0, 1),
			IntLiteral(-1, 1))
		assertEquals("b", node.exampleValues.head.get)

		// s[1:1:-1] -> ''
		node = new QuaternarySubstring(
			str,
			IntLiteral(1, 1),
			IntLiteral(1, 1),
			IntLiteral(-1, 1))
		assertEquals("", node.exampleValues.head.get)

		// s[1:2:-1] -> ''
		node = new QuaternarySubstring(
			str,
			IntLiteral(1, 1),
			IntLiteral(2, 1),
			IntLiteral(-1, 1))
		assertEquals("", node.exampleValues.head.get)

		// s[1:3:-1] -> ''
		node = new QuaternarySubstring(
			str,
			IntLiteral(1, 1),
			IntLiteral(3, 1),
			IntLiteral(-1, 1))
		assertEquals("", node.exampleValues.head.get)

		// s[0:-3:-1] -> ''
		node = new QuaternarySubstring(
			str,
			IntLiteral(0, 1),
			IntLiteral(-3, 1),
			IntLiteral(-1, 1))
		assertEquals("", node.exampleValues.head.get)

		// s[0:-2:-1] -> ''
		node = new QuaternarySubstring(
			str,
			IntLiteral(0, 1),
			IntLiteral(-2, 1),
			IntLiteral(-1, 1))
		assertEquals("", node.exampleValues.head.get)

		// s[0:-1:-1] -> ''
		node = new QuaternarySubstring(
			str,
			IntLiteral(0, 1),
			IntLiteral(-1, 1),
			IntLiteral(-1, 1))
		assertEquals("", node.exampleValues.head.get)

		// s[0:0:-1] -> ''
		node = new QuaternarySubstring(
			str,
			IntLiteral(0, 1),
			IntLiteral(0, 1),
			IntLiteral(-1, 1))
		assertEquals("", node.exampleValues.head.get)

		// s[0:1:-1] -> ''
		node = new QuaternarySubstring(
			str,
			IntLiteral(0, 1),
			IntLiteral(1, 1),
			IntLiteral(-1, 1))
		assertEquals("", node.exampleValues.head.get)

		// s[0:2:-1] -> ''
		node = new QuaternarySubstring(
			str,
			IntLiteral(0, 1),
			IntLiteral(2, 1),
			IntLiteral(-1, 1))
		assertEquals("", node.exampleValues.head.get)

		// s[0:3:-1] -> ''
		node = new QuaternarySubstring(
			str,
			IntLiteral(0, 1),
			IntLiteral(3, 1),
			IntLiteral(-1, 1))
		assertEquals("", node.exampleValues.head.get)

		// s[0:4:-1] -> ''
		for (i <- -3 to 4) {
			node = new QuaternarySubstring(
				str,
				IntLiteral(0, 1),
				IntLiteral(i, 1),
				IntLiteral(-1, 1))
			assertEquals("", node.exampleValues.head.get)
		}

		// s[-1:-3:-1] -> 'cb'
		node = new QuaternarySubstring(
			str,
			IntLiteral(-1, 1),
			IntLiteral(-3, 1),
			IntLiteral(-1, 1))
		assertEquals("cb", node.exampleValues.head.get)

		// s[-1:-2:-1] -> 'c'
		node = new QuaternarySubstring(
			str,
			IntLiteral(-1, 1),
			IntLiteral(-2, 1),
			IntLiteral(-1, 1))
		assertEquals("c", node.exampleValues.head.get)

		// s[-1:-1:-1] -> ''
		node = new QuaternarySubstring(
			str,
			IntLiteral(-1, 1),
			IntLiteral(-1, 1),
			IntLiteral(-1, 1))
		assertEquals("", node.exampleValues.head.get)

		// s[-1:0:-1] -> 'cb'
		node = new QuaternarySubstring(
			str,
			IntLiteral(-1, 1),
			IntLiteral(0, 1),
			IntLiteral(-1, 1))
		assertEquals("cb", node.exampleValues.head.get)

		// s[-1:1:-1] -> 'c'
		node = new QuaternarySubstring(
			str,
			IntLiteral(-1, 1),
			IntLiteral(1, 1),
			IntLiteral(-1, 1))
		assertEquals("c", node.exampleValues.head.get)

		// s[-1:2:-1] -> ''
		node = new QuaternarySubstring(
			str,
			IntLiteral(-1, 1),
			IntLiteral(2, 1),
			IntLiteral(-1, 1))
		assertEquals("", node.exampleValues.head.get)

		// s[-1:3:-1] -> ''
		node = new QuaternarySubstring(
			str,
			IntLiteral(-1, 1),
			IntLiteral(3, 1),
			IntLiteral(-1, 1))
		assertEquals("", node.exampleValues.head.get)

		// s[-2:-3:-1] -> 'b'
		node = new QuaternarySubstring(
			str,
			IntLiteral(-2, 1),
			IntLiteral(-3, 1),
			IntLiteral(-1, 1))
		assertEquals("b", node.exampleValues.head.get)

		// s[-2:-2:-1] -> ''
		node = new QuaternarySubstring(
			str,
			IntLiteral(-2, 1),
			IntLiteral(-2, 1),
			IntLiteral(-1, 1))
		assertEquals("", node.exampleValues.head.get)

		// s[-2:-1:-1] -> ''
		node = new QuaternarySubstring(
			str,
			IntLiteral(-2, 1),
			IntLiteral(-2, 1),
			IntLiteral(-1, 1))
		assertEquals("", node.exampleValues.head.get)

		// s[-2:0:-1] -> 'b'
		node = new QuaternarySubstring(
			str,
			IntLiteral(-2, 1),
			IntLiteral(0, 1),
			IntLiteral(-1, 1))
		assertEquals("b", node.exampleValues.head.get)

		// s[-2:1:-1] -> ''
		node = new QuaternarySubstring(
			str,
			IntLiteral(-2, 1),
			IntLiteral(1, 1),
			IntLiteral(-1, 1))
		assertEquals("", node.exampleValues.head.get)

		// s[-2:2:-1] -> ''
		node = new QuaternarySubstring(
			str,
			IntLiteral(-2, 1),
			IntLiteral(2, 1),
			IntLiteral(-1, 1))
		assertEquals("", node.exampleValues.head.get)

		// s[-2:3:-1] -> ''
		node = new QuaternarySubstring(
			str,
			IntLiteral(-2, 1),
			IntLiteral(3, 1),
			IntLiteral(-1, 1))
		assertEquals("", node.exampleValues.head.get)

	}

	// TODO Write the unit tests for other nodes
	// Ternary Operations

	@Test def intDivisionNode(): Unit =
	{
		val node = IntDivision(
			IntLiteral(-1, 1),
			IntLiteral(4, 1))
		assertEquals(-1, node.exampleValues.head.get)
	}

	@Test def stringConcatNode(): Unit = ()

	@Test def stringStepNode(): Unit = ()

	@Test def intAdditionNode(): Unit = ()

	@Test def intSubtractionNode(): Unit = ()

	@Test def findNode(): Unit = ()

	@Test def containsNode(): Unit = ()

	@Test def stringReplaceNode(): Unit = ()

	// List Operations
	@Test def stringSplitNode(): Unit = ()

	@Test def stringJoinNode(): Unit = ()

	@Test def stringStepListNode(): Unit =
	{
		val x = StringVariable("x", Map("x" -> "abcde") :: Map("x" -> "a") :: Map("x" -> "ab") :: Nil)
		val step = StringStep(x, IntLiteral(1, x.exampleValues.length))
		assertEquals("x[::1]", step.code)
		assertEquals(List("abcde", "a", "ab"), step.exampleValues.map(_.get))

		val step2 = StringStep(x, IntLiteral(-1, x.exampleValues.length))
		assertEquals("x[::-1]", step2.code)
		assertEquals(List("edcba", "a", "ba"), step2.exampleValues.map(_.get))

		val step3 = StringStep(x, IntLiteral(2, x.exampleValues.length))
		assertEquals("x[::2]", step3.code)
		assertEquals(List("ace", "a", "a"), step3.exampleValues.map(_.get))

		val step4 = StringStep(x, IntLiteral(-2, x.exampleValues.length))
		assertEquals("x[::-2]", step4.code)
		assertEquals(List("eca", "a", "b"), step4.exampleValues.map(_.get))

		val step5 = StringStep(x, IntLiteral(0, x.exampleValues.length))
		assertEquals(None, step5.exampleValues.head)
	}

	@Test def listStep(): Unit =
	{
		val x =  new StringListNode
		{
			override val _values: List[Option[Iterable[String]]] = List("abc" :: "def" :: "ghi" :: Nil).map(Some(_))
			override protected val parenless: Boolean = true
			override val code: String = "['abc', 'def']"
			override val height: Int = 1
			override val terms: Int = 1
			override val children: Iterable[ASTNode] = Nil

			override def includes(varName: String): Boolean = false

			override lazy val usesVariables: Boolean = false

			override def updateValues(contexts: Contexts): StringListNode = ???
		}
		val step = ListStep[String](x, IntLiteral(1, x.exampleValues.length))
		assertEquals(List("abc", "def", "ghi"), step.exampleValues.head.get)

		val step2 = ListStep[String](x, IntLiteral(-1, x.exampleValues.length))
		assertEquals(List("ghi", "def", "abc"), step2.exampleValues.map(_.get).head)

		val step3 = ListStep[String](x, IntLiteral(2, x.exampleValues.length))
		assertEquals(List("abc", "ghi"), step3.exampleValues.map(_.get).head)

		val step4 = ListStep[String](x, IntLiteral(-2, x.exampleValues.length))
		assertEquals(List("ghi", "abc"), step4.exampleValues.map(_.get).head)
	}

	@Test def substringListNode(): Unit = ()

	@Test def stringToIntListNode(): Unit = ()

	@Test def sortedStringListNode(): Unit = ()

	@Test def stringCount(): Unit =
	{
		val x = StringVariable("x", Map("x" -> "") :: Map("x" -> "abc") :: Map("x" -> "bc") :: Map("x" -> "aaaabc") :: Map("x" -> "abcabc") :: Nil)
		val count = Count(x, StringLiteral("a", x.exampleValues.length))
		assertEquals("x.count(\"a\")", count.code)
		assertEquals(List(0, 1, 0, 4, 2), count.exampleValues.map(_.get))

		val count2 = Count(x, StringLiteral("aa", x.exampleValues.length))
		assertEquals("x.count(\"aa\")", count2.code)
		assertEquals(List(0, 0, 0, 2, 0), count2.exampleValues.map(_.get))
	}

	@Test def printingNodes(): Unit =
	{
		assertEquals("2", IntLiteral(2, 1).code)
		val inp = StringVariable("inp", Map("inp" -> "'abc'") :: Nil)
		val addStrings = StringConcat(inp, StringLiteral(" ", 1))
		assertEquals("inp + \" \"", addStrings.code)
		val substr = TernarySubstring(addStrings, IntLiteral(0, 1), IntLiteral(1, 1))
		assertEquals("(inp + \" \")[0:1]", substr.code)
		val substr2 = TernarySubstring(inp, IntLiteral(0, 1), IntLiteral(1, 1))
		assertEquals("inp[0:1]", substr2.code)

		val split = StringSplit(addStrings, StringLiteral(",", 1))
		assertEquals("(inp + \" \").split(\",\")", split.code)
		val split2 = StringSplit(inp, StringLiteral(",", 1))
		assertEquals("inp.split(\",\")", split2.code)

		val step = StringStep(inp, IntLiteral(-2, 1))
		assertEquals("inp[::-2]", step.code)
		val step2 = StringStep(addStrings, IntLiteral(-2, 1))
		assertEquals("(inp + \" \")[::-2]", step2.code)

		val find = Find(addStrings, inp)
		assertEquals("(inp + \" \").find(inp)", find.code)

		val find2 = Find(step2, inp)
		assertEquals("(inp + \" \")[::-2].find(inp)", find2.code)

		val addNumbers = IntAddition(IntAddition(IntLiteral(1, 1), IntLiteral(2, 1)), IntAddition(IntLiteral(3, 1), IntLiteral(4, 1)))
		assertEquals("1 + 2 + 3 + 4", addNumbers.code)

		val divNumbers = IntDivision(addNumbers, IntLiteral(1, 1))
		assertEquals("(1 + 2 + 3 + 4) // 1", divNumbers.code)

		val divNumbers2 = IntDivision(IntLiteral(1, 1), addNumbers)
		assertEquals("1 // (1 + 2 + 3 + 4)", divNumbers2.code)
	}

	@Test def listCompNode(): Unit =
	{
		val contexts: List[Map[String, Any]] = List(
			Map("lst" -> List("Sorin", "Lerner")),
			Map("lst" -> List("Kasra", "Ferdowsi", "Fard")))
		val lst = new ListVariable[String]("lst", contexts, Types.String)

		val innerContexts: List[Map[String, Any]] = List(
			Map("var" -> "Sorin"),
			Map("var" -> "Lerner"),
			Map("var" -> "Kasra"),
			Map("var" -> "Ferdowsi"),
			Map("var" -> "Fard"))
		val innerStr = StringVariable("var", innerContexts)
		val map = BinarySubstring(innerStr, IntLiteral(0, 5))
		val lcn = new StringToStringListCompNode(lst, map, "var")

		assertEquals(List(Some(List("S", "L")), Some(List("K", "F", "F"))), lcn.exampleValues)
	}

	@Test def doubleVariableNode(): Unit = {
		val contexts: List[Map[String, Any]] = List(
			Map("x" -> 4.7)
		)
		val varNode = DoubleVariable("x", contexts)
		assertTrue(varNode.isInstanceOf[DoubleNode])
		assertTrue(varNode.isInstanceOf[VariableNode[Double]])
		assertEquals("x", varNode.code)
		assertEquals(List(Some(4.7)),varNode.exampleValues)
	}

	@Test def maxDoublesNode(): Unit = {
		val node: DoublesMax = DoublesMax(new DoubleListNode
		{
			override val _values: List[Option[Iterable[Double]]] = List(-1123.0 :: 2.0 :: 1.0 :: Nil).map(Some(_))
			override protected val parenless: Boolean = true
			override val code: String = "[-1123.0, 2.0, 1.0]"
			override val height: Int = 1
			override val terms: Int = 1
			override val children: Iterable[ASTNode] = Nil

			override def includes(varName: String): Boolean = false

			override lazy val usesVariables: Boolean = false

			override def updateValues(contexts: Contexts): DoubleListNode = ???
		})
		assertEquals(1, node.exampleValues.length)
		assertEquals(Some(2.0), node.exampleValues.head)
		assertEquals(Types.Double, node.nodeType)
		assertEquals("max([-1123.0, 2.0, 1.0])", node.code)
		assertEquals(2, node.height)
		assertEquals(2, node.terms)
		assertEquals(node.children.size, 1)
	}

	@Test def getDoubleListElement(): Unit = {
		val ctx = Map("l" -> List(0.0, 1.2)) :: Nil
		val l = ListVariable[Double]("l", ctx, Types.Double)
		val one = IntLiteral(1,ctx.length)
		val getExpr:ASTNode = DoubleListLookup(l,one)
		assertEquals(Types.Double, getExpr.nodeType)
		assertEquals(List(Some(1.2)), getExpr.exampleValues)
		assertEquals("l[1]", getExpr.code)
	}

	@Test def lAndTest(): Unit = {
		var node: LAnd = LAnd(BoolLiteral(false, 1), BoolLiteral(true, 1))
		assertEquals(1, node.exampleValues.length)
		assertEquals(false, node.exampleValues.head.get)
		assertEquals(Types.Bool, node.nodeType)
		assertEquals("False and True", node.code)
	}
}