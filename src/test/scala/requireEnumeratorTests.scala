//import edu.ucsd.snippy.ast.Types.Types
//import edu.ucsd.snippy.ast._
//import edu.ucsd.snippy.enumeration._
//import edu.ucsd.snippy.predicates.SingleVariablePredicate
//import edu.ucsd.snippy.solution.ConditionalSingleEnumSingleVarSolutionEnumerator
//import edu.ucsd.snippy.utils.Utils
//import edu.ucsd.snippy.utils.Utils.getBinaryPartitions
//import edu.ucsd.snippy.vocab.{BasicVocabMaker, RequiredVocabMaker, VocabFactory, VocabMaker}
//import edu.ucsd.snippy.{InputParser, SynthesisTask}
//import org.junit.Assert.assertEquals
//import org.junit.Test
//import org.scalatestplus.junit.JUnitSuite
//
//import java.io.File
//import scala.collection.mutable
//import scala.io.Source.fromFile
//import scala.util.control.Breaks.{break, breakable}
//
//
//class main extends JUnitSuite {
//
//	// we assume that the new examples are appended after the old ones. so the past indices are the same as the new ones
//	def knownPartitioning(pastPartition:(Set[Int],Set[Int])):List[Any]=>List[(Set[Int], Set[Int])] = {
//		(indices:List[Any]) => {
//			getBinaryPartitions(indices).filter(part =>
//				(pastPartition._1.subsetOf(part._1) && pastPartition._2.subsetOf(part._1)) || // both in the first
//				(pastPartition._1.subsetOf(part._2) && pastPartition._1.subsetOf(part._2)) || //both in the second
//				(pastPartition._1.subsetOf(part._1) && pastPartition._2.subsetOf(part._2)) || // first in the first, second in the second
//				(pastPartition._1.subsetOf(part._2) && pastPartition._2.subsetOf(part._1))) // first in the second, second in the first
//		}
//	}
//
//	/*val task: SynthesisTask = SynthesisTask.fromString(
//		"""{
//		  |  "varNames": ["rs"],
//		  |  "previousEnvs": {},
//		  |  "envs": [
//		  |    {
//		  |      "#": "",
//		  |      "$": "",
//		  |      "x": "0",
//		  |      "rs": "-2",
//		  |      "time": 1,
//		  |    },
//		  |    {
//		  |      "#": "",
//		  |      "$": "",
//		  |      "x": "-3",
//		  |      "rs": "-6",
//		  |      "time": 2,
//		  |    },
//		  |    {
//		  |      "#": "",
//		  |      "$": "",
//		  |      "x": "-1",
//		  |      "rs": "-3",
//		  |      "time": 3,
//		  |    },
//		  |
//		  |
//		  |  ]
//		  |}""".stripMargin).*/
//
//	/*for i in list:
//		if i > 0:
//			rs = rs + [i]
//		else:
//			rs = rs + [-i]
//	 */
//	val path = "C:\\Users\\tomerkatz\\Desktop\\LooPy\\synthesizer\\src\\test\\resources\\Tomer\\move2.json"
//	val file = new File(path)
//	val taskStr = fromFile(file).mkString
//	val task = SynthesisTask.fromString(taskStr)
//
//	val bank = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
//	val mini = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
//	val probEnumerator = new ProbEnumerator(task.vocab, task.oeManager, task.contexts, false, 0, bank, mini, 100)
//	//var enumerator = new ConditionalSingleEnumSingleVarSolutionEnumerator(
//	print(task.enumerator.getClass+"\n");
//	//val pred = task.predicate.asInstanceOf[MultilineMultivariablePredicate]
//	val pred2: SingleVariablePredicate = task.predicate.asInstanceOf[SingleVariablePredicate]
//	val part = knownPartitioning(Set(0), Set(1))
//	//print("all partitions: "+getBinaryPartitions(task.contexts.indices.toList)+"\n")
//	//print(part(task.contexts.indices.toList)+"\n");
//
//	val basicEnumerator = new BasicEnumerator(task.vocab, task.oeManager, task.contexts)
//	val variables = task.contexts.flatMap(_.keys)
//		.toSet[String]
//		.map(varName => varName -> Utils.getTypeOfAll(task.contexts.map(ex => ex.get(varName)).filter(_.isDefined).map(_.get)))
//		.filter(!_._2.equals(Types.Unknown))
//		.toList
//	//val enum :ConditionalSingleEnumMultivarSolutionEnumerator = new ConditionalSingleEnumMultivarSolutionEnumerator(pred, variables, List(),part)
//	val enum2 :ConditionalSingleEnumSingleVarSolutionEnumerator = new ConditionalSingleEnumSingleVarSolutionEnumerator(probEnumerator, pred2.varName,pred2.retType,pred2.values,task.contexts,part, 0)
//	//enum.graph.do_print((node)=>node.toString, (edge)=>edge.toString)
//	val t1 = System.nanoTime
//	for (solution <- enum2) {
//		solution match {
//			case Some(assignment) =>
//				val code = Some(assignment.code()).get
//				if(code.nonEmpty) {
//					print(code + "\n")
//					print((System.nanoTime - t1) / 1e9d)
//					break
//				}
//			case _ => ()
//		}
//	}
//}
//
//
//class requireEnumeratorTests extends JUnitSuite {
//
//	def ASTVisitor(root: ASTNode, nodeToReplace: ASTNode, replacement: ASTNode): ASTNode = {
//		if (root === nodeToReplace) replacement
//		else root.updateChildren(root.children.map(child => ASTVisitor(child, nodeToReplace, replacement)).toSeq, List())
//	}
//
//
//
//	@Test def testRequiredEnumerator(): Unit = {
//		val wordsAST: ASTNode = UnarySplit(StringVariable("s", Map("s"-> "hello, world")::Map( "s"->"hello, __world__") :: Nil));
//		val index: IntLiteral =  IntLiteral(-1, 2);
//		val list: ListLiteral[String] = ListLiteral[String](Types.String, List("hello, world", "hello, __world__"), 2)
//		val p2AST: StringListLookup = StringListLookup(list,index);
//		val resultAST: ASTNode = StringListLookup(UnarySplit(StringVariable("s", Map("s"-> "hello, world") :: Nil)), IntLiteral(-1, 1));
//
//		val task: SynthesisTask = SynthesisTask.fromString(
//			"""{
//			  |  "varNames": ["rs"],
//			  |  "previousEnvs": {},
//			  |  "envs": [
//			  |    {
//			  |      "#": "",
//			  |      "$": "",
//			  |      "s": "'hello , world'",
//			  |      "rs": "'r'",
//			  |      "time": 1,
//			  |    },
//|          {
//			  |      "#": "",
//			  |      "$": "",
//			  |      "s": "'hello  , __world__'",
//			  |      "rs": "'r'",
//			  |      "time": 1,
//			  |    },
//			  |  ]
//			  |}""".stripMargin)
//
//		//Some(new RequiredVocabMaker(p2AST, List(list))
////		val parser = new ExpressionParser(Map("s" -> Types.Int), new Contexts(task.contexts))
////		val ast = parser.parse("s*2*2")
////		print(ast)
//
//		val oeManager = new RequiresValuesManager();
//		val bank = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
//		val mini = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
//
//		val requiredVocabMaker = new RequiredVocabMaker(wordsAST, List(), 0, new Contexts(task.contexts));
//		val pred: SingleVariablePredicate= task.predicate.asInstanceOf[SingleVariablePredicate];
//
//		val parameters = task.contexts.flatMap(_.keys)
//			.toSet[String]
//			.map(varName => varName -> Utils.getTypeOfAll(task.contexts.map(ex => ex.get(varName)).filter(_.isDefined).map(_.get)))
//			.filter(!_._2.equals(Types.Unknown))
//			.toList
//		val vocab: VocabFactory = VocabFactory(parameters, List(), List(requiredVocabMaker));
//
//		val enumerator = new ProbEnumerator(vocab, oeManager, task.contexts, false, 0, bank, mini, 100);
//		val enum = new ConditionalSingleEnumSingleVarSolutionEnumerator(enumerator, pred.varName, pred.retType, pred.values, task.contexts)
//
//		var counter= 0;
//		var flag = false
//		breakable {
//			while (enum.hasNext) {
//				val node = enum.next();
//				if (flag) counter += 1;
//				node match {
//					case Some(assgin) =>
//
//						if (flag) {
//							println(assgin.code());
//							break()
//						} else {
//							flag = true
//						}
//
//					case _ => ()
//
//				}
//				//				print(node.code());
//				//				print("....")
//				//				print(node.exampleValues)
//				//				print("\n");
//				//				if (node.code === resultAST.code) {
//
//				//					println("Found the node!");
//				//					flag = true;
//				//				}
//			}
//		}
//		if (flag) {
//			println("Found the node again after " + counter + " programs from last time");
//			break;
//		}
//
//		assertEquals("1","1");
//	}
//}
//
//class anotherRequireEnumeratorTests extends JUnitSuite{
//
//
//	def ASTVisitor(root: ASTNode, nodeToReplace: ASTNode, replacement: ASTNode): ASTNode = {
//		if (root === nodeToReplace) replacement
//		else root.updateChildren(root.children.map(child => ASTVisitor(child, nodeToReplace, replacement)).toSeq, List())
//
//	}
//
//	def requiredVocabMaker(program: ASTNode, nodesToHoles: List[ASTNode]): VocabMaker = {
//		new BasicVocabMaker {
//			override val head: String = ""
//			override val arity: Int = nodesToHoles.length
//			override val returnType: Types = program.nodeType
//			override val childTypes: List[Types] = nodesToHoles.map(_.nodeType)
//			override val nodeType: Class[_ <: ASTNode] = program.getClass
//
//			override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode = {
//				var newProgram: ASTNode = program.asInstanceOf[ASTNode]
//				for (i <- children.indices) {
//					newProgram = ASTVisitor(newProgram, nodesToHoles(i), children(i))
//				}
//				newProgram
//			}
//		}
//	}
//
////	@Test def SecondTestRequiredEnumerator(): Unit = {
////		val wordsAST: ASTNode = UnarySplit(StringVariable("s", Map("s" -> "hello, world") :: Nil));
////		val index: IntLiteral = IntLiteral(-1, 1);
////		val list: ListLiteral[String] = ListLiteral[String](Types.String, List("hello, world"), 1)
////		val p2AST: BinarySubstring = BinarySubstring(StringListLookup(list, index), index);
////		val resultAST: ASTNode = BinarySubstring(StringListLookup(UnarySplit(StringVariable("s", Map("s" -> "hello, world") :: Nil)), IntLiteral(-1, 1)), IntLiteral(-1, 1));
////
////
////		val requiredMaker: BasicVocabMaker = new BasicVocabMaker {
////			override val head: String = ""
////			override val returnType: Types = p2AST.nodeType
////			override val arity: Int = 1
////			override val childTypes: List[Types] = p2AST.children.head.children.map(_.nodeType).toList
////			override val nodeType: Class[_ <: ASTNode] = p2AST.getClass
////
////			override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode = BinarySubstring(StringListLookup(children.head.asInstanceOf[ListNode[String]], IntLiteral(-1, 1)), IntLiteral(-1, 1));
////		}
////
////
////
////		val task: SynthesisTask = SynthesisTask.fromString(
////			"""{
////			  |  "varNames": ["rs"],
////			  |  "previousEnvs": {},
////			  |  "envs": [
////			  |    {
////			  |      "#": "",
////			  |      "$": "",
////			  |      "s": "'hello, world'",
////			  |      "rs": "'d'",
////			  |      "t":   "'o'"
////			  |      "time": 1,
////			  |    },
////			  |  ]
////			  |}""".stripMargin)
////
////		val oeManager = new RequiresValuesManager();
////		val bank = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
////		val mini = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
////		val enumerator = new ProbEnumerator(task.vocab, oeManager, task.contexts, false, 0, bank, mini, 100);
////		var counter = 0;
////		var flag = false
////		breakable {
////			while (enumerator.hasNext) {
////				val node = enumerator.next();
////				if (flag) counter += 1;
////				print(node.code);
////				print("....")
////				print(node.exampleValues)
////				print("\n");
////				if (node.code === resultAST.code) {
////					if (flag) {
////						println("Found the node again after " + counter + " programs from last time");
////						break;
////					}
////					println("Found the node!");
////					enumerator.oeManager.remove(node);
////					flag = true;
////				}
////			}
////		}
////		assertEquals("1", "1");
////	}
//
//
//}
//
//class parse extends JUnitSuite {
//	@Test def parseTest(): Unit = {
//		val parser = new InputParser;
//		print(parser.parse("'hello, world'"));
//	}
//}
