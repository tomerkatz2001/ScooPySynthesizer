import edu.ucsd.snippy.SynthesisTask
import edu.ucsd.snippy.ast.Types.Types
import org.scalatestplus.junit.JUnitSuite
import edu.ucsd.snippy.ast._
import edu.ucsd.snippy.enumeration._
import edu.ucsd.snippy.predicates.MultilineMultivariablePredicate
import edu.ucsd.snippy.solution.{ConditionalSingleEnumMultivarSolutionEnumerator}
import edu.ucsd.snippy.vocab.{BasicVocabMaker, VocabMaker}
import org.junit.Assert.assertEquals
import org.junit.Test

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}


class main extends JUnitSuite {
	val task: SynthesisTask = SynthesisTask.fromString(
		"""{
		  |  "varNames": ["rs", "x"],
		  |  "previousEnvs": {},
		  |  "envs": [
		  |    {
		  |      "#": "",
		  |      "$": "",
		  |      "s": "'hello, world'",
		  |      "x": "'hello,'",
		  |      "rs": "'world'",
		  |      "time": 1,
		  |    },
		  |  ]
		  |}""".stripMargin)
	val bank = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
	val mini = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
	val enumerator = new ProbEnumerator(task.vocab, task.oeManager, task.contexts, false, 0, bank, mini, 100)
	//var enumerator = new ConditionalSingleEnumSingleVarSolutionEnumerator(
	print(task.enumerator.getClass);
	val enum :ConditionalSingleEnumMultivarSolutionEnumerator = task.enumerator.asInstanceOf[ConditionalSingleEnumMultivarSolutionEnumerator]
	enum.graph.do_print((node)=>node.toString, (edge)=>edge.toString)
	for (solution <- task.enumerator) {
		solution match {
			case Some(assignment) =>
				val code = Some(assignment.code())
				print(code)
				break
			case _ => ()
		}
	}
}


class requireEnumeratorTests extends JUnitSuite {

	def ASTVisitor(root: ASTNode, nodeToReplace: ASTNode, replacement: ASTNode): ASTNode = {
		if (root === nodeToReplace) replacement
		else root.updateChildren(root.children.map(child => ASTVisitor(child, nodeToReplace, replacement)).toSeq)



	}

	def requiredVocabMaker(program: ASTNode, nodesToHoles: List[ASTNode]): VocabMaker = {
		new BasicVocabMaker {
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
		}
	}

	@Test def testRequiredEnumerator(): Unit = {
		val wordsAST: ASTNode = UnarySplit(StringVariable("s", Map("s"-> "hello, world") :: Nil));
		val index: IntLiteral =  IntLiteral(-1, 1);
		val list: ListLiteral[String] = ListLiteral[String](Types.String, List("hello, world"), 1)
		val p2AST: StringListLookup = StringListLookup(list,index, true);
		val resultAST: ASTNode = StringListLookup(UnarySplit(StringVariable("s", Map("s"-> "hello, world") :: Nil)), IntLiteral(-1, 1));


		val requiredMaker: BasicVocabMaker = new BasicVocabMaker {
			override val head: String = ""
			override val returnType: Types = p2AST.nodeType
			override val arity: Int = 1
			override val childTypes: List[Types] = p2AST.children.map(_.nodeType).toList
			override val nodeType: Class[_ <: ASTNode] = p2AST.getClass

			override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode = StringListLookup(children.head.asInstanceOf[ListNode[String]], IntLiteral(-1, 1), true);
		}

		val task: SynthesisTask = SynthesisTask.fromString(
			"""{
			  |  "varNames": ["rs"],
			  |  "previousEnvs": {},
			  |  "envs": [
			  |    {
			  |      "#": "",
			  |      "$": "",
			  |      "s": "'hello, world'",
			  |      "rs": "'world'",
			  |      "time": 1,
			  |    },
			  |  ]
			  |}""".stripMargin,Some(requiredVocabMaker(p2AST, List(list))))

		val oeManager = new RequiresValuesManager();
		val bank = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
		val mini = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
		val enumerator = new ProbEnumerator(task.vocab, oeManager, task.contexts, false, 0, bank, mini, 100);
		var counter= 0;
		var flag = false
		breakable {
			while (enumerator.hasNext) {
				val node = enumerator.next();
				if (flag) counter += 1;
				print(node.code);
				print("....")
				print(node.exampleValues)
				print("\n");
				if (node.code === resultAST.code) {
					if (flag) {
						println("Found the node again after " + counter + " programs from last time");
						break;
					}
					println("Found the node!");
					flag = true;
				}
			}
		}
		assertEquals("1","1");
	}
}

class anotherRequireEnumeratorTests extends JUnitSuite{


	def ASTVisitor(root: ASTNode, nodeToReplace: ASTNode, replacement: ASTNode): ASTNode = {
		if (root === nodeToReplace) replacement
		else root.updateChildren(root.children.map(child => ASTVisitor(child, nodeToReplace, replacement)).toSeq)


	}

	def requiredVocabMaker(program: ASTNode, nodesToHoles: List[ASTNode]): VocabMaker = {
		new BasicVocabMaker {
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
		}
	}

	@Test def SecondTestRequiredEnumerator(): Unit = {
		val wordsAST: ASTNode = UnarySplit(StringVariable("s", Map("s" -> "hello, world") :: Nil));
		val index: IntLiteral = IntLiteral(-1, 1);
		val list: ListLiteral[String] = ListLiteral[String](Types.String, List("hello, world"), 1)
		val p2AST: BinarySubstring = BinarySubstring(StringListLookup(list, index, true), index);
		val resultAST: ASTNode = BinarySubstring(StringListLookup(UnarySplit(StringVariable("s", Map("s" -> "hello, world") :: Nil)), IntLiteral(-1, 1)), IntLiteral(-1, 1));


		val requiredMaker: BasicVocabMaker = new BasicVocabMaker {
			override val head: String = ""
			override val returnType: Types = p2AST.nodeType
			override val arity: Int = 1
			override val childTypes: List[Types] = p2AST.children.head.children.map(_.nodeType).toList
			override val nodeType: Class[_ <: ASTNode] = p2AST.getClass

			override def apply(children: List[ASTNode], contexts: List[Map[String, Any]]): ASTNode = BinarySubstring(StringListLookup(children.head.asInstanceOf[ListNode[String]], IntLiteral(-1, 1), true), IntLiteral(-1, 1));
		}



		val task: SynthesisTask = SynthesisTask.fromString(
			"""{
			  |  "varNames": ["rs"],
			  |  "previousEnvs": {},
			  |  "envs": [
			  |    {
			  |      "#": "",
			  |      "$": "",
			  |      "s": "'hello, world'",
			  |      "rs": "'world'",
			  |      "time": 1,
			  |    },
			  |  ]
			  |}""".stripMargin, Some(requiredVocabMaker(p2AST, List(list))))

		val oeManager = new RequiresValuesManager();
		val bank = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
		val mini = mutable.Map[Int, mutable.ArrayBuffer[ASTNode]]()
		val enumerator = new ProbEnumerator(task.vocab, oeManager, task.contexts, false, 0, bank, mini, 100);
		var counter = 0;
		var flag = false
		breakable {
			while (enumerator.hasNext) {
				val node = enumerator.next();
				if (flag) counter += 1;
				print(node.code);
				print("....")
				print(node.exampleValues)
				print("\n");
				if (node.code === resultAST.code) {
					if (flag) {
						println("Found the node again after " + counter + " programs from last time");
						break;
					}
					println("Found the node!");
					flag = true;
				}
			}
		}
		assertEquals("1", "1");
	}
}

