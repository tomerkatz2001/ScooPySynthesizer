import edu.ucsd.snippy.Snippy.synthesize
import edu.ucsd.snippy.SynthesisTask
import edu.ucsd.snippy.SynthesisTask.Context
import edu.ucsd.snippy.scoopy.{ParsedComment, ScopeSpecification}
import org.junit.Assert.assertEquals
import org.junit.Test
import org.scalatestplus.junit.JUnitSuite

class ScopeSpecsTests extends JUnitSuite{
	@Test def assignSpec(): Unit = {
		ScopeSpecification.required=0

		val code = "'x = 9+4*6'";
		val contexts:List[Context] = List(Map.empty) ;
		val spec = ScopeSpecification.assign(code,contexts)
		assertEquals(Tuple2(List(), List()),
			spec.partition)
		assertEquals(List(),
			spec.scopeExamples)
		assertEquals(List(),
			spec.requiredVocabMakers)
		assertEquals(1,
			spec.optionalVocabMakers.size)
	}

	@Test def condSpec(): Unit = {
		ScopeSpecification.required=0

		val code1 = "'x = 9+4*6'";
		val contexts1:List[Context] = List(Map.empty) ;
		val spec1 = ScopeSpecification.assign(code1,contexts1)
		val code2 = "'x = 9+4*6'";
		val contexts2:List[Context] = List(Map.empty) ;
		val spec2 = ScopeSpecification.assign(code2,contexts2)
		val spec = ScopeSpecification.cond(spec1,spec2)
		assertEquals(Tuple2(List(), List()), spec.partition)
		assertEquals(List(), spec.scopeExamples)
		assertEquals(List(), spec.requiredVocabMakers)
		assertEquals(2, spec.optionalVocabMakers.size)
	}

	@Test def scopeOfCondTest():Unit={
		ScopeSpecification.required=0

		val code = "'x= y+2'";
		val code2 = "'x = y+3'";
		val examples = List(Map("'y'" -> 1, "'x'" -> 3), Map("'y'" -> 2, "'x'" -> 5), Map("'y'" -> 0, "'x'" -> 2))
		val outputVarNames = List("'x'")
		val contexts: List[Context] = examples.map {
			env => env.filter(entry => !outputVarNames.contains(entry._1))
		}

		val spec1 = ScopeSpecification.assign(code,contexts)
		val spec1_2 = ScopeSpecification.scope(spec1,List(examples(0)), outputVarNames)
		print("done then scope\n")

		val spec2 = ScopeSpecification.assign(code2,contexts)
		val spec2_2 = ScopeSpecification.scope(spec2,List(examples(1)), outputVarNames)
		print("done else scope\n")

		val condSpec = ScopeSpecification.cond(spec1_2,spec2_2)
		val scopeSpec = ScopeSpecification.scope(condSpec,List(examples(2)), outputVarNames)
		print("done scope of cond\n")

		assertEquals(examples, scopeSpec.scopeExamples)
		assertEquals((List(0), List(1)), scopeSpec.partition)
		val synthTask = SynthesisTask.fromSpec(scopeSpec)
		val solution = synthesize(synthTask, 10);
		assert(solution._1.isDefined)
		print(solution._1.get)

	}

	@Test def scopeTest(): Unit= {
		ScopeSpecification.required=0

		val code = "'x = y+2'";
		val examples = List(Map("'y'" -> 1, "'x'"->3), Map("'y'" -> 2, "'x'"->4))
		val outputVarNames = List("'x'")
		val contexts: List[Context] = examples.map {
			env => env.filter(entry => !outputVarNames.contains(entry._1))
		}
		val spec = ScopeSpecification.assign(code,contexts)
		val scopeSpec = ScopeSpecification.scope(spec, examples, outputVarNames)

		assertEquals(examples, scopeSpec.scopeExamples);


		val synthTask = SynthesisTask.fromSpec(scopeSpec)
		val solution = synthesize(synthTask, 10);
		assert(solution._1.isDefined)
		assertEquals("'x' = 'y' + 2", solution._1.get)

	}

	@Test def doubleScopeTest():Unit={
		ScopeSpecification.required=0

		val code = "'x = y+2'";
		val examples = List(Map("'y'" -> 1, "'x'" -> 3), Map("'y'" -> 2, "'x'" -> 3))
		val outputVarNames = List("'x'")
		val contexts: List[Context] = examples.map {
			env => env.filter(entry => !outputVarNames.contains(entry._1))
		}
		val spec = ScopeSpecification.assign(code,contexts)
		val scopeSpec = ScopeSpecification.scope(spec, List(examples(0)), outputVarNames, false)
		val scopeSpec2 = ScopeSpecification.scope(scopeSpec, List(examples(1)), outputVarNames, false)

		assertEquals(examples, scopeSpec2.scopeExamples);
		assertEquals(List(), scopeSpec2.requiredVocabMakers)

	}


}

class fromSpecToTaskTests extends JUnitSuite{

	@Test def ScopeToTask():Unit={
		ScopeSpecification.required=0
		val examples = List(Map("'x'"-> 33, "'y'"-> 4))
		val outputVarNames = List("'x'")
		val contexts: List[Context] = examples.map {
			env => env.filter(entry => !outputVarNames.contains(entry._1))
		}
		val spec0 = ScopeSpecification.assign("'x = 9+y*6'",contexts)

		val spec1 = ScopeSpecification.scope(spec0, examples,outputVarNames )

		val synthTask = SynthesisTask.fromSpec(spec1)
		val solution = synthesize(synthTask, 10);
		assert(solution._1.isDefined)
		assertEquals(1, solution._3)
		print(solution._1.get)
	}

}

class fromTreeTests extends  JUnitSuite{

	@Test def OneDepthTreeTest(): Unit = {
		ScopeSpecification.required=0
		val scopeTree = Map("0"->Map.empty)
		val example = Map("'x'"-> 12, "'y'"-> 6)
		val ScopesIno = Map("0"->new ParsedComment("0",List(example), List("'x'"), List("'x = y + y'")))
		val spec = ScopeSpecification.fromTree(scopeTree, ScopesIno)
		assertEquals(List(example), spec.scopeExamples)
		assertEquals(1, spec.requiredVocabMakers.size)
	}

	@Test def doubleScopeTree():Unit = {
		ScopeSpecification.required=0

		val scopeTree = Map("1" -> Map("NoBranch"->Map("0"->Map.empty)))
		val examples = List(Map("'x'" -> 12, "'y'" -> 6), Map("'x'" -> 6, "'y'" -> 3))
		val comment1 = new ParsedComment("0", List(examples(0)), List("'x'"), List("'x = y + y'"))
		val comment2 = new ParsedComment("1", List(examples(1)), List("'x'"), List())
		val ScopesIno = Map("0" -> comment1, "1" -> comment2)
		val spec = ScopeSpecification.fromTree(scopeTree, ScopesIno, false)
		assertEquals(examples, spec.scopeExamples)
		assertEquals(1, spec.optionalVocabMakers.size)
		assertEquals(0, spec.requiredVocabMakers.size)
	}

	@Test def condTree(): Unit = {
		ScopeSpecification.required=0

		val scopeTree = Map("2" -> Map("T" -> Map("0" -> Map.empty), "F" -> Map("1" -> Map.empty)))
		val examples = List(Map("'x'" -> 12, "'y'" -> 6), Map("'x'" -> 3, "'y'" -> 3), Map("'x'" -> 2, "'y'" -> 2))
		val comment1 = new ParsedComment("0", List(examples(0)), List("'x'"), List("'x = y + y'"))
		val comment2 = new ParsedComment("1", List(examples(1)), List("'x'"), List("'x = y'"))
		val comment3 = new ParsedComment("2", List(examples(2)), List("'x'"), List())
		val ScopesIno = Map("0" -> comment1, "1" -> comment2, "2" -> comment3)
		val spec = ScopeSpecification.fromTree(scopeTree, ScopesIno)
		assertEquals(examples, spec.scopeExamples)
		assertEquals(2, spec.optionalVocabMakers.size)
		assertEquals((List(0), List(1)), spec.partition)
	}

	@Test def oneBranchCondTree(): Unit = {
		ScopeSpecification.required=0

		val scopeTree = Map("1" -> Map("T" -> Map("0" -> Map.empty)))
		val examples = List(Map("'x'" -> 12, "'y'" -> 6), Map("'x'" -> 6, "'y'" -> 3), Map("'x'" -> 2, "'y'" -> 2))
		val comment1 = new ParsedComment("0", List(examples(0), examples(1)), List("'x'"), List("'x = y + y'"))
		val comment3 = new ParsedComment("1", List(examples(2)), List("'x'"), List())
		val ScopesIno = Map("0" -> comment1, "1" -> comment3)
		val spec = ScopeSpecification.fromTree(scopeTree, ScopesIno)
		assertEquals(examples, spec.scopeExamples)
		assertEquals(1, spec.optionalVocabMakers.size)
		assertEquals((List(0,1), List()), spec.partition)
	}

}

class parseTests extends  JUnitSuite{
	@Test def basicTask():Unit={
		ScopeSpecification.required=0

		val json = """{"scopesTree":{"0":{}},"scopes":{"0":{"commentId":"0","commentExamples":[{"x":12,"y":6}, {"x":24, "y":12}],"outputVarNames":["x"],"assignments":["'x = y + y'"]}}}"""
	val spec = ScopeSpecification.fromString(json)
		assertEquals(2, spec.scopeExamples.size)
		assertEquals(0, spec.requiredVocabMakers.size)
		assertEquals(1, spec.optionalVocabMakers.size)


	}

	@Test def condTest():Unit={
		ScopeSpecification.required=0

		val json = """{"scopesTree":{"0":{"T":{"1":{}},"F":{"2":{}}}},"scopes":{"0":{"commentId":"0","commentExamples":[{"x":9,"y":3}],"outputVarNames":["x"],"assignments":[]},"1":{"commentId":"1","commentExamples":[{"x":12,"y":6}],"outputVarNames":["x"],"assignments":["'x = y + y'"]},"2":{"commentId":"2","commentExamples":[{"x":3,"y":1}],"outputVarNames":["x"],"assignments":["'x = 3*y'"]}}}"""
		val spec = ScopeSpecification.fromString(json)
		assertEquals(3, spec.scopeExamples.size)

	}
}