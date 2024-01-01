//import edu.ucsd.snippy.scoopy.ScopeSpecification
//import org.junit.Assert.assertEquals
//import org.junit.Test
//import org.scalatestplus.junit.JUnitSuite
//
//class ScopeSpecsTests extends JUnitSuite {
//
//
//	@Test def assignSpec(): Unit = {
//
//		val code = "x = 9+4*6";
//		val spec = ScopeSpecification.assign(code)
//		assertEquals(Tuple2(List(), List()),
//			spec.partition)
//		assertEquals(List(),
//			spec.getPrevEnvsAndEnvs()._2)
//		assertEquals(List(),
//			spec.required)
//		assertEquals(Set("x"),
//			spec.outputVarNames)
//	}
//
//	@Test def multiVarAssignSpec(): Unit = {
//		//todo: fix it later
//
//		val code = "x,y = (5,7)";
//		val spec = ScopeSpecification.assign(code)
//		assertEquals(Tuple2(List(), List()),
//			spec.partition)
//		assertEquals(List(),
//			spec.getPrevEnvsAndEnvs()._2)
//		assertEquals(List(),
//			spec.required)
//		assertEquals(Set("x", "y"),
//			spec.outputVarNames)
//	}
//
//	@Test def scopeSpec(): Unit = {
//		val code = "x=y+2"
//		val outputVarNames = Set("x")
//		val examples = List(Map("y" -> 1, "x" -> 3), Map("y" -> 2, "x" -> 4), Map("y" -> 0, "x" -> 2))
//		val assignSpec = ScopeSpecification.assign(code)
//		val spec = ScopeSpecification.scope(assignSpec, examples, outputVarNames)
//		assertEquals(Tuple2(List(), List()), spec.partition)
//		assertEquals(examples, spec.getPrevEnvsAndEnvs()._2)
//		assertEquals(List(), spec.required)
//	}
//
//	@Test def doubleScopeTest(): Unit = {
//		val code = "'x = y+2'";
//		val examples = List(Map("y" -> 2, "x" -> 4), Map("y" -> 2, "x" -> 4))
//		val outputVarNames = Set("x")
//		val spec = ScopeSpecification.assign(code)
//		val scopeSpec = ScopeSpecification.scope(spec, List(examples(0)), outputVarNames)
//		val scopeSpec2 = ScopeSpecification.scope(scopeSpec, List(examples(1)), outputVarNames)
//		assertEquals(examples, scopeSpec2.getPrevEnvsAndEnvs()._2);
//		//assertEquals(0, scopeSpec2.required.length);
//		assertEquals((List(), List()), scopeSpec2.partition);
//		assertEquals(Set("x"), scopeSpec2.outputVarNames);
//	}
//
//	@Test def toScopeable(): Unit = {
//		val code = "x=y+2"
//		val outputVarNames = Set("x")
//		val examples = List(Map("y" -> 1, "x" -> 3), Map("y" -> 2, "x" -> 4), Map("y" -> 0, "x" -> 2))
//		val assignSpec = ScopeSpecification.assign(code)
//		val specScope = ScopeSpecification.scope(assignSpec, examples, outputVarNames)
//		val spec = ScopeSpecification.toScopeable(specScope)
//		assertEquals(Tuple2(List(), List()), spec.partition)
//		assertEquals(List(), spec.getPrevEnvsAndEnvs()._2)
//		//assertEquals(1, spec.required.length)
//		assertEquals("x = y + 2", spec.solve()._4.get.code())
//		//assertEquals("if (True) y + 2 else x", spec.solve())// without postprocessing
//	}
//
//	@Test def concatScope(): Unit = {
//		val code = "x=y+2"
//		val outputVarNames = Set("x")
//		val examples = List(Map("y" -> 1, "x" -> 3, "z"-> 4), Map("y" -> 2, "x" -> 8, "z" -> 5), Map("y" -> 0, "x" -> 2, "z" -> 3))
//		val assignSpec = ScopeSpecification.assign(code)
//		val code2 = "z = x"
//		val outputVarNames2 = Set("z")
//		val assignSpec2 = ScopeSpecification.assign(code2)
//		val concatSpec = ScopeSpecification.concat(List(assignSpec, assignSpec2))
//		assertEquals(Tuple2(List(), List()), concatSpec.partition)
//		assertEquals(List(), concatSpec.getPrevEnvsAndEnvs()._2)
//		//assertEquals(0, concatSpec.required.length)
//		assertEquals(Set("x", "z"), concatSpec.outputVarNames)
//
//	}
//
//
//	@Test def condSpec(): Unit = {
//
//		val code1 = "x = 9+4*6";
//		val spec1 = ScopeSpecification.assign(code1)
//		val code2 = "x = 9+4*5";
//		val spec2 = ScopeSpecification.assign(code2)
//		val spec = ScopeSpecification.cond(spec1, spec2)
//		assertEquals(Tuple2(List(), List()), spec.partition)
//		assertEquals(List(), spec.getPrevEnvsAndEnvs()._2)
//		assertEquals(List(), spec.required)
//	}
//
//	@Test def scopeOfCondTest(): Unit = {
//		val code = "x= y+2";
//		val code2 = "x = y+3"
//		val examples = List(Map("y" -> 1, "x" -> 3), Map("y" -> 5, "x" -> 8), Map("y" -> 0, "x" -> 2))
//		val outputVarNames = Set("x")
//
//		val spec1 = ScopeSpecification.assign(code)
//		val spec1_scoped = ScopeSpecification.scope(spec1, List(examples(0)), outputVarNames)
//
//		val spec2 = ScopeSpecification.assign(code2)
//		val spec2_scoped = ScopeSpecification.scope(spec2, List(examples(1)), outputVarNames)
//
//		val condSpec = ScopeSpecification.cond(spec1_scoped, spec2_scoped)
//		val scopeSpec = ScopeSpecification.scope(condSpec, List(examples(2)), outputVarNames)
//
//		assertEquals(examples, scopeSpec.getPrevEnvsAndEnvs()._2)
//		assertEquals((List(0), List(1)), scopeSpec.partition)
//		assertEquals(examples, scopeSpec.getPrevEnvsAndEnvs()._2)
//		assertEquals(List(), scopeSpec.required)
//	}
//
//	@Test def concatCond(): Unit = {
//		val thenCode = "x=y+2"
//		val elseCode = "x=y+3"
//		val postCondCode = "z = y"
//		val condOutputVarNames = Set("x")
//		val examples = List(Map("y" -> 1, "x" -> 3, "z" -> 1), Map("y" -> 2, "x" -> 5, "z" -> 2), Map("y" -> 0, "x" -> 2, "z" -> 0))
//		val ThenAssignSpec = ScopeSpecification.assign(thenCode)
//		val ThenScopeSpec = ScopeSpecification.scope(ThenAssignSpec, (examples(0)::examples(2)::Nil), condOutputVarNames)
//		val ElseAssignSpec = ScopeSpecification.assign(elseCode)
//		val ElseScopeSpec = ScopeSpecification.scope(ElseAssignSpec, (examples(1)::Nil), condOutputVarNames)
//
//		val condSpec = ScopeSpecification.cond(ThenScopeSpec, ElseScopeSpec)
//
//		val postCondAssignSpec = ScopeSpecification.assign(postCondCode)
//
//		val concatSpec = ScopeSpecification.concat(List(condSpec, postCondAssignSpec))
//		assertEquals(Tuple2(List(), List()), concatSpec.partition)
//		assertEquals(List(), concatSpec.getPrevEnvsAndEnvs()._2)
//		//assertEquals(1, concatSpec.required.length)
//		assertEquals(Set("x", "z"), concatSpec.outputVarNames)
//	}
//	/*
//	@Test def synthSpec():Unit={
//		val code = "'x=y+2'"
//		val outputVarNames = List("'x'")
//		val examples = List(Map("'y'" -> 1, "'x'" -> 3), Map("'y'" -> 2, "'x'" -> 4), Map("'y'" -> 0, "'x'" -> 2))
//		val contexts: List[Context] = examples.map {
//			env => env.filter(entry => !outputVarNames.contains(entry._1))
//		}
//		val assignSpec = ScopeSpecification.assign(code, contexts)
//		val specScope = ScopeSpecification.scope(assignSpec, examples, outputVarNames)
//		val spec = ScopeSpecification.toScopeable(specScope)
//		val synthesisTask = spec.required.head.apply(List())
//		assertEquals(List("'x'"), synthesisTask.outputVariables)
//	}
//
//
//	@Test def scopeTest(): Unit= {
//
//		val code = "'x = y+2'";
//		val examples = List(Map("'y'" -> 1, "'x'"->3), Map("'y'" -> 2, "'x'"->4))
//		val outputVarNames = List("'x'")
//		val contexts: List[Context] = examples.map {
//			env => env.filter(entry => !outputVarNames.contains(entry._1))
//		}
//		val spec = ScopeSpecification.assign(code,contexts)
//		val scopeSpec = ScopeSpecification.scope(spec, examples, outputVarNames)
//
//		assertEquals(examples, scopeSpec.scopeExamples);
//
//
//		val synthTask = SynthesisTask.fromSpec(scopeSpec)
//		val solution = synthesize(synthTask, 10);
//		assert(solution._1.isDefined)
//		assertEquals("'x' = 'y' + 2", solution._1.get)
//
//	}
//
//
//
//	}
//*/
//}
///*
//
//class fromSpecToTaskTests extends JUnitSuite{
//
//	@Test def ScopeToTask():Unit={
//		ScopeSpecification.required=0
//		val examples = List(Map("'x'"-> 33, "'y'"-> 4))
//		val outputVarNames = List("'x'")
//		val contexts: List[Context] = examples.map {
//			env => env.filter(entry => !outputVarNames.contains(entry._1))
//		}
//		val spec0 = ScopeSpecification.assign("'x = 9+y*6'",contexts)
//
//		val spec1 = ScopeSpecification.scope(spec0, examples,outputVarNames )
//
//		val synthTask = SynthesisTask.fromSpec(spec1)
//		val solution = synthesize(synthTask, 10);
//		assert(solution._1.isDefined)
//		assertEquals(1, solution._3)
//		print(solution._1.get)
//	}
//
//}
//*/
//
//class fromTreeTests extends  JUnitSuite{
//
////	@Test def simpleTree(): Unit ={
////		val json = """{"scopesTree":{"0":{}},"scopes":{"0":{"commentId":"0","commentExamples":[{"x":"9","y":"3"}],"outputVarNames":["x"],"assignments":["x = y * 3"]}}}"""
////		val spec = ScopeSpecification.fromString(json)
////		assertEquals("scope", spec.scopeType)
////		assertEquals(List(Map("x"-> 9, "y"-> 3)), spec.getPrevEnvsAndEnvs()._2)
////		assertEquals(Set("x"), spec.outputVarNames)
////		assertEquals((List(), List()), spec.partition)
////
////	}
////
////	@Test def doubleScope(): Unit = {
////		val json = """{"scopesTree":{"0":{"NB":{"1":{}}}},"scopes":{"0":{"commentId":"0","commentExamples":[{"x":"12","y":"3"}],"outputVarNames":["x"],"assignments":[]},"1":{"commentId":"1","commentExamples":[{"x":"12","y":"6"}],"outputVarNames":["x"],"assignments":["'x = y + y'"]}}}"""
////		val spec = ScopeSpecification.fromString(json)
////		assertEquals(List(Map("x" -> 12, "y" -> 6), Map("x" -> 12, "y" -> 3)), spec.getPrevEnvsAndEnvs()._2)
////		//assertEquals(0, spec.required.length)
////
////	}
////	@Test def condTest(): Unit = {
////
////		val json = """{"scopesTree":{"0":{"T":{"1":{}},"F":{"2":{}}}},"scopes":{"0":{"commentId":"0","commentExamples":[{"x":"9","y":"3"}],"outputVarNames":["x"],"assignments":[]},"1":{"commentId":"1","commentExamples":[{"x":"12","y":"6"}],"outputVarNames":["x"],"assignments":["'x = y + y'"]},"2":{"commentId":"2","commentExamples":[{"x":"3","y":"1"}],"outputVarNames":["x"],"assignments":["'x = 3*y'"]}}}"""
////		val spec = ScopeSpecification.fromString(json)
////		assertEquals(List( Map("x"->12, "y" -> 6), Map("x"->3, "y"->1), Map("x"-> 9, "y"-> 3)), spec.getPrevEnvsAndEnvs()._2)
////		assertEquals(Set("x"), spec.outputVarNames)
////		assertEquals((List(0), List(1)), spec.partition)
////
////	}
////
////
////	@Test def concatOneVarTest(): Unit = {
////		val json = """{"scopesTree":{"0":{"NB":{"1":{}, "2"{}}}},"scopes":{"0":{"commentId":"0","commentExamples":[{"x":"12","y":"3"}],"outputVarNames":["x"],"assignments":[]},"1":{"commentId":"1","commentExamples":[{"x":"12","y":"6"}],"outputVarNames":["x"],"assignments":["'x = y + y'"]},"2":{"commentId":"2","commentExamples":[{"x":"2","y":"1"}],"outputVarNames":["x"],"assignments":["'x = y + y'"]}}}"""
////		val spec = ScopeSpecification.fromString(json)
////		assertEquals(List(Map("x"-> 12, "y"-> 3)), spec.getPrevEnvsAndEnvs()._2)
////		//assertEquals(2, spec.required.length)
////		print(ScopeSpecification.toScopeable(spec).solve()._4.get.code())
////
////	}
//
//
//	/*
//	@Test def OneDepthTreeTest(): Unit = {
//		val scopeTree = Map("0"->Map.empty)
//		val example = Map("'x'"-> 12, "'y'"-> 6)
//		val ScopesIno = Map("0"->new ParsedComment("0",List(example), List("'x'"), List("'x = y + y'")))
//		val spec = ScopeSpecification.fromTree(scopeTree, ScopesIno)
//		assertEquals(List(example), spec.scopeExamples)
//		assertEquals(1, spec.requiredVocabMakers.size)
//	}
//
//	@Test def doubleScopeTree():Unit = {
//		ScopeSpecification.required=0
//
//		val scopeTree = Map("1" -> Map("NoBranch"->Map("0"->Map.empty)))
//		val examples = List(Map("'x'" -> 12, "'y'" -> 6), Map("'x'" -> 6, "'y'" -> 3))
//		val comment1 = new ParsedComment("0", List(examples(0)), List("'x'"), List("'x = y + y'"))
//		val comment2 = new ParsedComment("1", List(examples(1)), List("'x'"), List())
//		val ScopesIno = Map("0" -> comment1, "1" -> comment2)
//		val spec = ScopeSpecification.fromTree(scopeTree, ScopesIno, false)
//		assertEquals(examples, spec.scopeExamples)
//		assertEquals(1, spec.optionalVocabMakers.size)
//		assertEquals(0, spec.requiredVocabMakers.size)
//	}
//
//	@Test def condTree(): Unit = {
//		ScopeSpecification.required=0
//
//		val scopeTree = Map("2" -> Map("T" -> Map("0" -> Map.empty), "F" -> Map("1" -> Map.empty)))
//		val examples = List(Map("'x'" -> 12, "'y'" -> 6), Map("'x'" -> 3, "'y'" -> 3), Map("'x'" -> 2, "'y'" -> 2))
//		val comment1 = new ParsedComment("0", List(examples(0)), List("'x'"), List("'x = y + y'"))
//		val comment2 = new ParsedComment("1", List(examples(1)), List("'x'"), List("'x = y'"))
//		val comment3 = new ParsedComment("2", List(examples(2)), List("'x'"), List())
//		val ScopesIno = Map("0" -> comment1, "1" -> comment2, "2" -> comment3)
//		val spec = ScopeSpecification.fromTree(scopeTree, ScopesIno)
//		assertEquals(examples, spec.scopeExamples)
//		assertEquals(2, spec.optionalVocabMakers.size)
//		assertEquals((List(0), List(1)), spec.partition)
//	}
//
//	@Test def oneBranchCondTree(): Unit = {
//		ScopeSpecification.required=0
//
//		val scopeTree = Map("1" -> Map("T" -> Map("0" -> Map.empty)))
//		val examples = List(Map("'x'" -> 12, "'y'" -> 6), Map("'x'" -> 6, "'y'" -> 3), Map("'x'" -> 2, "'y'" -> 2))
//		val comment1 = new ParsedComment("0", List(examples(0), examples(1)), List("'x'"), List("'x = y + y'"))
//		val comment3 = new ParsedComment("1", List(examples(2)), List("'x'"), List())
//		val ScopesIno = Map("0" -> comment1, "1" -> comment3)
//		val spec = ScopeSpecification.fromTree(scopeTree, ScopesIno)
//		assertEquals(examples, spec.scopeExamples)
//		assertEquals(1, spec.optionalVocabMakers.size)
//		assertEquals((List(0,1), List()), spec.partition)
//	}
//
//}
//
//class parseTests extends  JUnitSuite{
//	@Test def basicTask():Unit={
//		ScopeSpecification.required=0
//
//		val json = """{"scopesTree":{"0":{}},"scopes":{"0":{"commentId":"0","commentExamples":[{"x":12,"y":6}, {"x":24, "y":12}],"outputVarNames":["x"],"assignments":["'x = y + y'"]}}}"""
//	val spec = ScopeSpecification.fromString(json)
//		assertEquals(2, spec.scopeExamples.size)
//		assertEquals(0, spec.requiredVocabMakers.size)
//		assertEquals(1, spec.optionalVocabMakers.size)
//
//
//	}
//
//
//	 */
//
//}
