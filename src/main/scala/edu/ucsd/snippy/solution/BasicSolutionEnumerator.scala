package edu.ucsd.snippy.solution

import edu.ucsd.snippy.enumeration.Enumerator
import edu.ucsd.snippy.predicates.Predicate
import edu.ucsd.snippy.utils.Assignment

@deprecated
class BasicSolutionEnumerator(val predicate: Predicate, val enumerator: Enumerator) extends SolutionEnumerator
{
	var solution: Option[Assignment] = None

	override def step(): Unit =
		if (solution.isEmpty && enumerator.hasNext) {
			val program = enumerator.next()
			this.solution = predicate.evaluate(program)
		}

	override def programsSeen: Int = enumerator.programsSeen
}

