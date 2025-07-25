package edu.ucsd.snippy.predicates

import edu.ucsd.snippy.ast.ASTNode
import edu.ucsd.snippy.enumeration.Contexts
import edu.ucsd.snippy.utils.BasicMultivariableAssignment

import scala.collection.mutable

class BasicMultivariablePredicate(val predicates: Map[String, SingleVariablePredicate]) extends Predicate {
	val programs: mutable.Map[String, Option[ASTNode]] = new mutable.HashMap().addAll(predicates.keys.map(_ -> None))
	val envs: List[Map[String, Any]] = predicates.values.head.envs //TODO make sure the head have the same env as the rest
	val contexts: Contexts = predicates.values.head.contexts //TODO make sure the head have the same context as the rest
	override def evaluate(program: ASTNode): Option[BasicMultivariableAssignment] = {
		this.predicates.foreachEntry((name, pred) => pred.evaluate(program) match {
			case Some(assignment) => this.programs.addOne(name, Some(assignment.program))
			case None => ()
		})

		if (this.programs.values.forall(_.isDefined)) {
			val rs = programs.map(tup => (tup._1, tup._2.get)).toList
			Some(BasicMultivariableAssignment(rs.map(_._1), rs.map(_._2)))
		} else {
			None
		}
	}
}