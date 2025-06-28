package edu.ucsd.snippy.predicates

import edu.ucsd.snippy.ast.ASTNode
import edu.ucsd.snippy.enumeration.{Contexts, OEValuesManager}
import edu.ucsd.snippy.utils.{Assignment, Utils}

object Predicate {
	def getPredicate(
		varName: String,
		envs: List[Map[String, Any]],
		oeManager: OEValuesManager,
		contexts: Contexts): SingleVariablePredicate =
	{
		val values = envs.flatMap(map => map.filter(_._1 == varName).values)
		new SingleVariablePredicate(oeManager, varName, Utils.getTypeOfAll(values), values, envs, contexts)
	}
}

trait Predicate
{
	val envs:List[Map[String, Any]];
	val contexts :Contexts
	def getEnvs():List[Map[String, Any]]  ={
		val newEnvs:List[Map[String, Any]] = envs.zipWithIndex.map {
			case (env, idx) => env.map(tup => if (contexts.contexts(idx).contains(tup._1)) Map(s"${tup._1}_in" ->contexts.contexts(idx)(tup._1)) else Map[String, Any]()).toList
		}.map(l=>l.reduce((x,y)=> x ++ y))
		return newEnvs.zip(envs).map(x =>  x._1++x._2)

	}

	def evaluate(program: ASTNode): Option[Assignment]
}