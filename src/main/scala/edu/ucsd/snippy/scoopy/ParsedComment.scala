package edu.ucsd.snippy.scoopy

import edu.ucsd.snippy.SynthesisTask.reserved_names
import edu.ucsd.snippy.{DebugPrints, InputParser}

class ParsedComment (val commentId:String, val rawCommentExamples: List[Map[String, Any]],val outputVarNames:List[String], val assignments:List[String]){


//	def getContexts():Contexts= new Contexts(getContextsFromExamples(commentExamples, outputVarNames.toSet))

//	def inputVarNames():List[String]={
//		commentExamples.map {
//			env => env.filter(entry => !outputVarNames.contains(entry._1))
//		}.flatMap(_.keys).toSet.toList
//	}



}

object ParsedComment{
	def apply(input:Map[String, Any]):ParsedComment={
		val commentId = input("commentId").toString
		val commentExamplesTmp = input("rawCommentExamples").asInstanceOf[List[Map[String, Any]]]
		//val commentExamples = commentExamplesTmp.map(cleanupInputs(_))
		val outputVarNames = input("outputVarNames").asInstanceOf[List[String]]
		val assignments = input("assignments").asInstanceOf[List[String]]
		new ParsedComment(commentId, commentExamplesTmp, outputVarNames, assignments)
	}

	def cleanupInputs(input: Map[String, Any]): Map[String, Any] = {
		val parser = new InputParser
		input
			.filter(v => !reserved_names.contains(v._1))
			// TODO Is there a cleaner way to do this?
			.filter(_._2.isInstanceOf[String])
			.map(variable => parser.parse(variable._2.asInstanceOf[String]) match {
				case None =>
					DebugPrints.eprintln(s"Input not recognized: $variable")
					(variable._1, null)
				case Some(v) =>
					(variable._1, v)
			})
			.filter(v => v._2 != null)
	}
}