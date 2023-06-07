package edu.ucsd.snippy.scoopy

import edu.ucsd.snippy.SynthesisTask.{Context, reserved_names}
import edu.ucsd.snippy.{DebugPrints, InputParser}

class ParsedComment (commentId:String, commentExamples: List[Map[String, Any]], outputVarNames:List[String], assignments:List[String]){
	var siblingsVars = List();
	def getOutputVarNames(): List[String] = outputVarNames

	def getAssigns():List[String] = assignments

	def getContext():List[Context]={
		commentExamples.map {
			env => env.filter(entry => !outputVarNames.contains(entry._1))
		}
	}

	def inputVarNames():List[String]={
		commentExamples.map {
			env => env.filter(entry => !outputVarNames.contains(entry._1))
		}.flatMap(_.keys).toSet.toList ++ siblingsVars
	}

	def getExamples():List[Map[String, Any]]=commentExamples

}

object ParsedComment{
	def fromMap(input:Map[String, Any]):ParsedComment={
		val commentId = input("commentId").toString
		val commentExamplesTmp = input("commentExamples").asInstanceOf[List[Map[String, Any]]]
		val commentExamples = commentExamplesTmp.map(cleanupInputs(_))
		val outputVarNames = input("outputVarNames").asInstanceOf[List[String]]
		val assignments = input("assignments").asInstanceOf[List[String]]
		new ParsedComment(commentId, commentExamples, outputVarNames, assignments)
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