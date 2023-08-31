package edu.ucsd.snippy

import edu.ucsd.snippy.SynthesisTask.Context
import edu.ucsd.snippy.ast.Types.{IntList, StringList, Types}
import edu.ucsd.snippy.ast._
import edu.ucsd.snippy.enumeration.Contexts
import edu.ucsd.snippy.solution.Variable
import edu.ucsd.snippy.utils.Utils
import org.apache.commons.lang3.mutable.Mutable
import spray.json.{DefaultJsonProtocol, DeserializationException, JsArray, JsBoolean, JsNumber, JsObject, JsString, JsValue, RootJsonFormat, SerializationException, enrichAny}
import spray.json._

import scala.collection.{Seq, mutable}
import scala.sys.process.stringSeqToProcess
import scala.tools.nsc.doc.base.comment.Subscript


class PythonParser(contextsList:List[Context]){

	val typesMap: mutable.Map[String, Types] =
		collection.mutable.Map(
		contextsList.flatMap(_.keys)
		.toSet[String]
		.map(varName => varName -> Utils.getTypeOfAll(contextsList.map(ex => ex.get(varName)).filter(_.isDefined).map(_.get)))
		.filter(!_._2.equals(Types.Unknown))
		.toSeq: _*
		)
	val contexts = new Contexts(contextsList)
	trait ASTNodeJsonProtocol extends  DefaultJsonProtocol {
		implicit object ASTNodeJsonFormat extends RootJsonFormat[ASTNode] {
			def write(node: ASTNode) = node match {
				case binOp: BinaryOpNode[_] =>
					JsObject(
						"type" -> JsString("BinOp"),
						"left" -> binOp.lhs.toJson,
						"op" -> JsString(binOp.code),
						"right" -> binOp.rhs.toJson
					)
				// Add more cases for other ASTNode types
				case _ =>
					throw new SerializationException("Serialization of this ASTNode type is not supported.")
			}

			def read(value: JsValue) = value match {
				case obj: JsObject =>
					obj.getFields("_type") match {
						case Seq(JsString("Module")) =>
							obj.getFields("body") match {
								case Seq(body) => body.convertTo[ASTNode]
							}
						case Seq(JsString("Expr")) =>
							obj.getFields("value") match {
								case Seq(value) => value.convertTo[ASTNode]
							}

						case Seq(JsString("BinOp")) =>
							obj.getFields("left", "op", "right") match {
								case Seq(left, opObj, right) =>
									val op = opObj.asInstanceOf[JsObject].getFields("_type").head.toString().replace("\"", "")
									op match {
										case "Add" => plus(left.convertTo[ASTNode], right.convertTo[ASTNode])
										case "Sub" => minus(left.convertTo[ASTNode], right.convertTo[ASTNode])
										case "Mult" => times(left.convertTo[ASTNode], right.convertTo[ASTNode])
										case "Div" => div(left.convertTo[ASTNode], right.convertTo[ASTNode])
										case "Mod" => mod(left.convertTo[ASTNode], right.convertTo[ASTNode])
										case "FloorDiv" => intDivision(left.convertTo[ASTNode], right.convertTo[ASTNode])
									}
								case _ => throw new DeserializationException("BinOp error")
							}
							case(Seq(JsString("UnaryOp")))=>
							obj.getFields("op", "operand") match {
								case Seq(opObj, operand) =>
									val op = opObj.asInstanceOf[JsObject].getFields("_type").head.toString().replace("\"", "")
									op match {
										case "USub" => IntLiteral((operand.convertTo[ASTNode].asInstanceOf[IntNode]._values.head.get * -1) ,contexts.contextLen)
										case "UAdd" => operand.convertTo[ASTNode]
									}
								case _ => throw new DeserializationException("UnaryOp error")
							}
						case Seq(JsString("Constant")) =>
							obj.getFields("value") match {
								case Seq(value) =>
									value match {
										case JsString(v) => StringLiteral(v, contexts.contextLen)
										case JsNumber(x) =>
											x.toString().contains('.') match {
												case true => DoubleLiteral(x.toDouble, contexts.contextLen)
												case false => IntLiteral(x.toInt, contexts.contextLen)
											}

										case JsBoolean(v) => BoolLiteral(v, contexts.contextLen)
										case _ => throw new DeserializationException("Constant error")
									}
								case _ => throw new DeserializationException("Constant error")
							}
						case Seq(JsString("Call"))=>
							obj.getFields("func", "args") match {
								case Seq(func, args) =>
									val funcName = try{func.asInstanceOf[JsObject].getFields("attr").head.toString().replace("\"", "")}
									catch {case _: Throwable => func.asInstanceOf[JsObject].getFields("id").head.toString().replace("\"", "")}
									val calledOn = try{func.asInstanceOf[JsObject].getFields("value").head.convertTo[ASTNode]}
									catch {case _: Throwable => null}
									val argsList = try{args.asInstanceOf[JsArray].elements.map(_.convertTo[ASTNode])}
									catch {case x:Throwable=> print(x.toString) ;null}
									funcName match {
										case "capitalize" => Capitalize(calledOn.asInstanceOf[StringNode])
										case "split" => UnarySplit(calledOn.asInstanceOf[StringNode])
										case "upper" => StringUpper(calledOn.asInstanceOf[StringNode])
										case "join" => StringJoin(calledOn.asInstanceOf[StringNode], argsList.head.asInstanceOf[ListNode[String]])
										case "list" => ToList(argsList.head.asInstanceOf[StringNode])
										case "sum" => Sum(argsList.head.asInstanceOf[ListNode[Int]])
										case "len" => Length(argsList.head.asInstanceOf[IterableNode])
										case "count" => Count(calledOn.asInstanceOf[StringNode], argsList(1).asInstanceOf[StringNode])
										case "str" => IntToString(argsList.head.asInstanceOf[IntNode])
										case "isalpha" => IsAlpha(calledOn.asInstanceOf[StringNode])
										case "sorted" => SortedIntList(argsList.head.asInstanceOf[ListNode[Int]])
										case "set" =>  argsList.head match {
											case(x:ListNode[Int])=>ToSet(x)
											case x:ListNode[String]=>ToSet(x)
											case x:ListNode[Double]=>ToSet(x)}
										case _ => throw new DeserializationException("need to implement" + funcName)
									}
							}
						case Seq(JsString("ListComp"))=>
							obj.getFields("elt", "generators") match {
								case Seq(elt, gen) =>

									gen.asInstanceOf[JsArray].elements.head.asInstanceOf[JsObject].getFields("iter", "target") match {
										case Seq(iter, target)=>
											val iterNode = iter.asInstanceOf[JsObject].convertTo[ASTNode]
											val name = target.asInstanceOf[JsObject].getFields("id").head.toString().replace("\"", "")
											val targetNode = iterNode match {
												case StringList => {
													typesMap += (f"'$name'" -> Types.String)
													StringVariable(f"'$name'",contexts.contexts)}
												case IntList => {
													typesMap += (f"'$name'"  -> Types.IntList)
													IntVariable(f"'$name'",contexts.contexts)
												}
												case _ => {
													typesMap += (f"'$name'"  -> Types.String)
													StringVariable(f"'$name'",contexts.contexts)
												}// list comp od string
											}


											val eltNode = elt.asInstanceOf[JsObject].convertTo[ASTNode]
											listComp(eltNode, iterNode, targetNode)
									}
								case _ => throw new DeserializationException("listCompError")
							}
						case Seq(JsString("Subscript"))=> {
							obj.getFields("value", "slice") match {
								case Seq(value, slice) =>
									val valueNode = value.asInstanceOf[JsObject].convertTo[ASTNode]
									val sliceNode = slice.asInstanceOf[JsObject].convertTo[ASTNode]
									if(sliceNode.getClass == IntLiteral)
										valueNode match {
											case n:ListNode[Int] => ListStep[Int](valueNode.asInstanceOf[ListNode[Int]], sliceNode.asInstanceOf[IntNode])
											case n:ListNode[String] => ListStep[String](valueNode.asInstanceOf[ListNode[String]], sliceNode.asInstanceOf[IntNode])
											case n:ListNode[Double] => ListStep[Double](valueNode.asInstanceOf[ListNode[Double]], sliceNode.asInstanceOf[IntNode])
											case _ => throw new DeserializationException("Subscript error")
										}
									else
										valueNode
								case _ => throw new DeserializationException("Subscript error")
							}
						}
						case(Seq(JsString("Slice")))=>
							obj.getFields("lower", "upper", "step") match {
								case Seq(lower, upper, step) =>
									val lowerNode = try{lower.asInstanceOf[JsObject].convertTo[ASTNode]}
									catch {case e:Exception=>null}
									val upperNode = try{upper.asInstanceOf[JsObject].convertTo[ASTNode]}
									catch {case e:Exception=>null}
									val stepNode = try{step.asInstanceOf[JsObject].convertTo[ASTNode]}
									catch {case e:Exception=>null}
									if(upperNode!=null)
										upperNode
									else
										stepNode
								case _ => throw new DeserializationException("Slice error")
							}
						case Seq(JsString("Compare")) =>
							obj.getFields("comparators", "left", "ops") match {
								case Seq(comparator, left, ops) =>
									val comparatorNode = comparator.asInstanceOf[JsArray].elements.head.convertTo[ASTNode]
									val leftNode = left.asInstanceOf[JsObject].convertTo[ASTNode]
									val opsNode = ops.asInstanceOf[JsArray].elements.head.asInstanceOf[JsObject]
									val op = opsNode.getFields("_type").head.toString().replace("\"", "")
									op match {
										case "Gt" => GreaterThan(leftNode.asInstanceOf[IntNode], comparatorNode.asInstanceOf[IntNode])
										case "In" => (comparatorNode , leftNode) match {
											case (n: ListNode[Int], x:IntNode)  => ListContains(x, n)
											case (n: ListNode[String], x:StringNode) => ListContains(x,n)
											case (n: StringNode, x:StringNode) => Contains(x, n)
											case _ => throw new DeserializationException("In error")
										}

									}
							}
						case Seq(JsString("Name")) =>
							obj.getFields("id") match {
								case Seq(id) =>
									val formattedId = id.toString().replace("\"", "")
									val idType= if (typesMap.contains(formattedId)) typesMap(formattedId) else Types.Unknown
									idType match {
										case Types.Int => IntVariable(formattedId, contexts.contexts)
										case Types.Bool => BoolVariable(formattedId, contexts.contexts)
										case Types.String => StringVariable(formattedId, contexts.contexts)
										case Types.IntList => ListVariable(formattedId, contexts.contexts, Types.Int)
										case Types.BoolList => ListVariable(formattedId, contexts.contexts, Types.Bool)
										case Types.StringList => ListVariable(formattedId, contexts.contexts, Types.String)
										case Types.DoubleList => ListVariable(formattedId, contexts.contexts, Types.Double)
										case Types.IntSet => SetVariable(formattedId, contexts.contexts, Types.Int)
										case Types.StringSet => SetVariable(formattedId, contexts.contexts, Types.String)
										case Types.DoubleSet => SetVariable(formattedId, contexts.contexts, Types.Double)
										case Types.StringStringMap => MapVariable(formattedId, contexts.contexts, Types.String, Types.String)
										case Types.IntStringMap => MapVariable(formattedId, contexts.contexts, Types.Int, Types.String)
										case Types.IntIntMap => MapVariable(formattedId, contexts.contexts, Types.Int, Types.Int)
										case Types.StringIntMap => MapVariable(formattedId, contexts.contexts, Types.String, Types.Int)

									}

								case _ => throw new DeserializationException("Name error")
							}
							case Seq(JsString("Assign"))=>
							obj.getFields("value") match {
								case Seq(value) =>value.convertTo[ASTNode]
							}
						case Seq(JsString("If")) =>
							obj.getFields("body", "orelse", "test") match {
								case Seq(body, orelse, test) =>
									val bodyNode = body.asInstanceOf[JsArray].elements.head.convertTo[ASTNode]
									val orelseNode = orelse.asInstanceOf[JsArray].elements.head.convertTo[ASTNode]
									val testNode: BoolNode = test.asInstanceOf[JsObject].convertTo[ASTNode].asInstanceOf[BoolNode]
									SemiCondNode(testNode, bodyNode, orelseNode)
							}
						case _ => throw new DeserializationException(s"bad _type. ")
					}
				case objList: JsArray =>
					objList.elements.head.convertTo[ASTNode]
			}
		}
	}

	object ASTNodeJsonProtocol extends ASTNodeJsonProtocol

	def parse(code1:String): ASTNode ={
		typesMap += ("no_dups" -> Types.IntList)
		var code:String = code1.replace("\n","\\n").replace("\t","\\t").replace("\".\"", "\"\".\"\"")
		if (code1.charAt(0) == '\'' && code1.charAt(1) == '\'') {
			code = code1.substring(1, code1.length )
		}
		val subCommand="import json;import sys;from ast import parse;from ast2json import ast2json;print(json.dumps(ast2json(parse("+code+")), indent=4))"
		val command =
			Seq("python"
				, "-c"
				, subCommand
			).!!

		import ASTNodeJsonProtocol._
		val jsonAst = command.parseJson
		jsonAst.convertTo[ASTNode]
	}

	def plus(a: ASTNode, b: ASTNode): ASTNode = {
		(a, b) match {
			case (a: IntNode, b: IntNode) => IntAddition(a, b)
			case (a: StringNode, b: StringNode) => StringConcat(a, b)
			case (a: DoubleNode, b: DoubleNode) => DoublesAddition(a, b)
		}
	}

	def minus(a: ASTNode, b: ASTNode): ASTNode = {
		(a, b) match {
			case (a: IntNode, b: IntNode) => IntSubtraction(a, b)
			case (a: DoubleNode, b: DoubleNode) => DoublesSubtraction(a, b)
		}
	}

	def times(node1: ASTNode, node2: ASTNode): ASTNode = {
		(node1, node2) match {
			case (a: IntNode, b: IntNode) => IntMultiply(a, b)
			case (a: DoubleNode, b: DoubleNode) => DoublesMultiply(a, b)
			case (a: StringNode, b: IntNode) => StringMultiply(a, b)
		}
	}

	def intDivision(node1: ASTNode, node2: ASTNode): ASTNode = {
		(node1, node2) match {
			case (a: IntNode, b: IntNode) => IntDivision(a, b)
		}
	}

	def mod(node1: ASTNode, node2: ASTNode): ASTNode = {
		(node1, node2) match {
			case (a: IntNode, b: IntNode) => Modulo(a, b)
		}
	}

	def div(node1: ASTNode, node2: ASTNode): ASTNode = {
		(node1, node2) match { //div two ints will make problems
			case (a: DoubleNode, b: DoubleNode) => DoublesDivision(a, b)
		}
	}

	def listComp(eltNode: ASTNode, iterNode: ASTNode, targetNode: ASTNode): ASTNode = {
		(eltNode, iterNode) match {
			case (eltNode: StringNode, iterNode: StringListNode) => StringToStringListCompNode(iterNode, eltNode, targetNode.asInstanceOf[StringVariable].name)
			case(eltNode: IntNode, iterNode: StringListNode) => StringToIntListCompNode(iterNode, eltNode, targetNode.asInstanceOf[IntVariable].name)
			case(eltNode: IntNode, iterNode: StringNode) => StringToIntListCompNode(UnarySplit(iterNode), eltNode, targetNode.asInstanceOf[StringVariable].name)
			case(eltNode: StringNode, iterNode: StringNode) => StringToStringListCompNode(UnarySplit(iterNode), eltNode, targetNode.asInstanceOf[StringVariable].name)
			case(eltNode: StringNode, iterNode: IntListNode) => IntToStringListCompNode(iterNode, eltNode, targetNode.asInstanceOf[StringVariable].name)
			case(eltNode: IntNode, iterNode: IntListNode) => IntToIntListCompNode(iterNode, eltNode, targetNode.asInstanceOf[IntVariable].name)
			case(eltNode:StringNode, iterNode:ListNode[String]) => StringToStringListCompNode(iterNode, eltNode, targetNode.asInstanceOf[StringVariable].name)
		}
	}

}


