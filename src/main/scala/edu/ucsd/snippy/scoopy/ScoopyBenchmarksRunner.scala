package edu.ucsd.snippy.scoopy

import edu.ucsd.snippy.utils.Assignment
import net.liftweb.json
import net.liftweb.json.JObject

import java.io.File
import java.time.{Duration, LocalDateTime}
import java.util.concurrent._
import scala.io.Source.fromFile

object ScoopyBenchmarksRunner extends App{
	val executorService: ExecutorService = Executors.newCachedThreadPool()

	def runBenchmark(dir: File, benchTimeout: Int = 10, pnt: Boolean = true): Unit = {
		val suite = if (dir.getParentFile.getName == "resources") "" else dir.getParentFile.getName
		val group = dir.getName
		val benchmarks =
		dir.listFiles()
			.filter(_.getName.contains(".json"))
			.filter(!_.getName.contains(".out"))
			.zipWithIndex

		benchmarks.foreach(benchmark => {
				val file = benchmark._1
				val name: String = file.getName.substring(0, file.getName.indexOf('.'))
				var time: Int = -1
				var count: Int = 0
				var correct: String = "Timeout"
				var variables: List[String] = List()

				try {
					val taskStr = fromFile(file).mkString
					val task = json.parse(taskStr).asInstanceOf[JObject].values
					val start = LocalDateTime.now()
					val spec = (ScopeSpecification.fromString(taskStr))
					val operators = spec.appliedOperators
					//val task = SynthesisTask.fromSpec(spec, List())
					variables = spec.outputVarNames.toList
					val Examples = spec.totalExamplesCount
					val cond = if(operators.contains("cond")) "+" else "-"
					val concat = if(operators.contains("concat")) "+" else "-"
					//variables = task.outputVariables.toList
					val row = s"$suite;$group;$name;$variables;$Examples;$cond;$concat; ";
					//val row = f"${suite}%10s|${group}%10s|${name}%20s|${variables}%20s|${Examples}%8s|${cond}%5s|${concat}%5s|";
					if (pnt) print(row)


					val callable: Callable[(Option[String], Int, Int,  Option[Assignment])] = () => spec.solve(benchTimeout)
					val promise = this.executorService.submit(callable)
					val rs = try {
						promise.get(benchTimeout, TimeUnit.SECONDS)
					} catch {
						case _: TimeoutException => (None, Duration.between(start, LocalDateTime.now()).toMillis.toInt, -1, None)
						case _: InterruptedException => (None, Duration.between(start, LocalDateTime.now()).toMillis.toInt, -1, None)
						case e: ExecutionException => throw e.getCause
					} finally {
						promise.cancel(true)
					}

					rs match {
						case (Some(program: String), tim: Int, coun: Int, Some(assignment: Assignment)) =>
							time = Duration.between(start, LocalDateTime.now()).toMillis.toInt
							count = coun
							println("\nthe program is: \n" + program)
							//print("\nthe program is: \n" + assignment.code())
							correct = task("solutions") match {
								case solutions if solutions.asInstanceOf[List[String]].contains(program) => "+"
								case Some(_) =>  "-";
								case None => "?"
								case _ =>  "-"
							}
						case (None, _, coun: Int, _) =>
							count = coun
					}
				} catch {
					case e: AssertionError => throw e
					case _: java.lang.OutOfMemoryError => correct = "OutOfMemory"
					case e: Throwable => correct = e.getStackTrace.mkString("\n")
				}

				if (pnt) {
					val row = s"$time;$count;$correct";
					//val row = f"${time}%10s|${count}%10s|${correct}%10s"
					println(row)
				}
				Runtime.getRuntime.gc()
			})
	}

		val patha = "C:\\Users\\tomerkatz\\Desktop\\test"
		val pathb = "src/test/resources/scoopy"
		val benchmarksDir = new File(pathb);
		assert(benchmarksDir.isDirectory)


		var filterArgs = args
		val timeout: Int = args.lastOption.map(_.toIntOption) match {
			case Some(Some(t)) => {
				filterArgs = args.dropRight(1)
				t
			}
			case _ => 10 * 60
		}
		//val headers = f"${"suite"}%10s|${"group"}%10s|${"name"}%20s|${"variables"}%20s|${"examples"}%8s|${"cond"}%2s|${"concat"}%2s|${"time"}%10s|${"count"}%10s|${"correct"}%10s"
		val headers = "suite;group;name;variables;examples_count;cond;concat;time;count;correct"
		println(headers)
		val benchmarks = if (filterArgs.nonEmpty) {
			benchmarksDir.listFiles()
				.flatMap(f => if (f.isDirectory) f :: f.listFiles().toList else Nil)
				.filter(_.isDirectory)
				.filter(dir => if (dir.getParentFile.getName == "resources") filterArgs.contains(dir.getName)
				else filterArgs.contains(dir.getName) || filterArgs.contains(dir.getParentFile.getName))
				.toList
		} else {
			benchmarksDir.listFiles()
				.flatMap(f => if (f.isDirectory) f :: f.listFiles().toList else Nil)
				.filter(_.isDirectory)
				.sortBy(_.getName)(Ordering.String)
				.toList
		}
		// First, warm up
		//benchmarks.foreach(this.runBenchmark(_, 30, pnt = false))

		// Then actually run
		benchmarks.foreach(this.runBenchmark(_, 10, pnt = true))



}
