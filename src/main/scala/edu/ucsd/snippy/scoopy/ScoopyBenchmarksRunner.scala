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
					val topLevelExamples = spec.getPrevEnvsAndEnvs()._2.length
					val cond = if(operators.contains("cond")) "+" else "-"
					val concat = if(operators.contains("concat")) "+" else "-"
					//variables = task.outputVariables.toList

					if (pnt) print(s"$suite;$group;$name;$variables;$cond;$concat; ")


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
							//println("\nthe program is: \n" + program)
							print("\nthe program is: \n" + assignment.code())
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
					println(s"$time;$count;$correct")
				}
				Runtime.getRuntime.gc()
			})
	}


		val benchmarksDir = new File("synthesizer/src/test/resources/scoopy")
		assert(benchmarksDir.isDirectory)


		var filterArgs = args
		val timeout: Int = args.lastOption.map(_.toIntOption) match {
			case Some(Some(t)) => {
				filterArgs = args.dropRight(1)
				t
			}
			case _ => 10 * 60
		}

		println("suite;group;name;variables;top_level_examples;cond;concat;time;count;correct")
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
		benchmarks.foreach(this.runBenchmark(_, 700, pnt = true))

		// Then actually run
		//benchmarks.foreach(this.runBenchmark(_, timeout))



}
