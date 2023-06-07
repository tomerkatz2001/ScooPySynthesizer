package edu.ucsd.snippy.scoopy

import edu.ucsd.snippy.Snippy.synthesize
import edu.ucsd.snippy.SynthesisTask

import java.io.File
import java.time.{Duration, LocalDateTime}
import java.util.concurrent._
import scala.io.Source.fromFile

object ScoopyBenchmarksRunner extends App{
	val executorService: ExecutorService = Executors.newCachedThreadPool()

	def runBenchmark(dir: File, benchTimeout: Int = 7, pnt: Boolean = true): Unit = {
		val suite = if (dir.getParentFile.getName == "resources") "" else dir.getParentFile.getName
		val group = dir.getName
		val benchmarks =
		dir.listFiles()
			.filter(_.getName.contains(".exampled.json"))
			.filter(!_.getName.contains(".out"))
			.zipWithIndex

		benchmarks.foreach(benchmark => {
			ScopeSpecification.required=0
				val file = benchmark._1
				val name: String = file.getName.substring(0, file.getName.indexOf('.'))
				var time: Int = -1
				var count: Int = 0
				var correct: String = "Timeout"
				var variables: List[String] = List()

				try {
					val taskStr = fromFile(file).mkString
					val start = LocalDateTime.now()
					val spec = ScopeSpecification.fromString(taskStr)
					val task = SynthesisTask.fromSpec(spec)
					variables = task.outputVariables

					if (pnt) print(s"$suite,$group,$name,$variables, \n")


					val callable: Callable[(Option[String], Int, Int)] = () => synthesize(task, benchTimeout)
					val promise = this.executorService.submit(callable)
					val rs = try {
						promise.get(benchTimeout + 10, TimeUnit.SECONDS)
					} catch {
						case _: TimeoutException => (None, Duration.between(start, LocalDateTime.now()).toMillis.toInt, -1)
						case _: InterruptedException => (None, Duration.between(start, LocalDateTime.now()).toMillis.toInt, -1)
						case e: ExecutionException => throw e.getCause
					} finally {
						promise.cancel(true)
					}

					rs match {
						case (Some(program: String), tim: Int, coun: Int) =>
							time = Duration.between(start, LocalDateTime.now()).toMillis.toInt
							count = coun
							println("the proram is: " + program)

//							correct = task.get("solutions") match {
//								case Some(solutions) if solutions.asInstanceOf[List[String]].contains(program) => "+"
//								case Some(_) => "-"
//								case None => "?"
//							}
						case (None, _, coun: Int) =>
							count = coun
					}
				} catch {
					case e: AssertionError => throw e
					case _: java.lang.OutOfMemoryError => correct = "OutOfMemory"
					case e: Throwable => correct = e.toString
				}

				if (pnt) println(s"$time,$count,$correct")
				Runtime.getRuntime.gc()
			})
	}


		val benchmarksDir = new File("synthesizer/src/test/resources/Tomer/evaluation")
		assert(benchmarksDir.isDirectory)


		var filterArgs = args
		val timeout: Int = args.lastOption.map(_.toIntOption) match {
			case Some(Some(t)) => {
				filterArgs = args.dropRight(1)
				t
			}
			case _ => 5 * 60
		}

		println("suite,group,name,variables,time,count,correct")
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
		benchmarks.foreach(this.runBenchmark(_, 10, pnt = true))

		// Then actually run
		//benchmarks.foreach(this.runBenchmark(_, timeout))



}
