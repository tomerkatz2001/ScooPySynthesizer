package edu.ucsd.snippy

import edu.ucsd.snippy.Snippy.synthesize
import edu.ucsd.snippy.utils.Assignment
import net.liftweb.json
import net.liftweb.json.JObject

import java.io.File
import java.time.{Duration, LocalDateTime}
import java.util.concurrent._
import scala.io.Source.fromFile

object BenchmarksCSV extends App
{
	val executorService: ExecutorService = Executors.newCachedThreadPool()

	def runBenchmark(dir: File, benchTimeout: Int = 7, pnt: Boolean = true): Unit = {
		val suite = if (dir.getParentFile.getName == "resources") "" else dir.getParentFile.getName
		val group = dir.getName
		dir.listFiles()
			.filter(_.getName.contains("json"))
			.filter(!_.getName.contains(".out"))
			.sorted
			.zipWithIndex
			.foreach(benchmark => {
				val file = benchmark._1
				val name: String = file.getName.substring(0, file.getName.indexOf('.'))
				var time: Int = -1
				var count: Int = 0
				var correct: String = "Timeout"
				var variables = -1

				try {
					val taskStr = fromFile(file).mkString
					val task = json.parse(taskStr).asInstanceOf[JObject].values
					val examples = task("envs").asInstanceOf[List[Any]].length
					variables = task("varNames").asInstanceOf[List[String]].length

					if (pnt) print(s"$suite,$group,$name,$variables,$examples,")

					val start = LocalDateTime.now()
					val callable: Callable[(Option[String], Int, Int, Option[Assignment])] = () => synthesize(taskStr, benchTimeout)

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
						case (Some(program: String), tim: Int, coun: Int, _ ) =>
							//print("the program is: " + program + "\n");
							time = tim
							count = coun
							correct = task.get("solutions") match {
								case Some(solutions) if solutions.asInstanceOf[List[String]].contains(program) => "+"
								case Some(_) => "-"
								case None => "?"
							}
						case (None, _, coun: Int, _) =>
							count = coun
					}
				} catch {
					case e: AssertionError => throw e
					case _: java.lang.OutOfMemoryError => correct = "OutOfMemory"
					case e: Throwable => correct = s"Error ${e}" //sys.process.stderr.println(e)
				}

				if (pnt) println(s"$time,$count,$correct")
				Runtime.getRuntime.gc()
			})
	}
	val patha = "src/test/resources/scoopy-flat"
	val pathb = "C:\\Users\\tomerkatz\\Desktop\\LooPy\\synthesizer\\src\\test\\resources\\scoopy-flat-plus"
	val pathc = "C:\\Users\\tomerkatz\\Desktop\\loopy_new_bench"
	val benchmarksDir = new File(pathc)
	assert(benchmarksDir.isDirectory)

	DebugPrints.debug = false
	DebugPrints.info = false

	var filterArgs = args
	val timeout: Int = args.lastOption.map(_.toIntOption) match {
		case Some(Some(t)) => {
			filterArgs = args.dropRight(1)
			t
		}
		case _ => 10 * 60
	}

	println("suite,group,name,variables,examples_count,time,count,correct")
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
	benchmarks.foreach(this.runBenchmark(_, 10))
}
