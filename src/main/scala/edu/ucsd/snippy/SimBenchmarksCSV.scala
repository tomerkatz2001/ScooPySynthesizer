package edu.ucsd.snippy

import edu.ucsd.snippy.Snippy.synthesize
import net.liftweb.json
import net.liftweb.json.JObject

import java.io.File
import scala.io.Source.fromFile

object SimBenchmarksCSV extends App
{
	def runBenchmark(dir: File, benchTimeout: Int = 7, print: Boolean = true): Unit = {
		val suite = if (dir.getParentFile.getName == "resources") "" else dir.getParentFile.getName
		val group = dir.getName
		dir.listFiles()
			.filter(_.getName.contains(".json"))
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
					variables = task("varNames").asInstanceOf[List[String]].length

					synthesize(taskStr, benchTimeout, simAssign = true) match {
						case (Some(program: String), tim: Int, coun: Int, _) =>
							time = tim
							count = coun
							println("the program:" + program)

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
					case e: Throwable => correct = e.toString()
				}

				if (print) println(s"$suite,$group,$name,$variables,$time,$count,$correct")
				Runtime.getRuntime.gc()
			})
	}

	val benchmarksDir = new File("C:\\Users\\tomerkatz\\Desktop\\LooPy\\synthesizer\\src\\test\\resources\\Tomer\\Loopy")
	assert(benchmarksDir.isDirectory)

	DebugPrints.debug = false
	DebugPrints.info = false

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
	benchmarks.foreach(this.runBenchmark(_, 10, print = true))

	// Then actually run
	//benchmarks.foreach(this.runBenchmark(_, timeout))
}
