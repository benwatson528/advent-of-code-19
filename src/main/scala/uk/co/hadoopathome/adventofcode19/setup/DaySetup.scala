package uk.co.hadoopathome.adventofcode19.setup

import scala.io.Source
import scala.sys.process.Process
import scala.util.Using

object DaySetup {

  def main(args: Array[String]): Unit = {
    val day = "1"
    val puzzleUrl = "https://adventofcode.com/2019/day/" + day
    val puzzleName = getPuzzleName(puzzleUrl)

    val scriptPath = "src/main/resources/script/create-day.sh"
    val scriptRunCommand = Set("bash", scriptPath, day, puzzleName).mkString(" ")
    println("Running day creation command with:" + scriptRunCommand)
    val process = Process(scriptRunCommand).run()
    println("Exit value = " + process.exitValue())
  }

  private def getPuzzleName(puzzleUrl: String): String = {
    val entirePage = Using(Source.fromURL(puzzleUrl)) { source => source.mkString }.get
    val puzzleName = entirePage.split("--- ")(1).split(" ---")(0).split(": ")(1)
    puzzleName.replaceAll("\\s", "")
  }
}
