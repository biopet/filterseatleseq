package nl.biopet.tools.seattleseqkit.mergegenes

import java.io.PrintWriter

import nl.biopet.utils.tool.ToolCommand

import scala.io.Source

object MergeGenes extends ToolCommand[Args] {
  def emptyArgs = Args()
  def argsParser = new ArgsParser(this)

  def main(args: Array[String]): Unit = {
    val cmdArgs = cmdArrayToArgs(args)

    logger.info("Start")

    val counts = cmdArgs.inputFiles.map {
      case (sample, file) =>
        val reader = Source.fromFile(file)
        val it = reader.getLines()
        sample -> it
          .filter(!_.startsWith("#"))
          .map(_.split("\t"))
          .map(x => (x(0), x(1).toInt))
          .toMap
    }
    val genes = counts.flatMap(_._2).keySet.toList.sorted
    val samples = counts.keySet.toList.sorted

    val writer = new PrintWriter(cmdArgs.outputFile)
    writer.println("Gene\t" + samples.mkString("\t"))

    for (gene <- genes) {
      writer.print(gene + "\t")
      writer.println(samples.map(counts(_).getOrElse(gene, 0)).mkString("\t"))
    }

    writer.close()

    logger.info("Done")
  }

  def descriptionText: String =
    """
      |
    """.stripMargin

  def manualText: String =
    """
      |
    """.stripMargin

  def exampleText: String =
    """
      |
    """.stripMargin

}
