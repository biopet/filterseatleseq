/*
 * Copyright (c) 2018 Biopet
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

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
