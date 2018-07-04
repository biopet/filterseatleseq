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

import java.io.{File, PrintWriter}

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

    writeCounts(mergeGeneCounts(counts), cmdArgs.outputFile)

    logger.info("Done")
  }

  def mergeGeneCounts(
      input: Map[String, Map[String, Int]]): Map[String, Map[String, Int]] = {
    val genes = input.values.flatMap(_.keySet).toSet
    val samples = input.keySet

    genes
      .map(gene =>
        gene -> samples.map(s => s -> input(s).getOrElse(gene, 0)).toMap)
      .toMap
  }

  def writeCounts(counts: Map[String, Map[String, Int]],
                  outputFile: File): Unit = {
    val genes = counts.keys.toList.sorted
    val samples =
      counts.flatMap { case (_, g) => g.keys }.toList.distinct.sorted
    val writer = new PrintWriter(outputFile)
    writer.println("Gene\t" + samples.mkString("\t"))

    for (gene <- genes) {
      writer.print(gene + "\t")
      val geneCounts = counts(gene)
      writer.println(samples.map(geneCounts.getOrElse(_, 0)).mkString("\t"))
    }
    writer.close()
  }

  def descriptionText: String =
    """
      |This tool can merge gene counts from the filter step into 1 combined matrix. Genes that are not there will be filled with 0.
    """.stripMargin

  def manualText: String =
    """
      |The count files per sample are not required to have counts for all genes. Everything that is not share will become 0.
      |The number of files is unlimited, more files only means more memory.
    """.stripMargin

  def exampleText: String =
    s"""
      |Default run to merge 3 samples:
      |${example("-i",
                 "<sample1 key>=<gene count file>",
                 "-i",
                 "<sample2 key>=<gene count file>",
                 "-i",
                 "<sample3 key>=<gene count file>",
                 "-o",
                 "<output file>")}
      |
    """.stripMargin

}
