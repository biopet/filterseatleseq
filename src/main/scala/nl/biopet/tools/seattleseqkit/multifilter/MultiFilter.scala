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

package nl.biopet.tools.seattleseqkit.multifilter

import java.io._

import nl.biopet.tools.seattleseqkit.filter.Filter
import nl.biopet.tools.seattleseqkit.mergegenes.MergeGenes
import nl.biopet.utils.tool.ToolCommand

object MultiFilter extends ToolCommand[Args] {
  def emptyArgs = Args()
  def argsParser = new ArgsParser(this)

  def main(args: Array[String]): Unit = {
    val cmdArgs = cmdArrayToArgs(args)

    logger.info("Start")

    val sampleCounts = cmdArgs.inputFiles.par.map {
      case (sample, file) =>
        logger.info(s"Start on sample: $sample")
        sample -> Filter.filterSingleFile(
          file,
          cmdArgs.intervals.get(sample),
          new File(cmdArgs.outputDir, sample + ".tsv"),
          Some(new File(cmdArgs.outputDir, sample + ".genes.tsv")),
          cmdArgs.fieldMustContain,
          cmdArgs.fieldMustBeBelow
        )
    }

    val geneCounts = MergeGenes.mergeGeneCounts(sampleCounts.seq)

    MergeGenes.writeCounts(geneCounts, new File(cmdArgs.outputDir, "genes.tsv"))
    MergeGenes.writeCounts(geneCounts.filter {
      case (_, v) => v.values.count(_ != 0) >= cmdArgs.multiSampleTreshold
    }, new File(cmdArgs.outputDir, "genes.multi_sample.tsv"))

    logger.info("Done")
  }

  def descriptionText: String =
    """
      |This tool can filter a seattle seq file.
      |A given bed file will only select variants inside this regions.
      |Filtering on specific fields is also possible.
    """.stripMargin

  def manualText: String =
    """
      |The seattle files should have the columns 'chromosome', 'position' and 'geneList' to work.
      |The gene output files are counted per gene and not per transcript. One variant can be counted twice here when the location is on more genes.
      |
    """.stripMargin

  def exampleText: String =
    s"""
      |Run with regions selection:
      |${example("-i",
                 "<sample>=<input file>",
                 "-o",
                 "<output dir>",
                 "--intervals",
                 "<sample>=<bed file>")}
      |
      |Run where a field should contain the given text:
      |${example("-i",
                 "<sample>=<input file>",
                 "-o",
                 "<output dir>",
                 "--fieldMustContain",
                 "<field>=<text>")}
      |
    """.stripMargin
}
