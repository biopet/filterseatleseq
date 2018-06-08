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

package nl.biopet.tools.seattleseqkit.filter

import java.io._
import java.util.zip.GZIPInputStream

import nl.biopet.utils.ngs.intervals.{BedRecord, BedRecordList}
import nl.biopet.utils.tool.ToolCommand

import scala.collection.mutable.ListBuffer
import scala.io.{BufferedSource, Source}

object Filter extends ToolCommand[Args] {
  def emptyArgs = Args()
  def argsParser = new ArgsParser(this)

  def main(args: Array[String]): Unit = {
    val cmdArgs = cmdArrayToArgs(args)

    logger.info("Start")

    val regions = cmdArgs.intervals.map(BedRecordList.fromFile)

    val openFile: BufferedSource = Source.fromInputStream(
      if (cmdArgs.inputFile.getName.endsWith(".gz"))
        new GZIPInputStream(
          new BufferedInputStream(new FileInputStream(cmdArgs.inputFile)))
      else new FileInputStream(cmdArgs.inputFile))

    val lineIt = openFile.getLines()
    val headerLine = lineIt.next()
    val header = headerLine.split("\t").zipWithIndex.toMap
    val chrIdx = header("chromosome")
    val posIdx = header("position")
    val genesIds = header("geneList")

    val fieldMustContain =
      cmdArgs.fieldMustContain.map(x => (header(x._1), x._2))

    val mustBeBelowFields =
      cmdArgs.fieldMustbeBelow.map(x => (header(x._1), x._2))

    val writer = new PrintWriter(cmdArgs.outputFile)
    val geneCounts = new ListBuffer[(String, String, Int)]()
    writer.println(headerLine)
    lineIt.filter(!_.startsWith("#")).foreach { line =>
      val values = line.split("\t")
      val contig = values(chrIdx)
      val pos = values(posIdx).toInt

      val regionCheck = regions.forall { r =>
        r.overlapWith(BedRecord(contig, pos, pos)).nonEmpty
      }

      val mustContain =
        fieldMustContain.forall(x => values(x._1).contains(x._2))

      val mustBeBelow = mustBeBelowFields.forall {
        case (key, cutoff) =>
          if (values(key) == "NA") true
          else values(key).toDouble <= cutoff
      }

      if (regionCheck && mustContain && mustBeBelow) {
        values(genesIds).split(",").foreach { gene =>
          geneCounts.+=((gene, contig, pos))
        }
        writer.println(line)
      }
    }

    val geneWriter = cmdArgs.geneColapseOutput.map(new PrintWriter(_))
    geneWriter.foreach { w =>
      w.println("#Gene\tcounts")
      geneCounts.groupBy(_._1).foreach {
        case (gene, values) =>
          val count = values.map(x => (x._2, x._3)).distinct.size
          w.println(gene + "\t" + count)
      }
    }

    writer.close()
    openFile.close()
    geneWriter.foreach(_.close())
    logger.info("Done")
  }

  def descriptionText: String =
    """
      |This tool can filter a seattle seq file. A given bed file will only select variants inside this regions.
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
