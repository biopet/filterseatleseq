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

import scala.collection.mutable
import scala.io.{BufferedSource, Source}

object Filter extends ToolCommand[Args] {
  def emptyArgs = Args()
  def argsParser = new ArgsParser(this)

  def main(args: Array[String]): Unit = {
    val cmdArgs = cmdArrayToArgs(args)

    logger.info("Start")

    filterSingleFile(
      cmdArgs.inputFile,
      cmdArgs.intervals,
      cmdArgs.outputFile,
      cmdArgs.geneColapseOutput,
      cmdArgs.fieldMustContain,
      cmdArgs.fieldMustBeBelow,
      cmdArgs.fieldMustBeAbove
    )

    logger.info("Done")
  }

  def filterSingleFile(
      inputFile: File,
      intervals: Option[File],
      outputFile: File,
      geneColapseOutput: Option[File],
      fieldMustContain2: List[(String, String)],
      fieldMustBeBelow: List[(String, Double)],
      fieldMustBeAbove: List[(String, Double)]): Map[String, Int] = {
    val regions = intervals.map(BedRecordList.fromFile)

    val openFile: BufferedSource = Source.fromInputStream(
      if (inputFile.getName.endsWith(".gz"))
        new GZIPInputStream(
          new BufferedInputStream(new FileInputStream(inputFile)))
      else new FileInputStream(inputFile))

    val lineIt = openFile.getLines()
    val headerLine = lineIt.next()
    val headerValues = headerLine.split("\t")
    val header = headerValues.zipWithIndex.toMap
    val chrIdx = header("chromosome")
    val posIdx = header("position")
    val genesIds = header("geneList")

    val fieldMustContain =
      fieldMustContain2.map {
        case (heading, value) =>
          (header.getOrElse(
             heading,
             throw new IllegalArgumentException(s"Key '$heading' not found")),
           value)
      }

    val mustBeBelowFields =
      fieldMustBeBelow.map {
        case (heading, value) =>
          (header.getOrElse(
             heading,
             throw new IllegalArgumentException(s"Key '$heading' not found")),
           value)
      }

    val mustBeAboveFields =
      fieldMustBeAbove.map {
        case (heading, value) =>
          (header.getOrElse(
             heading,
             throw new IllegalArgumentException(s"Key '$heading' not found")),
           value)
      }

    val writer = new PrintWriter(outputFile)
    val positions = mutable.Set.empty[(String, String, Int)]
    writer.println(headerLine)
    lineIt.zipWithIndex
      .filter(_._1.nonEmpty)
      .filter(!_._1.startsWith("#"))
      .foreach {
        case (line, lineIdx) =>
          try {
            val values = line.split("\t")
            val contig = values(chrIdx)
            val pos = values(posIdx).toInt

            val regionCheck = regions.forall { r =>
              r.overlapWith(BedRecord(contig, pos, pos)).nonEmpty
            }

            lazy val mustContain =
              fieldMustContain.forall {
                case (idx, value) => values(idx).contains(value)
              }

            lazy val mustBeBelow = mustBeBelowFields.forall {
              case (key, cutoff) =>
                values(key) match {
                  case "NA" | "unknown" => true
                  case x =>
                    try {
                      x.toDouble <= cutoff
                    } catch {
                      case e: NumberFormatException =>
                        logger.warn(
                          s"Value '$x' is not a number, ignoring field '${headerValues(
                            key)}' for line ${lineIdx + 2}, file: $inputFile")
                        true
                    }
                }

            }

            lazy val mustBeAbove = mustBeAboveFields.forall {
              case (key, cutoff) =>
                values(key) match {
                  case "NA" | "unknown" => false
                  case x =>
                    try {
                      x.toDouble >= cutoff
                    } catch {
                      case e: NumberFormatException =>
                        logger.warn(
                          s"Value '$x' is not a number, ignoring field '${headerValues(
                            key)}' for line ${lineIdx + 2}, file: $inputFile")
                        true
                    }
                }
            }

            if (regionCheck && mustContain && mustBeBelow && mustBeAbove) {
              values(genesIds).split(",").foreach { gene =>
                positions.+=((gene, contig, pos))
              }
              writer.println(line)
            }
          } catch {
            case e: Exception =>
              throw new IllegalStateException(
                s"Something did go wrong at line ${lineIdx + 2}, file: $inputFile",
                e)
          }
      }

    val geneCounts = positions.groupBy { case (gene, _, _) => gene }.map {
      case (gene, list) => gene -> list.size
    }
    val geneWriter = geneColapseOutput.map(new PrintWriter(_))
    geneWriter.foreach(_.println("#Gene\tcounts"))
    geneWriter.foreach { w =>
      geneCounts.foreach {
        case (gene, count) =>
          w.println(gene + "\t" + count)
      }
    }

    writer.close()
    openFile.close()
    geneWriter.foreach(_.close())

    geneCounts
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
                 "<input file>",
                 "-o",
                 "<output file>",
                 "--intervals",
                 "<bed file>")}
      |
      |Run where a field should contain the given text:
      |${example("-i",
                 "<input file>",
                 "-o",
                 "<output file>",
                 "--fieldMustContain",
                 "<field>=<text>")}
      |
    """.stripMargin
}
