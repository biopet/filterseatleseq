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

import java.io.File

import nl.biopet.utils.test.tools.ToolTest
import org.testng.annotations.Test
import nl.biopet.utils.io.getLinesFromFile

class FilterTest extends ToolTest[Args] {
  def toolCommand: Filter.type = Filter
  @Test
  def testNoArgs(): Unit = {
    intercept[IllegalArgumentException] {
      Filter.main(Array())
    }
  }

  @Test
  def testDefault(): Unit = {
    val outputFile = File.createTempFile("test.", ".txt")
    outputFile.deleteOnExit()
    Filter.main(
      Array("-i",
            resourcePath("/seattleseq.text.txt"),
            "-o",
            outputFile.getAbsolutePath))
    val lines = getLinesFromFile(outputFile)
    lines.size shouldBe 2
  }

  @Test
  def testCompressed(): Unit = {
    val outputFile = File.createTempFile("test.", ".txt")
    outputFile.deleteOnExit()
    Filter.main(
      Array("-i",
            resourcePath("/seattleseq.text.txt.gz"),
            "-o",
            outputFile.getAbsolutePath))
    val lines = getLinesFromFile(outputFile)
    lines.size shouldBe 2
  }

  @Test
  def testRegion(): Unit = {
    val outputFile = File.createTempFile("test.", ".txt")
    outputFile.deleteOnExit()
    Filter.main(
      Array("-i",
            resourcePath("/seattleseq.text.txt"),
            "-o",
            outputFile.getAbsolutePath,
            "--intervals",
            resourcePath("/region.bed")))
    val lines = getLinesFromFile(outputFile)
    lines.size shouldBe 2

    Filter.main(
      Array("-i",
            resourcePath("/seattleseq.text.txt"),
            "-o",
            outputFile.getAbsolutePath,
            "--intervals",
            resourcePath("/false_region.bed")))
    val lines2 = getLinesFromFile(outputFile)
    lines2.size shouldBe 1
  }

  @Test
  def testMustContain(): Unit = {
    val outputFile = File.createTempFile("test.", ".txt")
    outputFile.deleteOnExit()
    Filter.main(
      Array("-i",
            resourcePath("/seattleseq.text.txt"),
            "-o",
            outputFile.getAbsolutePath,
            "--fieldMustContain",
            "chromosome=chr1"))
    val lines = getLinesFromFile(outputFile)
    lines.size shouldBe 2

    Filter.main(
      Array("-i",
            resourcePath("/seattleseq.text.txt"),
            "-o",
            outputFile.getAbsolutePath,
            "--fieldMustContain",
            "chromosome=chr2"))
    val lines2 = getLinesFromFile(outputFile)
    lines2.size shouldBe 1
  }

  @Test
  def testMustBeBelow(): Unit = {
    val outputFile = File.createTempFile("test.", ".txt")
    outputFile.deleteOnExit()
    Filter.main(
      Array("-i",
            resourcePath("/seattleseq.text.txt"),
            "-o",
            outputFile.getAbsolutePath,
            "--fieldMustBeBelow",
            "EU=10.0"))
    val lines = getLinesFromFile(outputFile)
    lines.size shouldBe 2

    Filter.main(
      Array("-i",
            resourcePath("/seattleseq.text.txt"),
            "-o",
            outputFile.getAbsolutePath,
            "--fieldMustBeBelow",
            "EU=1.0"))
    val lines2 = getLinesFromFile(outputFile)
    lines2.size shouldBe 1
  }

  @Test
  def testGenes(): Unit = {
    val outputFile = File.createTempFile("test.", ".txt")
    val outputFileGenes = File.createTempFile("test.", ".txt")
    outputFile.deleteOnExit()
    Filter.main(
      Array("-i",
            resourcePath("/seattleseq.text.txt"),
            "-o",
            outputFile.getAbsolutePath,
            "--geneColapseOutput",
            outputFileGenes.getAbsolutePath))
    val lines = getLinesFromFile(outputFile)
    lines.size shouldBe 2

    val geneLines = getLinesFromFile(outputFileGenes)
    geneLines.size shouldBe 2
  }
}
