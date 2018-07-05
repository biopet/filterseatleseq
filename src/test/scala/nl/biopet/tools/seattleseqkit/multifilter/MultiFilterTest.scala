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

import java.io.File

import nl.biopet.utils.io.getLinesFromFile
import nl.biopet.utils.test.tools.ToolTest
import org.testng.annotations.Test

class MultiFilterTest extends ToolTest[Args] {
  def toolCommand: MultiFilter.type = MultiFilter
  @Test
  def testNoArgs(): Unit = {
    intercept[IllegalArgumentException] {
      MultiFilter.main(Array())
    }
  }

  @Test
  def testDefault(): Unit = {
    val outputDir = File.createTempFile("test.", ".txt")
    outputDir.delete()
    outputDir.mkdir()
    outputDir.deleteOnExit()
    MultiFilter.main(
      Array("-i",
            "S1=" + resourcePath("/seattleseq.text.txt"),
            "-o",
            outputDir.getAbsolutePath))
    val lines = getLinesFromFile(new File(outputDir, "S1.tsv"))
    lines.size shouldBe 2
  }

  @Test
  def testCompressed(): Unit = {
    val outputDir = File.createTempFile("test.", ".txt")
    outputDir.delete()
    outputDir.mkdir()
    outputDir.deleteOnExit()
    MultiFilter.main(
      Array("-i",
            "S1=" + resourcePath("/seattleseq.text.txt.gz"),
            "-o",
            outputDir.getAbsolutePath))
    val lines = getLinesFromFile(new File(outputDir, "S1.tsv"))
    lines.size shouldBe 2
  }

  @Test
  def testRegion(): Unit = {
    val outputDir = File.createTempFile("test.", ".txt")
    outputDir.delete()
    outputDir.mkdir()
    outputDir.deleteOnExit()
    MultiFilter.main(
      Array("-i",
            "S1=" + resourcePath("/seattleseq.text.txt"),
            "-o",
            outputDir.getAbsolutePath,
            "--intervals",
            "S1=" + resourcePath("/region.bed")))
    val lines = getLinesFromFile(new File(outputDir, "S1.tsv"))
    lines.size shouldBe 2

    MultiFilter.main(
      Array("-i",
            "S1=" + resourcePath("/seattleseq.text.txt"),
            "-o",
            outputDir.getAbsolutePath,
            "--intervals",
            "S1=" + resourcePath("/false_region.bed")))
    val lines2 = getLinesFromFile(new File(outputDir, "S1.tsv"))
    lines2.size shouldBe 1
  }

  @Test
  def testMustContain(): Unit = {
    val outputDir = File.createTempFile("test.", ".txt")
    outputDir.delete()
    outputDir.mkdir()
    outputDir.deleteOnExit()
    MultiFilter.main(
      Array("-i",
            "S1=" + resourcePath("/seattleseq.text.txt"),
            "-o",
            outputDir.getAbsolutePath,
            "--fieldMustContain",
            "chromosome=chr1"))
    val lines = getLinesFromFile(new File(outputDir, "S1.tsv"))
    lines.size shouldBe 2

    MultiFilter.main(
      Array("-i",
            "S1=" + resourcePath("/seattleseq.text.txt"),
            "-o",
            outputDir.getAbsolutePath,
            "--fieldMustContain",
            "chromosome=chr2"))
    val lines2 = getLinesFromFile(new File(outputDir, "S1.tsv"))
    lines2.size shouldBe 1
  }

  @Test
  def testMustBeBelow(): Unit = {
    val outputDir = File.createTempFile("test.", ".txt")
    outputDir.delete()
    outputDir.mkdir()
    outputDir.deleteOnExit()
    MultiFilter.main(
      Array("-i",
            "S1=" + resourcePath("/seattleseq.text.txt"),
            "-o",
            outputDir.getAbsolutePath,
            "--fieldMustBeBelow",
            "EU=10.0"))
    val lines = getLinesFromFile(new File(outputDir, "S1.tsv"))
    lines.size shouldBe 2

    MultiFilter.main(
      Array("-i",
            "S1=" + resourcePath("/seattleseq.text.txt"),
            "-o",
            outputDir.getAbsolutePath,
            "--fieldMustBeBelow",
            "EU=1.0"))
    val lines2 = getLinesFromFile(new File(outputDir, "S1.tsv"))
    lines2.size shouldBe 1
  }

  @Test
  def testMustBeAbove(): Unit = {
    val outputDir = File.createTempFile("test.", ".txt")
    outputDir.delete()
    outputDir.mkdir()
    outputDir.deleteOnExit()
    MultiFilter.main(
      Array("-i",
            "S1=" + resourcePath("/seattleseq.text.txt"),
            "-o",
            outputDir.getAbsolutePath,
            "--fieldMustBeAbove",
            "EU=10.0"))
    val lines = getLinesFromFile(new File(outputDir, "S1.tsv"))
    lines.size shouldBe 1

    MultiFilter.main(
      Array("-i",
            "S1=" + resourcePath("/seattleseq.text.txt"),
            "-o",
            outputDir.getAbsolutePath,
            "--fieldMustBeAbove",
            "EU=1.0"))
    val lines2 = getLinesFromFile(new File(outputDir, "S1.tsv"))
    lines2.size shouldBe 2
  }

  @Test
  def testGenes(): Unit = {
    val outputDir = File.createTempFile("test.", ".txt")
    outputDir.delete()
    outputDir.mkdir()
    outputDir.deleteOnExit()
    MultiFilter.main(
      Array("-i",
            "S1=" + resourcePath("/seattleseq.text.txt"),
            "-o",
            outputDir.getAbsolutePath))
    val lines = getLinesFromFile(new File(outputDir, "S1.tsv"))
    lines.size shouldBe 2

    val geneLines = getLinesFromFile(new File(outputDir, "S1.genes.tsv"))
    geneLines.size shouldBe 2
  }
}
