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

import java.io.File

import nl.biopet.utils.io.getLinesFromFile
import nl.biopet.utils.test.tools.ToolTest
import org.testng.annotations.Test

class MergeGenesTest extends ToolTest[Args] {
  def toolCommand: MergeGenes.type = MergeGenes
  @Test
  def testNoArgs(): Unit = {
    intercept[IllegalArgumentException] {
      MergeGenes.main(Array())
    }
  }

  @Test
  def testDefault(): Unit = {
    val outputFile = File.createTempFile("test.", ".txt")
    outputFile.deleteOnExit()
    MergeGenes.main(
      Array("-i",
            "genes1=" + resourcePath("/genes1.txt"),
            "-i",
            "genes2=" + resourcePath("/genes2.txt"),
            "-o",
            outputFile.getAbsolutePath))
    val lines = getLinesFromFile(outputFile)
    lines.size shouldBe 4

    lines shouldBe List(
      "Gene\tgenes1\tgenes2",
      "g1\t1\t1",
      "g2\t2\t0",
      "g3\t0\t3"
    )
  }
}
