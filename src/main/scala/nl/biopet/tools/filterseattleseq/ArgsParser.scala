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

package nl.biopet.tools.filterseattleseq

import java.io.File

import nl.biopet.utils.tool.{AbstractOptParser, ToolCommand}

class ArgsParser(toolCommand: ToolCommand[Args])
    extends AbstractOptParser[Args](toolCommand) {
  opt[File]('i', "inputFile")
    .required()
    .action((x, c) => c.copy(inputFile = x))
    .text("Seattle seq input file")
  opt[File]('o', "outputFile")
    .required()
    .action((x, c) => c.copy(outputFile = x))
    .text("Seattle seq output file")
  opt[File]("geneColapseOutput")
    .action((x, c) => c.copy(geneColapseOutput = Some(x)))
    .text("Output file to count per gene hits")
  opt[File]("intervals")
    .action((x, c) => c.copy(intervals = Some(x)))
    .text("Intervals bed file")
  opt[(String, String)]("fieldMustContain")
    .action((x, c) => c.copy(fieldMustContain = x :: c.fieldMustContain))
    .text("Field must contain given text")
    .valueName("<key>=<text>")
  opt[(String, Double)]("fieldMustbeBelow")
    .action((x, c) => c.copy(fieldMustbeBelow = x :: c.fieldMustbeBelow))
    .text("Field must be below given numeric value")
    .valueName("<key>=<double>")
}
