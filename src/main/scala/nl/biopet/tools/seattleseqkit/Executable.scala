package nl.biopet.tools.seattleseqkit

import nl.biopet.tools.seattleseqkit.filter.Filter
import nl.biopet.tools.seattleseqkit.mergegenes.MergeGenes
import nl.biopet.utils.tool.ToolCommand
import nl.biopet.utils.tool.multi.MultiToolCommand

object Executable extends MultiToolCommand {

  def subTools: Map[String, List[ToolCommand[_]]] =
    Map("Tool" -> List(Filter, MergeGenes))

  def descriptionText: String = extendedDescriptionText

  def manualText: String = extendedManualText

  def exampleText: String = extendedExampleText

  override def extendedUsage = true

}
