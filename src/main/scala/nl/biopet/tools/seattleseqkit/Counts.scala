package nl.biopet.tools.seattleseqkit

case class Counts(het: Int, hom: Int) {
  def total: Int = het + hom
}
