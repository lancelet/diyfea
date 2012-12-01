package diyfea

import no.uib.cipr.matrix.UpperSymmPackMatrix
import no.uib.cipr.matrix.DenseVector
import scala.collection.immutable.IndexedSeq

trait Element {
  def nodes: IndexedSeq[Node]
  def K: UpperSymmPackMatrix
  def edgeTraction(t: EdgeTraction): DenseVector
}