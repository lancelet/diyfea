package diyfea

import scala.collection.breakOut
import scala.collection.immutable.IndexedSeq
import scala.collection.immutable.SortedMap

object TrussExample extends App {

  // nodes of the truss
  val A = Node(0, 0)
  val B = Node(3, 0)
  val C = Node(6, 0)
  val D = Node(9, 0)
  val E = Node(6,-2)
  val F = Node(3,-3)
  val G = Node(0,-3)
  val nodes = IndexedSeq(A, B, C, D, E, F, G)
  
  // elastic material
  val mat = ElasticMaterial(1.0, 0.33)
  
  // elements of the truss
  val AB = "AB" -> TrussElement(A, B, mat, 1)
  val BC = "BC" -> TrussElement(B, C, mat, 1)
  val CD = "CD" -> TrussElement(C, D, mat, 1)
  val GF = "GF" -> TrussElement(G, F, mat, 1)
  val FE = "FE" -> TrussElement(F, E, mat, 1)
  val ED = "ED" -> TrussElement(E, D, mat, 1)
  val AG = "AG" -> TrussElement(A, G, mat, 1)
  val BF = "BF" -> TrussElement(B, F, mat, 1)
  val CE = "CE" -> TrussElement(C, E, mat, 1)
  val AF = "AF" -> TrussElement(A, F, mat, 1)
  val BE = "BE" -> TrussElement(B, E, mat, 1)
  val ordering: Ordering[String] = Ordering.String
  val elMap: SortedMap[String,TrussElement] = SortedMap.empty(ordering) ++ 
  	IndexedSeq(AB, BC, CD, GF, FE, ED, AG, BF, CE, AF, BE) 
  val elements: IndexedSeq[TrussElement] = elMap.values.toIndexedSeq

  // BCs
  val BC_A = diyfea.BC(A, Some(0), Some(0))
  val BC_G = diyfea.BC(G, Some(0), None)
  val bcs = IndexedSeq(BC_A, BC_G)
  
  // Forces
  val F_D = ConcentratedForce(D, 0.0, -14000)
  val forces = IndexedSeq(F_D)
  
  // Compute the FEA solution
  val fea = FEAProblem(nodes, elements, bcs, forces)
  
  // Inspect each element for the stress / force
  for ((name, e) <- elMap) {
    val insp = TrussElementInspector(e, fea)
    println("%s = %f N" format(name, insp.stress))
  }

}