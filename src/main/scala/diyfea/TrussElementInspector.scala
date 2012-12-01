package diyfea

import no.uib.cipr.matrix.DenseVector

/** Inspection class to obtain results from a truss element.
  */
case class TrussElementInspector(e: TrussElement, fea: FEAProblem) {
  
  // Displacement of node 1
  val u1: (Double, Double) = fea.getU(e.n1)
  // Displacement of node 2
  val u2: (Double, Double) = fea.getU(e.n2)

  // Vector of local displacements for the element
  private def d: DenseVector = {
    val v = new DenseVector(4)
    v.set(0, u1._1)
    v.set(1, u1._2)
    v.set(2, u2._1)
    v.set(3, u2._2)
    v
  }

  // Stress in the truss element
  val stress: Double = {
    // find the nodal forces from the displacements and the stiffness matrix
    //  F = Kd
    val f = new DenseVector(4)
    e.K.mult(d, f) // f = Ke * uvec
    
    // dot product of the nodal force at one end with a normal vector along 
    //  the truss element
    val nx = e.dx / e.l
    val ny = e.dy / e.l
    val f_truss = -nx * f.get(0) - ny * f.get(1)

    // convert the force to a stress by dividing by the element area
    f_truss / e.a
  }

}
