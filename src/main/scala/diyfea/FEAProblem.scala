package diyfea

import scala.collection.immutable.IndexedSeq
import scala.collection.immutable.Seq
import no.uib.cipr.matrix.DenseMatrix
import no.uib.cipr.matrix.DenseVector
import no.uib.cipr.matrix.UpperSymmPackMatrix

case class FEAProblem(
  nodes: IndexedSeq[Node],
  elements: IndexedSeq[Element],
  bcs: Seq[BC],
  forces: Seq[Force]
) {

  // establish partitioned DOF mapping
  private val nodeToDOF = DefaultNodeToDOF(nodes)
  private val globalToPartDOFMapping = 
    PartDOFMapping.getMapping(nodes, bcs)(nodeToDOF)
  
  // find number of DOF in the system
  val nDOF: Int = nodes.length * 2
  val nEssentialDOF: Int = globalToPartDOFMapping.nEssential
  val nFreeDOF: Int = globalToPartDOFMapping.nFree
  assert(nDOF == (nEssentialDOF + nFreeDOF),
    "total DOF = %d, was not equal to E(%d) + F(%d)".
      format(nDOF, nEssentialDOF, nFreeDOF))
  
  // dump out DOF (debugging)
  if (false) {
    println("------------------")
    println("nDOF          = %d" format nDOF)
    println("nEssentialDOF = %d" format nEssentialDOF)
    println("nFreeDOF      = %d" format nFreeDOF)
    println("------------------")
    for (nodeIndex <- 0 until nodes.length) {
      val node = nodes(nodeIndex)
      println("Node: %d" format nodeIndex)
      println("  xDOF(global) = %4d : xDOF(part) = %4d".format(
          nodeToDOF.getDOF(node, NodeX),
          globalToPartDOFMapping(nodeToDOF.getDOF(node, NodeX))))
      println("  yDOF(global) = %4d : yDOF(part) = %4d".format(
          nodeToDOF.getDOF(node, NodeY),
          globalToPartDOFMapping(nodeToDOF.getDOF(node, NodeY))))
    }
  }
  
  /** Gets the displacements of a node.
    *
    * @param n node
    * @return (x,y) displacements
    */
  def getU(n: Node): (Double, Double) = {
    // fetch the partitioned DOF of the node
    val dofx = globalToPartDOFMapping(nodeToDOF.getDOF(n, NodeX))
    val dofy = globalToPartDOFMapping(nodeToDOF.getDOF(n, NodeY))
    
    // utility function to get displacements
    def getUDOF(partitionedDOF: Int): Double = {
      if (partitionedDOF < nEssentialDOF) dE.get(partitionedDOF)
      else dF.get(partitionedDOF - nEssentialDOF)
    }
    
    // return displacements of x and y DOF
    (getUDOF(dofx), getUDOF(dofy))
  }
  
  /** Gets the forces on a node.
    * 
    * @param n node
    * @return (fx,fy) forces
    */
  def getF(n: Node): (Double, Double) = {
    // fetch the partitioned DOF of the node
    val dofx = globalToPartDOFMapping(nodeToDOF.getDOF(n, NodeX))
    val dofy = globalToPartDOFMapping(nodeToDOF.getDOF(n, NodeY))

    // utility function to get forces
    def getFDOF(partitionedDOF: Int): Double = {
      if (partitionedDOF < nEssentialDOF) fE.get(partitionedDOF)
      else fF.get(partitionedDOF - nEssentialDOF)
    }
    
    // return forces of x and y DOF
    (getFDOF(dofx), getFDOF(dofy))
  }
  
  //---------------------------------------------------------- FEA Calculations
  
  // system matrices
  private val KE: UpperSymmPackMatrix = new UpperSymmPackMatrix(nEssentialDOF)
  private val KF: UpperSymmPackMatrix = new UpperSymmPackMatrix(nFreeDOF)
  private val KEF: DenseMatrix = new DenseMatrix(nEssentialDOF, nFreeDOF)
  private val dE: DenseVector = new DenseVector(nEssentialDOF)
  private val dF: DenseVector = new DenseVector(nFreeDOF)
  private val fE: DenseVector = new DenseVector(nEssentialDOF)
  private val fF: DenseVector = new DenseVector(nFreeDOF)
  
  // form the system stiffness matrices, displacement BCs and applied forces
  formStiffnessMatrices()
  formDE()
  formFF()
  
  // solve the system
  solveDF() // displacements of free nodes
  solveFE() // reaction forces of essential (displacement BC) nodes
  
  /** Sets the free nodal forces. */
  private def formFF() {
    fF.zero()
    for (force <- forces) {
      force match {
        case ConcentratedForce(n, fx, fy) => {
          val dofx = globalToPartDOFMapping(nodeToDOF.getDOF(n, NodeX))
          val dofy = globalToPartDOFMapping(nodeToDOF.getDOF(n, NodeY))
          if (fx != 0.0) {
            fF.set(dofx - nEssentialDOF, fx)
          }
          if (fy != 0.0) {
            fF.set(dofy - nEssentialDOF, fy)
          }
        }
        case t: EdgeTraction => {
          val f: DenseVector = t.e.edgeTraction(t)
          // dof maps local node numbers to partitioned node numbers
	      val dof: Seq[(Int,Int)] = 
	        DOFMapping.localToPart(t.e.nodes, nodes, 
	          globalToPartDOFMapping).dof
          for ((l,g) <- dof; if (g >= nEssentialDOF)) {
            fF.add(g - nEssentialDOF, f.get(l))
          }
        }
      }
    }
  }
  
  /** Sets the essential (displacement) boundary conditions. */
  private def formDE() {
    dE.zero()
	for (bc <- bcs) {
	  if (bc.isXConstrained) {
	    val dof = globalToPartDOFMapping(nodeToDOF.getDOF(bc.n, NodeX))
	    assert(dof < nEssentialDOF)
	    dE.set(dof, bc.x.get)
	  }
	  if (bc.isYConstrained) {
	    val dof = globalToPartDOFMapping(nodeToDOF.getDOF(bc.n, NodeY))
	    assert(dof < nEssentialDOF)
	    dE.set(dof, bc.y.get)
	  }
	}    
  }
    
  /** Sets the stiffness matrics (KE, KF and KEF) for the system. */
  private def formStiffnessMatrices() {
    KE.zero()
    KF.zero()
    KEF.zero()
    
	/** Utility function to add an element into the appropriate system 
	  * stiffness matrix.  The function checks which matrix a component belongs
	  * in by looking at its row and column.  The element is then inserted
	  * into the appropriate matrix.  The partitioned system matrix layout
	  * looks like this:
	  *  [   KE, KEF ]
	  *  [ symm,  KF ]
	  * Elements which lie below the diagonal of the matrix (col < row) are
	  * just ignored.
	  */
	def addStiffnessElement(row: Int, col: Int, e: Double) {
	  // only consider elements in the upper-triangular half of the matrix
	  if (col >= row) {
  	    if (row < nEssentialDOF && col < nEssentialDOF) { // KE
  	      KE.add(row, col, e)
	    } else if (row < nEssentialDOF) { // KEF
	      KEF.add(row, col - nEssentialDOF, e)
	    } else { // KF
	      KF.add(row - nEssentialDOF, col - nEssentialDOF, e)
	    }
	  }
	}
	
	// Iterate over the elements to form the system stiffness matrices,
	//  KE, KF and KEF
	for (e <- elements) {
	  // dof maps local node numbers to partitioned node numbers
	  val dof: Seq[(Int,Int)] = 
	    DOFMapping.localToPart(e.nodes, nodes, globalToPartDOFMapping).dof
	  val Ke = e.K
	  for ((li,gi) <- dof; (lj,gj) <- dof) {
	    addStiffnessElement(gi, gj, Ke.get(li,lj))
	  }
	}
	
  }
  
  /** Solves for dF, the displacement of the free DOF. */
  private def solveDF() {
    dF.zero()
    val y = fF.copy()
    KEF.transMultAdd(-1, dE, y) // y = -1 * KEF^T*dE + fF
    KF.solve(y, dF) // dF = inv(KF) * y // inv(KF) * (fF - KEF^T*dE)
  }
  
  /** Solves for fE, the reaction forces of the essential DOF. */ 
  private def solveFE() {
    fE.zero()
    val a = new DenseVector(nEssentialDOF)
    val b = new DenseVector(nEssentialDOF)
    KE.mult(dE, a)
    KEF.mult(dF, b)
    fE.add(a)
    fE.add(b)
  }
    
}