package diyfea

import math.sqrt
import scala.collection.immutable.IndexedSeq
import scala.collection.immutable.Vector
import no.uib.cipr.matrix.DenseMatrix
import no.uib.cipr.matrix.DenseVector
import no.uib.cipr.matrix.UpperSymmPackMatrix
import no.uib.cipr.matrix.Matrices

case class LinPlaneStressElement(
  n1: Node, n2: Node, n3: Node, n4: Node, 
  mat: ElasticMaterial
) extends Element
{

  def nodes: IndexedSeq[Node] = Vector(n1, n2, n3, n4)

  def K: UpperSymmPackMatrix = {
    val k = new UpperSymmPackMatrix(8)
    val ri = 0.577350269189626
    case class GaussPt(weight: Double, xi: Double, eta: Double)
    val gaussPts = Vector(
      GaussPt(1.0, +ri, +ri),
      GaussPt(1.0, +ri, -ri),
      GaussPt(1.0, -ri, +ri),
      GaussPt(1.0, -ri, -ri)
    )
    for (g <- gaussPts) {
      k.add(g.weight, evalF(g.xi, g.eta))
    }
    k
  }

  def edgeTraction(t: EdgeTraction): DenseVector = {
    require(t.edge >= 1 && t.edge <= 4, "edge must be 1 to 4")
    val (nEnd, nStart, niEnd, niStart) = t.edge match {
      case 1 => (n2, n1, 1, 0)
      case 2 => (n3, n2, 2, 1)
      case 3 => (n4, n3, 3, 2)
      case 4 => (n1, n4, 0, 3)
    }
    val dx = nEnd.x - nStart.x
    val dy = nEnd.y - nStart.y
    val l = sqrt(dx*dx + dy*dy)
    val dof_x1 = 2 * niStart
    val dof_y1 = 2 * niStart + 1
    val dof_x2 = 2 * niEnd
    val dof_y2 = 2 * niEnd + 1
    val fxMag = t.fx * l / 2.0
    val fyMag = t.fy * l / 2.0
    val f = new DenseVector(8)
    f.set(dof_x1, fxMag)
    f.set(dof_x2, fxMag)
    f.set(dof_y1, fyMag)
    f.set(dof_y2, fyMag)
    f
  }
  
  private def evalF(xi: Double, eta: Double): UpperSymmPackMatrix = {
    // find factors for shape function derivatives
    // derivatives of Ni wrt xi
    val dN1dxi = +0.25 * (1.0 + eta)
    val dN2dxi = -0.25 * (1.0 + eta)
    val dN3dxi = -0.25 * (1.0 - eta)
    val dN4dxi = +0.25 * (1.0 - eta)
    // derivatives of Ni wrt eta
    val dN1deta = +0.25 * (1.0 + xi)
    val dN2deta = +0.25 * (1.0 - xi)
    val dN3deta = -0.25 * (1.0 - xi)
    val dN4deta = -0.25 * (1.0 + xi)

    // components of the Jacobian matrix
    val dxdxi = dN1dxi * n1.x + dN2dxi * n2.x + dN3dxi * n3.x + dN4dxi * n4.x
    val dydxi = dN1dxi * n1.y + dN2dxi * n2.y + dN3dxi * n3.y + dN4dxi * n4.y
    val dxdeta = dN1deta * n1.x + dN2deta * n2.x + dN3deta * n3.x + 
      dN4deta * n4.x
    val dydeta = dN1deta * n1.y + dN2deta * n2.y + dN3deta * n3.y + 
      dN4deta * n4.y
      
    // populate Jacobian matrix and the inverse of the Jacobian
    val J = new DenseMatrix(2, 2)
    J.set(0,0, dxdxi);  J.set(0,1, dydxi)
    J.set(1,0, dxdeta); J.set(1,1, dydeta)
    val I = Matrices.identity(2)
    val Jinv = new DenseMatrix(2, 2)
    J.solve(I, Jinv)
    val detJ = J.get(0,0) * J.get(1,1) - J.get(0,1) * J.get(1,0)
    
    // derivatives of shape function wrt x and y
    val dN1dx = Jinv.get(0,0) * dN1dxi + Jinv.get(0,1) * dN1deta
    val dN1dy = Jinv.get(1,0) * dN1dxi + Jinv.get(1,1) * dN1deta
    val dN2dx = Jinv.get(0,0) * dN2dxi + Jinv.get(0,1) * dN2deta
    val dN2dy = Jinv.get(1,0) * dN2dxi + Jinv.get(1,1) * dN2deta
    val dN3dx = Jinv.get(0,0) * dN3dxi + Jinv.get(0,1) * dN3deta
    val dN3dy = Jinv.get(1,0) * dN3dxi + Jinv.get(1,1) * dN3deta
    val dN4dx = Jinv.get(0,0) * dN4dxi + Jinv.get(0,1) * dN4deta
    val dN4dy = Jinv.get(1,0) * dN4dxi + Jinv.get(1,1) * dN4deta
    
    // compose B matrix (shape function derivatives)
    val B = new DenseMatrix(3, 8)
    B.set(0,0,dN1dx); B.set(2,0,dN1dy)
    B.set(1,1,dN1dy); B.set(2,1,dN1dx)
    B.set(0,2,dN2dx); B.set(2,2,dN2dy)
    B.set(1,3,dN2dy); B.set(2,3,dN2dx)
    B.set(0,4,dN3dx); B.set(2,4,dN3dy)
    B.set(1,5,dN3dy); B.set(2,5,dN3dx)
    B.set(0,6,dN4dx); B.set(2,6,dN4dy)
    B.set(1,7,dN4dy); B.set(2,7,dN4dx)

    // compute Fdense = detJ * B^T * D * B
    val DB = new DenseMatrix(3, 8)
    D.mult(B, DB)                   // DB = D * B
    val Fdense = new DenseMatrix(8, 8)
    B.transAmult(detJ, DB, Fdense)  // Fdense = detJ * B^T * D * B
    
    // return the result as a packed upper symmetric matrix
    val f = new UpperSymmPackMatrix(Fdense)
    f
  }
    
  /** Constitutive matrix for plane stress. */
  def D: UpperSymmPackMatrix = {
    val d = new UpperSymmPackMatrix(3)
    val c = mat.e / (1.0 - (mat.nu * mat.nu))
    d.set(0,0,c); d.set(0,1,c*mat.nu)
    d.set(1,1,c)
    d.set(2,2,c*(1.0-mat.nu)/2.0)
    d
  }
}