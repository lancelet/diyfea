package diyfea

import math.{pow, sqrt}
import no.uib.cipr.matrix.UpperSymmPackMatrix
import no.uib.cipr.matrix.DenseVector
import no.uib.cipr.matrix.DenseMatrix
import no.uib.cipr.matrix.Matrices

case class LinPlaneStressElementInspector(
  e: LinPlaneStressElement,
  fea: FEAProblem
) {
  val u1: (Double, Double) = fea.getU(e.n1)
  val u2: (Double, Double) = fea.getU(e.n2)
  val u3: (Double, Double) = fea.getU(e.n3)
  val u4: (Double, Double) = fea.getU(e.n4)
  
  // Returns x,y coordinates using shape function interpolation
  def xy(xi: Double, eta: Double): (Double, Double) = {
    val n1 = 0.25 * (1 + xi) * (1 + eta)
    val n2 = 0.25 * (1 - xi) * (1 + eta)
    val n3 = 0.25 * (1 - xi) * (1 - eta)
    val n4 = 0.25 * (1 + xi) * (1 - eta)
    val x = (n1 * e.n1.x) + (n2 * e.n2.x) + (n3 * e.n3.x) + (n4 * e.n4.x)
    val y = (n1 * e.n1.y) + (n2 * e.n2.y) + (n3 * e.n3.y) + (n4 * e.n4.y)
    (x,y)
  }
  
  // constitutive matrix
  private val D: UpperSymmPackMatrix = e.D
  
  // Returns von-Mises stress at a coordinate
  def vonMises(xi: Double, eta: Double): Double = {
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
    val dxdxi = dN1dxi * e.n1.x + dN2dxi * e.n2.x + dN3dxi * e.n3.x + 
      dN4dxi * e.n4.x
    val dydxi = dN1dxi * e.n1.y + dN2dxi * e.n2.y + dN3dxi * e.n3.y + 
      dN4dxi * e.n4.y
    val dxdeta = dN1deta * e.n1.x + dN2deta * e.n2.x + dN3deta * e.n3.x + 
      dN4deta * e.n4.x
    val dydeta = dN1deta * e.n1.y + dN2deta * e.n2.y + dN3deta * e.n3.y + 
      dN4deta * e.n4.y
      
    // populate Jacobian matrix and the Cinverse of the Jacobian
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
    
    // set up the compacted displacement matrix
    val d: DenseVector = new DenseVector(8)
    d.set(0,u1._1); d.set(1, u1._2)
    d.set(2,u2._1); d.set(3, u2._2)
    d.set(4,u3._1); d.set(5, u3._2)
    d.set(6,u4._1); d.set(7, u4._2)
    
    // compute D * B * d
    val DB: DenseMatrix = new DenseMatrix(3, 8)
    D.mult(B, DB)
    val sigma: DenseVector = new DenseVector(3)
    DB.mult(d, sigma)
    val sx = sigma.get(0)
    val sy = sigma.get(1)
    val txy = sigma.get(2)
    
    // find principle stresses
    val c1 = 0.5 * (sx + sy)
    val c2 = sqrt(pow(sx - sy, 2) / 4.0 + pow(txy, 2))
    val s1 = c1 + c2
    val s2 = c1 - c2
    
    // find von-mises stress
    //sqrt(0.5 * pow(s1 - s2, 2))
    sqrt(s1 * s1 - s1 * s2 + s2 * s2)
  }
  
}
