package diyfea

import scala.collection.breakOut
import scala.collection.immutable.IndexedSeq
import scala.collection.immutable.Map
import scala.collection.immutable.Seq
import scala.collection.immutable.SortedMap
import scala.collection.mutable.MapBuilder
import scala.collection.mutable.{Set => MSet}

/** Case objects (like an enum) specifying whether we want the X or Y
  * coordinate of a node. */
sealed trait NodeCoordinate
case object NodeX extends NodeCoordinate
case object NodeY extends NodeCoordinate

/** Converts a node coordinate to a degree-of-freedom. */
trait NodeToDOF {
  def getDOF(n: Node, nc: NodeCoordinate): Int
}

/** Converts a global node coordinate to a degree-of-freedom using a
  * default mapping.
  * 
  * The default mapping is that, for node number i
  *  - the x DOF corresponds to 2 * i
  *  - the y DOF corresponds to 2 * i + 1
  */
class DefaultNodeToDOF(globalNodes: IndexedSeq[Node]) extends NodeToDOF {
  private val nodeMap: Map[Node, Int] =
    (for ((n,i) <- globalNodes.zipWithIndex) yield n -> i)(breakOut)
  def getDOF(n: Node, nc: NodeCoordinate): Int = {
    val nodeIndex = nodeMap(n)
    nc match {
      case NodeX => 2 * nodeIndex
      case NodeY => 2 * nodeIndex + 1
    }
  }
}
object DefaultNodeToDOF {
  def apply(globalNodes: IndexedSeq[Node]): DefaultNodeToDOF =
    new DefaultNodeToDOF(globalNodes)
}

/** Function which maps a DOF from one ordering to another. */
trait DOFMap extends Function1[Int,Int] {
  def dof: Seq[(Int, Int)]
}

/** Mapping between global and partitioned degrees of freedom.
  * 
  * This mapping should be set up using the PartDOFMapping.getMapping() method.
  * 
  * @param eDOF [global -> partitioned] mapping for essential DOF
  * @param fDOF [global -> partitioned] mapping for free DOF
  */
case class PartDOFMapping
  (val eDOF: SortedMap[Int, Int], val fDOF: SortedMap[Int, Int])
  extends DOFMap
{
  val nEssential: Int = eDOF.size
  val nFree: Int = fDOF.size

  /** Converts a globalDOF to a partitioned DOF.  Partitioned DOF are
    * separated into essential DOF (numbered first) and free DOF (numbered
    * second.
    */
  def apply(globalDOF: Int): Int = {
    if (eDOF.contains(globalDOF)) {
      eDOF(globalDOF)
    } else {
      assert(fDOF.contains(globalDOF),
        "global DOF was not found in either Essential or Free lists")
      fDOF(globalDOF) + nEssential
    }
  }
  
  val dof: Seq[(Int,Int)] = {
    val e: Seq[(Int,Int)] = 
      (for ((g,p) <- eDOF) yield (g,p))(breakOut)
    val f: Seq[(Int,Int)] = 
      (for ((g,p) <- fDOF) yield (g,p+nEssential))(breakOut)
    e ++ f
  }
}

object PartDOFMapping {

  /** Constructs the mappings between global DOF and essential and free DOFs.
    *
    * Each DOF in the FEA system fits into one of two (partitioned) categories:
    *  1. An essential DOF, with a constrained displacement.
    *  2. A free DOF, with an applied force (applied force may be zero).
    * 
    * This method takes the list of global nodes and BCs, and constructs a
    * pair of maps:
    *  1. A map from global DOF to essential DOFs
    *  2. A map from global DOF to free DOFs
    * 
    * The DOF numbers for each node are as follows:
    *  xDOF = 2 * nodes.indexOf(node)
    *  yDOF = 2 * nodes.indexOf(node) + 1
    * 
    * @param nodes IndexedSeq of nodes
    * @param bcs Seq of BCs
    * 
    * @return Partitioned DOF mapping.
    */
  def getMapping(nodes: IndexedSeq[Node], bcs: Seq[BC])
    (nodeToDOF: NodeToDOF = DefaultNodeToDOF(nodes)): PartDOFMapping = {
    
    // check that all nodes that are referred to by the bcs are contained in
    //  the IndexedSeq of nodes that have been passed in
    val bcNodeInNodes = (bc: BC) => nodes.contains(bc.n)
    assert(bcs.forall(bcNodeInNodes(_)))
    
    // check that each node is referenced at most once in the BCs
    {
      val nodeSet = MSet.empty[Node]
      for (bc <- bcs) {
        require(!nodeSet.contains(bc.n),
          "more than one BC refers to the same node")
        nodeSet += bc.n
      }
    }
    
    // construct a map from nodes to BC that refers to them
    val nBCsMap: Map[Node, BC] = (for (bc <- bcs) yield bc.n -> bc)(breakOut)
    
    // builders for essential and free DOF maps
    val eDOFBuilder = new MapBuilder[Int,Int,SortedMap[Int,Int]](
      SortedMap.empty[Int,Int])
    val fDOFBuilder = new MapBuilder[Int,Int,SortedMap[Int,Int]](
      SortedMap.empty[Int,Int])

    // helper stuff - counters and adder methods - to compose essential and 
    //  free DOF maps
    var eDOFIndex = 0
    var fDOFIndex = 0
    def addEssentialDOF(globalDOF: Int) = {
      eDOFBuilder += (globalDOF -> eDOFIndex)
      eDOFIndex = eDOFIndex + 1
    }
    def addFreeDOF(globalDOF: Int) = {
      fDOFBuilder += (globalDOF -> fDOFIndex)
      fDOFIndex = fDOFIndex + 1
    }

    // compose the essential and free DOF maps
    for (nodeIndex <- 0 until nodes.length) {
      val node: Node = nodes(nodeIndex)
      val xDOF = nodeToDOF.getDOF(node, NodeX)
      val yDOF = nodeToDOF.getDOF(node, NodeY)
      val optBC: Option[BC] = nBCsMap.get(node)
      val xConstrained = optBC.map(_.isXConstrained).getOrElse(false)
      val yConstrained = optBC.map(_.isYConstrained).getOrElse(false)
      
      if (xConstrained) addEssentialDOF(xDOF)
      else addFreeDOF(xDOF)
      
      if (yConstrained) addEssentialDOF(yDOF)
      else addFreeDOF(yDOF)
    }
    
    // return the mapping result
    PartDOFMapping(eDOFBuilder.result, fDOFBuilder.result)
    
  }
   
}

case class LocalToGlobalDOFMapping(
  localNodes: IndexedSeq[Node],
  globalNodes: IndexedSeq[Node]
)(
  nodeToDOF: NodeToDOF = DefaultNodeToDOF(globalNodes)
) extends DOFMap {
  def apply(localDOF: Int): Int = {
    val nodeIndex = localDOF / 2
    val nodeCoordinate: NodeCoordinate = (localDOF % 2) match {
      case 0 => NodeX
      case 1 => NodeY
    }
    val node: Node = localNodes(nodeIndex)
    nodeToDOF.getDOF(node, nodeCoordinate)
  }
  
  val dof: Seq[(Int,Int)] =
    for (i <- 0 until (localNodes.length * 2)) yield (i, apply(i))
}

case class LocalToPartDOFMapping(
  localToGlobal: LocalToGlobalDOFMapping,
  globalToPart: PartDOFMapping
) extends DOFMap {
  def apply(localDOF: Int): Int = {
    val globalDOF = localToGlobal(localDOF)
    val partitionedDOF = globalToPart(globalDOF)
    partitionedDOF
  }
  
  val dof: Seq[(Int,Int)] =
    for (i <- 0 until (localToGlobal.localNodes.length * 2)) 
      yield (i, apply(i))
}

object DOFMapping {
  
  def localToPart(localNodes: IndexedSeq[Node], globalNodes: IndexedSeq[Node], 
    globalToPart: PartDOFMapping): LocalToPartDOFMapping = 
  {
    val localToGlobal = LocalToGlobalDOFMapping(localNodes, globalNodes)()
    LocalToPartDOFMapping(localToGlobal, globalToPart)
  }
  
}
