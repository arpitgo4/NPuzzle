import scala.annotation.tailrec

/**
 * Created by Arpit Goyal on 7/18/2015.
 */
case class StateTree(root : Node) {

  def expandState(node : Node) : Unit = {
    val matrix = node.state
    var (zeroX,zeroY) = (0,0)

    for(i <- 0 until matrix.length; j <- 0 until matrix(i).length)
      if(matrix(i)(j) == 0) {
        zeroX = i
        zeroY = j
      }

    def copyArray(array : Array[Array[Int]]) : Array[Array[Int]] = {
      val result = Array.ofDim[Int](array.length, array.length)
      for(i <- 0 until array.length; j <- 0 until array(i).length)
        result(i)(j) = array(i)(j)
      result
    }

    var stateUP, stateLEFT, stateDOWN, stateRIGHT : Node = null

    // upward slided state
    try{
      val state = copyArray(matrix)
      state(zeroX)(zeroY) = state(zeroX-1)(zeroY)
      state(zeroX-1)(zeroY) = 0
      stateUP = Node(state)
    }catch{case e : Exception =>}

    // left slided state
    try{
      val state = copyArray(matrix)
      state(zeroX)(zeroY) = state(zeroX)(zeroY-1)
      state(zeroX)(zeroY-1) = 0
      stateLEFT = Node(state)
    }catch{case e : Exception =>}

    // down slided state
    try{
      val state = copyArray(matrix)
      state(zeroX)(zeroY) = state(zeroX+1)(zeroY)
      state(zeroX+1)(zeroY) = 0
      stateDOWN = Node(state)
    }catch{case e : Exception =>}

    // right slided state
    try{
      val state = copyArray(matrix)
      state(zeroX)(zeroY) = state(zeroX)(zeroY+1)
      state(zeroX)(zeroY+1) = 0
      stateRIGHT = Node(state)
    }catch{case e : Exception =>}

    if(stateUP != null) node.children = node.children :+ stateUP
    if(stateLEFT != null) node.children = node.children :+ stateLEFT
    if(stateDOWN != null) node.children = node.children :+ stateDOWN
    if(stateRIGHT != null) node.children = node.children :+ stateRIGHT

  }

  def insertNode(node : Node, parent : Node) : Unit = {
    val n = DFS(parent).last
    if(n == parent) n.children = n.children :+ node
    else s"$parent not found!"
  }

  def BFS(node : Node) : List[Node] = {
    @tailrec
    def search(stack : List[Node], route : List[Node], goalNode : Node) : List[Node] = stack match {
      case Nil => route
      case head :: tail => if(equal(head.state,goalNode.state)) (route :+ head)
      else search(tail ::: head.children, route :+ head, goalNode)
    }
    search(List[Node](root), List[Node](), node)
  }

  def DFS(node : Node) : List[Node] = {
    @tailrec
    def search(stack : List[Node], route : List[Node], goalNode : Node) : List[Node] = stack match {
      case Nil => route
      case head :: tail => if(equal(head.state,goalNode.state)) (route :+ head)
                        else search(head.children ::: tail, route :+ head, goalNode)
    }
    search(List[Node](root), List[Node](), node)
  }

  def displayNode(node : Node) : Unit = {
    node.state.foreach(arg => println(arg.mkString))
    println
  }

  def equal(arr1 : Array[Array[Int]], arr2 : Array[Array[Int]]) : Boolean = {
    if(arr1.length == 0 || arr2.length == 0) return false
    for(row <- 0 until arr1.length; col <- 0 until arr1(row).length)
      if(arr1(row)(col) != arr2(row)(col)) return false
    true
  }
}

case class Node(state : Array[Array[Int]], var children : List[Node] = List[Node](), var heuristicVal : Double = 0,var closed : Boolean = false)

object StateTree extends App {

  def displayNode(node : Node) : Unit = {
    node.state.foreach(arg => println(arg.mkString))
    println
  }

  val child1 = Node(Array(
      Array(1,3,2),
      Array(4,5,6),
      Array(7,8,0)
    ))

  val child2 = Node(Array(
    Array(1,8,2),
    Array(4,3,6),
    Array(7,0,5)
  ))

  val root = Node(Array
    (Array(1,2,3), Array(4,5,6), Array(7,8,0)))
  val tree = StateTree(root)
  tree.expandState(root)
  tree.DFS(Node(Array(
    Array(1,2,3),
    Array(4,5,0),
    Array(7,8,6)
  ))).foreach(displayNode)

}
