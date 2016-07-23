import scala.annotation.tailrec
import scala.swing.Panel
import scala.util.control.Breaks._

/**
 * Created by Arpit Goyal on 7/19/2015.
 */
case class NPuzzle(tree : StateTree) {

  def this(tree : StateTree, puzzle : Panel){
    this(tree)
    this.guiPanel = puzzle
  }

  var guiPanel : Panel = null


  def hillClimbSearch(parent : Node, goal : Node) : List[Node] = {
    @tailrec
    def search(stack : List[Node] ,route : List[Node], goal : Node) : List[Node] = {
      if(stack.isEmpty) return route
      if(tree.equal(stack.head.state, goal.state)){
        NPuzzleGUI.puzzlegui = stack.head
        guiPanel.repaint
        try{Thread.sleep(NPuzzleGUI.pauseTime)}catch{case e : Exception=>}
        route :+ stack.head
      }
      else {
       // tree.displayNode(stack.head)
        NPuzzleGUI.puzzlegui = stack.head
        guiPanel.repaint
        try{Thread.sleep(NPuzzleGUI.pauseTime)}catch{case e : Exception=>}
        search(heuristic(stack.head, goal, route:+stack.head) ::: stack.tail, route:+stack.head, goal)
      }
    }
    search(List[Node](parent), List[Node](), goal)
  }

  def heuristic(parent : Node, goal : Node, route : List[Node]) : List[Node] = {
    tree.expandState(parent)
    val children = parent.children
    for(child <- children)
      route.foreach(r => if(tree.equal(r.state, child.state)) child.closed = true)

    for(child <- children.filter(_.closed == false))
      child.heuristicVal = hval(child.state, goal.state)

    children.filter(_.closed == false).sortBy(_.heuristicVal)
  }

  def hval(child : Array[Array[Int]], goal : Array[Array[Int]]) : Double = {
    var h : Double = 0
    for(row <- 0 until child.length)
      for(col <- 0 until child(row).length){

        breakable{
        for(goalrow <- 0 until goal.length) {
          for (goalcol <- 0 until goal(goalrow).length)
            if (child(row)(col) == goal(goalrow)(goalcol)) {
              h += eulerDistance((row, col), (goalrow, goalcol))
              break
            }
        }
        }
      }
    h
  }

  def eulerDistance(child : (Int, Int), goal : (Int, Int)) : Double = {
    val (cX, cY) = (child._1, child._2)
    val (gX, gY) = (goal._1, goal._2)
    val sumOfSquares = ((gX-cX)*(gX-cX)) + ((gY-cY)*(gY-cY))
    math.sqrt(sumOfSquares)
  }

}

object NPuzzle extends App {

  val goalState = Node(Array(
    Array(1,2,3),
    Array(4,5,6),
    Array(7,8,0)))

 /* val goalState = Node(Array(
  Array(2,3,6),
  Array(1,8,0),
  Array(4,7,5))) */

  val initialState = Node(Array(
    Array(4,1,2),
    Array(3,0,6),
    Array(5,7,8)))

  val statetree = StateTree(initialState)

  val npuzzle = NPuzzle(statetree)

/*  val list = npuzzle.hillClimbSearch(initialState, goalState)
  println("\n\nSteps : ")
  list.foreach(StateTree.displayNode) */


}
