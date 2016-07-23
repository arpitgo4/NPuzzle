
import java.awt.{Font, BasicStroke, Color, Toolkit}

import scala.io.Source
import scala.swing._
import scala.swing.BorderPanel.Position.{Center, South}

/**
 * Created by Arpit Goyal on 7/19/2015.
 */
case class NPuzzleGUI(n : Int) {


  val puzzle = new Panel {

    val screenSize = Toolkit.getDefaultToolkit().getScreenSize()
    val width = screenSize.getWidth()
    val height = screenSize.getHeight()

    override protected def paintComponent(g : Graphics2D) : Unit = {
      super.paintComponent(g)
      val NUM_BLOCKS = n
      val BLOCK_SIZE = 200
      val RECT_LENGTH = BLOCK_SIZE * NUM_BLOCKS + 10
      val RECT_HEIGHT = BLOCK_SIZE * NUM_BLOCKS + 10
      val RECT_X = (width/2 - RECT_LENGTH/2).toInt
      val RECT_Y = 10
      val BLOCK_X = RECT_X + 5
      val BLOCK_Y = RECT_Y + 5
      g.setColor(Color.BLACK)
      g.setStroke(new BasicStroke(5))
      g.drawRect(RECT_X, RECT_Y, RECT_LENGTH, RECT_HEIGHT)
      g.setFont(new Font(Font.MONOSPACED, Font.BOLD, 60))

      var (x, y) = (BLOCK_X, BLOCK_Y)
      for(i <- 1 to NUM_BLOCKS; j <- 1 to NUM_BLOCKS) {
        if(NPuzzleGUI.puzzlegui.state(i-1)(j-1) != 0){
          g.setColor(Color.BLACK)
          g.drawRect(x, y, BLOCK_SIZE, BLOCK_SIZE)
          g.setColor(Color.CYAN)
          g.fillRect(x, y, BLOCK_SIZE, BLOCK_SIZE)
          g.setColor(Color.BLACK)
          g.drawString(NPuzzleGUI.puzzlegui.state(i-1)(j-1).toString, x+(BLOCK_SIZE/2),y+(BLOCK_SIZE/2))
        }
        x += BLOCK_SIZE
        if (j == NUM_BLOCKS) {
          x = BLOCK_X
          y += BLOCK_SIZE
        }
      }
    }

  }


  val timeComboBox = new ComboBox("0 ms" :: (10 to 100 by 20).map(_.toString + " ms").toList ::: (100 to 1000 by 100).map(_.toString + " ms").toList)
  var thread = new Thread

  val controlPanel = new FlowPanel {
    contents += Button("START"){
      thread.stop
      NPuzzleGUI.pauseTime = timeComboBox.selection.item.split(" ")(0).toInt
      thread = new Thread(new Runnable{
        override def run(): Unit = {
          NPuzzleGUI.npuzzle.hillClimbSearch(NPuzzleGUI.initialState, NPuzzleGUI.goalState)
        }
      })
      thread.start
    }
    contents += Button("STOP"){
      thread.stop
    }
    contents += timeComboBox

  }

  val frame = new MainFrame{
    title = "NPuzzle"
    contents = new BorderPanel {
      layout += puzzle -> Center
      layout += controlPanel -> South
    }
    size = new Dimension(1000, 600)
    centerOnScreen()
  }
  frame.visible = true
  frame.maximize

}

object NPuzzleGUI extends App {

 /* val goalState = Node(Array(
    Array(1,2,3),
    Array(4,5,6),
    Array(7,8,0)))  */

 /* val initialState = Node(Array(
    Array(4,1,2),
    Array(3,0,6),
    Array(5,7,8)))  */

 /* val initialState = Node(Array(
    Array(4,1,2),
    Array(3,0,6),
    Array(5,7,8)))  */


  val initialState = Node((for(line <- Source.fromFile("./puzzleInitial.txt").getLines) yield line.split(",").map(_.toInt).toArray).toArray)

  val goalState = Node((for(line <- Source.fromFile("./puzzleGoal.txt").getLines) yield line.split(",").map(_.toInt).toArray).toArray)

  val statetree = StateTree(initialState)

  val gui = NPuzzleGUI(initialState.state.length)

  var pauseTime = 0

  var puzzlegui : Node = statetree.root

  val npuzzle = new NPuzzle(statetree, gui.puzzle)
}
