package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val b = Block(Pos(1,1),Pos(1,1))
    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("legalNeighbors of start") {
    new Level1 {
      // println(s"legalNeighbors of start = ${Block(startPos,startPos).legalNeighbors}")
      val legalNeighborsOfStartPos = List((Block(Pos(1,2),Pos(1,3)),Right), (Block(Pos(2,1),Pos(3,1)),Down))
      assert(Block(startPos,startPos).legalNeighbors == legalNeighborsOfStartPos)
    }
  }

  test("neighborsWithHistory for level 1") {
    new Level1 {
      // val b = Block(Pos(1,1),Pos(1,1))
      // println(b.legalNeighbors.getClass)
      val history = neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up)).toSet
      val result = Set( 
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        )
      assert(history == result)
    }
  }

  test("newNeighborsOnly for level 1") {
    new Level1 {
      val explored = Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
      val filtered = newNeighborsOnly(neighborsWithHistory(b, List(Left,Up)), explored)  
      val res = Set( (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up)) ).toStream
      assert(filtered == res)
    }
  }


  test("optimal solution for level 1") {
    new Level1 {
      // println(s"pathfromstart:${pathsFromStart.toList}")
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
