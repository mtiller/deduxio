package com.xogeny.puzzles.deduxio.alpha.solver

import com.xogeny.puzzles.deduxio.alpha.repr._
import com.xogeny.puzzles.deduxio.alpha.cons._
import scala.util.Random

/**
 * Created by mtiller on 1/4/14.
 */

/**
 * This class is used to generate puzzles given certain parameters
 * @param nvars The number of variables in the puzzle
 * @param size The size of the board (this assumes a square nxn board)
 * @param n The largest number to appear on a square
 * @param colors The set of colors used for squares and paths
 */
class ProblemGenerator(val nvars: Int, val size: (Int,Int), val n: Int, val colors: List[Color]) {
  type CurList = List[Pair[Color,List[Int]]];
  val letters = List("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "L", "M", "N", "O", "P", "Q", "R", "S", "T");

  /**
   * This is the entry point
   * @param seed A random seed number
   * @return This returns a Problem object and a solution
   */
  def generate(seed: Long) = {
    val (w, h) = size
    val board = randomBoard(seed);
    val s = randomSolution(board, nvars)
    val p = Problem(board, s.keys.toSet)
    (p, s)
  }

  /**
   * Generate a random board
   * @param seed Random number seed
   * @return A random board
   */
  def randomBoard(seed: Long): Board = {
    val (w, h) = size
    val nums = 0 to w*h-1;
    Random.setSeed(seed);
    val balls = (1 to n).toList flatMap { n => colors map { c => Space(c, n, 0, 0 )}}
    val rballs = Random.shuffle(balls);
    val taken = rballs.take (w*h);
    val spaces = nums map { i =>
      val orig = taken(i)
      Space(orig.color, orig.number, i % w, i/w)
    }
    val paths = randomPaths(Board(spaces.toList, Paths(Map())), colors take n, seed)
    Board(spaces.toList, paths)
  }

  /**
   * A random solution
   * @param board The board for which the solution is being generated
   * @param n The number of variables
   * @return The random solution
   */
  def randomSolution(board: Board, n: Int): Map[String,Int] = {
    require(n>0);
    val names = letters take n
    val values = Random.shuffle((0 to board.spaces.length-1).toList)
    Map() ++ (names zip values)
  }

  /**
   * Generates a random set of paths for a given board
   * @param iboard The initial board
   * @param colors The colors to use for the paths
   * @param seed The random seed
   * @return A set of random paths
   */
  def randomPaths(iboard: Board, colors: List[Color], seed: Long): Paths = {
    Random.setSeed(seed)
    val start = Random.shuffle(iboard.elements.toList) map { _._1 }
    val npaths = colors.length;
    val cur = for(i <- (0 to npaths-1).toList) yield (colors(i), List(start(i)));
    growPaths(iboard, cur, Set(), start.toSet--(start take npaths).toSet)
  }

  /**
   * A private route that "grows" that paths from their existing extends until they cannot grow any
   * furthure
   * @param board The specified board
   * @param cur The current set of paths (still active)
   * @param dead Colors of paths that can no longer grow
   * @param left Spaces that haven't been assigned a path
   * @return The current path definitions
   */
  private def growPaths(board: Board, cur: CurList, dead: Set[Color], left: Set[Int]): Paths = left.size match {
    /* If all squares are on a path, we are done */
    case 0 => Paths(Map() ++ cur)
    /* If not, look at the next color to extend */
    case _ => cur match {
      /* If that color is "dead" (cannot be extended), then move on to next element in list */
      case (c, path) :: r if dead.contains(c) => growPaths(board, (cur tail) ::: ((cur head) :: Nil), dead, left)
      case (c, path) :: r => {
        val hadj = board.adjacent(path head) map { (true, _) };  // adjacent to head of current path
        val tadj = board.adjacent(path last) map { (false, _) }; // adjacent to end of current path
        val adjs = (hadj ++ tadj) filter { x => left.contains(x._2) } // Keep ones that are "left"
        val adj = Random.shuffle(adjs.toList) // Randomize
        adj match {
          // Check if all colors have been processed.  If so, we are done.
          case Nil if dead.size+1==cur.length => Paths(Map() ++ cur)
          // If no adjacents, skip this color, mark it as dead and continue with next color
          case Nil => growPaths(board, (cur tail) ::: ((cur head) :: Nil), dead + c, left);
          // If we find an adjacent element, check if it is at the front.  If so, add it to head of path and continue
          case (athead, choice) :: y if athead => growPaths(board, (cur tail) ::: ((c, choice :: path) :: Nil), dead, left-choice)
          // We found an element at the end.  Add it to the end of the path and continue
          case (_, choice) :: y => growPaths(board, (cur tail) ::: ((c, path ::: (choice :: Nil)) :: Nil), dead, left-choice)
        }
      }
    }
  }

  /**
   * This method returns a random "plan".  A plan indicates what variables should depend on what other
   * variables.  This is used to make sure that a given puzzle at least attempts to follow a sequential
   * solution process (in so much as the constraints allow).
   * @param seed Random seed
   * @return A collection of pairs indicating what are acceptable combinations to appear in secondary constraints
   */
  def randomPlan(seed: Int): List[(String,String)] = {
    val vars = letters take nvars;
    Random.setSeed(seed);
    process(Random.shuffle(vars));
  }

  /**
   * This method is a utility method used by the randomPlan method
   * @param vars The set of variables involved
   * @return A collection of pairs indicating what are acceptable combinations to appear in secondary constraints
   */
  private def process(vars: List[String]): List[(String,String)] = vars match {
    case x :: a :: b :: r => {  /* x depends on a and b */
    val head = List(x -> a, x -> b)
      r match {
        case Nil => (x -> a) :: (x -> b) :: Nil
        case _ => {
          val tlen = r.length
          val n = Random.nextInt(tlen)
          val atail = r take n
          val btail = r drop n
          head ::: process(a :: atail) ::: process(b :: btail)
        }
      }
    }
    case x :: a :: Nil => {  /* x depends only on a */
      (x, a) :: Nil
    }
    case x :: Nil => Nil  /* x is a leaf */
    case Nil => Nil /* done */
  }
}
