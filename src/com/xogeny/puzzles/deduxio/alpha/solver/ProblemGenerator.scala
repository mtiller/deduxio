package com.xogeny.puzzles.deduxio.alpha.solver

import com.xogeny.puzzles.deduxio.alpha.repr._
import com.xogeny.puzzles.deduxio.alpha.cons._
import scala.util.Random

/**
 * Created by mtiller on 1/4/14.
 */
object ProblemGenerator {
  type CurList = List[Pair[Color,List[Int]]];

  def generate(w: Int, h: Int, seed: Int, n: Int, colors: List[Color]) = {
    val board = randomBoard(w, h, seed, n, colors);
    val s = randomSolution(board, n)
    val p = Problem(board, s.keys.toSet)
    val pgens = List(IsNumber, IsColor);
    val sgens = List(SamePath);
    val cons = (pgens ::: sgens) flatMap { _.valid(p, s) }
    (p, s)
  }

  def randomBoard(w: Int, h: Int, seed: Long, n: Int, colors: List[Color]): Board = {
    val nums = 0 to w*h-1;
    Random.setSeed(seed);
    val balls = (1 to n).toList flatMap { n => colors map { c => Space(c, n, 0, 0 )}}
    val rballs = Random.shuffle(balls);
    val taken = rballs.take (w*h);
    val spaces = nums map { i =>
      val orig = taken(i)
      Space(orig.color, orig.number, i % w, i/w)
    }
    val paths = randomPaths(Board(spaces.toList, Paths(Map())), (colors take n), seed)
    Board(spaces.toList, paths)
  }

  def randomSolution(board: Board, n: Int): Map[String,Int] = {
    require(n>0);
    val letters = List("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "L");
    val names = letters take n
    val values = Random.shuffle((0 to board.spaces.length-1).toList)
    Map() ++ (names zip values)
  }

  def randomPaths(iboard: Board, colors: List[Color], seed: Long): Paths = {
    Random.setSeed(seed)
    val start = Random.shuffle(iboard.elements.toList) map { _._1 }
    val npaths = colors.length;
    val cur = for(i <- (0 to npaths-1).toList) yield (colors(i), List(start(i)));
    growPaths(iboard, cur, Set(), start.toSet--(start take npaths).toSet)
  }

  private def growPaths(board: Board, cur: CurList, dead: Set[Color], left: Set[Int]): Paths = left.size match {
    /* If all squares are on a path, we are done */
    case 0 => Paths(Map() ++ cur)
    /* If not, look at the next color to extend */
    case _ => cur match {
      /* If that color is "dead" (cannot be extended), then move on to next element in list */
      case (c, path) :: r if dead.contains(c) => growPaths(board, (cur tail) ::: ((cur head) :: Nil), dead, left)
      case (c, path) :: r => {
        val adjs2 = board.adjacent(path head) filter { left.contains(_) }
        adjs2.toList match {
          case Nil => {
            val tadjs2 = board.adjacent(path last) filter { left.contains(_) }
            tadjs2.toList match {
              case Nil => {
                val newdead = dead + c;
                /* No more colors we can process, we're done */
                if (newdead.size==cur.length) Paths(Map() ++ cur);
                else growPaths(board, (cur tail) ::: ((cur head) :: Nil), newdead, left);
              }
              case x => {
                val choice = Random.shuffle(x).head
                growPaths(board, (cur tail) ::: ((c, path ::: (choice :: Nil)) :: Nil), dead, left-choice)
              }
            }
          }
          case x => {
            val choice = Random.shuffle(x).head
            growPaths(board, (cur tail) ::: ((c, choice :: path) :: Nil), dead, left-choice)
          }
        }
      }
    }
  }
}
