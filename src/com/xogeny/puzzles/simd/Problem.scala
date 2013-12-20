package com.xogeny.puzzles.simd

import JaCoP.core.{Domain, IntVar, Store}
import JaCoP.constraints.Alldifferent
import JaCoP.search._
import scala.util.Random

/**
 * Created by mtiller on 12/14/13.
 */


/**
 * Properties of each space:
 *   * Color (shape?)
 *   * Number
 *   * Path colors
 *   * x
 *   * y
 */

case class Space(color: Color, number: Int, path: Set[Color], x: Int, y: Int) {
  val name: String = "s("+x+","+y+")";
  def toJSON = {
    val pathstr = path.toList map { p => s""" "$p" """ } mkString ","
    s"""{"color": "$color", "number": $number, "path": [$pathstr], "x": $x, "y": $y}"""
  }
}

case object Board {
  def adjacent(board: Board, e: Int): List[Int] = {
    val s = board.spaces(e);
    val adj = for(i <- (0 to board.spaces.length-1).toList;
        a <- Some(board.spaces(i));
        if e!=i;
        if a.x==s.x || a.x==s.x+1 || a.x==s.x-1;
        if a.y==s.y || a.y==s.y+1 || a.y==s.y-1) yield if (a.x==s.x || a.y==s.y) List(i, i, i) else List(i)
    adj.flatten
  }
  def buildPaths(board: Board, cur: List[(Color, List[Int])], dead: Set[Color], left: Set[Int]): Map[Color,List[Int]] = left.size match {
    /* If all squares are on a path, we are done */
    case 0 => Map() ++ cur
    /* If not, look at the next color to extend */
    case _ => cur match {
      /* If that color is "dead" (cannot be extended), then move on to next element in list */
      case (c, path) :: r if dead.contains(c) => buildPaths(board, (cur tail) ::: ((cur head) :: Nil), dead, left)
      case (c, path) :: r => {
        val adjs = adjacent(board, path head) filter { left.contains(_) }
        if (adjs==Nil) {
          var tadjs = adjacent(board, path last) filter { left.contains(_) }
          if (tadjs==Nil) {
            val newdead = dead + c;
            /* No more colors we can process, we're done */
            if (newdead.size==cur.length) Map() ++ cur;
            else buildPaths(board, (cur tail) ::: ((cur head) :: Nil), newdead, left);
          }
          else {
            val choice = Random.shuffle(tadjs.toList).head
            buildPaths(board, (cur tail) ::: ((c, path ::: (choice :: Nil)) :: Nil), dead, left-choice)
          }
        }
        else {
          val choice = Random.shuffle(adjs.toList).head
          buildPaths(board, (cur tail) ::: ((c, choice :: path) :: Nil), dead, left-choice)
        }
      }
    }
  }
  def pickPaths(board: Board, cur: List[(Color,(Int,Int))], p: Map[Color,Set[Int]], left: Set[Int]): Map[Color,Set[Int]] = cur match {
    case (c, (h, t)) :: r => left.size match {
      case 0 => p
      case _ => {
        val adjs = adjacent(board, h) filter { left.contains(_)}
        if (adjs==Nil) {
          val tadjs = adjacent(board, t) filter { left.contains(_)}
          if (tadjs==Nil) {
            //println("Couldn't find an adjacent space to "+h+" or "+t+" on the "+c+" path among "+left+" after "+p)
            pickPaths(board, cur tail, p, left)
          } else {
            val path = p.get(c).get
            val choice = Random.shuffle(tadjs.toList).head
            val newcur = (cur tail) ::: (c, (choice, t)) :: Nil
            //println("New cur = "+newcur)
            val newp = p + (c -> (path+choice));
            val newleft = left - choice;
            pickPaths(board, newcur, newp, newleft)
          }
        }
        else {
          val path = p.get(c).get
          val choice = Random.shuffle(adjs.toList).head
          val newcur = (cur tail) ::: (c, (h, choice)) :: Nil
          //println("New cur = "+newcur)
          val newp = p + (c -> (path+choice));
          val newleft = left - choice;
          pickPaths(board, newcur, newp, newleft)
        }
      }
    }
    case x => p
  }

  def randomSolution(board: Board, n: Int): Map[String,Int] = {
    require(n>0);
    val letters = List("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "L");
    val names = "X" :: (letters take n-1)
    val values = Random.shuffle((0 to board.spaces.length-1).toList)
    Map() ++ (names zip values)
  }

  /**
   *
   * @param w Width of board
   * @param h Height of board
   * @param seed Random seed
   * @param n Ball numbers (1-n)
   * @param colors Ball colors
   * @return A random board
   */
  def random(w: Int, h: Int, seed: Long, n: Int, colors: List[Color]) = {
    val nums = 0 to w*h-1;
    Random.setSeed(seed);
    val balls = (1 to n).toList flatMap { n => colors map { c => Space(c, n, Set(), 0, 0 )}}
    val rballs = Random.shuffle(balls);
    val taken = rballs.take (w*h);
    val spaces = nums map { i =>
      val orig = taken(i)
      Space(orig.color, orig.number, orig.path, i % w, i/w)
    }
    val iboard = Board(spaces.toList, Map())
    val start = Random.shuffle(nums.toList)
    // TODO: Don't start in a corner
    val cur = for(i <- (0 to colors.length-1).toList) yield (colors(i), List(start(i)));
    //println("Cur = "+cur);
    val paths = buildPaths(iboard, cur, Set(), nums.toSet)
    println("Paths = "+paths)
    val newspaces = nums.toList map { i =>
      val s = iboard.spaces(i);
      Space(s.color, s.number, paths.keys.toSet filter { p => paths.get(p).get.contains(i) }, s.x, s.y)
    }
    Board(newspaces, paths)
  }
}
case class Board(spaces: List[Space], path: Map[Color,List[Int]]) {
  def toJSON = {
    "["+(spaces map { _.toJSON } mkString ",")+"]"
  }
}

case class Problem(board: Board, ballNames: List[String]) {
  val store = new Store();
  val balls: Map[String, IntVar] = Map() ++ (ballNames map { n =>
    val v = new IntVar(store, n, 0, board.spaces.length-1)
    n -> v;
  });
  val vars: List[IntVar] = ballNames map { balls.get(_).get }
  store.impose(new Alldifferent(vars.toArray))
  def impose(c: SimdConstraint): Unit = c.constraints(this) foreach { store.impose(_) };
  def impose(cl: List[SimdConstraint]): Unit = cl foreach { impose(_) }
  def ball(n: String) = balls.get(n).get;

  def solveAll(): List[Map[String,Domain]] = {
    var search: DepthFirstSearch[IntVar]  = new DepthFirstSearch[IntVar]();
    search.setPrintInfo(false);
    var select: SelectChoicePoint[IntVar]  =
      new InputOrderSelect[IntVar](store, vars.toArray,
        new IndomainMin[IntVar]());
    search.getSolutionListener.searchAll(true);
    search.getSolutionListener.recordSolutions(true);
    val result = search.labeling(store, select);

    if (result) {
      val solnums = (1 to search.getSolutionListener.solutionsNo()).toList;
      solnums map { i =>
        val sol = search.getSolution(i);
        val elems = (0 to sol.length-1).toList;
        Map() ++ (elems map { j: Int => ballNames(j) -> sol(j) })
      }
    } else Nil
  }
  def solve() = {
    var search: Search[IntVar]  = new DepthFirstSearch[IntVar]();
    var select: SelectChoicePoint[IntVar]  =
      new InputOrderSelect[IntVar](store, vars.toArray,
        new IndomainMin[IntVar]());
    val result = search.labeling(store, select);

    if ( result ) {
      System.out.println("Solution: ");
      vars map { p => println(p) }
    } else {
      System.out.println("*** No");
    }
  }
}
