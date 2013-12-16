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
}

case object Board {
  def random(w: Int, h: Int, seed: Long, n: Int, colors: List[Color]) = {
    Random.setSeed(seed);
    val balls = (1 to n).toList flatMap { n => colors map { c => Space(c, n, Set(), 0, 0 )}}
    val rballs = Random.shuffle(balls);
    val taken = rballs.take (w*h);
    val spaces = (0 to w*h-1) map { i =>
      val orig = taken(i)
      Space(orig.color, orig.number, orig.path, i % w, i/w)
    }
    Board(spaces.toList)
  }
}
case class Board(spaces: List[Space]);

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
    search.getSolutionListener().searchAll(true);
    search.getSolutionListener().recordSolutions(true);
    val result = search.labeling(store, select);

    if (result) {
      val solnums = (1 to search.getSolutionListener().solutionsNo()).toList;
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
