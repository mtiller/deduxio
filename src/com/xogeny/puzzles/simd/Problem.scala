package com.xogeny.puzzles.simd

import JaCoP.core.{IntVar, Store}
import JaCoP.constraints.Alldifferent
import JaCoP.search._

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

case class Board(spaces: List[Space]);

case class Problem(board: Board, ballNames: List[String]) {
  val store = new Store();
  val balls: Map[String, IntVar] = Map() ++ (ballNames map { n =>
    val v = new IntVar(store, n, 0, board.spaces.length-1)
    n -> v;
  });
  val vars: List[IntVar] = balls.values.toList
  store.impose(new Alldifferent(vars.toArray))
  def impose(c: SimdConstraint): Unit = c.constraints(this) foreach { store.impose(_) };
  def impose(cl: List[SimdConstraint]): Unit = cl foreach { impose(_) }
  def ball(n: String) = balls.get(n).get;

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
