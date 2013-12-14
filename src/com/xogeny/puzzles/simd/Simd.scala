package com.xogeny.puzzles.simd

import JaCoP.constraints.{XeqC, Constraint}
import JaCoP.core.{Store, IntVar}
import sun.jvm.hotspot.opto.Block
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
 * Constraints:
 *   * IsColor
 *   * IsNumber
 *   * IsOnPathWith
 *   * IsOnPathOfColor
 *   * IsLessThan
 *   * IsGreaterThan
 *   * IsAdjacent
 */

object Color {
  def lowest = Black;
  def highest = Purple;
}

abstract sealed class Color(c: Int);
case object Black extends Color(0);
case object Red extends Color(1);
case object Green extends Color(2);
case object Blue extends Color(3);
case object Yellow extends Color(4);
case object Purple extends Color(5);

case class Space(c: Color, number: Int, path: Set[Color], x: Int, y: Int) {
  val name: String = "s("+x+","+y+")";
}

class Ball(name: String, board: Problem) {
  val v = new IntVar(board.store, name, 0, board.board.spaces.length-1);
  board.vars = v :: board.vars;
}

case class Board(val spaces: List[Space]) {

}

case class Problem(val board: Board) {
  val store = new Store();
  var vars: List[IntVar] = Nil;
  def impose(c: SimdConstraint) = c.constraints map { store.impose(_) };

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

trait SimdConstraint {
  def constraints: List[Constraint];
}

case class IsNumber(v: Ball, n: Int, prob: Problem) extends SimdConstraint {
  def constraints = (0 to (prob.board.spaces.length-1)).toList filter { s => prob.board.spaces(s).number==n } map { s => new XeqC(v.v, s); }
}

/**
 * Used to build statement of Simd problems
 * @param r
 * @param c
 */
class Simd(r: Int, c: Int) {

}

object TestSimd {
  def main(args: Array[String]): Unit = {
    val board = Board(List(Space(Black, 1, Set(), 0, 0), Space(Black, 2, Set(), 0, 0)))
    val prob = Problem(board)
    val alpha = new Ball("alpha", prob);
    val c1 = IsNumber(alpha, 1, prob);
    println("constraits: "+c1.constraints);
    prob.impose(c1);
    println(prob.store);
    prob.solve();
  }
}