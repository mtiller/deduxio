package com.xogeny.puzzles.simd

import JaCoP.constraints._
import JaCoP.core.{Store, IntVar}
import JaCoP.search._
import java.util
import scala.Some

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

case class Space(color: Color, number: Int, path: Set[Color], x: Int, y: Int) {
  val name: String = "s("+x+","+y+")";
}

case class Board(val spaces: List[Space]);

case class Problem(val board: Board, ballNames: List[String]) {
  val store = new Store();
  val balls: Map[String, IntVar] = Map() ++ (ballNames map { n =>
    val v = new IntVar(store, n, 0, board.spaces.length-1)
    n -> v;
  });
  val vars: List[IntVar] = balls.values.toList
  store.impose(new Alldifferent(vars.toArray))
  def impose(c: SimdConstraint) = c.constraints map { store.impose(_) };
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

abstract class SimdConstraint(prob: Problem) {
  def constraints: List[Constraint];
}

abstract class PrimaryConstraint(prob: Problem, ball: String) extends SimdConstraint(prob) {
  def satisfies(s: Space): Boolean;
  def constraints: List[Constraint] = {
    val candidates = ((0 to (prob.board.spaces.length-1)).toList filter { s => satisfies(prob.board.spaces(s)) }).toArray
    val eqs = candidates map { new XeqC(prob.ball(ball), _) }
    var ret = new util.ArrayList[PrimitiveConstraint];
    eqs foreach { ret.add(_) }
    List(new Or(ret));
  }
}

case object IsNumber {
  def matching(v: String, s: Space, prob: Problem): List[IsNumber] = List(IsNumber(v, s.number, prob))
}
case class IsNumber(v: String, n: Int, prob: Problem) extends PrimaryConstraint(prob, v) {
  def satisfies(s: Space) = s.number==n;
}

case object IsColor {
  def matching(v: String, s: Space, prob: Problem): List[IsColor] = List(IsColor(v, s.color, prob))
}
case class IsColor(v: String, c: Color, prob: Problem) extends PrimaryConstraint(prob, v) {
  def satisfies(s: Space) = s.color==c;
}

abstract class SecondaryConstraint(b1: String, b2: String, prob: Problem) extends SimdConstraint(prob) {
  def constraints: List[Constraint] = {
    val spaces = (0 to (prob.board.spaces.length-1));
    val candidates = for(i <- spaces;
                         j <- spaces;
                         if i!=j;
                         si <- Some(prob.board.spaces(i));
                         sj <- Some(prob.board.spaces(j));
                         if satisfies(si,sj)) yield new And(new XeqC(prob.ball(b1), i), new XeqC(prob.ball(b2), j))
    var ret = new util.ArrayList[PrimitiveConstraint];
    candidates foreach { ret.add(_) }
    List(new Or(ret));
  }
  def satisfies(s1: Space, s2: Space): Boolean;
}

case class SameColor(b1: String, b2: String, prob: Problem) extends SecondaryConstraint(b1, b2, prob) {
  def satisfies(s1: Space, s2: Space) = s1.color==s2.color;
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
    simpleSolve();
  }

  def simpleSolve() = {
    val board = Board(List(Space(Black, 1, Set(), 0, 0), Space(Black, 2, Set(), 0, 0)))
    val prob = Problem(board, List("alpha", "beta"))
    val c1 = IsNumber("alpha", 1, prob);
    val c2 = SameColor("alpha", "beta", prob);
    println("constraits: "+c1.constraints);
    prob.impose(c1);
    println(prob.store);
    prob.solve();
  }
}