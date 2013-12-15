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

trait ConstraintGenerator {
  def allValid(board: Board, sol: Map[String, Int]): List[SimdConstraint];
}

object SimdConstraint extends ConstraintGenerator {
  def allValid(board: Board, sol: Map[String,Int]): List[SimdConstraint] = {
    IsNumber.allValid(board, sol) ::: IsColor.allValid(board, sol) ::: SameColor.allValid(board, sol)
  }
}

abstract class SimdConstraint {
  def constraints(prob: Problem): List[Constraint];
}

abstract class PrimaryConstraint(ball: String) extends SimdConstraint {
  def satisfies(s: Space): Boolean;
  def constraints(prob: Problem): List[Constraint] = {
    val candidates = ((0 to (prob.board.spaces.length-1)).toList filter { s => satisfies(prob.board.spaces(s)) }).toArray
    val eqs = candidates map { new XeqC(prob.ball(ball), _) }
    if (eqs.length==1) List(eqs(0))
    else {
      var ret = new util.ArrayList[PrimitiveConstraint];
      eqs foreach { ret.add(_) }
      List(new Or(ret));
    }
  }
}

case object IsNumber extends ConstraintGenerator {
  def allValid(board: Board, sol: Map[String, Int]): List[SimdConstraint] = {
    sol.toList map { p => IsNumber(p._1, board.spaces(p._2).number) }
  }
}
case class IsNumber(v: String, n: Int) extends PrimaryConstraint(v) {
  def satisfies(s: Space) = s.number==n;
}

case object IsColor extends ConstraintGenerator {
  def allValid(board: Board, sol: Map[String, Int]): List[SimdConstraint] = {
    sol.toList map { p => IsColor(p._1, board.spaces(p._2).color) }
  }
}
case class IsColor(v: String, c: Color) extends PrimaryConstraint(v) {
  def satisfies(s: Space) = s.color==c;
}

abstract class SecondaryConstraint(b1: String, b2: String) extends SimdConstraint {
  def constraints(prob: Problem): List[Constraint] = {
    val spaces = 0 to (prob.board.spaces.length-1);
    val candidates = for(i <- spaces;
                         j <- spaces;
                         if i!=j;
                         si <- Some(prob.board.spaces(i));
                         sj <- Some(prob.board.spaces(j));
                         if satisfies(si,sj)) yield new And(new XeqC(prob.ball(b1), i), new XeqC(prob.ball(b2), j))
    if (candidates.length==1) List(candidates(0))
    else {
      var ret = new util.ArrayList[PrimitiveConstraint];
      candidates foreach { ret.add(_) }
      List(new Or(ret));
    }
  }
  def satisfies(s1: Space, s2: Space): Boolean;
}

case object SameColor extends ConstraintGenerator {
  def allValid(board: Board, sol: Map[String, Int]): List[SimdConstraint] = {
    val names = sol.keys.toList
    val valid = for(i <- names;
                    j <- names;
                    si <- sol.get(i) map { board.spaces(_) };
                    sj <- sol.get(j) map { board.spaces(_) };
                    if i!=j && si.color==sj.color) yield SameColor(i, j)
    println("Valid = "+valid)
    valid
  }
}

case class SameColor(b1: String, b2: String) extends SecondaryConstraint(b1, b2) {
  def satisfies(s1: Space, s2: Space) = s1.color==s2.color;
}

/**
 * Used to build statement of Simd problems
 * @param r
 * @param c
 */
class Simd(r: Int, c: Int) {

}

class ProblemGenerator(board: Board, root: String, other: List[String], diff: Int=1) {
  def solve: List[SimdConstraint] = other match {
    case Nil => solveRootOnly
    case y => {
      val cons = y flatMap { n => new ProblemGenerator(board, n, Nil, diff).solve }
      cons
    }
  }
  def solveRootOnly: List[SimdConstraint] = {
    Nil
  }
}

object TestSimd {
  def main(args: Array[String]): Unit = {
    simpleSolve();
  }

  def simpleSolve() = {
    val board = Board(List(Space(Black, 1, Set(), 0, 0), Space(Black, 2, Set(), 0, 0)))
    val pos = SimdConstraint.allValid(board, Map("alpha" -> 0, "beta" -> 1))
    println("Possible constraints: "+pos);
    val prob = Problem(board, List("alpha", "beta"))
    prob.impose(pos);
    println(prob.store);
    prob.solve();
  }
}