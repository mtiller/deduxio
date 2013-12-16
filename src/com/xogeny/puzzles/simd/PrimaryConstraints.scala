package com.xogeny.puzzles.simd

import JaCoP.constraints.{Or, PrimitiveConstraint, XeqC, Constraint}
import java.util

/**
 * Created by mtiller on 12/14/13.
 */


/**
 * Constraints:
 *   * IsColor
 *   * IsNumber
 *   * IsOnPathOfColor
 */


abstract class PrimaryConstraint(val ball: String, val priority: Int, val sf: (Space => Boolean)) extends SimdConstraint {
  def satisfies(s: Space) = sf(s)
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

abstract class PrimaryConstraintGenerator(f: (String, Space) => PrimaryConstraint) extends ConstraintGenerator[PrimaryConstraint] {
  def allValid(board: Board, sol: Map[String, Int]): List[PrimaryConstraint] = {
    sol.toList map { p => f(p._1, board.spaces(p._2)) }
  }
}

// TODO: Create inverse constraints

case object IsNumber extends PrimaryConstraintGenerator({ (n, s) => new IsNumber(n,s.number) })
case class IsNumber(v: String, n: Int) extends PrimaryConstraint(v, 30, {_.number==n})

case object IsNotNumber extends PrimaryConstraintGenerator({ (n, s) => new IsNotNumber(n,s.number) })
case class IsNotNumber(v: String, n: Int) extends PrimaryConstraint(v, 20, {_.number!=n})

case object IsRow extends PrimaryConstraintGenerator({ (n, s) => new IsRow(n,s.y) })
case class IsRow(v: String, n: Int) extends PrimaryConstraint(v, 30, {_.y==n})

case object IsNotRow extends PrimaryConstraintGenerator({ (n, s) => new IsNotRow(n,s.y) })
case class IsNotRow(v: String, n: Int) extends PrimaryConstraint(v, 20, {_.y!=n})

case object IsColumn extends PrimaryConstraintGenerator({ (n, s) => new IsColumn(n,s.x) })
case class IsColumn(v: String, n: Int) extends PrimaryConstraint(v, 30, {_.x==n})

case object IsNotColumn extends PrimaryConstraintGenerator({ (n, s) => new IsNotColumn(n,s.x) })
case class IsNotColumn(v: String, n: Int) extends PrimaryConstraint(v, 20, {_.x!=n})

case object IsColor extends PrimaryConstraintGenerator({ (n, s) => new IsColor(n,s.color) })
case class IsColor(v: String, c: Color) extends PrimaryConstraint(v, 30, {_.color==c})

case object IsNotColor extends PrimaryConstraintGenerator({ (n, s) => new IsNotColor(n,s.color) })
case class IsNotColor(v: String, c: Color) extends PrimaryConstraint(v, 20, {_.color!=c})

case object IsOnPath extends ConstraintGenerator[PrimaryConstraint] {
  def allValid(board: Board, sol: Map[String, Int]): List[PrimaryConstraint] = {
    sol.toList flatMap { p => board.spaces(p._2).path map { c => new IsOnPath(p._1, c)} }
  }
}
case class IsOnPath(v: String, c: Color) extends PrimaryConstraint(v, 30, {_.path.contains(c)})

