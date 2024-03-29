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


sealed abstract class PrimaryConstraint(val ball: String, priority: Int, val sf: (Space => Boolean)) extends SimdConstraint(priority) {
  def isPrimary: Boolean = true;
  def involves(s: String): Boolean = ball==s;
  def isNegative: Boolean = false;
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
case class IsNumber(v: String, n: Int) extends PrimaryConstraint(v, 5, {_.number==n}) {
  def toJSON = s"""{"type": "IsNumber", "ball": "$v", "number": $n}"""
}

case object IsNotNumber extends PrimaryConstraintGenerator({ (n, s) => new IsNotNumber(n,s.number) })
case class IsNotNumber(v: String, n: Int) extends PrimaryConstraint(v, 0, {_.number!=n}) {
  override def isNegative = true;
  def toJSON = s"""{"type": "IsNotNumber", "ball": "$v", "number": $n}"""
}

case object IsRow extends PrimaryConstraintGenerator({ (n, s) => new IsRow(n,s.y) })
case class IsRow(v: String, n: Int) extends PrimaryConstraint(v, 2, {_.y==n}) {
  def toJSON = s"""{"type": "IsRow", "ball": "$v", "row": $n}"""
}

case object IsNotRow extends PrimaryConstraintGenerator({ (n, s) => new IsNotRow(n,s.y) })
case class IsNotRow(v: String, n: Int) extends PrimaryConstraint(v, 0, {_.y!=n}) {
  override def isNegative = true;
  def toJSON = s"""{"type": "IsNotRow", "ball": "$v", "row": $n}"""
}

case object IsColumn extends PrimaryConstraintGenerator({ (n, s) => new IsColumn(n,s.x) })
case class IsColumn(v: String, n: Int) extends PrimaryConstraint(v, 2, {_.x==n}) {
  def toJSON = s"""{"type": "IsColumn", "ball": "$v", "column": $n}"""
}

case object IsNotColumn extends PrimaryConstraintGenerator({ (n, s) => new IsNotColumn(n,s.x) })
case class IsNotColumn(v: String, n: Int) extends PrimaryConstraint(v, 0, {_.x!=n}) {
  override def isNegative = true;
  def toJSON = s"""{"type": "IsNotColumn", "ball": "$v", "column": $n}"""
}

case object IsColor extends PrimaryConstraintGenerator({ (n, s) => new IsColor(n,s.color) })
case class IsColor(v: String, c: Color) extends PrimaryConstraint(v, 5, {_.color==c}) {
  def toJSON = s"""{"type": "IsColor", "ball": "$v", "color": "$c"}"""
}

case object IsNotColor extends PrimaryConstraintGenerator({ (n, s) => new IsNotColor(n,s.color) })
case class IsNotColor(v: String, c: Color) extends PrimaryConstraint(v, 0, {_.color!=c}) {
  override def isNegative = true;
  def toJSON = s"""{"type": "IsNotColor", "ball": "$v", "color": "$c"}"""
}

case object IsOnPath extends ConstraintGenerator[PrimaryConstraint] {
  def allValid(board: Board, sol: Map[String, Int]): List[PrimaryConstraint] = {
    sol.toList flatMap { p => board.spaces(p._2).path map { c => new IsOnPath(p._1, c)} }
  }
}
case class IsOnPath(v: String, c: Color) extends PrimaryConstraint(v, 0, {_.path.contains(c)}) {
  def toJSON = s"""{"type": "IsOnPath", "ball": "$v", "color": "$c"}"""
}
