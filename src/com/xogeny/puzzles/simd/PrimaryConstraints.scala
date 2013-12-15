package com.xogeny.puzzles.simd

/**
 * Created by mtiller on 12/14/13.
 */


/**
 * Constraints:
 *   * IsColor
 *   * IsNumber
 *   * IsOnPathOfColor
 */

case object IsNumber extends ConstraintGenerator[PrimaryConstraint] {
  def allValid(board: Board, sol: Map[String, Int]): List[PrimaryConstraint] = {
    sol.toList map { p => IsNumber(p._1, board.spaces(p._2).number) }
  }
}
case class IsNumber(v: String, n: Int) extends PrimaryConstraint(v) {
  def satisfies(s: Space) = s.number==n;
}

case object IsColor extends ConstraintGenerator[PrimaryConstraint] {
  def allValid(board: Board, sol: Map[String, Int]): List[PrimaryConstraint] = {
    sol.toList map { p => IsColor(p._1, board.spaces(p._2).color) }
  }
}
case class IsColor(v: String, c: Color) extends PrimaryConstraint(v) {
  def satisfies(s: Space) = s.color==c;
}

case object IsOnPath extends ConstraintGenerator[PrimaryConstraint] {
  def allValid(board: Board, sol: Map[String, Int]): List[PrimaryConstraint] = {
    sol.toList flatMap { p => board.spaces(p._2).path.toList map { c: Color => IsOnPath(p._1, c) } }
  }
}
case class IsOnPath(v: String, c: Color) extends PrimaryConstraint(v) {
  def satisfies(s: Space) = s.path.contains(c)
}

