package com.xogeny.puzzles.simd

/**
 * Created by mtiller on 12/14/13.
 */

/**
 * Constraints:
 *   * SameColor
 *   * SameNumber
 *   * IsOnPathWith
 *   * IsLessThan
 *   * IsGreaterThan
 *   * IsAdjacent
 */

case object SameColor extends ConstraintGenerator[SecondaryConstraint] {
  def satisfies(s1: Space, s2: Space) = s1.color==s2.color;
  def allValid(board: Board, sol: Map[String, Int]): List[SecondaryConstraint] = {
    allValidPairsWhere(board, sol) { (ni, si, nj, sj) => if (satisfies(si, sj)) Some(SameColor(ni, nj)) else None }
  }
}

case class SameColor(ball1: String, ball2: String) extends SecondaryConstraint(ball1, ball2) {
  def satisfies(s1: Space, s2: Space) = SameColor.satisfies(s1, s2);
}

case object SameNumber extends ConstraintGenerator[SecondaryConstraint] {
  def satisfies(s1: Space, s2: Space) = s1.number==s2.number;
  def allValid(board: Board, sol: Map[String, Int]): List[SecondaryConstraint] = {
    allValidPairsWhere(board, sol) { (ni, si, nj, sj) => if (satisfies(si, sj)) Some(SameNumber(ni, nj)) else None }
  }
}

case class SameNumber(ball1: String, ball2: String) extends SecondaryConstraint(ball1, ball2) {
  def satisfies(s1: Space, s2: Space) = SameNumber.satisfies(s1, s2);
}

case object IsOnPathWith extends ConstraintGenerator[SecondaryConstraint] {
  def satisfies(s1: Space, s2: Space) = s1.path.intersect(s2.path).size>0;
  def allValid(board: Board, sol: Map[String, Int]): List[SecondaryConstraint] = {
    allValidPairsWhere(board, sol) { (ni, si, nj, sj) => if (satisfies(si, sj)) Some(IsOnPathWith(ni, nj)) else None }
  }
}

case class IsOnPathWith(ball1: String, ball2: String) extends SecondaryConstraint(ball1, ball2) {
  def satisfies(s1: Space, s2: Space) = IsOnPathWith.satisfies(s1, s2);
}

case object LessThan extends ConstraintGenerator[SecondaryConstraint] {
  def satisfies(s1: Space, s2: Space) = s1.number<s2.number;
  def allValid(board: Board, sol: Map[String, Int]): List[SecondaryConstraint] = {
    allValidPairsWhere(board, sol) { (ni, si, nj, sj) => if (satisfies(si, sj)) Some(LessThan(ni, nj)) else None }
  }
}

case class LessThan(ball1: String, ball2: String) extends SecondaryConstraint(ball1, ball2) {
  def satisfies(s1: Space, s2: Space) = LessThan.satisfies(s1, s2);
}

case object GreaterThan extends ConstraintGenerator[SecondaryConstraint] {
  def satisfies(s1: Space, s2: Space) = s1.number>s2.number;
  def allValid(board: Board, sol: Map[String, Int]): List[SecondaryConstraint] = {
    allValidPairsWhere(board, sol) { (ni, si, nj, sj) => if (satisfies(si, sj)) Some(GreaterThan(ni, nj)) else None }
  }
}

case class GreaterThan(ball1: String, ball2: String) extends SecondaryConstraint(ball1, ball2) {
  def satisfies(s1: Space, s2: Space) = GreaterThan.satisfies(s1, s2);
}