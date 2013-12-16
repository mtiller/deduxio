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

case object NotSameColor extends ConstraintGenerator[SecondaryConstraint] {
  def satisfies(s1: Space, s2: Space) = s1.color!=s2.color;
  def allValid(board: Board, sol: Map[String, Int]): List[SecondaryConstraint] = {
    allValidPairsWhere(board, sol) { (ni, si, nj, sj) => if (satisfies(si, sj)) Some(NotSameColor(ni, nj)) else None }
  }
}

case class NotSameColor(ball1: String, ball2: String) extends SecondaryConstraint(ball1, ball2) {
  def satisfies(s1: Space, s2: Space) = NotSameColor.satisfies(s1, s2);
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

case object NotSameNumber extends ConstraintGenerator[SecondaryConstraint] {
  def satisfies(s1: Space, s2: Space) = s1.number!=s2.number;
  def allValid(board: Board, sol: Map[String, Int]): List[SecondaryConstraint] = {
    allValidPairsWhere(board, sol) { (ni, si, nj, sj) => if (satisfies(si, sj)) Some(NotSameNumber(ni, nj)) else None }
  }
}

case class NotSameNumber(ball1: String, ball2: String) extends SecondaryConstraint(ball1, ball2) {
  def satisfies(s1: Space, s2: Space) = NotSameNumber.satisfies(s1, s2);
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

case object SameRow extends ConstraintGenerator[SecondaryConstraint] {
  def satisfies(s1: Space, s2: Space) = s1.y==s2.y;
  def allValid(board: Board, sol: Map[String, Int]): List[SecondaryConstraint] = {
    allValidPairsWhere(board, sol) { (ni, si, nj, sj) => if (satisfies(si, sj)) Some(SameRow(ni, nj)) else None }
  }
}

case class SameRow(ball1: String, ball2: String) extends SecondaryConstraint(ball1, ball2) {
  def satisfies(s1: Space, s2: Space) = SameRow.satisfies(s1, s2);
}

case object SameCol extends ConstraintGenerator[SecondaryConstraint] {
  def satisfies(s1: Space, s2: Space) = s1.x==s2.x;
  def allValid(board: Board, sol: Map[String, Int]): List[SecondaryConstraint] = {
    allValidPairsWhere(board, sol) { (ni, si, nj, sj) => if (satisfies(si, sj)) Some(SameCol(ni, nj)) else None }
  }
}

case class SameCol(ball1: String, ball2: String) extends SecondaryConstraint(ball1, ball2) {
  def satisfies(s1: Space, s2: Space) = SameCol.satisfies(s1, s2);
}

case object DifferentRow extends ConstraintGenerator[SecondaryConstraint] {
  def satisfies(s1: Space, s2: Space) = s1.y!=s2.y;
  def allValid(board: Board, sol: Map[String, Int]): List[SecondaryConstraint] = {
    allValidPairsWhere(board, sol) { (ni, si, nj, sj) => if (satisfies(si, sj)) Some(DifferentRow(ni, nj)) else None }
  }
}

case class DifferentRow(ball1: String, ball2: String) extends SecondaryConstraint(ball1, ball2) {
  def satisfies(s1: Space, s2: Space) = DifferentRow.satisfies(s1, s2);
}

case object DifferentColumn extends ConstraintGenerator[SecondaryConstraint] {
  def satisfies(s1: Space, s2: Space) = s1.x!=s2.x;
  def allValid(board: Board, sol: Map[String, Int]): List[SecondaryConstraint] = {
    allValidPairsWhere(board, sol) { (ni, si, nj, sj) => if (satisfies(si, sj)) Some(DifferentColumn(ni, nj)) else None }
  }
}

case class DifferentColumn(ball1: String, ball2: String) extends SecondaryConstraint(ball1, ball2) {
  def satisfies(s1: Space, s2: Space) = DifferentColumn.satisfies(s1, s2);
}

case object AdjacentTo extends ConstraintGenerator[SecondaryConstraint] {
  def satisfies(s1: Space, s2: Space) = (s1.x+1==s2.x && s1.y==s2.y) ||
                                        (s1.x-1==s2.x && s1.y==s2.y) ||
                                        (s1.x==s2.x && s1.y+1==s2.y) ||
                                        (s1.x==s2.x && s1.y-1==s2.y)
  def allValid(board: Board, sol: Map[String, Int]): List[SecondaryConstraint] = {
    allValidPairsWhere(board, sol) { (ni, si, nj, sj) => if (satisfies(si, sj)) Some(AdjacentTo(ni, nj)) else None }
  }
}

case class AdjacentTo(ball1: String, ball2: String) extends SecondaryConstraint(ball1, ball2) {
  def satisfies(s1: Space, s2: Space) = AdjacentTo.satisfies(s1, s2);
}

case object NotAdjacentTo extends ConstraintGenerator[SecondaryConstraint] {
  def satisfies(s1: Space, s2: Space) = !((s1.x+1==s2.x && s1.y==s2.y) ||
    (s1.x-1==s2.x && s1.y==s2.y) ||
    (s1.x==s2.x && s1.y+1==s2.y) ||
    (s1.x==s2.x && s1.y-1==s2.y))
  def allValid(board: Board, sol: Map[String, Int]): List[SecondaryConstraint] = {
    allValidPairsWhere(board, sol) { (ni, si, nj, sj) => if (satisfies(si, sj)) Some(NotAdjacentTo(ni, nj)) else None }
  }
}

case class NotAdjacentTo(ball1: String, ball2: String) extends SecondaryConstraint(ball1, ball2) {
  def satisfies(s1: Space, s2: Space) = NotAdjacentTo.satisfies(s1, s2);
}