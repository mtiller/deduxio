package com.xogeny.puzzles.simd

/**
 * Created by mtiller on 12/14/13.
 */

/**
 * Constraints:
 *   * IsOnPathWith
 *   * IsLessThan
 *   * IsGreaterThan
 *   * IsAdjacent
 */

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

class SecondaryConstraints {

}
