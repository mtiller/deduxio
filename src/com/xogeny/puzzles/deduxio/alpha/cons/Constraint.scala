package com.xogeny.puzzles.deduxio.alpha.cons

import com.xogeny.puzzles.deduxio.alpha.repr._

/**
 * Created by mtiller on 1/3/14.
 */

/**
 * This is the base class for all constraints we will consider.
 */
abstract class Constraint {
  /**
   * Indicates if this constraint is consistent (true) for the given board and solution
   * @param board Specified board
   * @param sol Specified solution
   * @return true if constraint is consistent, otherwise false.
   */
  def consistent(board: Board, sol: Map[String,Int]): Boolean = this match {
    case p: PrimaryConstraint => p.satisfies(board, board.spaces(sol.get(p.v).get))
    case s: SecondaryConstraint => s.satisfies(board, board.spaces(sol.get(s.v1).get), board.spaces(sol.get(s.v2).get))
  }
}
