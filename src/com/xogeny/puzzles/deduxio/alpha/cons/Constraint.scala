package com.xogeny.puzzles.deduxio.alpha.cons

import com.xogeny.puzzles.deduxio.alpha.repr._

/**
 * Created by mtiller on 1/3/14.
 */

abstract class Constraint {
  def consistent(board: Board, sol: Map[String,Int]): Boolean = this match {
    case p: PrimaryConstraint => p.satisfies(board, board.spaces(sol.get(p.v).get))
    case s: SecondaryConstraint => s.satisfies(board, board.spaces(sol.get(s.v1).get), board.spaces(sol.get(s.v2).get))
  }
}
