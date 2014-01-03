package com.xogeny.puzzles.deduxio.alpha.cons

import com.xogeny.puzzles.deduxio.alpha.repr._

/**
 * Created by mtiller on 1/3/14.
 */

sealed abstract class Constraint {
  def evaluate(board: Board): Set[Int];
}

abstract class PrimaryConstraint(val v: String) extends Constraint {
  def satisfies(s: Space): Boolean;
  def evaluate(board: Board): Set[Int] = (for (s <- board.elements; if (satisfies(s._2))) yield s._1).toSet;
}

abstract class SecondaryConstraint(val v1: String, val v2: String) extends Constraint {
  def evaluate(board: Board): (Set[Int],Set[Int]);
}
