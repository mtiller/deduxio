package com.xogeny.puzzles.deduxio.alpha.cons

import com.xogeny.puzzles.deduxio.alpha.repr._

/**
 * Created by mtiller on 1/3/14.
 */

sealed abstract class Constraint {
}

abstract class PrimaryConstraint(val v: String) extends Constraint {
  def satisfies(board: Board, s: Space): Boolean;
  def evaluate(board: Board): Set[Int] = (for (s <- board.elements; if satisfies(board, s._2)) yield s._1).toSet;
}

abstract class SecondaryConstraint(val v1: String, val v2: String) extends Constraint {
  def satisfies(board: Board, s1: Space, s2: Space): Boolean;
  def evaluate(board: Board): Set[(Int,Int)] = (for (s1 <- board.elements;
                                                     s2 <- board.elements;
                                                     if s1!=s2;
                                                     if satisfies(board, s1._2, s2._2)) yield (s1._1, s2._1)).toSet;
}
