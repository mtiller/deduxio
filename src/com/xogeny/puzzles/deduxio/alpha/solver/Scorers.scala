package com.xogeny.puzzles.deduxio.alpha.solver

import com.xogeny.puzzles.deduxio.alpha.cons._

/**
 * Created by mtiller on 1/4/14.
 */
trait Scorer {
  def score(c: Constraint): Int;
}

case object PvS extends Scorer {
  def score(c: Constraint) = c match {
    case p: PrimaryConstraint => -5;
    case _ => 5;
  }
}