package com.xogeny.puzzles.deduxio.alpha.cons

import com.xogeny.puzzles.deduxio.alpha.repr._

/**
 * Created by mtiller on 1/3/14.
 */

trait ConstraintGenerator {
}

trait PrimaryConstraintGenerator {
  def valid(prob: Problem): Set[PrimaryConstraint] = {
    for (v <- prob.vars;
         s <- prob.board.spaces;
         c <- generate(v, s)) yield c;
  }
  def generate(v: String, s: Space): Option[PrimaryConstraint];
}

case object IsNumber extends PrimaryConstraintGenerator {
  def generate(v: String, s: Space): Option[PrimaryConstraint] = Some(IsNumber(v, s.number))
}

case class IsNumber(V: String, val number: Int) extends PrimaryConstraint(V) {
  def satisfies(s: Space): Boolean = s.number==number
}
