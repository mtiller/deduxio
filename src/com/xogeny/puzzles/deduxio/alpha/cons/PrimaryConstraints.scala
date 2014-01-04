package com.xogeny.puzzles.deduxio.alpha.cons

import com.xogeny.puzzles.deduxio.alpha.repr._

/**
 * Created by mtiller on 1/4/14.
 */
abstract class PrimaryConstraint(val v: String) extends Constraint {
  def satisfies(board: Board, s: Space): Boolean;
  def evaluate(board: Board): Set[Int] = (for (s <- board.elements; if satisfies(board, s._2)) yield s._1).toSet;
}

trait PrimaryConstraintGenerator extends ConstraintGenerator {
  def valid(prob: Problem): Set[Constraint] = {
    for (v <- prob.vars;
         s <- prob.board.spaces;
         c <- generate(prob.board, v, s)) yield c;
  }
  def generate(board: Board, v: String, s: Space): List[PrimaryConstraint];
}

case object IsNumber extends PrimaryConstraintGenerator {
  def generate(board: Board, v: String, s: Space): List[PrimaryConstraint] = List(IsNumber(v, s.number))
}

case class IsNumber(V: String, number: Int) extends PrimaryConstraint(V) {
  def satisfies(board: Board, s: Space): Boolean = s.number==number
}

case object IsColor extends PrimaryConstraintGenerator {
  def generate(board: Board, v: String, s: Space): List[PrimaryConstraint] = List(IsColor(v, s.color))
}

case class IsColor(V: String, color: Color) extends PrimaryConstraint(V) {
  def satisfies(board: Board, s: Space): Boolean = s.color==color
}


