package com.xogeny.puzzles.deduxio.alpha.cons

import com.xogeny.puzzles.deduxio.alpha.repr._

/**
 * Created by mtiller on 1/3/14.
 */

trait ConstraintGenerator {
  def valid(prob: Problem): Set[Constraint];
}

trait PrimaryConstraintGenerator extends ConstraintGenerator {
  def valid(prob: Problem): Set[Constraint] = {
    for (v <- prob.vars;
         s <- prob.board.spaces;
         c <- generate(prob.board, v, s)) yield c;
  }
  def generate(board: Board, v: String, s: Space): List[PrimaryConstraint];
}

trait SecondaryConstraintGenerator extends ConstraintGenerator {
  def valid(prob: Problem): Set[Constraint] = {
    for (v1 <- prob.vars;
         v2 <- prob.vars;
         if v1!=v2;
         s1 <- prob.board.spaces;
         s2 <- prob.board.spaces;
         if s1!=s2;
         c <- generate(prob.board, v1, s1, v2, s2)) yield c;
  }
  def generate(board: Board, v1: String, s1: Space, v2: String, s2: Space): List[SecondaryConstraint];
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

case object SamePath extends SecondaryConstraintGenerator {
  def generate(board: Board, v1: String, s1: Space, v2: String, s2: Space) = (board.paths(s1) & board.paths(s2)).size match {
    case 0 => Nil
    case _ => List(SamePath(v1, v2));
  }
}

case class SamePath(V1: String, V2: String) extends SecondaryConstraint(V1, V2) {
  def satisfies(board: Board, s1: Space, s2: Space): Boolean = (board.paths(s1) & board.paths(s2)).size>0
}