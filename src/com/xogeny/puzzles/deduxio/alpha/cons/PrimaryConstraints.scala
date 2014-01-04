package com.xogeny.puzzles.deduxio.alpha.cons

import com.xogeny.puzzles.deduxio.alpha.repr._

/**
 * Created by mtiller on 1/4/14.
 */
sealed abstract class PrimaryConstraint(val v: String) extends Constraint {
  def satisfies(board: Board, s: Space): Boolean;
  def evaluate(board: Board): Set[Int] = (for (s <- board.elements; if satisfies(board, s._2)) yield s._1).toSet;
}

trait PrimaryConstraintGenerator extends ConstraintGenerator {
  def valid(prob: Problem, sol: Map[String,Int]): Set[Constraint] = {
    for (v: String <- prob.vars;
         s: Space <- (sol.get(v) map { prob.board.spaces(_) }).toList;
         c <- generate(prob.board, v, s)) yield c;
  }
  def generate(board: Board, v: String, s: Space): List[Constraint];
}

sealed abstract class PositivePrimaryConstraint(V: String) extends PrimaryConstraint(V);

class SpaceBasedConstraintGenerator(f: ((String, Board, Space) => PositivePrimaryConstraint))
  extends PrimaryConstraintGenerator {
  def generate(board: Board, v: String, s: Space) = List(f(v, board, s))
}

case object IsNumber extends SpaceBasedConstraintGenerator({ (v, b, s) => new IsNumber(v, s.number) })
case class IsNumber(V: String, number: Int) extends PositivePrimaryConstraint(V) {
  def satisfies(board: Board, s: Space): Boolean = s.number==number
}

case object IsColumn extends SpaceBasedConstraintGenerator({ (v, b, s) => new IsColumn(v, s.x) })
case class IsColumn(V: String, column: Int) extends PositivePrimaryConstraint(V) {
  def satisfies(board: Board, s: Space): Boolean = s.x==column
}

case object IsRow extends SpaceBasedConstraintGenerator({ (v, b, s) => new IsRow(v, s.y) })
case class IsRow(V: String, row: Int) extends PositivePrimaryConstraint(V) {
  def satisfies(board: Board, s: Space): Boolean = s.y==row
}

case object IsColor extends SpaceBasedConstraintGenerator({ (v, b, s) => new IsColor(v, s.color) })
case class IsColor(V: String, color: Color) extends PositivePrimaryConstraint(V) {
  def satisfies(board: Board, s: Space): Boolean = s.color==color
}

case object IsOnPath extends PrimaryConstraintGenerator {
  def generate(board: Board, v: String, s: Space): List[PrimaryConstraint] = board.paths(s).toList map { c => IsOnPath(v, c) }
}
case class IsOnPath(V: String, color: Color) extends PositivePrimaryConstraint(V) {
  def satisfies(board: Board, s: Space): Boolean = board.paths(s).contains(color)
}

case class PrimaryNot(c: PositivePrimaryConstraint) extends PrimaryConstraint(c.v) {
  def satisfies(board: Board, s: Space): Boolean = !c.satisfies(board, s)
}

/* Negative Generators */

case object IsNotColor extends PrimaryConstraintGenerator {
  def generate(board: Board, v: String, s: Space): List[PrimaryConstraint] = {
    for(c <- board.colors.toList;
        if c!=s.color) yield PrimaryNot(IsColor(v, c));
  }
}

case object IsNotColumn extends PrimaryConstraintGenerator {
  def generate(board: Board, v: String, s: Space): List[PrimaryConstraint] = {
    for(c <- board.columns.toList;
        if c!=s.x) yield PrimaryNot(IsColumn(v, c));
  }
}

case object IsNotNumber extends PrimaryConstraintGenerator {
  def generate(board: Board, v: String, s: Space): List[PrimaryConstraint] = {
    for(n <- board.numbers.toList;
        if n!=s.number) yield PrimaryNot(IsNumber(v, n));
  }
}

case object IsNotRow extends PrimaryConstraintGenerator {
  def generate(board: Board, v: String, s: Space): List[PrimaryConstraint] = {
    for(c <- board.rows.toList;
        if c!=s.y) yield PrimaryNot(IsRow(v, c));
  }
}