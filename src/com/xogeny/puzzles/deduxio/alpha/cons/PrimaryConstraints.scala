package com.xogeny.puzzles.deduxio.alpha.cons

import com.xogeny.puzzles.deduxio.alpha.repr._

/**
 * Created by mtiller on 1/4/14.
 */

/**
 * A PrimaryConstraint is one that involves only one variable.  Note that this class
 * is sealed.  This means all primary constraints are defined in this file.
 * @param v The variable associated with the constraint
 */
sealed abstract class PrimaryConstraint(val v: String) extends Constraint {
  def satisfies(board: Board, s: Space): Boolean;

  /**
   * The evaluate method is used to, on a given board, which squares satisfy this constraint.
   * @param board The specified board
   * @return A list of squares on the specified board (that satisfy this constraint)
   */
  def evaluate(board: Board): Set[Int] = (for (s <- board.elements; if satisfies(board, s._2)) yield s._1).toSet;
}

/**
 * This class is used to generate sets of primary constraints.  It works by looping over all variables
 * in the problem, then identifying the Space associated with that variable in the solution and then
 * generating a list of constraints (for the specified constraint type) that are consistent with that
 * solution for that variable.
 */
trait PrimaryConstraintGenerator extends ConstraintGenerator {
  /**
   * This method computes the set of all valid (consistent) constraints of a given type for a specified
   * board and solution
   * @param prob The specified board (along with solution variables)
   * @param sol The specified solution
   * @return A set of constraints that are consistent with the board and solution
   */
  def valid(prob: Problem, sol: Map[String,Int]): Set[Constraint] = {
    for (v: String <- prob.vars;
         s: Space <- (sol.get(v) map { prob.board.spaces(_) }).toList;
         c <- generate(prob.board, v, s)) yield c;
  }

  /**
   * This method is filled in by each subclass.  It generates the list of all constraints (for that
   * subclass) that can be applied to the specifid space on the specified board.
   * @param board The specified board
   * @param v The variable to be solved for
   * @param s The spaces that the constraint will apply to
   * @return A list of all constraint that are consistent with the specified space
   */
  def generate(board: Board, v: String, s: Space): List[Constraint];
}

/**
 * This class represents all "normal" primary constraints but NOT their inverses.
 * @param V The variable to be solved for
 */
sealed abstract class PositivePrimaryConstraint(V: String) extends PrimaryConstraint(V);

/**
 * This is a utility class where the function of the generator is tied entirely simple function
 * @param f A function that takes a variable name, board and space and returns a PositivePrimaryConstraint
 */
class SpaceBasedConstraintGenerator(f: ((String, Board, Space) => PositivePrimaryConstraint))
  extends PrimaryConstraintGenerator {
  def generate(board: Board, v: String, s: Space) = List(f(v, board, s))
}

/* Generator and constraint that check the number of a space */
case object IsNumber extends SpaceBasedConstraintGenerator({ (v, b, s) => new IsNumber(v, s.number) })
case class IsNumber(V: String, number: Int) extends PositivePrimaryConstraint(V) {
  def satisfies(board: Board, s: Space): Boolean = s.number==number
}

/* Generator and constraint that check the column of a space */
case object IsColumn extends SpaceBasedConstraintGenerator({ (v, b, s) => new IsColumn(v, s.x) })
case class IsColumn(V: String, column: Int) extends PositivePrimaryConstraint(V) {
  def satisfies(board: Board, s: Space): Boolean = s.x==column
}

/* Generator and constraint that check the row of a space */
case object IsRow extends SpaceBasedConstraintGenerator({ (v, b, s) => new IsRow(v, s.y) })
case class IsRow(V: String, row: Int) extends PositivePrimaryConstraint(V) {
  def satisfies(board: Board, s: Space): Boolean = s.y==row
}

/* Generator and constraint that check the color of a space */
case object IsColor extends SpaceBasedConstraintGenerator({ (v, b, s) => new IsColor(v, s.color) })
case class IsColor(V: String, color: Color) extends PositivePrimaryConstraint(V) {
  def satisfies(board: Board, s: Space): Boolean = s.color==color
}

/* Generator and constraint that check if a named space is on a given path */
case object IsOnPath extends PrimaryConstraintGenerator {
  def generate(board: Board, v: String, s: Space): List[PrimaryConstraint] = board.paths(s).toList map { c => IsOnPath(v, c) }
}
case class IsOnPath(V: String, color: Color) extends PositivePrimaryConstraint(V) {
  def satisfies(board: Board, s: Space): Boolean = board.paths(s).contains(color)
}

/**
 * This class inverts the meaning of a PositivePrimaryConstraint.  Its argument is deliberately
 * a PositivePrimaryConstraint and not a PrimaryConstraint to avoid double negation.
 * @param c The constraint being negated.
 */
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
