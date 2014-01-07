package com.xogeny.puzzles.deduxio.alpha.solver

import com.xogeny.puzzles.deduxio.alpha.cons._
import com.xogeny.puzzles.deduxio.alpha.repr._

/**
 * Created by mtiller on 1/3/14.
 */

/**
 * This is an interface for a generic solver (although at present only one is implemented)
 */
trait Solver {
  def unique: Boolean;
  def solve(): Set[Map[String,Int]];
  def multiple: Boolean; // Has multiple solutions (for cases where we don't actually need to compute them all)
  def impose(c: Constraint): Solver;
  def impose(cl: List[Constraint]): Solver = cl match {
    case Nil => this
    case x :: y => impose(y).impose(x)
  }
}

/**
 * Companion object for the SetSolver class
 */
object SetSolver {
  /**
   * This method returns a solver for a given problem
   * @param prob The specified problem
   * @return An initialized SetSolver object
   */
  def forProblem(prob: Problem): SetSolver = {
    SetSolver(prob, Map() ++ (prob.vars map { v => v -> (0 to prob.board.spaces.length-1).toSet}), Nil)
  }
}

/**
 * The SetSolver class
 * @param prob The problem being solved
 * @param vals The possible values remaining for each variable (after imposition of primary constraints)
 * @param cons The set of secondary constraints imposed
 */
case class SetSolver(prob: Problem, vals: Map[String,Set[Int]], cons: List[SecondaryConstraint]) extends Solver {
  val board = prob.board;

  def unique = vals.values forall { _.size==1 }
  def multiple = solveAtMost(2).size>1

  def impose(c: Constraint): SetSolver = c match {
    case p: PrimaryConstraint => {
      val orig = vals.get(p.v).get
      val consistent = orig filter { e => p.satisfies(board, board.spaces(e)) }
      SetSolver(prob, vals + (p.v -> consistent), cons)
    }
    case s: SecondaryConstraint => {
      SetSolver(prob, vals, s :: cons)
    }
  }

  override def impose(cl: List[Constraint]): SetSolver = cl match {
    case Nil => this
    case x :: y => impose(y).impose(x)
  }

  def solve(): Set[Map[String,Int]] = solutionStream(Map[String,Int](), None, prob.vars.toList, cons).toSet;
  def solveAtMost(atmost: Int): Set[Map[String,Int]] = (solutionStream(Map[String,Int](), None, prob.vars.toList, cons) take atmost).toSet

  /**
   * The solution is returned as a stream (lazily evaluated).  This is very useful in trying to efficiently assess
   * whether a given problem has 0, 1, or more than 1 solution.  This method is called recursively.
   * @param sol Current solution (variables that have already been bound)
   * @param last The last variable (if any) that was solved for.
   * @param left The variables that are to be solved for still
   * @param scons The set of unprocessed secondary constraints
   * @return A stream of solutions
   */
  private def solutionStream(sol: Map[String,Int], last: Option[String],
                             left: List[String], scons: List[SecondaryConstraint]): Stream[Map[String,Int]] = left match {
    case Nil => Stream(sol)
    case next :: tail => {
      val activeInfo = last map { x: String =>                          // If there was a last variable solved for
        val (active, rem) = scons.partition { _.involves(x, next) }     // Get newly active constraints
        ({ ns: Int => active.forall { _.consistent(board, sol + (next -> ns)) }}, rem)
      }
      val (consistent, rem) = activeInfo.getOrElse(({ns: Int => true}, scons))
      for(ne <- vals.get(next).get.toStream;    // Consider each potential value for current variable
          if !sol.values.toSet.contains(ne);    // Make sure that value hasn't already been assigned
          if consistent(ne);      // Make sure it is consistent with activated constraints
          s  <- solutionStream(sol + (next -> ne), Some(next), tail, rem)) yield s;
    }
  }
}
