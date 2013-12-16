package com.xogeny.puzzles.simd

import scala.util.Random

/**
 * Created by mtiller on 12/16/13.
 */
case class Plan(cur: List[String], children: List[Plan]) {
  def solvedBelow: List[String] = children flatMap { _.solved }
  def solved: List[String] = cur ::: solvedBelow
}

/* TODO: Stuff to try:
   * Throw in a bunch of variables.  See what sticks.  If a variable isn't referenced by a secondary
     constraint, consider eliminating it.  But test first to make sure getting rid of the variable
     still gives a unique solution for the root variable.  It may not because of the "AllDifferent"
     constraint regarding variables locations.
   * Allow Plan to include a list of Strings for the 'cur' variable.  This would open the door
     for creating puzzles where there are no primary constraints (which could be quite interesting).
 */

case class ProblemGenerator(board: Board, sol: Map[String,Int]) {
  def trim[T <: SimdConstraint](vars: List[String], c: List[T], keep: List[T]): List[T] = c match {
    case Nil => keep
    case x :: y => {
      val prob = Problem(board, vars)
      prob.impose(y)
      prob.impose(keep)
      val sols = prob.solveAll()
      println("# of solutions:"+sols.length)
      // TODO: Ideally, this should just check to make sure the primary variable has only
      // one possible value across all solutions.
      if (sols.length==1) {
        println("We can get rid of "+x);
        trim(vars, y, keep) // x wasn't needed
      }
      else {
        println("We have to keep "+x+" because otherwise we get "+sols.length+" solutions")
        trim(vars, y, x :: keep) // x was needed
      }
    }
  }
  def checkValid(valid: List[SimdConstraint], vars: List[String]) = {
    val prob = Problem(board, vars)
    prob.impose(valid)
    val sols = prob.solveAll();
    if (sols.length!=1) {
      println("Valid: "+valid)
      throw new RuntimeException("Even with all valid constraints, we had "+sols.length+" solutions")
    }
  }
  def solve(plan: Plan): List[SimdConstraint] = plan match {
    case Plan(x :: Nil, Nil) => {
      println("Solving for: "+x);
      val valid = SimdConstraint.allValidPrimary(board, sol) filter { c => x==c.ball }
      checkValid(valid, List(x))
      val res = trim(List(x), Random.shuffle(valid), Nil)
      println("Constraints: "+res)
      res
    }
    case Plan(xl, Nil) => {
      println("Solving for: "+xl);
      val valid = SimdConstraint.allValidSecondary(board, sol) filter { c => xl.contains(c.b1) || xl.contains(c.b2) }
      checkValid(valid, xl)
      val res = trim(xl, Random.shuffle(valid), Nil)
      println("Constraints: "+res)
      res
    }
    case p @ Plan(x, y) => {
      val solved = p.solvedBelow;
      val cons = y flatMap { solve(_) }
      println("Solving for: "+x+", given: "+y)
      val valid = SimdConstraint.allValidSecondary(board, sol) filter { c => x.contains(c.b1) || x.contains(c.b2) }
      checkValid(valid ::: cons, x ::: solved)
      trim(x ::: solved, Random.shuffle(valid ::: cons), Nil)
    }
  }
}

