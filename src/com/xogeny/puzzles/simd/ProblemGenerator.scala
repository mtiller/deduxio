package com.xogeny.puzzles.simd

import scala.util.Random

/**
 * Created by mtiller on 12/16/13.
 */
case class Plan(cur: List[String], children: List[Plan]=Nil) {
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

case class ProblemGenerator(board: Board, sol: Map[String,Int], verbose: Boolean=false) {
  def shuffle(l: List[SimdConstraint]): List[SimdConstraint] = {
    Random.shuffle(l).sortWith { (x, y) => x.priority < y.priority }
  }
  def trim[T <: SimdConstraint](vars: List[String], c: List[T], keep: List[T]): List[T] = c match {
    case Nil => keep
    case x :: y => {
      val prob = Problem(board, vars)
      prob.impose(y)
      prob.impose(keep)
      val sols = prob.solveAll()
      if (verbose) println("# of solutions:"+sols.length)
      // TODO: Ideally, this should just check to make sure the primary variable has only
      // one possible value across all solutions.
      if (sols.length==1) {
        if (verbose) println("We can get rid of "+x);
        trim(vars, y, keep) // x wasn't needed
      }
      else {
        if (verbose) println("We have to keep "+x+" because otherwise we get "+sols.length+" solutions")
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
      println("Solving (1) for: "+x);
      val valid = SimdConstraint.allValidPrimary(board, sol) filter { c => x==c.ball }
      checkValid(valid, List(x))
      val res = trim(List(x), shuffle(valid), Nil)
      println("Constraints: "+res)
      res
    }
    case p @ Plan(xl, Nil) => {
      val vars = p.solved;
      println("Solving (2) for: "+xl);
      val valid = SimdConstraint.allValidSecondary(board, sol) filter { c => vars.contains(c.b1) && vars.contains(c.b2) }
      checkValid(valid, xl)
      val res = trim(xl, shuffle(valid), Nil)
      println("Constraints: "+res)
      res
    }
    case p @ Plan(x, y) => {
      val solved = p.solvedBelow;
      val vars = p.solved;
      val cons = y flatMap { solve(_) }
      println("Solving (3) for: "+x+", given: "+y)
      println("Cons for "+y+": "+cons)
      /* If you add primary constraints here, you'll end up isolating the secondary variables so that they
         don't really matter.
       */
      val valid = SimdConstraint.allValidSecondary(board, sol) filter { c => vars.contains(c.b1) && vars.contains(c.b2) }
      println("Valid for "+x+": "+valid)
      checkValid(valid ::: cons, x ::: solved)
      trim(x ::: solved, shuffle(valid ::: cons), Nil)
    }
  }
}

