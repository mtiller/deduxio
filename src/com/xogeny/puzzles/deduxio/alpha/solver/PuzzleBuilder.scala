package com.xogeny.puzzles.deduxio.alpha.solver

import com.xogeny.puzzles.deduxio.alpha.repr._
import com.xogeny.puzzles.deduxio.alpha.cons._

/**
 * Created by mtiller on 1/4/14.
 */
case class PuzzleBuilder(prob: Problem, sol: Map[String,Int], scorer: Scorer) {
  val baseConstraints = {
    val pgens = List(IsNumber, IsColor);
    val sgens = List(SamePath);
    val cons = (pgens ::: sgens) flatMap { _.valid(prob, sol) }
    cons.sortBy { -scorer.score(_) }
  }
  private def investigate(given: List[Constraint], left: List[Constraint], solver: Solver): Option[List[Constraint]] = left match {
    case Nil => None  // Assume that this yields exactly one solution, so we haven't found the smallest subset
    case x :: y => investigate(x :: given, y, solver.impose(x)) match {  // Investigate without x
      case None => if (solver.solve().size>1) Some(x :: given) else None  // If subset not found, see if x is critical
      case Some(c) => Some(c)  // If subset was found, return it
    }
  }
  def craft() = investigate(Nil, baseConstraints, SetSolver.forProblem(prob)) match {
    case None => throw new RuntimeException("Couldn't find a problem in "+baseConstraints);
    case Some(x) => x
  }
}
