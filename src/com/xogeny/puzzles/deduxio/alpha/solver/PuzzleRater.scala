package com.xogeny.puzzles.deduxio.alpha.solver

import com.xogeny.puzzles.deduxio.alpha.cons._
import com.xogeny.puzzles.deduxio.alpha.repr._

/**
 * Created by mtiller on 1/5/14.
 */
class PuzzleRater(prob: Problem, cons: List[Constraint], sol: Map[String,Int]) {
  def analyze() = {
    val solver = SetSolver.forProblem(prob).impose(cons);
    //val steps = computeSteps(solver, prob.vars, Map());
    val steps = solver.vals map { p => (p._1 -> p._2.size)}
    steps
  }
  private def computeSteps(solver: SetSolver, left: Set[String], sofar: Map[String,Int]): Map[String,Int] = left.size match {
    case 0 => sofar
    case _ => {
      println("Vals with "+left+" left: "+solver.vals);
      val unsolved = solver.vals filter { p => left.contains(p._1) }
      val (evar, esteps) = unsolved.minBy[Int] { p => p._2.size }
      println("Easiest variable: "+evar);
      println("  # of steps: "+esteps);
      val space = prob.board.spaces(sol.get(evar).get)
      println("  Space: "+space);
      val c1 = IsRow(evar, space.y);
      val c2 = IsColumn(evar, space.x)
      val nsolver = solver.impose(c1).impose(c2);
      val nleft = left-evar;
      println("Left to solve for: "+nleft);
      val nres = sofar + (evar -> esteps.size);
      computeSteps(nsolver, nleft, nres)
    }
  }
}
