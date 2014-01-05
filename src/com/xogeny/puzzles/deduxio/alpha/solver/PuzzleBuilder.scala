package com.xogeny.puzzles.deduxio.alpha.solver

import com.xogeny.puzzles.deduxio.alpha.repr._
import com.xogeny.puzzles.deduxio.alpha.cons._
import scala.util.Random

case object PuzzleBuilder {
  val pgens = List(IsColor, IsColumn, IsNumber, IsOnPath, IsRow, IsNotColor, IsNotColumn, IsNotNumber, IsNotRow);
  val sgens = List(AdjacentTo, /* GreaterThan, */ /*LessThan, */ SameColor, SameColumn, SameNumber, SamePath, SameRow,
    NotAdjacentTo, NotSameColor, NotSameColumn, NotSameNumber, NotSameRow);
}
/**
 * Created by mtiller on 1/4/14.
 */
case class PuzzleBuilder(seed: Long, prob: Problem, sol: Map[String,Int], scorer: Scorer) {
  /* Generators to use */

  def sufficient(solver: Solver, keep: List[Constraint], left: List[Constraint]): List[Constraint] = left match {
    case Nil => keep // This might not be sufficient if we are considering only secondary constraints
    case x :: y if solver.unique => keep
    case x :: y => sufficient(solver.impose(x), x :: keep, y);
  }
  val fast = false;
  val baseConstraints = {
    Random.setSeed(seed);
    /* Compute set of all consistent primary constraints (positive and negative) and then randomize and sort them */
    val pcons = Random.shuffle(PuzzleBuilder.pgens flatMap { _.valid(prob, sol) }) sortBy { scorer.score(_) }
    println("This problem has "+pcons.length+" consistent primary constraints")
    /* Find a subset that is sufficient to fully identify the solution */
    val psuf = if (fast) sufficient(SetSolver.forProblem(prob), Nil, pcons) else pcons;
    println("Took "+psuf.length+" primaries out of "+pcons.length+" to be sufficient")

    /* Compute set of all consistent secondary constraints (positive and negative) and then randomize and sort them */
    val scons = Random.shuffle(PuzzleBuilder.sgens flatMap { _.valid(prob, sol) }) sortBy { scorer.score(_) }
    println("This problem has "+scons.length+" consistent secondary constraints");

    /* Find a subset that is sufficient to fully identify the solution */
    val ssuf = if (fast) sufficient(SetSolver.forProblem(prob), Nil, scons) else scons;
    println("Took "+ssuf.length+" secondaries out of "+scons.length+" to be sufficient")

    psuf ::: ssuf
  }
  private def trim(given: List[Constraint], left: List[Constraint], solver: Solver): List[Constraint] = left match {
    case Nil => given
    case x :: y => {
      val sgy = solver.impose(given ::: y);
      val ns = sgy.solve().size;
      if (ns>1) { println("We need "+x+" otherwise we get "+ns+" solutions"); trim(x :: given, y, solver) }
      else { println("We don't need "+x+" without it we still have "+ns+" solutions"); trim(given, y, solver) }
    }
  }
  def craft() = trim(Nil, baseConstraints, SetSolver.forProblem(prob))
}
