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
    cons.sortBy { scorer.score(_) }
  }
  private def investigate(left: List[Constraint], solver: Solver) = {
    
  }
  def craft() = {

  }
}
