package com.xogeny.puzzles.deduxio.alpha.solver

import com.xogeny.puzzles.deduxio.alpha.cons._
import com.xogeny.puzzles.deduxio.alpha.repr._

/**
 * Created by mtiller on 1/3/14.
 */

object SetSolver {
  def forProblem(prob: Problem) = {
    SetSolver(prob, Map() ++ (prob.vars map { v => v -> (0 to prob.board.spaces.length-1).toSet}), Nil)
  }
}

case class SetSolver(prob: Problem, vals: Map[String,Set[Int]], cons: List[SecondaryConstraint]) {
  val board = prob.board;
  val involving: Map[String, List[SecondaryConstraint]] = {
    val im = Map[String, List[SecondaryConstraint]]() ++ (vals.keys.toList map { v => v -> Nil})
    cons.foldLeft(im) { (m, c) =>
      val l1 = c :: m.get(c.v1).get
      val l2 = c :: m.get(c.v2).get
      m ++ List(c.v1 -> l1, c.v2 -> l2)
    }
  }
  def impose(cl: List[Constraint]): SetSolver = cl match {
    case Nil => this
    case x :: y => impose(y).impose(x)
  }
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
  def solve(): Set[Map[String,Int]] = solve(Map[String,Int](), None, prob.vars.toList, cons);


  private def solve(sol: Map[String,Int], last: Option[String],
                    left: List[String], scons: List[SecondaryConstraint]): Set[Map[String,Int]] = left match {
    case Nil => Set(sol)
    case next :: tail => {
      val activeInfo = last map { x: String =>                          // If there was a last variable solved for
        val (active, rem) = scons.partition { _.involves(x, next) }     // Get newly active constraints
        ({ ns: Space => active.forall { _.satisfies(board, board.spaces(sol.get(x).get), ns)}}, rem)
      }
      val (consistent, rem) = activeInfo.getOrElse(({ns: Space => true}, scons))
      for(ne <- vals.get(next).get;             // Consider each potential value for current variable
          if !sol.values.toSet.contains(ne);    // Make sure that value hasn't already been assigned
          if consistent(board.spaces(ne));      // Make sure it is consistent with activated constraints
          s  <- solve(sol + (next -> ne), Some(next), tail, rem)) yield s;
    }
  }
}

