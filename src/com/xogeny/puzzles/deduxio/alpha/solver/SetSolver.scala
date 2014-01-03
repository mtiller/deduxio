package com.xogeny.puzzles.deduxio.alpha.solver

import com.xogeny.puzzles.deduxio.alpha.cons._
import com.xogeny.puzzles.deduxio.alpha.repr._

/**
 * Created by mtiller on 1/3/14.
 */

object SetSolver {
  def forProblem(prob: Problem) = {
    SetSolver(prob.board, Map() ++ (prob.vars map { v => v -> (0 to prob.board.spaces.length-1).toSet}), Nil)
  }
}

case class SetSolver(board: Board, vals: Map[String,Set[Int]], cons: List[SecondaryConstraint]) {
  val involving: Map[String, List[SecondaryConstraint]] = {
    val im = Map[String, List[SecondaryConstraint]]() ++ (vals.keys.toList map { v => v -> Nil})
    cons.foldLeft(im) { (m, c) =>
      val l1 = c :: m.get(c.v1).get
      val l2 = c :: m.get(c.v2).get
      m ++ List(c.v1 -> l1, c.v2 -> l2)
    }
  }
  def impose(c: Constraint) = c match {
    case p: PrimaryConstraint => {
      val orig = vals.get(p.v).get
      val consistent = orig filter { e => p.satisfies(board, board.spaces(e)) }
      SetSolver(board, vals + (p.v -> consistent), cons)
    }
    case s: SecondaryConstraint => {
      SetSolver(board, vals, s :: cons)
    }
  }
}
