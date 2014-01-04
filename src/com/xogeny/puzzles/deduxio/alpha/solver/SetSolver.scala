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
  def impose(c: Constraint) = c match {
    case p: PrimaryConstraint => {
      val orig = vals.get(p.v).get
      val consistent = orig filter { e => p.satisfies(board, board.spaces(e)) }
      SetSolver(prob, vals + (p.v -> consistent), cons)
    }
    case s: SecondaryConstraint => {
      SetSolver(prob, vals, s :: cons)
    }
  }
  def solve(): List[Map[String,Int]] = solve(Map[String,Int](), None, prob.vars.toList, cons);
  private def solve(sol: Map[String,Int], last: Option[String],
                    left: List[String], scons: List[SecondaryConstraint]): List[Map[String,Int]] = left match {
    case Nil => sol :: Nil
    case next :: tail => {
      val sols = last match {
        case None => for(ne <- vals.get(next).get.toList;
                         ns <- Some(board.spaces(ne));
                         c <- scons) yield solve(sol + (next -> ne), Some(next), tail, scons);
        case Some(x) => {
          val (active, rem) = scons.partition { _.involves(x, next) }
          val ls = board.spaces(sol.get(x).get);
          for(ne <- vals.get(next).get.toList;
              ns <- Some(board.spaces(ne));
              c <- active;
              if c.satisfies(board, ls, ns)) yield solve(sol + (next -> ne), Some(next), tail, rem)
        }
      }
      sols.flatten
    }
  }
}
