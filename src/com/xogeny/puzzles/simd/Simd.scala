package com.xogeny.puzzles.simd

import JaCoP.core.{Store, IntVar}

/**
 * Created by mtiller on 12/14/13.
 */

/**
 * Used to build statement of Simd problems
 * @param r
 * @param c
 */
class Simd(r: Int, c: Int) {

}

case class Plan(cur: String, children: List[Plan]) {
  def solved: List[String] = this match {
    case Plan(x, Nil) => List(x)
    case Plan(x, y) => x :: (y flatMap { _.solved })
  }
}

case class ProblemGenerator(board: Board, sol: Map[String,Int]) {
  def solve(plan: Plan): List[SimdConstraint] = plan match {
    case Plan(x, Nil) => {
      val valid = SimdConstraint.allValidPrimary(board, sol) filter { c => c.ball==x }
      // TODO: Trim
      valid
    }
    case Plan(x, y) => {
      val solved = y flatMap { _.solved }
      val cons = y flatMap { solve(_) }
      val valid = SimdConstraint.allValidSecondary(board, sol) filter { c => c.b1==x || c.b2==x }
      // TODO: Trim
      valid
    }
  }
}

object TestSimd {
  def main(args: Array[String]): Unit = {
    //simpleSolve();
    testGenerator();
  }

  def testGenerator() = {
    val board = Board(List(Space(Black, 1, Set(Red), 0, 0), Space(Black, 2, Set(Green), 0, 0)))
    val sol = Map("alpha" -> 0, "beta" -> 1)
    val gen = ProblemGenerator(board, sol)
    val plan = Plan("alpha", List(Plan("beta", Nil)))
    val cons = gen.solve(plan)
    println(cons);
  }

  def simpleSolve() = {
    val board = Board(List(Space(Black, 1, Set(Red), 0, 0), Space(Black, 2, Set(Green), 0, 0)))
    val pos = SimdConstraint.allValid(board, Map("alpha" -> 0, "beta" -> 1))
    println("Possible constraints: "+pos);
    val prob = Problem(board, List("alpha", "beta"))
    prob.impose(pos);
    println(prob.store);
    prob.solve();
  }
}