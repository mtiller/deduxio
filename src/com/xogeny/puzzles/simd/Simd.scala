package com.xogeny.puzzles.simd

import JaCoP.core.{Store, IntVar}
import scala.util.Random

/**
 * Created by mtiller on 12/14/13.
 */



object TestSimd {
  def main(args: Array[String]): Unit = {
    //simpleSolve();
    testGenerator();
  }

  def testGenerator() = {
    val board = Board.random(4, 4, 0, 4, List(Red, Green, Blue, Yellow))
    println("Random board: "+board)
    val sol = Map("alpha" -> 2, "beta" -> 9, "delta" -> 5, "gamma" -> 11)
    val gen = ProblemGenerator(board, sol)
    //val plan = Plan(List("alpha"), List(Plan(List("beta"), Nil), Plan(List("delta"), Nil)))
    val plan = Plan(List("alpha", "beta"), List(Plan(List("delta"), Nil), Plan(List("gamma"), Nil)))
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