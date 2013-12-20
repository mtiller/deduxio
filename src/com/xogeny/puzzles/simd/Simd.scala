package com.xogeny.puzzles.simd

import JaCoP.core.{Store, IntVar}
import scala.util.Random

/**
 * Created by mtiller on 12/14/13.
 */

object TestSimd {
  def main(args: Array[String]): Unit = {
    //simpleSolve();
    generateProblem()
  }

  def generateProblem() = {
    var seed = 10;
    var ret = generatePossibleProblem(seed)
    while(ret==None) {
      ret = generatePossibleProblem(seed);
      seed = seed + 10;
    }
    val (board, sol, cons) = ret.get
    println("### Solution ###");
    println("  # Board #")
    println(board.toJSON)
    println("  # Constraints #")
    cons foreach { c => println(c) }
    println("  # Solution #")
    println(sol)
  }
  def generatePossibleProblem(seed: Int): Option[(Board, Map[String,Int], List[SimdConstraint])] = {
    val board = Board.random(5, 5, seed, 5, List(Red, Green, Blue, Yellow, Purple))
    val sol = Board.randomSolution(board, 5);
    println("Random board: "+board)
    println("Random solution: "+sol);
    //val sol = Map("X" -> 6, "A" -> 0, "B" -> 9, "C" -> 2, "D" -> 14)
    //val gen = ProblemGenerator(board, sol)
    val gen = Sculptor(board, sol, true)
    //val plan = Plan(List("alpha"), List(Plan(List("beta"), Nil), Plan(List("delta"), Nil)))
    //val plan = Plan(List("alpha", "beta"), List(Plan(List("delta"), Nil)))
    //val plan = Plan(List("X"), Plan(List("A"), List(Plan(List("B")))) :: Plan(List("C"), List(Plan(List("D")))) :: Nil)
    val plan = DTree(List(("X" -> "A"), ("X" -> "B"), ("B" -> "C"), ("B" -> "D")))
    val cons = gen.solve(plan)
    cons map { (board, sol, _) }
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