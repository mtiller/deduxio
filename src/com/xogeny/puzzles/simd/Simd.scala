package com.xogeny.puzzles.simd

import JaCoP.core.{Store, IntVar}
import scala.util.Random

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
  def trim[T <: SimdConstraint](c: List[T], keep: List[T]): List[T] = c match {
    case Nil => keep
    case x :: y => {
      val prob = Problem(board, sol.keys.toList)
      prob.impose(y)
      prob.impose(keep)
      val sols = prob.solveAll()
      // TODO: Ideally, this should just check to make sure the primary variable has only
      // one possible value across all solutions.
      if (sols.length==1) {
        println("We can get rid of "+x);
        trim(y, keep) // x wasn't needed
      }
      else {
        println("We have to keep "+x+" because otherwise we get "+sols.length+" solutions")
        trim(y, x :: keep) // x was needed
      }
    }
  }
  def solve(plan: Plan): List[SimdConstraint] = plan match {
    case Plan(x, Nil) => {
      val valid = SimdConstraint.allValidPrimary(board, sol) filter { c => c.ball==x }
      trim(Random.shuffle(valid), Nil)
    }
    case Plan(x, y) => {
      val solved = y flatMap { _.solved }
      val cons = y flatMap { solve(_) }
      val valid = SimdConstraint.allValidSecondary(board, sol) filter { c => c.b1==x || c.b2==x }
      trim(Random.shuffle(valid ::: cons), Nil)
    }
  }
}


object TestSimd {
  def main(args: Array[String]): Unit = {
    //simpleSolve();
    testGenerator();
  }

  def testGenerator() = {
    val board = Board.random(4, 4, 0, 4, List(Red, Green, Blue, Yellow))
    println("Random board: "+board)
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