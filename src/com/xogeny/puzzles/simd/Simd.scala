package com.xogeny.puzzles.simd

import scala.reflect.io.File

/**
 * Created by mtiller on 12/14/13.
 */

object TestSimd {
  def main(args: Array[String]): Unit = {
    //simpleSolve();
    //generateProblem()
    (0 to 2) map { sculptProblem(_, false) }
  }

  def sculptProblem(seed0: Int, verbose: Boolean) = {
    var seed = seed0;
    var ret = sculptPossibleProblem(seed, 6, verbose)
    if (ret==None) {
      println("Invalid problem")
    } else {
      val (board, sol, cons) = ret.get
      println("  # Constraints #")
      cons foreach { c => println(c) }
      println(sol)
      val html = SVGRender.render(board, cons, sol);
      File("gen_puzzle_"+seed0+".html").writeAll(html)
    }
  }

  def sculptPossibleProblem(seed: Int, nvars: Int, verbose: Boolean): Option[(Board, Map[String,Int], List[SimdConstraint])] = {
    val board = Board.random(5, 5, seed, 5, List(Red, Green, Blue, Yellow, Purple))
    //val plan = DTree(List("X" -> "A", "X" -> "B", "A" -> "C", "B" -> "D"))
    //val plan = DTree(List("X" -> "A", "X" -> "B"))
    val sol = Board.randomSolution(board, nvars);
    println("Random board: "+board)
    println("Random solution: "+sol);
    val valid = SimdConstraint.allValid(board, sol)
    //val tweaker = NullTweaker
    val plan = List("A" -> "B", "A" -> "C", "C" -> "D", "C" -> "X", "B" -> "E")
    //val plan = List("X" -> "A", "X" -> "B", "B" -> "C")
    val tweaker = TreeTweaker(plan)
    val gen = Sculptor(board, sol, valid, tweaker, verbose)
    val cons = gen.solve(sol.keys.toList)
    cons map { (board, sol, _) }
  }
}