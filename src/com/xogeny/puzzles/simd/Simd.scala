package com.xogeny.puzzles.simd

import scala.reflect.io.File

/**
 * Created by mtiller on 12/14/13.
 */

object TestSimd {
  def main(args: Array[String]): Unit = {
    //simpleSolve();
    //generateProblem()
    (0 to 2) map { sculptProblem(_, 4, 4, false) }
  }

  def sculptProblem(seed0: Int, sz: Int, nvars: Int, verbose: Boolean) = {
    var seed = seed0;
    var ret = sculptPossibleProblem(seed, sz, nvars, verbose)
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

  def sculptPossibleProblem(seed: Int, sz: Int, nvars: Int, verbose: Boolean): Option[(Board, Map[String,Int], List[SimdConstraint])] = {
    val colors = List(Red, Green, Blue, Yellow, Purple, Cyan)
    val board = Board.random(sz, sz, seed, sz, colors take sz)
    val sol = Board.randomSolution(board, nvars);
    println("Random board: "+board)
    println("Random solution: "+sol);
    val valid = SimdConstraint.allValid(board, sol)
    val plan = TreeTweaker.randomPlan(sol.keys.toList)
    println("Plan = "+plan)
    val tweaker = TreeTweaker(plan)
    val gen = Sculptor(board, sol, valid, tweaker, verbose)
    val cons = gen.solve(sol.keys.toList)
    cons map { (board, sol, _) }
  }
}