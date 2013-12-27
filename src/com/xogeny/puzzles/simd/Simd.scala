package com.xogeny.puzzles.simd

import scala.reflect.io.File

/**
 * Created by mtiller on 12/14/13.
 */

object TestSimd {
  def main(args: Array[String]): Unit = {
    val generator = Medium6x6
    (0 to 10) map { sculptProblem("5-6", _, generator) }
  }

  def sculptProblem(desc: String, seed0: Int, generator: ProblemGenerator) = {
    val (board, sol, cons) = generator.generate(seed0)
    println("  # Constraints #")
    cons foreach { c => println(c) }
    println(sol)
    val html = SVGRender.render(board, cons, sol);
    File("puzzle_"+desc+"_"+seed0+".html").writeAll(html)
  }
}