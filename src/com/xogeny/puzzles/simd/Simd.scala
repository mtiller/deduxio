package com.xogeny.puzzles.simd

import scala.reflect.io.File

/**
 * Created by mtiller on 12/14/13.
 */

object TestSimd {
  def main(args: Array[String]): Unit = {
    val generator = Simple5x5
    (0 to 10) map { sculptProblem("5-6", _, generator) }
  }

  def difficulty(maxc: Int) = maxc match {
    case 0 => "trivial0"
    case 1 => "trivial1"
    case 2 => "trivial"
    case 3 => "very-easy"
    case 4 => "easy"
    case 5 => "medium"
    case 6 => "hard"
    case _ => "very-hard"
  }

  def sculptProblem(desc: String, seed0: Int, generator: ProblemGenerator) = {
    val (board, sol, cons) = generator.generate(seed0)
    val a = new Analyzer(board, sol, cons)
    val (np, minc, maxc) = a.analyze()
    println("  # Constraints #")
    cons foreach { c => println(c) }
    println(sol)
    val html = SVGRender.render(board, cons, sol);
    File("puzzle_"+difficulty(maxc)+"_"+seed0+".html").writeAll(html)
  }
}