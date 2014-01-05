package com.xogeny.puzzles.deduxio.alpha.tool

import com.xogeny.puzzles.deduxio.alpha.solver._
import com.xogeny.puzzles.deduxio.alpha.repr._
import com.xogeny.puzzles.deduxio.alpha.render.SVGRenderer
import scala.reflect.io.File

/**
 * Created by mtiller on 1/5/14.
 */
object GenTool {
  def main(argc: Array[String]) = {
    (0 to 10) foreach { seed =>
      generatePuzzle(seed);
      println("Generated puzzle for seed "+seed)
    }
  }

  def generatePuzzle(seed: Int) = {
    val pgen = new ProblemGenerator(3, (4,4), 4, List(Red, Green, Blue, Yellow));
    val (prob, sol) = pgen.generate(seed)
    val builder = PuzzleBuilder(0, prob, sol, PvS)
    val cons = builder.craft();
    val svg = SVGRenderer.render(prob.board, cons, sol)
    File("output/puzzle_"+seed+".html").writeAll(svg)
  }
}
