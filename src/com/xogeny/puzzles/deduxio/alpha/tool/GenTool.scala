package com.xogeny.puzzles.deduxio.alpha.tool

import com.xogeny.puzzles.deduxio.alpha.solver._
import com.xogeny.puzzles.deduxio.alpha.repr._
import com.xogeny.puzzles.deduxio.alpha.render.SVGRenderer
import scala.reflect.io.File
import com.xogeny.puzzles.deduxio.alpha.cons._

/**
 * Created by mtiller on 1/5/14.
 */
object GenTool {
  def main(argc: Array[String]) = {
    (0 to 10) foreach { seed =>
      generatePuzzle(seed, false);
    }
  }

  def generatePuzzle(seed: Int, verbose: Boolean) = {
    println("Seed: "+seed);
    val pgen = new ProblemGenerator(3, (4,4), 4, List(Red, Green, Blue, Yellow));
    val (prob, sol) = pgen.generate(seed)
    val plan = pgen.randomPlan(seed);
    if (verbose) println("  plan = "+plan);
    val tscorer = TreeScorer(plan);
    val builder = PuzzleBuilder(0, prob, sol, tscorer)
    if (verbose) println("  Base Constraints: ");
    if (verbose) builder.baseConstraints foreach { c => println("    "+c+": "+tscorer.score(c)) }
    val cons = builder.craft();
    if (verbose) println("  Chosen: ");
    if (verbose) cons foreach { c: Constraint => println("    "+c) }
    val rater = new PuzzleRater(prob, cons, sol);
    val steps = rater.analyze();
    println("  Steps: "+steps);
    val sum = steps.foldLeft(0) { _ + _._2 }
    println("  Score: "+(sum/21.0))
    val svg = SVGRenderer.render(prob.board, cons, sol)
    File("output/puzzle_"+seed+".html").writeAll(svg)
  }
}
