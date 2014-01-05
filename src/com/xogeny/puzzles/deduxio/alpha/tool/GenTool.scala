package com.xogeny.puzzles.deduxio.alpha.tool

import com.xogeny.puzzles.deduxio.alpha.solver._
import com.xogeny.puzzles.deduxio.alpha.repr._
import com.xogeny.puzzles.deduxio.alpha.render.SVGRenderer
import scala.reflect.io.File
import com.xogeny.puzzles.deduxio.alpha.cons._

/**
 * Created by mtiller on 1/5/14.
 */

trait GeneratorConfiguration[T] {
  def generate(seed: Int, verbose: Boolean): (Problem, List[Constraint], Map[String,Int], T);
}

class StandardConfiguration(nvars: Int, size: Int) extends GeneratorConfiguration[Int] {
  def generate(seed: Int, verbose: Boolean): (Problem, List[Constraint], Map[String,Int], Int) = {
    println("Seed: "+seed);
    val pgen = new ProblemGenerator(nvars, (size,size), size, Color.all take size);
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
    println("  Score: "+sum)
    (prob, cons, sol, sum)
  }
}

case object P4x4_3 extends StandardConfiguration(3, 4);
case object P5x5_4 extends StandardConfiguration(4, 5);
case object P6x6_5 extends StandardConfiguration(5, 6);
case object P6x6_7 extends StandardConfiguration(7, 6);
case object P9x9_3 extends StandardConfiguration(3, 9);

object GenTool {
  def main(argc: Array[String]) = {
    (0 to 100) foreach { seed =>
      val config = P5x5_4;
      val (prob, cons, sol, sum) = config.generate(seed, false);
      val diff = "%04d".format(sum)
      val svg = SVGRenderer.render(prob.board, cons, sol)
      File("output/puzzle_"+diff+"_seed-"+seed+".html").writeAll(svg)
    }
  }
}
