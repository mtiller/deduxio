package com.xogeny.puzzles.simd

import scala.util.Random

/**
 * Created by mtiller on 12/16/13.
 */

trait ProblemGenerator {
  type Problem = (Board, Map[String, Int], List[SimdConstraint])
  def generate(seed: Int): Problem;
}

class TreeTweakedGenerator(sz: Int, nvars: Int, verbose: Boolean) extends ProblemGenerator {
  def generate(seed: Int) = {
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
    (board, sol, cons)
  }
}

class PlainGenerator(sz: Int, nvars: Int, verbose: Boolean) extends ProblemGenerator {
  def generate(seed: Int) = {
    val colors = List(Red, Green, Blue, Yellow, Purple, Cyan)
    val board = Board.random(sz, sz, seed, sz, colors take sz)
    val sol = Board.randomSolution(board, nvars);
    println("Random board: "+board)
    println("Random solution: "+sol);
    val valid = SimdConstraint.allValid(board, sol)
    val tweaker = NullTweaker
    val gen = Sculptor(board, sol, valid, tweaker, verbose)
    val cons = gen.solve(sol.keys.toList)
    (board, sol, cons)
  }
}

class CircularGenerator(sz: Int, nvars: Int, verbose: Boolean) extends ProblemGenerator {
  def generate(seed: Int) = {
    val colors = List(Red, Green, Blue, Yellow, Purple, Cyan)
    val board = Board.random(sz, sz, seed, sz, colors take sz)
    val sol = Board.randomSolution(board, nvars);
    println("Random board: "+board)
    println("Random solution: "+sol);
    val valid = SimdConstraint.allValid(board, sol)
    val tweaker = CircularTweaker
    val gen = Sculptor(board, sol, valid, tweaker, verbose)
    val cons = gen.solve(sol.keys.toList)
    (board, sol, cons)
  }
}

case object Simple4x4 extends TreeTweakedGenerator(4, 3, false);
case object Simple5x5 extends TreeTweakedGenerator(5, 4, false);
case object Medium5x5 extends TreeTweakedGenerator(5, 5, false);
case object Simple6x6 extends TreeTweakedGenerator(6, 2, false);
case object Medium6x6 extends CircularGenerator(6, 3, false);
case object Hard6x6 extends TreeTweakedGenerator(6, 3, false);
