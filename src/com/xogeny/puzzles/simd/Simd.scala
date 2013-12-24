package com.xogeny.puzzles.simd

import scala.reflect.io.File

/**
 * Created by mtiller on 12/14/13.
 */

object TestSimd {
  def main(args: Array[String]): Unit = {
    //simpleSolve();
    //generateProblem()
    (0 to 20) map { sculptProblem(_, false) }
  }

  def sculptProblem(seed0: Int, verbose: Boolean) = {
    var seed = seed0;
    var ret = sculptPossibleProblem(seed, 4, verbose)
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

  def generateProblem() = {
    var seed = 18;
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
    println("  Number of constraints: "+cons.length)
    println("  # Solution #")
    println(sol)
  }

  def sculptPossibleProblem(seed: Int, nvars: Int, verbose: Boolean): Option[(Board, Map[String,Int], List[SimdConstraint])] = {
    val board = Board.random(4, 4, seed, 4, List(Red, Green, Blue, Yellow))
    //val plan = DTree(List("X" -> "A", "X" -> "B", "A" -> "C", "B" -> "D"))
    //val plan = DTree(List("X" -> "A", "X" -> "B"))
    val sol = Board.randomSolution(board, nvars);
    println("Random board: "+board)
    println("Random solution: "+sol);
    val valid = SimdConstraint.allValid(board, sol)
    //val tweaker = NullTweaker
    val plan = List("X" -> "A", "X" -> "B", "B" -> "C")
    val tweaker = TreeTweaker(plan)
    val gen = Sculptor(board, sol, valid, tweaker, verbose)
    val cons = gen.solve(sol.keys.toList)
    cons map { (board, sol, _) }
  }

  def generatePossibleProblem(seed: Int): Option[(Board, Map[String,Int], List[SimdConstraint])] = {
    val board = Board.random(5, 5, seed, 5, List(Red, Green, Blue, Yellow, Purple))
    val sol = Board.randomSolution(board, 3);
    println("Random board: "+board)
    println("Random solution: "+sol);
    //val sol = Map("X" -> 6, "A" -> 0, "B" -> 9, "C" -> 2, "D" -> 14)
    val gen = ProblemGenerator(board, sol)
    //val plan = Plan(List("alpha"), List(Plan(List("beta"), Nil), Plan(List("delta"), Nil)))
    //val plan = Plan(List("alpha", "beta"), List(Plan(List("delta"), Nil)))
    val plan = Plan(List("X"), Plan(List("A"), List(Plan(List("B")))) :: Plan(List("C"), List(Plan(List("D")))) :: Nil)
    val cons = gen.solve(plan)
    cons map { (board, sol, _) }
  }

  def simpleSolve() = {
    val board = Board(List(Space(Black, 1, Set(Red), 0, 0), Space(Black, 2, Set(Green), 0, 0)), Paths(Map()))
    val pos = SimdConstraint.allValid(board, Map("alpha" -> 0, "beta" -> 1))
    println("Possible constraints: "+pos);
    val prob = Problem(board, List("alpha", "beta"))
    prob.impose(pos);
    println(prob.store);
    prob.solve();
  }
}