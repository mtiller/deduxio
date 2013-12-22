package com.xogeny.puzzles.simd

import JaCoP.core.{Store, IntVar}
import scala.util.Random

/**
 * Created by mtiller on 12/14/13.
 */

object TestSimd {
  def main(args: Array[String]): Unit = {
    //simpleSolve();
    //generateProblem()
    sculptProblem(10, allowPrimary=true, verbose=false);
  }

  def sculptProblem(seed0: Int, allowPrimary: Boolean, verbose: Boolean) = {
    var seed = seed0;
    var ret = sculptPossibleProblem(seed, allowPrimary, verbose)
    while(ret==None) {
      ret = sculptPossibleProblem(seed, allowPrimary, verbose);
      seed = seed + 10;
    }
    val (board, sol, cons) = ret.get
    val bjson = board.toJSON
    val cjson = "["+(cons map { _.toJSON } mkString ",")+"]"
    val sjson = "{"+(sol map { p => "\""+p._1+"\": "+p._2} mkString ",")+"}"
    println("### Solution ###");
    println("  # Board #")
    println(board)
    println("  # Constraints #")
    cons foreach { c => println(c) }
    println("  Number of constraints: "+cons.length)
    println("  # Solution #")
    println(sol)
    println("JSON:")
    println(s"""{"board": $bjson, "cons": $cjson, "sol": $sjson}""")
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

  def sculptPossibleProblem(seed: Int, allowPrimary: Boolean, verbose: Boolean): Option[(Board, Map[String,Int], List[SimdConstraint])] = {
    val board = Board.random(5, 5, seed, 5, List(Red, Green, Blue, Yellow, Purple))
    //val plan = DTree(List("X" -> "A", "X" -> "B", "A" -> "C", "B" -> "D"))
    val plan = DTree(List("X" -> "A", "X" -> "B", "B" -> "C"))
    //val plan = DTree(List("X" -> "A", "X" -> "B"))
    val sol = Board.randomSolution(board, plan.solved.length);
    println("Random board: "+board)
    println("Random solution: "+sol);
    //val conf = { l: List[SimdConstraint] => plan.conforms(l) && l.count({ case c: PrimaryConstraint => true; case _ => false })>=1 }
    val conf = { l: List[SimdConstraint] =>
      val c = l.count({_.isInstanceOf[PrimaryConstraint]})
      println(c+" primary constraints in "+l);
      plan.conforms(l) && c>=1
    }
    val gen = Sculptor(board, sol, conf, allowPrimary, verbose)
    val cons = gen.solve(plan)
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