package com.xogeny.puzzles.deduxio.tests.alpha.solver

import org.scalatest.FunSuite
import com.xogeny.puzzles.deduxio.alpha.repr._
import com.xogeny.puzzles.deduxio.alpha.cons._
import com.xogeny.puzzles.deduxio.alpha.solver._

/**
 * Created by mtiller on 1/4/14.
 */
class TestPuzzleBuilder extends FunSuite {
  test("Puzzle builder algorithm for 3x3 problem") {
    val (prob, sol) = ProblemGenerator.generate(0, (3, 3), 3, List(Red, Green, Blue))
    val builder = PuzzleBuilder(0, prob, sol, PvS)
    val cons = builder.craft();
    assert(cons.length>0)
  }
  test("Puzzle builder algorithm for 4x4 problem") {
    val (prob, sol) = ProblemGenerator.generate(0, (4, 4), 4, List(Red, Green, Blue, Yellow))
    val builder = PuzzleBuilder(0, prob, sol, PvS)
    val cons = builder.craft();
    assert(cons.length>0)
  }
  test("Puzzle builder algorithm for 5x5 problem") {
    val (prob, sol) = ProblemGenerator.generate(0, (5, 5), 5, List(Red, Green, Blue, Yellow, Purple))
    val builder = PuzzleBuilder(0, prob, sol, PvS)
    val cons = builder.craft();
    assert(cons.length>0)
  }
  test("Puzzle builder algorithm for 6x6 problem") {
    val (prob, sol) = ProblemGenerator.generate(0, (6, 6), 6, List(Red, Green, Blue, Yellow, Purple, Cyan))
    val builder = PuzzleBuilder(0, prob, sol, PvS)
    val cons = builder.craft();
    assert(cons.length>0)
  }

  test("Test uniqueness") {
    (0 to 100) foreach { seed =>
      println("Using seed "+seed);
      val (prob, sol) = ProblemGenerator.generate(seed, (3, 3), 3, List(Red, Green, Blue, Yellow))
      println("Board: "+prob.board)
      val builder = PuzzleBuilder(seed, prob, sol, PvS)
      val cons = builder.craft();
      println("Selected constraints: "+cons);
      builder.baseConstraints foreach { c: Constraint => assert(c.consistent(prob.board, sol) )}
      println("Base constraints are consistent");
      cons foreach { c: Constraint => assert(builder.baseConstraints.contains(c)) }
      println("Selected constraints are among base constraints")
      val incon = cons filter { !_.consistent(prob.board, sol); }
      println("Solution: "+sol);
      println("Inconsistent constraints: "+incon);
      cons foreach { c: Constraint => assert(c.consistent(prob.board, sol)) }
      println("Chosen constraints are consistent with solution")
      val bcsolver = builder.baseConstraints.foldLeft(SetSolver.forProblem(prob)) { (s, c) => s.impose(c) }
      sol.keys foreach { k => assert(bcsolver.vals.get(k).get.size==1) }
      sol.keys foreach { k => assert(bcsolver.vals.get(k).get==Set(sol.get(k).get))}
      builder.baseConstraints.foldLeft(SetSolver.forProblem(prob)) { (s, c) =>
        val ns = s.solve().size;
        println("Current # of solution: "+ns);
        assert(ns>0)
        println("imposing "+c);
        s.impose(c);
      }
      println("Using base constraints, bcsolver.vals is correct");
      assert(bcsolver.solve().size==1);
      println("Using base constraints, we get exactly one solution");
      val solver: SetSolver = cons.foldLeft(SetSolver.forProblem(prob)) { (s, c) => s.impose(c) }
      println("solver.vals = "+solver.vals);
      val ssol = solver.solve();
      println("Generated puzzle had "+ssol.size+" solutions");
      assert(cons.length>0)
      assert(ssol.size==1);
      assert(ssol==Set(sol));
    }
  }
}
