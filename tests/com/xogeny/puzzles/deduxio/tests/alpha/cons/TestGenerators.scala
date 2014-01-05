package com.xogeny.puzzles.deduxio.tests.alpha.cons

import org.scalatest.FunSuite
import com.xogeny.puzzles.deduxio.alpha.solver._
import com.xogeny.puzzles.deduxio.alpha.repr._
import com.xogeny.puzzles.deduxio.alpha.cons._

import com.xogeny.puzzles.deduxio.tests.alpha.Samples

/**
 * Created by mtiller on 1/4/14.
 */
class TestGenerators extends FunSuite {
  test("Run test generator failure") {
    val prob = Problem(Samples.B1, Set("A"))
    val sol1 = Map("A" -> 0);
    val sol2 = Map("A" -> 3);
    val cons1 = IsColor.valid(prob, sol1);
    val cons2 = IsColor.valid(prob, sol2);
    assert(cons1!=cons2);
    val consistent = cons1.toList forall { _.consistent(Samples.B1, sol2) }
    assert(!consistent);
  }
  test("Run through primary generators") {
    PuzzleBuilder.pgens foreach { g =>
      println("Testing "+g+" generator")
      val prob = Problem(Samples.B1, Set("A"));
      (0 to prob.board.elements.length-1) foreach { e =>
        val sol = Map("A" -> e);
        println("  "+sol);
        val cons = g.valid(prob, sol);
        cons.toList forall { c: Constraint =>
          c.consistent(Samples.B1, sol);
        }
      }
    }
  }
  test("Run through secondary generators") {
    PuzzleBuilder.sgens foreach { g =>
      println("Testing "+g+" generator");
      val prob = Problem(Samples.B1, Set("A", "B"));
      val nums = (0 to prob.board.elements.length-1);
      nums foreach { e1 =>
        nums foreach { e2 =>
          if (e1!=e2) {
            val sol = Map("A" -> e1, "B" -> e2);
            println("  "+sol);
            val cons = g.valid(prob, sol);
            cons.toList forall { c: Constraint =>
              c.consistent(Samples.B1, sol);
            }
          }
        }
      }
    }
  }
}
