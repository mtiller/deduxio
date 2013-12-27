package com.xogeny.puzzles.simd

import scala.util.Random

/**
 * Created by mtiller on 12/19/13.
 */

case class Sculptor(board: Board, sol: Map[String,Int],
                    valid: List[SimdConstraint],
                    tweaker: Tweaker,
                    verbose: Boolean=false) {
  def solve(vars: List[String]): List[SimdConstraint] = {
    val start = Random.shuffle(valid) sortBy { c => -(c.priority+tweaker.adjustPriority(c)) }
    if (verbose) {
      start foreach { c => println(c.priority+"+"+tweaker.adjustPriority(c)+":"+c)}
    }
    println("Initial (sorted) constraints: "+start)
    if (!isValid(vars, valid)) throw new RuntimeException("Invalid problem, couldn't find a unique solution (this should not happen");
    trim(start, vars, Nil);
  }

  def isValid(vars: List[String], cons: List[SimdConstraint]) = {
    val prob = Problem(board, vars)
    prob.impose(cons)
    val sols = prob.solveAll();
    sols.length==1;
  }

  def trim(cons: List[SimdConstraint], vars: List[String], keep: List[SimdConstraint]): List[SimdConstraint] = cons match {
    case Nil => {
      println("Processed all constraints, keeping: "+keep)
      keep
    }
    case (c :: y) => {
      if (isValid(vars, keep ::: y)) {
        if (verbose) println("We can get rid "+c)
        trim(y, vars, keep)
      }
      else {
        if (verbose) println("We have to keep "+c)
        trim(y, vars, c :: keep)
      }
    }
  }
}

