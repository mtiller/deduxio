package com.xogeny.puzzles.simd

import scala.util.Random

/**
 * Created by mtiller on 12/19/13.
 */
case class DTree(cur: List[Pair[String,String]]) {
  def solved: List[String] = (cur flatMap { p => List(p._1, p._2)}).distinct;
  def conforms(cons: List[SimdConstraint]): Boolean = {
    cur forall { p =>
      cons exists {
        case s: SecondaryConstraint => {
          //println(s"looking for ${s.b1} and ${s.b2} in ${p._1} and ${p._2}")
          val ret = (s.b1==p._1 && s.b2==p._2) || (s.b1==p._2 && s.b2==p._1)           //println(ret);
          ret
        }
        case _ => false
      }
    }
  }
}

case class Sculptor(board: Board, sol: Map[String,Int], conforms: (List[SimdConstraint] => Boolean), allowPrimary: Boolean = false, verbose: Boolean=false) {
  def solve(plan: DTree): Option[List[SimdConstraint]] = {
    val vars = plan.solved;
    val valid = if (allowPrimary) SimdConstraint.allValid(board, sol) else SimdConstraint.allValidSecondary(board, sol)
    val start = Random.shuffle(valid) sortBy { -_.priority }
    println("Initial constraints: "+start)
    if (isValid(vars, valid) && conforms(valid)) trim(start, vars, Nil);
    else None
  }

  def isValid(vars: List[String], cons: List[SimdConstraint]) = {
    val prob = Problem(board, vars)
    prob.impose(cons)
    val sols = prob.solveAll();
    sols.length==1;
  }

  def trim(cons: List[SimdConstraint], vars: List[String], keep: List[SimdConstraint]): Option[List[SimdConstraint]] = cons match {
    case Nil => {
      println("Processed all constraints, keeping: "+keep)
      Some(keep)
    }
    case (c :: y) => {
      if (conforms(y ::: keep)) {
        if (verbose) println("Conformant if we get rid of "+c);
        if (isValid(vars, keep ::: y)) {
          if (verbose) println("We can get rid "+c)
          trim(y, vars, keep)
        }
        else {
          if (verbose) println("We have to keep "+c)
          trim(y, vars, c :: keep)
        }
      }
      else {
        if (verbose) println("Non-conformant if we get rid of "+c+" [tail="+y+"]")
        trim(y, vars, c :: keep)
      }
    }
  }
}

