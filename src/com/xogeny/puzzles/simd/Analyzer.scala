package com.xogeny.puzzles.simd

/**
 * Created by mtiller on 12/28/13.
 */

/*
    The goal for this class is to take a problem and analyze it
    to collect some metrics.  These metrics could then be used to
    do things like estimate difficulty.
 */
class Analyzer(board: Board, sol: Map[String, Int], cons: List[SimdConstraint]) {
  private def consider[T](f: (List[SimdConstraint] => T), keep: List[SimdConstraint], left: List[SimdConstraint]): Map[List[SimdConstraint], T] = left match {
    case Nil => Map(keep -> f(keep))
    case x :: y => {
      consider(f, x :: keep, y) ++ consider(f, keep, y)
    }
  }
  private def flatMap[T](f: (List[SimdConstraint] => Option[T]), keep: List[SimdConstraint], left: List[SimdConstraint]): Map[List[SimdConstraint], T] = left match {
    case Nil => f(keep) match {
      case Some(x) => Map(keep -> x)
      case None => Map()
    }
    case x :: y => {
      flatMap(f, x :: keep, y) ++ flatMap(f, keep, y)
    }
  }
  private def filter[T](f: (List[SimdConstraint] => Boolean), keep: List[SimdConstraint], left: List[SimdConstraint]): List[List[SimdConstraint]] = left match {
    case Nil => f(keep) match {
      case true => List(keep)
      case false => List()
    }
    case x :: y => {
      filter(f, x :: keep, y) ::: filter(f, keep, y)
    }
  }
  def solvesWhich(c: List[SimdConstraint]): Option[Set[String]] = {
    val vars = sol.keys.toList;
    val prob = Problem(board, vars);
    prob.impose(c);
    val sols = prob.solveAll();
    val ssol = vars filter { b: String =>
      sols forall { s =>
        val d = s.get(b).getOrElse(Set())
        val uniq = d.size==1 && d==Set(sol.get(b).get)
        //if (uniq) println("Unique solution for "+b+" ("+d+") using "+c);
        uniq
      }
    }
    if (ssol.length==0) None else Some(ssol.toSet)
  }
  def solvesAtLeastOne(c: List[SimdConstraint]): Boolean = {
    println("Considering set: "+c)
    val vars = sol.keys.toList;
    val prob = Problem(board, vars);
    prob.impose(c);
    val sols = prob.solveAll();
    vars exists { b =>
      sols forall { s =>
        val d = s.get(b).getOrElse(Set())
        val uniq = d.size==1 && d==Set(sol.get(b).get)
        //if (uniq) println("Unique solution for "+b+" ("+d+") using "+c);
        uniq
      }
    }
  }
  def path(c: List[SimdConstraint], given: List[SimdConstraint], left: Set[String], sol: Map[String,Int]): Map[String,Int] = left.size match {
    case 0 => Map()
    case 1 => Map(left.toList.head -> c.length)
    case _ => {
      println("What can be uniquely solved for given "+given+" plus "+c);
      println("  Among: "+left);
      val solveForOne = flatMap(solvesWhich(_), Nil, cons) filter { p => (p._2 & left).size>0 }
      if (solveForOne.size==0) {
        println("Couldn't solve for anything uniquely...this shouldn't happen")
        throw new RuntimeException("There must be something more to solve for here?!?")
      }
      val smallest = solveForOne.keys.toList sortBy { cl =>
        val minusGiven = cl filter { !given.contains(_) }
        minusGiven.length
      }
      val mincons = smallest.head
      val solvesFor = (solveForOne.get(mincons).get & left).toList
      val neededcons = mincons filter { !given.contains(_) }
      val solvedVar = solvesFor.head
      val newgiven = neededcons ::: given
      val next = path(c filter { c => !newgiven.contains(c) }, newgiven, left - solvedVar, sol)
      println("Answer: "+solvedVar+" using "+neededcons);
      next + (solvedVar -> neededcons.length)
    }
  }
  def analyze() = {
    println("Analyzing");
    println("# of constraints: "+cons.length);
    val uniq = filter(solvesAtLeastOne(_), Nil, cons);
    val sizes = uniq map { _.length }
    val ncon = cons.length
    val minc = sizes min
    val nprimary = cons count { _.isInstanceOf[PrimaryConstraint] }
    val p = path(cons, Nil, sol.keys.toSet, sol)
    val maxc = p.values.toList max;
    //println("Sets of constraints that uniquely solve at least one variable:")
    //uniq foreach { p => println(p) }
    println("Number of constraints: "+ncon)
    println("Number of primary constraints: "+nprimary)
    println("Min constraints for single var: "+minc);
    println("Max constraints needed at a single stage: "+maxc)
    println("Path = "+p)
    (nprimary, minc, maxc)
  }
}
