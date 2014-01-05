package com.xogeny.puzzles.deduxio.alpha.solver

import com.xogeny.puzzles.deduxio.alpha.cons._

/**
 * Created by mtiller on 1/4/14.
 */
trait Scorer {
  def score(c: Constraint): Int;
}

case object PvS extends Scorer {
  def score(c: Constraint) = c match {
    case p: PrimaryConstraint => -5;
    case _ => 5;
  }
}

case class TreeScorer(tree: List[(String,String)]) extends Scorer {
  def degree(s: String) = {
    val others = tree flatMap { p => if (p._1==s) Some(p._2) else if (p._2==s) Some(p._2) else None }
    others.toSet.size
  }
  def score(c: Constraint) = c match {
    case pn: PrimaryNot => -10;
    case p: PrimaryConstraint if degree(p.v)==1 => 5;
    case p: PrimaryConstraint => -5;
    case s: SecondaryConstraint if tree.exists { p => p._1==s.v1 && p._2==s.v2 || p._1==s.v2 && p._2==s.v1 } => 20;
    case _ => 0;
  }
}