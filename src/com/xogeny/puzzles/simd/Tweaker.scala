package com.xogeny.puzzles.simd

/**
 * Created by mtiller on 12/24/13.
 */

trait Tweaker {
  def adjustPriority(c: SimdConstraint): Int;
}

case object NullTweaker extends Tweaker {
  def adjustPriority(c: SimdConstraint): Int = 0;
}

case class TreeTweaker(plan: List[Pair[String,String]]) extends Tweaker {
  def isLeaf(s: String) = plan forall { p => p._1 != s }
  def parent(s: String) = plan filter { p => p._2==s } map { _._1 }
  def dependsOn(s1: String, s2: String): Boolean = plan exists { p => p._1==s1 && p._2==s2 }
  def adjustPriority(c: SimdConstraint): Int = {
    val la = c match {
      case p: PrimaryConstraint if (isLeaf(p.ball)) => -30
      case p: PrimaryConstraint => 20
      case _ => 0
    }
    val da = c match {
      case s: SecondaryConstraint if (dependsOn(s.b1, s.b2)) => -30
      case s: SecondaryConstraint => 30
      case _ => 0
    }
    val na = if (c.isNegative) 30 else 0;
    la + da + na
  }
}
