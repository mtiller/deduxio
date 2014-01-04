package com.xogeny.puzzles.deduxio.alpha.repr

import scala.util.Random

/**
 * Created by mtiller on 1/3/14.
 */

case class Board(spaces: List[Space], paths: Paths) {
  val elements = (0 to spaces.length-1).toList map { i => i -> spaces(i)}
  val numbers = spaces.foldLeft(Set[Int]()) { (nums, s) => nums + s.number }
  val colors = spaces.foldLeft(Set[Color]()) { (cols, s) => cols + s.color }
  val rows = spaces.foldLeft(Set[Int]()) { (rws, s) => rws + s.y }
  val columns = spaces.foldLeft(Set[Int]()) { (cls, s) => cls + s.x }
  def element(s: Space): Int = spaces.indexOf(s)
  def paths(e: Int): Set[Color] = paths.map.keys.filter { k => paths.map.getOrElse(k, Set()).toList.contains(e) }.toSet
  def paths(s: Space): Set[Color] = paths(element(s))
  def adjacent(s: Space): Set[Int] = (for(i <- elements;
                                          if element(s)!=i._1;
                                          if i._2.adjacent(s)) yield i._1).toSet
  def adjacent(e: Int): Set[Int] = adjacent(spaces(e));
}
