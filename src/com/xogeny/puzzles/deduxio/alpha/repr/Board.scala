package com.xogeny.puzzles.deduxio.alpha.repr

import scala.util.Random

/**
 * Created by mtiller on 1/3/14.
 */


/**
 * This class represents a board full of squares
 * @param spaces These are the spaces on the board
 * @param paths These are the (colored) paths on the board
 */
case class Board(spaces: List[Space], paths: Paths) {
  /* This zips together the indices and spaces */
  val elements: List[(Int,Space)] = (0 to spaces.length-1).toList map { i => i -> spaces(i)}
  /* This is the set of numbers that appear on the spaces */
  val numbers = spaces.foldLeft(Set[Int]()) { (nums, s) => nums + s.number }
  /* This is the set of colors that appear on the spaces */
  val colors = spaces.foldLeft(Set[Color]()) { (cols, s) => cols + s.color }
  /* This is the set of row numbers that the spaces occupy */
  val rows = spaces.foldLeft(Set[Int]()) { (rws, s) => rws + s.y }
  /* This is the set of column numbers that the spaces occupy */
  val columns = spaces.foldLeft(Set[Int]()) { (cls, s) => cls + s.x }
  /* This returns the space number of a given space */
  def element(s: Space): Int = spaces.indexOf(s)
  /* This returns all paths that a given space number sits on */
  def paths(e: Int): Set[Color] = paths.map.keys.filter { k => paths.map.getOrElse(k, Set()).toList.contains(e) }.toSet
  /* This returns all paths that a given space sits on */
  def paths(s: Space): Set[Color] = paths(element(s))
  /* This method returns the set of all space numbers that are adjacent to a given space */
  def adjacent(s: Space): Set[Int] = (for(i <- elements;
                                          if element(s)!=i._1;
                                          if i._2.adjacent(s)) yield i._1).toSet
  /* This method returns the set of all space numbers that are adjaced to a given space number */
  def adjacent(e: Int): Set[Int] = adjacent(spaces(e));
}
