package com.xogeny.puzzles.deduxio.alpha.repr

/**
 * Created by mtiller on 1/3/14.
 */
case class Board(spaces: List[Space], paths: Paths) {
  def elements: List[(Int,Space)] = (0 to spaces.length-1).toList map { i => i -> spaces(i)}
  def element(s: Space): Int = spaces.indexOf(s)
  def paths(e: Int): Set[Color] = paths.map.keys.filter { k => paths.map.getOrElse(k, Set()).toList.contains(e) }.toSet
  def paths(s: Space): Set[Color] = paths(element(s))
  def adjacent(s: Space): Set[Int] = (for(i <- elements;
                                          a <- Some(i._2);
                                          if element(s)!=i._1;
                                          if (a.x==s.x && a.y==s.y+1) ||
                                             (a.x==s.x && a.y==s.y-1) ||
                                             (a.x==s.x+1 && a.y==s.y) ||
                                             (a.x==s.x-1 && a.y==s.y)) yield i._1).toSet
  def adjacent(e: Int): Set[Int] = adjacent(spaces(e));
}
