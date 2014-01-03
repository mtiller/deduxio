package com.xogeny.puzzles.deduxio.alpha.repr

/**
 * Created by mtiller on 1/3/14.
 */
case class Board(spaces: List[Space], paths: Paths) {
  def elements: List[(Int,Space)] = (0 to spaces.length-1).toList map { i => i -> spaces(i)}
  def element(s: Space): Int = spaces.indexOf(s)
  def paths(s: Space): Set[Color] = paths.map.filter { p => p._2.contains(element(s)) }.keys.toSet
  def adjacent(e: Int): List[Int] = for(i <- elements;
                                        s <- Some(spaces(e));
                                        a <- Some(i._2);
                                        if e!=i._1;
                                        if a.x==s.x || a.x==s.x+1 || a.x==s.x-1;
                                        if a.y==s.y || a.y==s.y+1 || a.y==s.y-1) yield i._1;
}
