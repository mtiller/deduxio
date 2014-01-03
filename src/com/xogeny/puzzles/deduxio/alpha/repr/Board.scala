package com.xogeny.puzzles.deduxio.alpha.repr

/**
 * Created by mtiller on 1/3/14.
 */
case class Board(spaces: List[Space], paths: Paths) {
  def adjacent(e: Int): List[Int] = {
    val s = spaces(e);
    val adj = for(i <- (0 to spaces.length-1).toList;
                  a <- Some(spaces(i));
                  if e!=i;
                  if a.x==s.x || a.x==s.x+1 || a.x==s.x-1;
                  if a.y==s.y || a.y==s.y+1 || a.y==s.y-1) yield if (a.x==s.x || a.y==s.y) List(i, i, i) else List(i)
    adj.flatten
  }
  def overlay(p: Paths) = {
    val nums = 0 to spaces.length-1
    val newspaces = nums.toList map { i =>
      val s = spaces(i);
      Space(s.color, s.number, p.map.keys.toSet filter { x => p.map.get(x).get.contains(i) }, s.x, s.y)
    }
    Board(newspaces, p)
  }
  def toJSON = {
    val sjson = "["+(spaces map { _.toJSON } mkString ",")+"]"
    val pjson = paths.toJSON
    s"""{"spaces": $sjson, "paths": $pjson}"""
  }
}
