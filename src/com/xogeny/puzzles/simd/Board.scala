package com.xogeny.puzzles.simd

import scala.util.Random

/**
 * Created by mtiller on 12/21/13.
 */
case class Space(color: Color, number: Int, path: Set[Color], x: Int, y: Int) {
  val name: String = "s("+x+","+y+")";
  def toJSON = {
    val pathstr = path.toList map { p => s""" "$p" """ } mkString ","
    s"""{"color": "$color", "number": $number, "path": [$pathstr], "x": $x, "y": $y}"""
  }
}

case object Board {
  def adjacent(board: Board, e: Int): List[Int] = {
    val s = board.spaces(e);
    val adj = for(i <- (0 to board.spaces.length-1).toList;
                  a <- Some(board.spaces(i));
                  if e!=i;
                  if a.x==s.x || a.x==s.x+1 || a.x==s.x-1;
                  if a.y==s.y || a.y==s.y+1 || a.y==s.y-1) yield if (a.x==s.x || a.y==s.y) List(i, i, i) else List(i)
    adj.flatten
  }

  def randomSolution(board: Board, n: Int): Map[String,Int] = {
    require(n>0);
    val letters = List("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "L");
    val names = "X" :: (letters take n-1)
    val values = Random.shuffle((0 to board.spaces.length-1).toList)
    Map() ++ (names zip values)
  }

  def randomSpaces() = {

  }

  /**
   *
   * @param w Width of board
   * @param h Height of board
   * @param seed Random seed
   * @param n Ball numbers (1-n)
   * @param colors Ball colors
   * @return A random board
   */
  def random(w: Int, h: Int, seed: Long, n: Int, colors: List[Color]) = {
    val nums = 0 to w*h-1;
    Random.setSeed(seed);
    val balls = (1 to n).toList flatMap { n => colors map { c => Space(c, n, Set(), 0, 0 )}}
    val rballs = Random.shuffle(balls);
    val taken = rballs.take (w*h);
    val spaces = nums map { i =>
      val orig = taken(i)
      Space(orig.color, orig.number, orig.path, i % w, i/w)
    }
    val iboard = Board(spaces.toList, Paths(Map()))
    val start = Random.shuffle(nums.toList)
    // TODO: Don't start in a corner
    val cur = for(i <- (0 to colors.length-1).toList) yield (colors(i), List(start(i)));
    //println("Cur = "+cur);
    val paths = Paths.random(iboard, cur, Set(), nums.toSet--(start take colors.length).toSet)
    iboard.overlay(paths)
  }
}

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

