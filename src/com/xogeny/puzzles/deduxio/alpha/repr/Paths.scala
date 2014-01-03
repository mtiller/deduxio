package com.xogeny.puzzles.deduxio.alpha.repr

/**
 * Created by mtiller on 1/3/14.
 */
case class Paths(map: Map[Color,List[Int]]) {
/*  def toJSON = {
    "["+(map map { p => s"""{"color": "${p._1}", "elems": [${p._2 mkString ","}]}""" })+"]"
  }
  */
}
