package com.xogeny.puzzles.deduxio.alpha.repr

/**
 * Created by mtiller on 1/3/14.
 */
case class Space(color: Color, number: Int, path: Set[Color], x: Int, y: Int) {
  val name: String = "s("+x+","+y+")";
  def toJSON = {
    val pathstr = path.toList map { p => s""" "$p" """ } mkString ","
    s"""{"color": "$color", "number": $number, "path": [$pathstr], "x": $x, "y": $y}"""
  }
}
