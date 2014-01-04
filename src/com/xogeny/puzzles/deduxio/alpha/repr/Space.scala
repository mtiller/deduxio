package com.xogeny.puzzles.deduxio.alpha.repr

/**
 * Created by mtiller on 1/3/14.
 */
case class Space(color: Color, number: Int, x: Int, y: Int) {
  val name: String = "s("+x+","+y+")";
  def adjacent(s: Space) = (x==s.x && y==s.y+1) || (x==s.x && y==s.y-1) ||
                           (x==s.x+1 && y==s.y) || (x==s.x-1 && y==s.y)
/*  def toJSON = {
    val pathstr = path.toList map { p => s""" "$p" """ } mkString ","
    s"""{"color": "$color", "number": $number, "path": [$pathstr], "x": $x, "y": $y}"""
  }
*/
}
