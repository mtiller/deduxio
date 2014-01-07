package com.xogeny.puzzles.deduxio.alpha.repr

/**
 * Created by mtiller on 1/3/14.
 */

object Color {
  val all = List(Red, Green, Blue, Yellow, Purple, Cyan, CA, CB, CC);
}

/**
 * This class is used to represent all colors in the puzzle.
 * @param c A color number (probably not used anywhere)
 * @param rgb The RGB string for the color.
 */
abstract sealed class Color(c: Int, val rgb: String);
case object Red extends Color(1, "#ff6060");
case object Green extends Color(2, "#60ff60");
case object Blue extends Color(3, "#8080ff");
case object Yellow extends Color(4, "#ffff80");
case object Purple extends Color(5, "#ff80ff");
case object Cyan extends Color(6, "#80ffff");
case object CA extends Color(7, "#004080");
case object CB extends Color(8, "#408000");
case object CC extends Color(9, "#800040");
