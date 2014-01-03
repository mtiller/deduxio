package com.xogeny.puzzles.deduxio.alpha.repr

/**
 * Created by mtiller on 1/3/14.
 */

object Color {
  def lowest = Black;
  def highest = Purple;
}

abstract sealed class Color(c: Int, val rgb: String);
case object Black extends Color(0, "#000000");
case object Red extends Color(1, "#ff6060");
case object Green extends Color(2, "#60ff60");
case object Blue extends Color(3, "#8080ff");
case object Yellow extends Color(4, "#ffff80");
case object Purple extends Color(5, "#ff80ff");
case object Cyan extends Color(6, "#80ffff");
