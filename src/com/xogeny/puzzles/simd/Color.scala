package com.xogeny.puzzles.simd

/**
 * Created by mtiller on 12/14/13.
 */

object Color {
  def lowest = Black;
  def highest = Purple;
}

abstract sealed class Color(c: Int, val rgb: String);
case object Black extends Color(0, "#000000");
case object Red extends Color(1, "#ff0000");
case object Green extends Color(2, "#00ff00");
case object Blue extends Color(3, "#0000ff");
case object Yellow extends Color(4, "#ffff00");
case object Purple extends Color(5, "#ff00ff");

