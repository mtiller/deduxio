package com.xogeny.puzzles.simd

/**
 * Created by mtiller on 12/14/13.
 */

object Color {
  def lowest = Black;
  def highest = Purple;
}

abstract sealed class Color(c: Int);
case object Black extends Color(0);
case object Red extends Color(1);
case object Green extends Color(2);
case object Blue extends Color(3);
case object Yellow extends Color(4);
case object Purple extends Color(5);

