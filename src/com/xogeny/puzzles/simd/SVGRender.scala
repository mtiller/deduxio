package com.xogeny.puzzles.simd

/**
 * Created by mtiller on 12/23/13.
 */
object SVGRender {
  def file(svg: String) =
    s"""
<!DOCTYPE html>
<html>
  <head>
    <link href="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.1/css/bootstrap-combined.min.css" rel="stylesheet">
    <script src="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.1/js/bootstrap.min.js"></script>
  </head>
  <body>
    <svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="960" height="1440">
      $svg
    </svg>
  </body>
</html>
"""
  def path(points: List[(Int,Int)], color: String) = {
    val path = points map { p => s"${p._1},${p._2}"} mkString " "
    s"""
    <polyline points="$path" style="fill:none;stroke:black;stroke-width:11" />
    <polyline points="$path" style="fill:none;stroke:$color;stroke-width:10" />
    """
  }
  def ball(x: Int, y: Int, color: String) = {
    s"""
      <circle cx="$x" cy="$y" r="25" stroke="black" stroke-width="1" fill="$color" />
      <text fill="black" x="$x" y="$y" font-size="32px"  style="text-anchor: middle;">1</text>
    """
  }
  def renderPath(board: Board, c: Color, elems: List[Int]) = {
    val coords = elems map { e => ((40+80*board.spaces(e).x) -> (40+80*board.spaces(e).y)) }
    path(coords, c.rgb)
  }
  def render(board: Board, cons: List[SimdConstraint], sol: Map[String,Int]) = {
    val paths = board.paths.map map { p: Pair[Color,List[Int]] => renderPath(board, p._1, p._2) } mkString "\n"
    file(paths)
  }
}
