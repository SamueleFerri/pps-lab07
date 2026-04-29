package ex3

import ex3.Solitaire.{placeMarks, render}

object Solitaire extends App:
  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  //println(render(solution = Seq((0, 0), (2, 1)), width = 3, height = 3))

  type Position = (Int, Int)
  type Solution = Seq[Position]

  def isLegalMove(position1: Position, position2: Position): Boolean =
    val dx = Math.abs(position1._1 - position2._1)
    val dy = Math.abs(position1._2 - position2._2)
    dx == 3 && dy == 0 || dx == 0 && dy == 3 || dx == 2 && dy == 2

  def isSafe(position: Position, sol: Solution, width: Int, height: Int): Boolean =
    val x = position._1
    val y = position._2
    x >= 0 && x < width && y >= 0 && y < height && !sol.contains(position)

  def placeMarks(n: Int, width: Int, height: Int): Iterable[Solution] = n match
    case 1 => Seq(Seq((width / 2, height / 2)))
    case _ =>
      for
        path <- placeMarks(n - 1, width, height)
        lastPos = path.last
        x <- 0 until width
        y <- 0 until height
        newPos = (x, y)
        if isSafe(newPos, path, width, height)
        if isLegalMove(lastPos, newPos)
      yield
        path :+ newPos

@main def runSolitaire(): Unit =
  val w = 5
  val h = 5
  val targetMoves = w * h

  val firstSolution = placeMarks(targetMoves, w, h).headOption

  firstSolution match
    case Some(solution) =>
      println(s"Solution founded in $targetMoves moves!")
      println(render(solution, w, h))
    case None =>
      println("No solution found for this dimension")