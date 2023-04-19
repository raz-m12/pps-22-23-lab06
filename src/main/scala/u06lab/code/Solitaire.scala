package u06lab.code

import u06lab.code.Solitaire.Solitaire.placeMarks
import scala.annotation.targetName

object Solitaire extends App:
  object Solitaire:
    extension (p: Position)
      private def +(q: Position): Position = (p._1 + q._1, p._2  + q._2)
      private def inside(board: Position): Boolean = p._1 >= 0 && p._1 < board._1 && p._2 >= 0 && p._2 < board._2
      private def index(): Int = p._1 * width + p._2

    extension (campo: Array[Int])
      private def emptyAt(i: Int): Boolean = campo(i) == 0
      private def toPositions: Seq[Position] =
        val arr = for i <- 1 to toPlace
            number = campo.indexOf(i)
        yield if number >= 0 then (number / width, number % width) else (-1, -1)

        arr.collect{case x if x != (-1,-1) => x}

    private type Position = (Int, Int)
    private val moves = List((-3, 0), (-2, 2), (0, 3), (2, 2), (3, 0), (2, -2), (0, -3), (-2, -2))

    val height = 7
    val width = 5
    private val toPlace = height * width
    private var solCount: Int = 0
    def placeMarks(n: (Int, Int) = (height, width)): Unit =
      val campo = Array.fill(n._1 * n._2)(0)

      val initial = (Math.ceil(height/2).toInt, Math.ceil(width/2).toInt)

      campo(initial._1 * n._2 + initial._2) = 1
      findSolution(campo, initial)



    private def findSolution(campo: Array[Int], current: Position): Unit =

      for (i <- moves.indices)
        val nextMove = current + moves(i)
        if(nextMove.inside(height, width))
          val next = nextMove.index()
          val cur = current.index()

          if (campo.emptyAt(next))
            campo(next) = campo(cur) + 1
            if campo(next) < toPlace then
              findSolution(campo, nextMove)
            else
              println(render(campo))
            campo(next) = 0


    def render(si: Array[Int]): String =
      solCount = solCount + 1
      println(s"sol ${solCount}")

      val positions = si.toPositions
      val rows =
        for y <- 0 until height
            row = for x <- 0 until width
            number = positions.indexOf((y, x)) + 1
            yield if number > 0 then "%-2d ".format(number) else "X  "
        yield row.mkString
      rows.mkString("\n")

  placeMarks()