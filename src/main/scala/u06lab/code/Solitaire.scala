package u06lab.code

import u06lab.code.Solitaire.Solitaire.placeMarks

object Solitaire extends App:
  object Solitaire:
    extension (p: Position)
      private def +(q: Position): Position = (p._1 + q._1, p._2  + q._2)
      private def inside(board: Position): Boolean = p._1 >= 0 && p._1 < board._1 && p._2 >= 0 && p._2 < board._2



    private type Position = (Int, Int)
    private val moves = List((-3, 0), (-2, 2), (0, 3), (2, 2), (3, 0), (2, -2), (0, -3), (-2, -2))

    val height = 7
    val width = 5
    private val toPlace = height * width
    def placeMarks(n: (Int, Int) = (height, width)): Unit =
      var campo = Array.fill(n._1 * n._2)(0)

      val initial = (Math.ceil(height/2).toInt, Math.ceil(width/2).toInt)

      campo(initial._1 * n._2 + initial._2) = 1
      findSolution(campo, initial)


    private var sol: Int = 0
    private def findSolution(campo: Array[Int], curMove: Position): Unit =

      var next:Int = 0
      // 8 possible moves
      for (i <- 0 until 8)

        // calculate next cell to place number
        val nextMove = curMove + moves(i)
        if(nextMove.inside(height, width))
          val next = nextMove._1 * width + nextMove._2
          val cur = curMove._1 * width + curMove._2

          if (campo(next) == 0)  // is currently empty

            campo(next) = campo(cur) + 1

            if campo(next) < toPlace then
              findSolution(campo, nextMove)
            else
              sol = sol + 1
              println(render((campo, sol)))

            campo(next) = 0


    def render(si: (Array[Int], Int)): String =
      println(s"sol ${si._2}")
      val reversed = si._1.toSeq
      var arr = for i <- 1 to toPlace
                    number = reversed.indexOf(i)
                    yield if number >= 0 then (number / width, number % width) else (-1, -1)

      arr = arr.collect{case x if x != (-1,-1) => x}
      val rows =
        for y <- 0 until height
            row = for x <- 0 until width
            number = arr.indexOf((y, x)) + 1
            yield if number > 0 then "%-2d ".format(number) else "X  "
        yield row.mkString
      rows.mkString("\n")

  placeMarks()