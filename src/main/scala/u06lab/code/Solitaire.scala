package u06lab.code

object Solitaire extends App:

  extension (p: Position)
    def +(q: Position): Position = (p._1 + q._1, p._2  + q._2)

  extension (p: Position)
    def between(board: Position): Boolean = p._1 >= 0 && p._1 < board._1 && p._2 >= 0 && p._2 < board._2

  type Position = (Int, Int)
  type Solution = Iterable[Position]
  type IterableFactory = Solution => Iterable[Solution]
  given IterableFactory = List(_).view


  val Mosse = List((-3, 0), (-2, 2), (0, 3), (2, 2), (3, 0), (2, -2), (0, -3), (-2, -2))
  //val Mosse = List((-1,0),(-1,1),(0,1),(1,1),(1,0),(1,-1),(0,-1),(-1,-1))
  val board = (7, 5)
  val toPlace = board._1 * board._2
  def placeMarks(n: (Int, Int) = board)(using factory: IterableFactory): Iterable[Solution] =
    var campo = Array.fill(n._1 * n._2)(0)
    val solutions: Iterable[Solution] = factory(Set())
    val initial = (Math.ceil(board._1/2).toInt, Math.ceil(board._2/2).toInt)

    campo(initial._1 * n._2 + initial._2) = 1
    findSolution(campo, solutions, initial, n._2)

    solutions

  var sol: Int = 0
  def findSolution(campo: Array[Int], solutions: Iterable[Solution], curMove: Position, width: Int): Unit =

    var next:Int = 0
    // 8 possible moves
    for (i <- 0 until 8) {

      // calculate next cell to place number
      var nextMove = curMove + Mosse(i)
      if(nextMove.between(board._1, board._2)) {
        var next = nextMove._1 * width + nextMove._2
        var cur = curMove._1 * width + curMove._2
        if (next >= 0 && next < campo.length) { // within field
          if (campo(next) == 0) { // is currently empty

            // previous + 1
            campo(next) = campo(cur) + 1

            if campo(next) < toPlace then
              findSolution(campo, solutions, nextMove, width)
            else
              sol = sol + 1
              println(render((campo, sol), board._2, board._1))
              solutions.toSeq :+ campo
            campo(next) = 0
          }
        }
      }
    }

  def render(si: (Array[Int], Int), width: Int, height: Int): String =
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

  placeMarks()//.zipWithIndex.foreach(s => println(render(s, size._2, size._1)))