package ex4

import java.util.OptionalInt

// Optional!
object ConnectThree extends App:
  val bound = 3
  enum Player:
    case X, O
    def other: Player = this match
      case X => O
      case _ => X

  case class Disk(x: Int, y: Int, player: Player)
  /**
   * Board:
   * y
   *
   * 3
   * 2
   * 1
   * 0
   *   0 1 2 3 <-- x
   */
  type Board = Seq[Disk]
  type Game = Seq[Board]

  import Player.*

  def find(board: Board, x: Int, y: Int): Option[Player] =
    board.find(disk => disk.x == x && disk.y == y).map(disk => disk.player)

  private def firstAvailableRow(board: Board, x: Int): Option[Int] =
    val y = board.count(disk => disk.x == x)
    if y <= bound then Some(y) else None

  private def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    for
      x <- 0 to bound
      y <- firstAvailableRow(board, x)
    yield
      board :+ Disk(x, y, player)

  private def computeAnyGame(player: Player, moves: Int): LazyList[Game] = moves match
    case 0 => LazyList(Seq(Seq()))
    case n =>
      for
        game <- computeAnyGame(player, n - 1)
        lastBoard = game.last
        nextGame <- if hasWon(lastBoard) then
          LazyList(game)
        else
          val currentPlayer = if lastBoard.size % 2 == 0 then player else player.other
          placeAnyDisk(lastBoard, currentPlayer).map(board => game :+ board)
      yield
        nextGame

  private def hasWon(board: Board): Boolean =
    board.exists { disk =>
      val player = Some(disk.player)
      val horizontal = find(board, disk.x + 1, disk.y) == player && find(board, disk.x + 2, disk.y) == player
      val vertical = find(board, disk.x, disk.y + 1) == player && find(board, disk.x, disk.y + 2) == player
      val diagonalUp = find(board, disk.x + 1, disk.y + 1) == player && find(board, disk.x + 2, disk.y + 2) == player
      val diagonalDown = find(board, disk.x + 1, disk.y - 1) == player && find(board, disk.x + 2, disk.y - 2) == player

      horizontal || vertical || diagonalUp || diagonalDown
    }

  private def printBoards(game: Seq[Board]): Unit =
    for
      y <- bound to 0 by -1
      board <- game.reverse
      x <- 0 to bound
    do
      print(find(board, x, y).map(_.toString).getOrElse("."))
      if x == bound then
        print(" ")
        if board == game.head then println()

  // Exercise 1: implement find such that..
  println("EX 1: ")
  println(find(List(Disk(0, 0, X)), 0, 0)) // Some(X)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 0, 1)) // Some(O)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 1, 1)) // None

  // Exercise 2: implement firstAvailableRow such that..
  println("EX 2: ")
  println(firstAvailableRow(List(), 0)) // Some(0)
  println(firstAvailableRow(List(Disk(0, 0, X)), 0)) // Some(1)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X)), 0)) // Some(2)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X)), 0)) // Some(3)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), 0)) // None
  // Exercise 2: implement placeAnyDisk such that..
  printBoards(placeAnyDisk(List(), X))
  // .... .... .... ....
  // .... .... .... ....
  // .... .... .... ....
  // ...X ..X. .X.. X...
  printBoards(placeAnyDisk(List(Disk(0, 0, O)), X))
  // .... .... .... ....
  // .... .... .... ....
  // ...X .... .... ....
  // ...O ..XO .X.O X..O
  println("EX 4: ")
// Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  computeAnyGame(O, 4).foreach { g =>
    printBoards(g)
    println()
  }
//  .... .... .... .... ...O
//  .... .... .... ...X ...X
//  .... .... ...O ...O ...O
//  .... ...X ...X ...X ...X
//
//
// .... .... .... .... O...
// .... .... .... X... X...
// .... .... O... O... O...
// .... X... X... X... X...

// Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
