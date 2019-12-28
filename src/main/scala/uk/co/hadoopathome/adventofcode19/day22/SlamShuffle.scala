package uk.co.hadoopathome.adventofcode19.day22

object SlamShuffle {

  def shuffleDeck(instructions: Vector[Instruction], deckSize: Int): Vector[Int] =
    instructions.foldLeft((0 until deckSize).toVector)((acc, ins) => applyInstruction(ins, acc))

  private def applyInstruction(instruction: Instruction, cards: Vector[Int]): Vector[Int] = instruction match {
    case CUT(n) if n > 0 => cards.drop(n) ++ cards.take(n)
    case CUT(n) if n < 0 => cards.takeRight(n.abs) ++ cards.dropRight(n.abs)
    case DEAL(0) => cards.reverse
    case DEAL(n) => dealWithRec(cards, Vector.fill(cards.size)(0), 0, n, cards.size)
  }

  @scala.annotation.tailrec
  private def dealWithRec(deckCards: Vector[Int], tableCards: Vector[Int], currentPosition: Int, increment: Int,
                          deckSize: Int): Vector[Int] = deckCards match {
    case x +: xs => dealWithRec(xs, tableCards.updated(currentPosition, x), (currentPosition + increment) % deckSize,
      increment, deckSize)
    case _ => tableCards
  }
}
