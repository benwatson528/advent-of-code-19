package uk.co.hadoopathome.adventofcode19.day14

object SpaceStoichiometry {
  private val ORE = "ORE"

  def findRequiredOre(equations: Vector[Equation]): Long = {
    val rootElements = equations.filter(e => e.lhs.size == 1 && e.lhs.contains(ORE)).map(_.rhs._1).toSet
    val finalEquation = replaceRec(equations.filterNot(e => e.lhs.size == 1 && e.lhs.contains(ORE)), rootElements)
    val boughtOre = buyOre(finalEquation, equations)
    boughtOre
  }

  @scala.annotation.tailrec
  private def replaceRec(equations: Vector[Equation], rootElements: Set[String]): Equation = {
    if (equations.size == 1) equations.head
    else {
      val solveable = equations.find(e => isSolveable(e, rootElements)).head
      val fieldThatCanBeReplaced = solveable.rhs._1
      val updatedEquations = equations.map(e =>
        if (e.lhs.contains(fieldThatCanBeReplaced)) replaceElement(e, fieldThatCanBeReplaced, solveable.lhs)
        else e).filterNot(_ == solveable)
      replaceRec(updatedEquations, rootElements)
    }
  }

  private def buyOre(finalEquation: Equation, equations: Vector[Equation]): Long = {
    finalEquation.lhs.map {
      element => {
        val minimumOreRequired = element._2
        val oreEquation = equations.find(_.rhs._1 == element._1).get
        val ceiling = math.ceil(minimumOreRequired.toDouble / oreEquation.rhs._2.toDouble)
        (oreEquation.lhs.head._2 * ceiling).toLong
      }
    }.sum
  }

  private def isSolveable(equation: Equation, rootElements: Set[String]): Boolean =
    equation.lhs.keys.forall(rootElements.contains)

  private def replaceElement(equation: Equation, elementToReplace: String, replacement: Map[String, Long]): Equation = {
    val lhs = equation.lhs
    val multiplier = lhs(elementToReplace)
    val shortened = lhs - elementToReplace
    val replacedMap = replacement.map(e => (e._1, e._2 * multiplier))
    val newLhs = shortened ++ replacedMap.map { case (k, v) => k -> (v + shortened.getOrElse(k, 0L)) }
    Equation(newLhs, equation.rhs)
  }
}
