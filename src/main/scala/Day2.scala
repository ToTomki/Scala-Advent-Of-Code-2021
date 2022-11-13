import common.DaySchema

import scala.util.{Failure, Success, Try}

object Day2 extends DaySchema {

  case class MoveOrder(direction: String, distance: Int)

  val mappedData: Seq[MoveOrder] = Try(loadInput().map(_.split(" ")).map(x => MoveOrder(x(0), x(1).toInt))) match {
    case Success(loadingTheInput) => loadingTheInput
    case Failure(exception: java.lang.ArrayIndexOutOfBoundsException) => throw new Exception(
      s"The input file was prepared incorrectly (probably some row contains three space-separated parts such as 'forward 3 4'). Path: $inputPath")
  }

  sealed trait PositionSchema {def horizontal: Int; def depth: Int}
  case class Position(horizontal: Int = 0, depth: Int = 0) extends PositionSchema
  case class PositionWithAim(horizontal: Int = 0, depth: Int = 0, aim: Int = 0) extends PositionSchema

  def calculatePosition(course: Seq[MoveOrder]): Position = {
    def directionMatcher(order: MoveOrder, positionToModify: Position): Position = {
      order.direction match {
        case "forward" => Position(positionToModify.horizontal + order.distance, positionToModify.depth)
        case "up" => Position(positionToModify.horizontal, positionToModify.depth - order.distance)
        case "down" => Position(positionToModify.horizontal, positionToModify.depth + order.distance)
        case _ => throw new Exception("The input file contains incorrect direction definition in at least one row.")
      }
    }

    def moveSubmarine(courseToTake: Seq[MoveOrder], position: Position): Position = {
      val moveOrder: MoveOrder = courseToTake.head
      courseToTake.tail match {
        case Nil => directionMatcher(moveOrder, position)
        case _ => moveSubmarine(courseToTake.tail, directionMatcher(moveOrder, position))
      }
    }
    moveSubmarine(course, Position())
  }

  def calculatePositionWithAim(course: Seq[MoveOrder]): PositionWithAim = {
    def directionMatcher(order: MoveOrder, positionToModify: PositionWithAim): PositionWithAim = {
      order.direction match {
        case "forward" => {
          val aimCorrection: Int = positionToModify.aim * order.distance
          PositionWithAim(
            positionToModify.horizontal + order.distance,
            positionToModify.depth + aimCorrection,
            positionToModify.aim
          )
        }
        case "up" => PositionWithAim(positionToModify.horizontal, positionToModify.depth, positionToModify.aim - order.distance)
        case "down" => PositionWithAim(positionToModify.horizontal, positionToModify.depth, positionToModify.aim + order.distance)
        case _ => throw new Exception("The input file contains incorrect direction definition in at least one row.")
      }
    }

    def moveSubmarineWithAim(courseToTake: Seq[MoveOrder], position: PositionWithAim): PositionWithAim = {
      val moveOrder: MoveOrder = courseToTake.head
      courseToTake.tail match {
        case Nil => directionMatcher(moveOrder, position)
        case _ => moveSubmarineWithAim(courseToTake.tail, directionMatcher(moveOrder, position))
      }
    }
    moveSubmarineWithAim(course, PositionWithAim())
  }

  val finalPosition: Position = calculatePosition(mappedData)
  val multipliedCoordinates: Int = finalPosition.horizontal * finalPosition.depth
  val finalPositionWithAim: PositionWithAim = calculatePositionWithAim(mappedData)
  val multipliedCoordinatesWithAim: Int = finalPositionWithAim.horizontal * finalPositionWithAim.depth


  println(s"Part 1.1: The horizontal position and depth I have after following the planned course: $finalPosition")
  println(s"Part 1.2: Multiplication of the final horizontal position and the final depth: $multipliedCoordinates")
  println(s"Part 2.1: The horizontal position and depth I have after following the planned course: $finalPositionWithAim. The aim: ${finalPositionWithAim.aim}")
  println(s"Part 2.2: Multiplication of the final horizontal position and the final depth: $multipliedCoordinatesWithAim")
}
