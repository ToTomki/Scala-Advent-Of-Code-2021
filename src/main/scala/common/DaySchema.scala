package common

import scala.io.Source.fromFile
import scala.util.{Failure, Success, Using}

trait DaySchema extends App {
  def day: String = getClass.getName.dropRight(1)
  def inputPath: String = s"data/input/$day.txt"

  protected def loadInput(path: String = inputPath): Seq[String] = {
    Using(fromFile(path))(_.mkString) match {
      case Success(value) =>
        value.split("\n").toSeq.map(_.trim)
      case Failure(exception: java.io.FileNotFoundException) =>
        throw new Exception(s"The input data for $day was not found in the provided path: $inputPath")
      case Failure(exception) =>
        throw new Exception(s"The input data for $day could not be loaded. Reason: $exception.")
    }
  }


}
