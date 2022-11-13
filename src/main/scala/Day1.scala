import common.DaySchema

import scala.annotation.tailrec

object Day1 extends DaySchema {
  val mappedData: Seq[Int] = loadInput().map(_.toInt)

  /**
   * Calculate how many times a depth is deeper than the previous measurement's depth.
   * */
  private def countDeepening(measurements: Seq[Int]): Int = {
    @tailrec
    def verifyDeepness(lastMeasurement: Int, remainingMeasurements: Seq[Int], acc: Int): Int = {
      remainingMeasurements.tail match {
        case Nil => if (remainingMeasurements.head > lastMeasurement) acc + 1 else acc
        case _ => verifyDeepness(remainingMeasurements.head, remainingMeasurements.tail,
          if (remainingMeasurements.head > lastMeasurement) acc + 1 else acc)
      }
    }

    verifyDeepness(measurements.head, measurements.tail, 0)
  }

  /**
   * Calculate sums of a three-measurement sliding window and count how many sums are larger than the previous sum
   */
  private def countDeepeningInWindow(measurements: Seq[Int]): Int = {
    val sumFromWindow: Iterator[Int] = measurements.sliding(3).map(_.sum)
    sumFromWindow.sliding(2).map(x => x.head < x(1)).count(_ == true)
  }

  println(s"Part 1: Number of measurements that are larger than the previous one: ${countDeepening(mappedData)}")
  println(s"Part 2: Number of three-measurement-long window greater than the previous window is equal to: ${countDeepeningInWindow(mappedData)}")
}
