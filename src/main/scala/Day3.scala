import `enum`.Day3Enum.{LOGIC_FALSE, LOGIC_TRUE}
import common.DaySchema

object Day3 extends DaySchema{

  val mappedData: Seq[String] = loadInput()
  // assumption: each row in a dataset has the same length (the data is valid)
  val rowLength: Int = mappedData.head.length
  val bitDataLengthMiddle: Int = mappedData.length / 2

  def calculateGammaRate(bitData: Seq[String]): List[String] = {
    def countBitEqualToOne(binaryNumbers: Seq[String], oneCounted: Int, bitNumber: Int, gammaList: List[String]): List[String] = {
      val countOfOnesInCurrentColumn: Int = if (binaryNumbers.head.charAt(bitNumber).toString == LOGIC_TRUE) oneCounted + 1 else oneCounted
      binaryNumbers.tail match {
        case Nil => {
          val nextBitNumber: Int = bitNumber - 1
          nextBitNumber match {
            case 0 => gammaList
            case _ => {
              val newGammaList = gammaList :+ (if (countOfOnesInCurrentColumn > bitDataLengthMiddle) LOGIC_TRUE else LOGIC_FALSE)
              countBitEqualToOne(bitData, 0, nextBitNumber, newGammaList)
            }
          }
        }
        case _ => countBitEqualToOne(binaryNumbers.tail, countOfOnesInCurrentColumn, bitNumber, gammaList)
      }
    }
    countBitEqualToOne(bitData, 0, rowLength - 1, List[String]())
  }

  println(Integer.parseInt(calculateGammaRate(mappedData).reverse.mkString, 2))

}
