import scala.annotation.{tailrec, targetName}

type Mat = List[List[Double]]

class Matrix(m: Option[List[List[Double]]]) {

  def transpose: Matrix = {
    def helper(m: Mat): Mat = {
      if (m.head.isEmpty) Nil
      else m.map(_.head) :: helper(m.map(_.tail))
    }

    data match {
      case None => new Matrix(None)
      case Some(data) => new Matrix(Some(helper(data)))
    }
  }

  def map(f: Double => Double): Matrix = {
    def applyToMatrix(matrix: Mat): Mat = {
      matrix.foldLeft(List[List[Double]]()) { (acc, row) =>
        acc :+ applyToRow(row)
      }
    }

    def applyToRow(row: List[Double]): List[Double] = {
      row.foldLeft(List[Double]()) { (accRow, elem) =>
        accRow :+ f(elem)
      }
    }

    m match {
      case None => new Matrix(None)
      case Some(matrixData) => new Matrix(Some(applyToMatrix(matrixData)))
    }
  }

  @targetName("multiply 2 matrix")
  def *(other: Matrix): Matrix = {
      @tailrec
      def helper(currMatr1: Mat, currMatr2: Mat, acc: Mat): Mat = {
        currMatr1 match {
          case Nil => acc.reverse
          case x :: xs =>
            val resRow = currMatr2.map(row =>
              x.zip(row).map {
                case (el1, el2) => el1 * el2 }.sum
            )
            helper(xs, currMatr2, resRow :: acc)
        }
      }

      (data, other.data) match {
        case (Some(m1), Some(m2)) =>
          if (width != other.height) Matrix(None)
          else Matrix(Some(helper(m1, m2.transpose, List.empty)))
        case _ => Matrix(None)
      }
    }

  @targetName("append a column of x")
  def ++(x: Double): Matrix = {
    def helper(m: Mat): Mat = {
      val columnToAdd: List[Double] = List.fill(data.get.length)(x)
      m match
        case Nil => List(columnToAdd)
        case r1 :: r => m.map(raw => raw :+ x)
    }

    data match
      case Some(m) => Matrix(Some(helper(m)))
      case None => Matrix(None)
      case _ => Matrix(None)
  }

  @targetName("diff of 2 matrix")
  def -(other: Matrix): Matrix = {
    (data, other.data) match {
      case (Some(matrix1), Some(matrix2)) =>
        if (matrix1.length != matrix2.length || matrix1.head.length != matrix2.head.length)
          Matrix(None)
        else {
          val result = matrix1.zip(matrix2).map {
            case (m1, m2) => m1.zip(m2).map {
              case (el1, el2) => el1 - el2
            }
          }
          Matrix(Some(result))
        }
      case _ => Matrix(None)
    }
  }

  def data: Option[Mat] = m

  def height: Option[Int] = {
    data match
      case None => None
      case Some(matrix) => Some(matrix.length)
  }

  def width: Option[Int] = {
    data match
      case None => None
      case Some(matrix) => Some(matrix.head.length)
  }

  override def toString: String = m.toString
}

object Matrix {
  def apply(data: Mat): Matrix = new Matrix(Some(data))

  def apply(data: Option[Mat]): Matrix  = new Matrix(data)

  def apply(dataset: Dataset): Matrix = {
    val m1 = dataset.data.tail.map(_.map(elem => elem.toDouble))
    new Matrix(Some(m1))
  }
}