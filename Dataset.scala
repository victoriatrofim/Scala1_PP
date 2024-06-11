import scala.util.Using

class Dataset(m: List[List[String]]) {
  val data: List[List[String]] = m

  override def toString: String =
    data
      .map(_.foldRight("")((x, y) => s"$x,$y"))
      .foldRight("")(_ + "\n" + _)

  def selectColumn(col: String): Dataset = {
    val transposedData = data.transpose
    val selectedColumn = transposedData.foldRight(List[List[String]]()) {
        (row, acc) =>
        if (row.head.equals(col)) row :: acc
        else acc
    }
    new Dataset(selectedColumn.transpose)
  }



  def selectColumns(cols: List[String]): Dataset = {
    val transposedData = transpose(data)
    val select = transposedData.foldRight(List[List[String]]()) {
      (row, acc) =>
        if (cols.contains(row.head)) row :: acc
        else acc
    }
    new Dataset(select.transpose)
  }

  def transpose(data: List[List[String]]): List[List[String]] = {
    if (data.isEmpty) Nil
    else data.transpose
  }

  def split(percentage: Double): (Dataset, Dataset) = {
      data match {
        case Nil => (Dataset(Nil), Dataset(Nil))
        case header :: tail =>
          val head = data.head

          val sortedData = tail.sortBy(_.head)

          val num = (1 / percentage).ceil.toInt

          val evalShort = sortedData.zipWithIndex.foldRight(List.empty[List[String]]) {
            (element, acc) =>
              // element._2 = indexul elementului; element._1 = lista de siruri
              if ((element._2 + 1) % num == 0) List(element._1) ::: acc
              else acc
          }

          val trainBig = sortedData.zipWithIndex.foldRight(List.empty[List[String]]) {
            (element, acc) =>
              if ((element._2 + 1) % num != 0) List(element._1) ::: acc
              else acc
          }
          (Dataset(head :: trainBig), Dataset(head :: evalShort))
      }
    }


  def size: Int = data.length

  def getRows: List[List[String]] = {
    if (data.isEmpty) List.empty
    else data.tail
  }

  def getHeader: List[String] = data.head
}

object Dataset {
  def apply(csv_filename: String): Dataset = {
    val lines = scala.io.Source.fromFile(csv_filename)
    val matrix = lines.getLines().toList.map(_.split(",").toList)
    new Dataset(matrix)
  }

  def apply(ds: List[List[String]]): Dataset = new Dataset(ds)
}

