import Dataset.*

import scala.annotation.tailrec
object Regression {
  def regression(dataset_file: String,
                 attribute_columns: List[String],
                 value_column: String,
                 test_percentage: Double,
                 alpha: Double,
                 gradient_descent_steps: Int): (Matrix, Double) = {

    val dataset = Dataset(dataset_file)
    val selectedDataset = dataset.selectColumns(attribute_columns)
    val (trainSet, validationSet) = selectedDataset.split(test_percentage)
    val m = trainSet.data.length + 1

    val X = Matrix(trainSet) ++ 1
    // y - pret real; trainset = first component of split
    val Y = Matrix(dataset.split(test_percentage)._1.selectColumn(value_column))
    val numAttributes = X.width
    val W = Matrix(List.fill(numAttributes.get)(List(0.0)))

    @tailrec
    def GDA(W: Matrix, steps: Int): Matrix = {
      if (steps == 0) return W
      // pret prezis
      val estimates = X * W
      val error = estimates - Y
      val gradient = (X.transpose * error).data match {
        case None => Matrix(None)
        case Some(mat) => Matrix(mat.map(_.map (elem => elem / m)))
      }
      val newW = gradient.data match {
        case None => Matrix(None)
        case Some(m) => W - Matrix(Some(m.map(_.map(_ * alpha))))
      }

      GDA(newW, steps - 1)
    }

    val lastW = GDA(W, gradient_descent_steps)

    val X_valid = Matrix(validationSet) ++ 1
    val Y_valid = Matrix(dataset.split(test_percentage)._2.selectColumn(value_column))
    val predictions = X_valid * lastW

    val totalError = (predictions - Y_valid).data match {
      case None => 0.0
      case Some(mat) => mat.map(_.sum).sum / mat.size
    }

    (lastW, totalError)
  }

  def main(args: Array[String]): Unit = {
    // Exemplu de utilizare
    print(regression("datasets/houseds.csv", List("GrLivArea", "YearBuilt"), "SalePrice", 0.1, 1e-7, 10000))
  }
}

