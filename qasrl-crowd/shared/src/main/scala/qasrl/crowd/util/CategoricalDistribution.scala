package qasrl.crowd.util

import cats.data.NonEmptyList
import cats.implicits._

// assume distinct; requires counts to be normalized
case class CategoricalDistribution[Label](private val distr: NonEmptyList[(Label, Double)]) {
  private val sortedPairs = distr.toList.toVector.sortBy(-_._2)
  private val mapping = distr.toList.toMap

  def apply(x: Label): Double = {
    mapping.get(x).getOrElse(0.0)
  }

  def sample(rand: scala.util.Random): Label = {
    sortedPairs.foldM[Either[Label, ?], Double](rand.nextDouble) {
      case (massRemaining, (label, prob)) =>
        if(prob >= massRemaining) Left(label)
        else Right(massRemaining - prob)
    } match {
      case Left(label) => label
      case Right(extraMass) => sortedPairs.last._1 // this shouldn't happen...
    }
  }

  def interpolate(other: CategoricalDistribution[Label], thisWeight: Double) =
    CategoricalDistribution(
      NonEmptyList.fromList(
        (mapping.keySet ++ other.mapping.keySet).toList.map { key =>
          key -> (
            (thisWeight * mapping.get(key).getOrElse(0.0)) +
              ((1.0 - thisWeight) * other.mapping.get(key).getOrElse(0.0))
          )
        }
      ).get
    )

  def filter(predicate: Label => Boolean) = NonEmptyList.fromList {
    val ls = distr.filter(p => predicate(p._1))
    val sum = ls.map(_._2).sum
    ls.map(p => p._1 -> p._2 / sum)
  }.map(CategoricalDistribution(_))

}
object CategoricalDistribution {
  // assume distinct
  def uniform[A](items: NonEmptyList[A]) = CategoricalDistribution(
    items.map(_ -> (1.0 / items.size))
  )
}
