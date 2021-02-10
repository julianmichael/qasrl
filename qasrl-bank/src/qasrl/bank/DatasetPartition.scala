package qasrl.bank

import io.circe.{Encoder, Decoder}
import io.circe.{KeyEncoder, KeyDecoder}
import io.circe.Json
import io.circe.DecodingFailure

sealed trait DatasetPartition {
  import DatasetPartition._
  override def toString = this match {
    case Train => "train"
    case Dev   => "dev"
    case Test  => "test"
  }
}

object DatasetPartition {
  case object Train extends DatasetPartition
  case object Dev extends DatasetPartition
  case object Test extends DatasetPartition

  @inline def train: DatasetPartition = Train
  @inline def dev: DatasetPartition = Dev
  @inline def test: DatasetPartition = Test

  def fromString(s: String): Option[DatasetPartition] = s match {
    case "train" => Some(Train)
    case "dev"   => Some(Dev)
    case "test"  => Some(Test)
    case _       => None
  }

  implicit val datasetPartitionKeyEncoder = KeyEncoder.instance[DatasetPartition](_.toString)
  implicit val datasetPartitionKeyDecoder = KeyDecoder.instance(DatasetPartition.fromString)

  implicit val datasetPartitionEncoder = Encoder.instance[DatasetPartition](
    part => Json.fromString(part.toString)
  )
  implicit val datasetPartitionDecoder = Decoder.instance(
    c =>
    c.as[String]
      .right
      .flatMap(
        str =>
        DatasetPartition.fromString(str) match {
          case Some(part) => Right(part)
          case None =>
            Left(DecodingFailure("Failed to parse dataset partition value", c.history))
        }
      )
  )

}
