package qasrl

import jjm.LowerCaseString
import jjm.implicits._

import io.circe.generic.JsonCodec
import monocle.macros.Lenses

@JsonCodec sealed trait Argument {
  def placeholder: List[String]
  def gap: Option[String]
  def wh: Option[String]

  def isNoun: Boolean = this match {
    case Noun(_) => true
    case _       => false
  }

  def isPrep: Boolean = this match {
    case Prep(_, _) => true
    case _          => false
  }

  def isLocative: Boolean = this match {
    case Locative => true
    case _        => false
  }
}

@JsonCodec @Lenses case class Noun(
  isAnimate: Boolean
) extends Argument {
  override def placeholder = List(if (isAnimate) "someone" else "something")
  override def gap = None
  override def wh = if (isAnimate) Some("Who") else Some("What")
}
object Noun

@JsonCodec @Lenses case class Prep(
  preposition: LowerCaseString,
  objOpt: Option[Noun]
) extends Argument {
  override def placeholder = objOpt.toList.flatMap(_.placeholder)
  override def gap = Some(preposition)
  override def wh = objOpt.flatMap(_.wh)
}
object Prep

case object Locative extends Argument {
  override def placeholder = List("somewhere")
  override def gap = None
  override def wh = Some("Where")
}

object Argument
