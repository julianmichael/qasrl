package qasrl.labeling

import cats.arrow.Arrow
import cats.implicits._

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.util.LowerCaseStrings._

/** Class for mapping QA-SRL questions to other forms (and back).
  * Works on a set of labels for a verb all at a time, to allow for reconstructing full questions
  * from argument lists.
  */
case class QuestionLabelMapper[A, B](
  mapping: (
    Vector[String], // sentence tokens
    InflectedForms, // forms of verb
    List[A] // labels
  ) => Map[A, B]
) {

  def apply(
    sentenceTokens: Vector[String],
    verbInflectedForms: InflectedForms,
    labels: List[A]
  ): List[Option[B]] = {
    val resultMap = mapping(sentenceTokens, verbInflectedForms, labels)
    labels.map(resultMap.get)
  }

}

object QuestionLabelMapper {

  def liftOptionalWithContext[A, B](
    f: (Vector[String], InflectedForms, A) => Option[B]
  ): QuestionLabelMapper[A, B] = QuestionLabelMapper[A, B](
    (sentenceTokens: Vector[String], verbInflectedForms: InflectedForms, labels: List[A]) => {
      labels.flatMap(a => f(sentenceTokens, verbInflectedForms, a).map(a -> _)).toMap
    }
  )

  def liftWithContext[A, B](
    f: (Vector[String], InflectedForms, A) => B
  ): QuestionLabelMapper[A, B] =
    liftOptionalWithContext((s: Vector[String], i: InflectedForms, a: A) => Option(f(s, i, a)))

  def liftOptional[A, B](
    f: A => Option[B]
  ): QuestionLabelMapper[A, B] =
    liftOptionalWithContext((_: Vector[String], _: InflectedForms, a: A) => f(a))

  implicit val questionLabelMapperInstances =
    new Arrow[QuestionLabelMapper] {
      override def compose[A, B, C](
        f: QuestionLabelMapper[B, C],
        g: QuestionLabelMapper[A, B]
      ): QuestionLabelMapper[A, C] = QuestionLabelMapper[A, C](
        (sentenceTokens: Vector[String], verbInflectedForms: InflectedForms, labels: List[A]) => {
          val bMapping = g.mapping(sentenceTokens, verbInflectedForms, labels)
          val bs = labels.flatMap(bMapping.get)
          val cMapping = f.mapping(sentenceTokens, verbInflectedForms, bs)
          bMapping.flatMap {
            case (a, b) =>
              cMapping.get(b).map(a -> _)
          }: Map[A, C]
        }
      )

      def first[A, B, C](fa: QuestionLabelMapper[A, B]): QuestionLabelMapper[(A, C), (B, C)] =
        QuestionLabelMapper[(A, C), (B, C)](
          (
            sentenceTokens: Vector[String],
            verbInflectedForms: InflectedForms,
            labels: List[(A, C)]
          ) => {
            val bMapping = fa.mapping(sentenceTokens, verbInflectedForms, labels.map(_._1))
            labels.flatMap {
              case (a, c) =>
                bMapping.get(a).map(b => (a, c) -> (b, c))
            }.toMap: Map[(A, C), (B, C)]
          }
        )

      def lift[A, B](f: A => B): QuestionLabelMapper[A, B] =
        liftWithContext((_: Vector[String], _: InflectedForms, a: A) => f(a))

      override def id[A]: QuestionLabelMapper[A, A] = lift(identity[A])

    }

  val mapToLowerCase = questionLabelMapperInstances.lift((s: String) => s.lowerCase)

}
