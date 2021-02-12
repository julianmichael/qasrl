package qasrl.labeling

import jjm.LowerCaseString
import jjm.ling.en.InflectedForms
import jjm.ling.en.VerbForm
import jjm.ling.en.VerbForm.PastParticiple
import jjm.implicits._

import cats.Order
import cats.Show
import cats.implicits._

import io.circe.Json

case class QuestionTemplate(
  wh: LowerCaseString,
  hasSubj: Boolean,
  isPassive: Boolean,
  hasObj: Boolean,
  prep: Option[LowerCaseString],
  obj2: Option[LowerCaseString]
) {

  def toTemplateString = List(
    Some(wh),
    Option("something".lowerCase).filter(_ => hasSubj),
    Some(if(isPassive) "verb[pss]" else "verb").map(_.lowerCase),
    Option("something".lowerCase).filter(_ => hasObj),
    prep,
    obj2
  ).flatten.mkString(" ")

  def toSlots = SlotBasedLabel[VerbForm](
    wh = wh,
    aux = if(isPassive) Some("is".lowerCase)
          else if(hasSubj) Some("does".lowerCase)
          else None,
    subj = if(hasSubj) Some("something".lowerCase) else None,
    verbPrefix = Nil,
    verb = if(isPassive) VerbForm.PastParticiple
           else if(hasSubj) VerbForm.Stem
           else VerbForm.PresentSingular3rd,
    obj = if(hasObj) Some("something".lowerCase) else None,
    prep = prep,
    obj2 = obj2
  )

  def toQuestionString = toSlots.renderQuestionString(
    InflectedForms(
      stem = "verb".lowerCase,
      presentSingular3rd = "verbs".lowerCase,
      past = "verbed".lowerCase,
      pastParticiple = "verbed".lowerCase,
      presentParticiple = "verbing".lowerCase
    ).apply
  )
}

object QuestionTemplate {
  def normalizeAdverbials(template: QuestionTemplate) = {
    if(template.wh == "what".lowerCase) template else {
      QuestionTemplate(template.wh, true, false, false, None, None)
    }
  }

  def normalizeToActive(template: QuestionTemplate) = {
    if(!template.isPassive) template else {
      // what is something given something on? --> what gave something something? // drop on-phrase
      //   what hasSubj isPassive <prep> -> !hasSubj !isPassive obj obj2
      // what is something given something by? --> what gave something something?
      //   what hasSubj isPassive by  -> !hasSubj !isPassive obj obj2
      // what is something punched by? --> what punched something?
      //   what hasSubj isPassive by  -> !hasSubj !isPassive obj
      // what is something punched on? --> what did something punch something on?
      //   what hasSubj isPassive <prep>  -> hasSubj !isPassive obj <prep>

      // what is given something?              --> what did something give something?
      //   what !hasSubj isPassive obj -> hasSubj !isPassive obj
      // what is punched?              --> what did something punch?
      //   what !hasSubj isPassive -> hasSubj !isPassive
      // what is punched by something? --> what did something punch?
      //   what !hasSubj isPassive by something -> hasSubj !isPassive <>
      // what is punched on something? --> what did something punch on something?
      //   what !hasSubj isPassive <prep> something -> hasSubj !isPassive <prep> something
      // what is looked at?            --> what did something look at?
      //   what !hasSubj isPassive <prep>  -> hasSubj !isPassive <prep>

      val askingBy = template.prep == "by".lowerCase && template.obj2.isEmpty
      val hasByPlaceholder = template.prep == "by".lowerCase &&
        template.obj2 == Some("something".lowerCase)

      if(template.wh == "what".lowerCase) {
        if(template.hasSubj) {
          template.copy(
            hasSubj = !askingBy,
            isPassive = false,
            prep = template.prep.filter(_ => !askingBy),
            obj2 = Some("something".lowerCase)
              .filter(_ => template.hasObj)
              .orElse(template.obj2)
          )
        } else {
          template.copy(
            hasSubj = true,
            isPassive = false,
            prep = template.prep.filter(_ => !hasByPlaceholder),
            obj2 = template.obj2.filter(_ => !hasByPlaceholder)
          )
        }
      } else {
        if(template.hasSubj) {
          template.copy(
            isPassive = false,
            hasObj = true,
            prep = template.prep.filter(_ => !template.hasObj && !hasByPlaceholder),
            obj2 = Some("something".lowerCase)
              .filter(_ => template.hasObj)
              .orElse(template.obj2)
              .filter(_ => !hasByPlaceholder)
          )
        } else {
          template.copy(
            hasSubj = true,
            isPassive = false
          )
        }
      }


      // where is something given something?   --> where did something give something something?
      //   wh-hasSubj isPassive obj -> !isPassive obj obj2
      // where is something given something by something?   --> where did something give something something?
      //   wh-hasSubj isPassive obj by something -> !isPassive obj obj2
      // where is something punched?   --> where did something punch something?
      //   wh-hasSubj isPassive  -> !isPassive obj
      // where is something punched by something? --> where did something punch something?
      //   wh-hasSubj isPassive by something  -> !isPassive obj
      // where is something punched on something? --> where did something punch something on something?
      //   wh-hasSubj isPassive <prep> something  -> !isPassive obj <prep> something
      // where is something punched by? --> where did something punch something by?
      //   wh-hasSubj isPassive by  -> !isPassive obj by
      // where is looked at?            --> where does something look at?
      //   XXX
    }
  }

  // extremely unnecessary amounts of computation here, lol
  def fromClausalQuestion(clausalQ: ClausalQuestion) = {
    fromQuestionSlots(
      SlotBasedLabel.getVerbTenseAbstractedSlotsForQuestion(
        Vector(), clausalQ.frame.verbInflectedForms,
        clausalQ.frame.questionsForSlot(clausalQ.slot)
      ).head.get
    )
  }

  def fromQuestionSlots(slots: SlotBasedLabel[VerbForm]) = QuestionTemplate(
    wh = if(slots.wh.toString == "who") "what".lowerCase else slots.wh,
    hasSubj = slots.subj.nonEmpty,
    isPassive = slots.verb == PastParticiple &&
      (slots.aux.toList ++ slots.verbPrefix).map(_.toString).toSet.intersect(
        Set("be", "been", "is", "isn't", "was", "wasn't")
      ).nonEmpty,
    hasObj = slots.obj.nonEmpty,
    prep = slots.prep,
    obj2 = slots.obj2.map(_.toString.replaceAll("someone", "something").lowerCase)
  )

  implicit val questionTemplateShow = Show.show[QuestionTemplate](_.toQuestionString)

  implicit val questionTemplateOrder = Order.by[QuestionTemplate, String](_.toQuestionString)

  import io.circe.{Encoder, Decoder}

  implicit val questionTemplateEncoder = implicitly[Encoder[Map[String, String]]]
    .contramap[QuestionTemplate] { questionTemplate =>
      import questionTemplate._
      Map(
        "abst-wh"    -> wh.toString,
        "abst-subj"  -> (if(hasSubj) "something" else "_"),
        "abst-verb"  -> (if(isPassive) "verb[pss]" else "verb"),
        "abst-obj"   -> (if(hasObj) "something" else "_"),
        "prep"       -> prep.fold("_")(_.toString),
        "abst-obj2"  -> obj2.fold("_")(_.toString)
      )
    }

  implicit val questionTemplateDecoder = Decoder.instance[QuestionTemplate] { c =>
    for {
      wh <- c.downField("abst-wh").as[String]
      subj <- c.downField("abst-subj").as[String]
      verb <- c.downField("abst-verb").as[String]
      obj <- c.downField("abst-obj").as[String]
      prep <- c.downField("prep").as[String]
      obj2 <- c.downField("abst-obj2").as[String]
    } yield QuestionTemplate(
      wh = wh.lowerCase,
      hasSubj = subj != "_",
      isPassive = verb == "verb[pss]",
      hasObj = obj != "_",
      prep = prep.lowerCase.some.filter(_ != "_"),
      obj2 = obj2.lowerCase.some.filter(_ != "_")
    )
  }
}
