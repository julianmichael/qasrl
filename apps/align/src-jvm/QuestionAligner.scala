package qasrl.apps.align

import qasrl._

import jjm.DependentMap
import jjm.implicits._

import cats.Id
import cats.implicits._

case class QuestionAligner[A](qaPairs: List[((Frame, ArgumentSlot), A)]) {

  val resolvedQAPairsByStructure = qaPairs.groupBy(
    p => ArgStructure(p._1._1.args, p._1._1.isPassive).forgetAnimacy
  )

  val resolvedQAPairsByObj2lessStructure = qaPairs.groupBy(
    p => ArgStructure(p._1._1.args remove Obj2, p._1._1.isPassive).forgetAnimacy
  )

  val intransitive = ArgStructure(
    DependentMap.empty[ArgumentSlot.Aux, Id]
      .put(Subj, Noun(false)),
    isPassive = false
  )
  val transitive = ArgStructure(
    DependentMap.empty[ArgumentSlot.Aux, Id]
      .put(Subj, Noun(false))
      .put(Obj, Noun(false)),
    isPassive = false
  )
  val passive = ArgStructure(
    DependentMap.empty[ArgumentSlot.Aux, Id]
      .put(Subj, Noun(false)),
    isPassive = true
  )
  val passiveBy = ArgStructure(
    DependentMap.empty[ArgumentSlot.Aux, Id]
      .put(Subj, Noun(false))
      .put(Obj2, Prep("by".lowerCase, Some(Noun(false)))),
    isPassive = true
  )

  val transitiveWhere = ArgStructure(
    DependentMap.empty[ArgumentSlot.Aux, Id]
      .put(Subj, Noun(false))
      .put(Obj, Noun(false))
      .put(Obj2, Locative),
    isPassive = false
  )
  val intransitiveWhere = ArgStructure(
    DependentMap.empty[ArgumentSlot.Aux, Id]
      .put(Subj, Noun(false))
      .put(Obj2, Locative),
    isPassive = false
  )
  val passiveWhere = ArgStructure(
    DependentMap.empty[ArgumentSlot.Aux, Id]
      .put(Subj, Noun(false))
      .put(Obj2, Locative),
    isPassive = true
  )

  type ClausalQ = (ArgStructure, ArgumentSlot)
  type ClausalQPair = (ClausalQ, ClausalQ)

  val structuralMappingPairs = Vector[ClausalQPair](
    (transitive -> Subj) -> (transitiveWhere -> Subj),
    (transitive -> Obj) -> (transitiveWhere -> Obj),
    (intransitive -> Subj) -> (intransitiveWhere -> Subj),
    (intransitive -> Obj) -> (intransitiveWhere -> Obj),
    (transitive -> Obj) -> (passive -> Subj),
    (transitive -> Obj) -> (passiveBy -> Subj),
    (transitive -> Subj) -> (passiveBy -> Obj2),
    (transitiveWhere -> Obj) -> (passiveWhere -> Subj),
    (transitiveWhere -> Obj2) -> (transitive -> Adv("where".lowerCase))
  )

  val structuralMapping = structuralMappingPairs
    .foldMap(p =>
      Map(
        p._1 -> Vector(p._2),
        p._2 -> Vector(p._1),
      )
    )

  def removeObj2(structure: ArgStructure): ArgStructure = {
    ArgStructure.args.modify(_ remove Obj2)(structure)
  }

  def getAlignedAnswers(clausalQ: (ArgStructure, ArgumentSlot)): Option[A] = {
    val queries = clausalQ +: structuralMapping.get(clausalQ).foldK
    queries.map(q =>
      resolvedQAPairsByStructure.get(q._1).flatMap(
        _.find(_._1._2 == q._2).map(_._2)
      )
    ).foldK.orElse {
      if(clausalQ._2 != Subj) None else {
        val obj2lessCQ = removeObj2(clausalQ._1) -> clausalQ._2
        val obj2lessQueries = obj2lessCQ +: structuralMapping.get(obj2lessCQ).foldK.filter(_._2 == Subj)
        obj2lessQueries.map(q =>
          resolvedQAPairsByObj2lessStructure.get(q._1).flatMap(
            _.find(_._1._2 == q._2).map(_._2)
          )
        ).foldK
      }
    }
    //   .orElse {
    //   if(clausalQ._2 != Obj2) None else {
    //     clausalQ._1.args.get(Obj2).get match {
    //       case Prep(prep, _) if !prep.endsWith("do".lowerCase) && !prep.endsWith("doing".lowerCase) =>
    //         val obj2lessTemplate = removeObj2(clausalQ._1)
    //         resolvedQAPairsByObj2lessStructure.get(obj2lessTemplate).map(
    //           _.collect { case ((_, Adv(_)), answers) =>
    //             answers.split("~!~").toList
    //               .filter(_.startsWith(prep))
    //               .filter(_.length > prep.length)
    //               .map(_.substring(prep.length + 1))
    //           }.flatten.map(Obj2 -> _)
    //         ).filter(_.nonEmpty)
    //       case _ => None
    //     }
    //   }
    // }
  }

  import jjm.metrics._
  import shapeless.syntax.singleton._
  import shapeless._

  val metrics = {
    "placeholder coverage (same clause (no tense), by clause (no tense))" ->>
      resolvedQAPairsByStructure.toList.foldMap { case (frame, questions) =>
        val (covered, uncovered) = frame.args.keySet
          .partition(slot => getAlignedAnswers(frame -> slot).nonEmpty)
        Proportion.Stats(covered.size, uncovered.size)
      } ::
    "placeholder coverage (same clause (no tense))" ->>
      resolvedQAPairsByStructure.toList.foldMap { case (frame, questions) =>
        val total = questions.size
        val (covered, uncovered) = frame.args.keySet
          .partition(slot => getAlignedAnswers(frame -> slot).nonEmpty)
        Proportion.Stats(covered.size * total, uncovered.size * total)
      } ::
    "uncovered placeholders (same clause (no tense))" ->>
      resolvedQAPairsByStructure.toList.foldMap { case (frame, questions) =>
        val total = questions.size
        val (covered, uncovered) = frame.args.keySet
          .partition(slot => getAlignedAnswers(frame -> slot).nonEmpty)
        FewClassCount(uncovered.toVector.map(unc => (frame -> unc).toString))
      } ::
    HNil
  }
}
