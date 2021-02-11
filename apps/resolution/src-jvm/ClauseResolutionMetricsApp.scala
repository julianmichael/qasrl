package qasrl.apps.resolution

import cats.Foldable
import cats.Order
import cats.data.NonEmptyList
import cats.implicits._
import cats.effect._

import com.monovore.decline._
import com.monovore.decline.effect._

import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files

import jjm.LowerCaseString
import jjm.ling.Text
import jjm.ling.en.InflectedForms
import jjm.ling.en.VerbForm
import jjm.metrics._
import jjm.implicits._

import qasrl.bank.Data
import qasrl.bank.SentenceId

import qasrl.data.{AnswerJudgment, Answer, InvalidQuestion}
import qasrl.data.Dataset
import qasrl.data.QuestionLabel
import qasrl.data.Sentence
import qasrl.data.VerbEntry

/**
 * Computes and prints metrics w.r.t. clause resolution on the QA-SRL Bank.
 * Also has a lot of old stuff commented out... can sort through at some
 * point. Also (TODO) this needs an update with proper command line args to
 * pass in the path of the QA-SRL Bank.
 */
object ClauseResolutionMetricsApp extends App {

  // import qfirst.protocols._

  // def getSentenceBeamString(
  //   pred: SentencePrediction[FactoringProtocol.Beam[QfirstBeamItem[Map[String, String]]]]
  // ) = ""

  // def printBeams = {
  //   import fs2.Stream
  //   import io.circe.generic.auto._
  //   FileUtil.readJsonLines[
  //     SentencePrediction[FactoringProtocol.Beam[QfirstBeamItem[Map[String, String]]]]
  //   ](Paths.get("predictions/qfirst-clausal_no_anim/predictions.jsonl")).flatMap(s =>
  //     Stream.eval(IO(println(getSentenceBeamString(s))))
  //   ).compile.drain
  // }

  // def run(args: List[String]): IO[ExitCode] = for {
  //   _ <- IO.unit
  //   // _ <- printBeams
  // } yield ExitCode.Success

  lazy val train = Data.readQasrlDataset(Paths.get("data/qasrl-v2_1").resolve("orig").resolve("train.jsonl.gz")).get
  lazy val dev = Data.readQasrlDataset(Paths.get("data/qasrl-v2_1").resolve("orig").resolve("dev.jsonl.gz")).get
  lazy val denseDev = Data.readQasrlDataset(Paths.get("data/qasrl-v2_1").resolve("dense").resolve("dev.jsonl.gz")).get
  implicit val datasetMonoid = Dataset.datasetMonoid(Dataset.printMergeErrors)

  lazy val sortSpec = {
    import Metric._
    import MapTree.SortQuery._
    val double = (mv: Metric) => mv match {
      case MetricMetadata(s) => 0.0
      case MetricBool(x) => if(x) 1.0 else 0.0
      case MetricInt(x) => x.toDouble
      case MetricDouble(x) => x
      case MetricIntOfTotal(x, _) => x.toDouble
    }
    val inc = value[String](double)
    val dec = value[String](double andThen (_ * -1))
    List(
      "predictions" :: "f1" :: inc,
      "full question" :: "f1" :: inc,
      "full question" :: "acc-lb" :: inc,
      "num predicted" :: inc,
      "mean" :: inc
    )
  }

  def getMetricsString[M: HasMetrics](m: M) =
    m.getMetrics.toStringPrettySorted(identity, x => x.render, sortSpec)

  import jjm.metrics.{Transformers => M}
  import shapeless._
  import shapeless.syntax.singleton._
  import shapeless.record._
  import monocle.function.{all => Optics}

  // import qfirst.frames.TemplateStateMachine.allPrepositions

  // def getPrepMetrics(sentence: Sentence, verb: VerbEntry) = (qLabel: QuestionLabel) => {
  //   val hasPrep = qLabel.answerJudgments
  //     .flatMap(_.judgment.getAnswer)
  //     .flatMap(_.spans)
  //     .map(s =>
  //     (
  //       if(allPrepositions.contains(sentence.sentenceTokens(s.begin).lowerCase)) 3
  //       else if(sentence.sentenceTokens.lift(s.begin - 1).map(_.lowerCase).exists(allPrepositions.contains)) 2
  //       else 1
  //     )
  //   ).max match {
  //     case 1 => "N"
  //     case 2 => "-"
  //     case 3 => "Y"
  //   }
  //   Bucketed(
  //     Map(
  //       Map("wh" -> qLabel.questionSlots.wh.toString) ->
  //         Bucketed(
  //           Map(
  //             Map("has prep" -> hasPrep) -> 1
  //           )
  //         )
  //     )
  //   )
  // }

  // lazy val prepMetrics = train.sentences.values.toList.foldMap { sentence =>
  //   sentence.verbEntries.values.toList.foldMap { verb =>
  //     verb.questionLabels.values.toList.foldMap {
  //       getPrepMetrics(sentence, verb)
  //     }
  //   }
  // }

  // val getSpanLengthMetrics = (aj: AnswerJudgment) => aj match {
  //   case InvalidQuestion => Counts(0)
  //   case Answer(spans) => spans.toList.foldMap(s => Counts(s.end - s.begin))
  // }

  // val spanLengthMetrics = train.sentences.values.toList.foldMap { sentence =>
  //   sentence.verbEntries.values.toList.foldMap { verb =>
  //     verb.questionLabels.values.toList.foldMap { qLabel =>
  //       qLabel.answerJudgments.toList.map(_.judgment).foldMap(getSpanLengthMetrics)
  //     }
  //   }
  // }

  // val getSpanCounts = (sentence: Sentence) => {
  //   sentence.verbEntries.values.toList.foldMap { verb =>
  //     verb.questionLabels.values.toList.foldMap { qLabel =>
  //       qLabel.answerJudgments.toList.map(_.sourceId).foldMap(s => Map(s -> 1))
  //     }.values.toList.foldMap(Counts(_))
  //   }
  // }

  // val getSentenceLength = (sentence: Sentence) => {
  //   Counts(sentence.sentenceTokens.size)
  // }

  // val getTanCounts = (verb: VerbEntry) => {
  //   "TANs" ->> Counts(verb.questionLabels.values.toList.map(l =>
  //     (l.tense, l.isPerfect, l.isProgressive, l.isNegated)
  //   ).toSet.size) ::
  //     "TANs Without Negation" ->> Counts(verb.questionLabels.values.toList.map(l =>
  //       (l.tense, l.isPerfect, l.isProgressive)
  //     ).toSet.size) :: HNil
  // }

  // val trainTanMetrics = train.sentences.values.toList.flatMap(_.verbEntries.values.toList).foldMap(getTanCounts)
  // val devTanMetrics = dev.sentences.values.toList.flatMap(_.verbEntries.values.toList).foldMap(getTanCounts)
  // println("Train TAN counts: " + getMetricsString(trainTanMetrics))
  // println("Train TAN hist:\n" + trainTanMetrics("TANs").histogramString(75))
  // println("Train TAN hist (no negation):\n" + trainTanMetrics("TANs Without Negation").histogramString(75))
  // println("Dev tan counts: " + getMetricsString(devTanMetrics))
  // println("Dev TAN hist:\n" + devTanMetrics("TANs").histogramString(75))
  // println("Dev TAN hist (no negation):\n" + devTanMetrics("TANs Without Negation").histogramString(75))

  // println(getMetricsString(spanLengthMetrics))
  // println(spanLengthMetrics.histogramString(75))
  // println("Train span counts: " + getMetricsString(train.sentences.values.toList.foldMap(getSpanCounts)))
  // println("Dev span counts: " + getMetricsString(dev.sentences.values.toList.foldMap(getSpanCounts)))

  // val trainMetrics = train.sentences.values.toList.foldMap(getSentenceLength)
  // println("Train sentence lengths: " + getMetricsString(trainMetrics))
  // println(trainMetrics.histogramString(75))

  // val getDenseMetrics = (sentence: Sentence) => {
  //   sentence.verbEntries.values.toList.foldMap(verb =>
  //     "num questions" ->> Counts(verb.questionLabels.size) ::
  //       "num valid questions" ->> Counts(filterGoldDense(verb)._2.size) ::
  //       HNil
  //   )
  // }

  // val denseMetrics = denseDev.sentences.values.toList.foldMap(getDenseMetrics)
  // println(getMetricsString(denseMetrics))

  import qasrl._
  import qasrl.labeling._

  // def getPreferredCompleteState(states: NonEmptyList[QuestionProcessor.ValidState]) = {
  //   // prioritize a frame with Obj (but no Obj2) over one with Obj2 (but no Obj)
  //   val completeStates = states.collect { case cs @ QuestionProcessor.CompleteState(_, _, _) => cs }
  //   completeStates.find(_.frame.args.get(Obj).nonEmpty).orElse(completeStates.headOption)
  // }

  import ClauseResolution._

  val getMetrics = (sentence: Sentence) => {
    sentence.verbEntries.values.toList.foldMap { verb =>
      val qLabels = verb.questionLabels.values.toList
      "questions" ->> Count(qLabels.map(_.questionSlots)) ::
        "questions without wh" ->> Count(qLabels.map(_.questionSlots).map(_.copy(wh = "what".lowerCase))) ::
        "questions per verb" ->> Counts(qLabels.map(_.questionSlots).toSet.size) ::
        "questions without wh per verb" ->> Counts(qLabels.map(_.questionSlots).map(_.copy(wh = "what".lowerCase)).toSet.size) ::
        "frames" ->> {
          val frameSets = qLabels.map(_.questionSlots).map(getFramesWithAnswerSlots(_))
          val locallyResolvedFramePairSets = locallyResolve(frameSets)
          def getAmbiguities(framePairSets: List[Set[(Frame, ArgumentSlot)]]) = {
            qLabels.map(_.questionSlots).zip(framePairSets).map(Function.tupled(classifyAmbiguity(_, _))).foldMap(FewClassCount(_))
          }
          val resolvedFramePairSets = qLabels.map(_.questionSlots).zip(locallyResolvedFramePairSets).map(Function.tupled(fallbackResolve(_, _)))
          "count (fully resolved) without answer slot" ->> Count(resolvedFramePairSets.map(p => (p._1.args, p._1.isPassive, p._1.isProgressive, p._1.isPerfect, p._1.isNegated, p._1.tense))) ::
          "count (fully resolved)" ->> Count(resolvedFramePairSets.map(p => (p._1.args, p._1.isPassive, p._1.isProgressive, p._1.isPerfect, p._1.isNegated, p._1.tense, p._2))) ::
          "per question" ->> frameSets.foldMap(l => Counts(l.size)) ::
            "ambiguities" ->> getAmbiguities(frameSets) ::
            "per question (locally resolved)" ->> locallyResolvedFramePairSets.foldMap(l => Counts(l.size)) ::
            "ambiguities (locally resolved)" ->> getAmbiguities(locallyResolvedFramePairSets) ::
            "per verb (fully resolved) without answer slot" ->> Counts(resolvedFramePairSets.map(_._1).toSet.size) ::
            HNil
        } :: HNil
    }
  }

  val data = train |+| dev
  val metrics = data.sentences.values.toList.foldMap(getMetrics)
  println(getMetricsString(metrics))
  println(metrics("frames").apply("per question").histogramString(75))
  println(metrics("frames").apply("per question (locally resolved)").histogramString(75))

  // def print2PrepExamples(dataset: Dataset) = {
  //   for(sentence <- dataset.sentences.values) {
  //     for(verb <- sentence.verbEntries.values) {
  //       val qLabels = verb.questionLabels.values.toList
  //       val hasTwoPreps = qLabels.exists(_.questionSlots.prep.exists(_.contains(" ".lowerCase)))
  //       def endsWithSomething(q: QuestionLabel) = q.questionString.endsWith("something?")
  //       def hasSubj(q: QuestionLabel) = q.questionSlots.subj.nonEmpty
  //       def isNounQ(q: QuestionLabel) = Set("who", "what").contains(q.questionSlots.wh.toString)
  //       val isGoodCandidate = qLabels.exists(q =>
  //         endsWithSomething(q) && hasSubj(q) && isNounQ(q)
  //       )
  //       if(hasTwoPreps && isGoodCandidate) {
  //         println("=============================================")
  //         println(Text.render(sentence.sentenceTokens))
  //         println(sentence.sentenceTokens(verb.verbIndex) + s"(${verb.verbIndex})")
  //         for(q <- qLabels) {
  //           val answersString = q.answerJudgments.flatMap(_.judgment.getAnswer).flatMap(_.spans).toList
  //             .sortBy(_.begin).map(s => Text.renderSpan(sentence.sentenceTokens, (s.begin until s.end).toSet))
  //             .mkString(" / ")
  //           println(f"${q.questionString}%60s " + answersString)
  //         }
  //       }
  //     }
  //   }
  // }

  // print2PrepExamples(dev)
  // print2PrepExamples(denseDev)
  // print2PrepExamples(train)

}

object RankerPrinting {
  // def printInstanceExample(vi: Int, instance: ClauseInstance) = IO {
  //   println(Text.render(instance.sentenceTokens))
  //   println(instance.sentenceTokens(vi) + s" (${instance.sentenceTokens(vi)}) ")
  //   instance.clauses.sortBy(-_._2).foreach { case (clause, prob) =>
  //     println(f"$prob%.3f ${clause.getClauseString(instance.sentenceTokens, instance.verbInflectedForms)}")
  //   }
  // }

  // def printExamplesWithString(
  //   instances: Map[String, Map[Int, ClauseInstance]],
  //   verbString: String,
  //   string: String
  // ) = {
  //   instances
  //     .filter(t => Text.render(t._2.head._2.sentenceTokens).contains(string))
  //     .toList.flatMap(t => t._2.toList.filter(p => p._2.sentenceTokens(p._1) == verbString))
  //     .traverse(Function.tupled(printInstanceExample))
  // }

  // def printExamples(path: String, n: Int) = for {
  //   clauseInstances <- MetricsApp.readRankingPredictions(Paths.get(path))
  //   _ <- clauseInstances.iterator.take(n).toList.traverse { case (sid, verbs) =>
  //     verbs.toList.traverse(Function.tupled(printInstanceExample))
  //   }
  // } yield ()

  // val exStrings = List(
  //   "qualified" -> "The game was the first for Great Britain",
  //   "consumed" -> "The theory of supply and demand is an organizing principle",
  //   "seen" -> "New South Wales have seen"
  // )

  // def printChosenExamples(path: String) = for {
  //   clauseInstances <- MetricsApp.readRankingPredictions(Paths.get(path))
  //   _ <- exStrings.traverse { case (vs, s) => printExamplesWithString(clauseInstances, vs, s) }
  // } yield ()

  // printExamples(args(0), args(1).toInt).unsafeRunSync
  // printChosenExamples(args(0)).unsafeRunSync
}
