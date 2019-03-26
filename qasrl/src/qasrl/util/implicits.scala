package qasrl.util

import cats.Monad
import cats.Order
import cats.arrow.Arrow
import cats.data.NonEmptyList
import cats.data.Ior
import cats.implicits._

import monocle.Lens
import monocle.function.At
import monocle.function.Index

import io.circe.{Encoder, Decoder}
import io.circe.{KeyEncoder, KeyDecoder}
import io.circe.HCursor
import io.circe.Json

import nlpdata.util.LowerCaseStrings._

object implicits {

  implicit def dependentMapAt[F[_], G[_], I]: At[DependentMap[F, G], F[I], Option[G[I]]] =
    At[DependentMap[F, G], F[I], Option[G[I]]](
      i => map => map.get(i))(
      i => optV => map => optV.fold(map.remove(i))(v => map.put(i, v))
    )
  implicit def dependentMapIndex[F[_], G[_], I]: Index[DependentMap[F, G], F[I], G[I]] = Index.fromAt

  implicit val lowerCaseStringEncoder: Encoder[LowerCaseString] =
    Encoder.instance[LowerCaseString](l => Json.fromString(l.toString))
  implicit val lowerCaseStringDecoder: Decoder[LowerCaseString] =
    Decoder.instance[LowerCaseString](c => c.as[String].map(_.lowerCase))

  trait DependentEncoder[F[_], G[_]] {
    def getEncoder[A](fa: F[A]): Encoder[G[A]]
  }

  trait DependentDecoder[F[_], G[_]] {
    def getDecoder[A](fa: F[A]): Decoder[G[A]]
  }

  implicit def dependentMapEncoder[F[_], G[_]](
    implicit keyEncoder: KeyEncoder[F[_]],
    dependentEncoder: DependentEncoder[F, G]
  ): Encoder[DependentMap[F, G]] = new Encoder[DependentMap[F, G]] {
    final def apply(m: DependentMap[F, G]) = Json.obj(
      m.iterator.map(pair =>
        keyEncoder(pair.fst) -> dependentEncoder.getEncoder(pair.fst)(pair.snd)
      ).toSeq: _*
    )
  }

  // TODO REALLY NEED TO FIX THIS UP!!!

  private case class Foo[F[_], A](fa: F[A]) {
    type Arg = A
  }

  implicit def dependentMapDecoder[F[_], G[_]](
    implicit keyDecoder: KeyDecoder[F[_]],
    dependentDecoder: DependentDecoder[F, G]
  ): Decoder[DependentMap[F, G]] = new Decoder[DependentMap[F, G]] {
    final def apply(c: HCursor): Decoder.Result[DependentMap[F, G]] = {
      // TODO aah replace the get
      c.keys.get.toList.foldM[Decoder.Result, DependentMap[F, G]](DependentMap.empty[F, G]) { (m, keyStr) =>
        import scala.language.existentials
        val key = keyDecoder(keyStr).get // TODO aah replace the get
        val value = dependentDecoder.getDecoder(key).tryDecode(c.downField(keyStr))
        val foo = Foo(key)
        type Arg = foo.Arg
        value.map(v => m.put[Arg](key.asInstanceOf[F[Arg]], v.asInstanceOf[G[Arg]]))
      }
    }
  }

  implicit class RichOption[A](val a: Option[A]) extends AnyVal {

    def ifEmpty[B](b: => B): Option[B] = a match {
      case Some(_) => None
      case None    => Some(b)
    }
  }

  implicit class RichNonEmptyList[A](val as: NonEmptyList[A]) extends AnyVal {

    def partition[B, C](f: A => Either[B, C]): Ior[NonEmptyList[B], NonEmptyList[C]] = {
      val init = f(as.head).fold(
        b => Ior.Left(NonEmptyList.of(b)),
        c => Ior.Right(NonEmptyList.of(c))
      ): Ior[NonEmptyList[B], NonEmptyList[C]]
      as.tail.map(f).foldLeft(init) {
        case (Ior.Left(bs), Left(b))      => Ior.Left(b :: bs)
        case (Ior.Right(cs), Left(b))     => Ior.Both(NonEmptyList.of(b), cs)
        case (Ior.Both(bs, cs), Left(b))  => Ior.Both(b :: bs, cs)
        case (Ior.Left(bs), Right(c))     => Ior.Both(bs, NonEmptyList.of(c))
        case (Ior.Right(cs), Right(c))    => Ior.Right(c :: cs)
        case (Ior.Both(bs, cs), Right(c)) => Ior.Both(bs, c :: cs)
      }
    }

    // taken from latest cats; holdover until version upgrade
    def sorted[AA >: A](implicit AA: Order[AA]): NonEmptyList[AA] = {
      NonEmptyList.fromListUnsafe(as.toList.sorted(AA.toOrdering))
    }

    // taken from latest cats; holdover until version upgrade
    def init: List[A] = as.tail match {
      case Nil => Nil
      case t   => as.head :: t.init
    }

    // taken from latest cats; holdover until version upgrade
    def last: A = as.tail.lastOption match {
      case None    => as.head
      case Some(a) => a
    }

  }

  implicit class RichNonEmptyListCompanion(val nel: NonEmptyList.type) extends AnyVal {

    // taken from latest cats; holdover until version upgrade
    def ofInitLast[A](init: List[A], last: A): NonEmptyList[A] =
      init match {
        case Nil    => NonEmptyList(last, Nil)
        case h :: t => NonEmptyList(h, t :+ last)
      }
  }

  // taken from latest cats; holdover until version upgrade
  implicit class RichArrow[F[_, _], A, B](val f: F[A, B])(implicit F: Arrow[F]) {

    def &&&[C](g: F[A, C]): F[A, (B, C)] = {
      F.andThen(F.lift((x: A) => (x, x)), F.split(f, g))
    }
  }

  implicit class RichIorSame[A](val ior: Ior[A, A]) extends AnyVal {

    def mergeM[M[_]: Monad](f: (A, A) => M[A]) = ior.fold(
      Monad[M].pure,
      Monad[M].pure,
      f
    )
  }

}
