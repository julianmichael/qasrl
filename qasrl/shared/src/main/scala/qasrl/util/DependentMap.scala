package qasrl.util

import cats.Eq
import cats.~>
import cats.Monoid
import cats.MonoidK
import cats.implicits._

final class DependentMap[F[_], G[_]] private (private val map: Map[F[_], G[_]]) {

  def get[A](key: F[A]): Option[G[A]] = map.get(key).asInstanceOf[Option[G[A]]]

  def put[A](key: F[A], value: G[A]): DependentMap[F, G] = new DependentMap[F, G](map + (key -> value).asInstanceOf[(F[_], G[_])])

  def remove[A](key: F[A]): DependentMap[F, G] = new DependentMap[F, G](map - key)

  def keys: Iterable[F[_]] = map.keys

  private[this] def getPair[A0](key: F[A0]): Option[DependentPair[F, G] { type A = A0 }] = get(key).map(DependentPair[F, G, A0](key, _))
  def iterator: Iterator[DependentPair[F, G]] = map.keys.iterator.map(key => getPair(key).get)

  def size: Int = map.size

  override def toString = map.toString

  def toNaturalTransformation = new (F ~> λ[A => Option[G[A]]]) {
    override def apply[A](fa: F[A]): Option[G[A]] = get(fa)
  }
}
object DependentMap {
  def empty[F[_], G[_]] = new DependentMap[F, G](Map.empty[F[_], G[_]])

  implicit def dependentMapEq[F[_], G[_]] = new Eq[DependentMap[F, G]] {
    override def eqv(x: DependentMap[F, G], y: DependentMap[F, G]): Boolean =
      x.map == y.map
  }

  implicit def dependentMapMonoid[F[_], G[_]: MonoidK]: Monoid[DependentMap[F, G]] = new Monoid[DependentMap[F, G]] {
    override def empty = new DependentMap[F, G](Map.empty[F[_], G[_]])
    override def combine(x: DependentMap[F, G], y: DependentMap[F, G]) = {
      (x.keys ++ y.keys).toSet.foldLeft(empty) {
        case (dMap, key) => dMap.put(key, (x.get(key).foldK <+> y.get(key).foldK))
      }
    }
  }
}
