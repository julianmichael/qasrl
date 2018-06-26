package qasrl

package object util {

  // superseded by `Align` once that's in cats
  import cats.data.Ior
  def mergeMaps[A, B](x: Map[A, B], y: Map[A, B]): Map[A, Ior[B, B]] = {
    val keySet = x.keySet ++ y.keySet
    keySet.iterator.map { key =>
      key -> Ior.fromOptions(x.get(key), y.get(key)).get // should always work
    }.toMap
  }

}
