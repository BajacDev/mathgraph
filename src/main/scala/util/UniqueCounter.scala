package mathgraph.util

import scala.collection.mutable.Map

// Generates unique counters for each element of a type K, starting at 0
class UniqueCounter[K] {
  private val elemIds = Map[K, Int]().withDefaultValue(-1)

  def next(key: K): Int = synchronized {
    elemIds(key) += 1
    elemIds(key)
  }
}
