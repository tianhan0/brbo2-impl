package brbo.common

class DisjointSet[T](isSame: (T, T) => Boolean,
                     betterRepresentative: (T, T) => Boolean) {
  // A map from representatives to elements that are the same as them
  private var map = Map[T, Set[T]]()

  def add(element: T): Unit = {
    map.keys.find({ representative => isSame(representative, element) }) match {
      case Some(representative) =>
        val newChildren = map(representative) + element
        if (betterRepresentative(element, representative)) {
          map = map - representative + (element -> newChildren)
        } else {
          map = map + (representative -> newChildren)
        }
      case None =>
        map = map + (element -> Set(element))
    }
  }

  def getMap: Map[T, Set[T]] = map
}
