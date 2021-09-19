package brbo.common

import org.apache.logging.log4j.LogManager

import scala.annotation.tailrec
import scala.collection.immutable.{HashMap, HashSet}

object MathUtils {
  private val logger = LogManager.getLogger(MathUtils.getClass.getName)

  // Generate Cartesian product
  def crossJoin[T](list: Traversable[Traversable[T]]): List[List[T]] = {
    // Even when the input is Set, ensure the output is list-typed
    list.toList.map(e => e.toList) match {
      case Nil => Nil
      case xs :: Nil => xs map (List(_))
      case x :: xs => for {
        i <- x
        j <- crossJoin(xs)
      } yield List(i) ++ j
    }
  }

  def crossJoin2[T](list1: Traversable[T], list2: Traversable[T]): List[(T, T)] = {
    crossJoin(List(list1, list2)).map(list => (list.head, list.tail.head))
  }

  // Given n groups and length k, generate all unique sequences of length k (i.e., modulo renamings)
  def generateUniqueSequences(pathLength: Int, numberOfGroups: Int): Set[List[Int]] = {
    def allPossiblePaths(remainingLength: Int, accumulated: List[Int]): Set[List[Int]] = {
      assert(remainingLength >= 0)
      if (remainingLength == 0) return Set(accumulated.reverse) // Not strictly necessary to reverse the path
      Range.apply(0, numberOfGroups).flatMap({
        nextId => allPossiblePaths(remainingLength - 1, nextId :: accumulated)
      }).toSet
    }

    def generatePermutations(path: List[Int]): Set[List[Int]] = {
      val noDuplicates: List[Int] = removeDuplicates(path)
      val numberOfGroupsUsed = noDuplicates.size
      val renamings: Set[Map[Int, Int]] =
        kGroupsFromN(numberOfGroupsUsed, numberOfGroups).map({
          renaming: List[Int] =>
            assert(renaming.length == noDuplicates.length)
            var map = new HashMap[Int, Int]
            var i = 0
            while (i < renaming.length) {
              map = map + (noDuplicates(i) -> renaming(i))
              i = i + 1
            }
            map
        })
      renamings.map({ renaming => path.map(node => renaming.getOrElse(node, -1)) })
    }

    var result = allPossiblePaths(pathLength, Nil)
    var actualResult = new HashSet[List[Int]]
    while (result.nonEmpty) {
      // TODO: For a set of permutations, always select the same one to be kept
      val toKeep = result.head
      result = result.diff(generatePermutations(toKeep)) // Remove duplicates
      actualResult = actualResult + toKeep
    }
    actualResult
  }

  def removeDuplicates[T](list: List[T]): List[T] = {
    @tailrec
    def helper(visited: Set[T], accumulated: List[T], list2: List[T]): List[T] = {
      list2 match {
        case Nil => accumulated.reverse
        case ::(head, tail) =>
          if (visited.contains(head)) helper(visited, accumulated, tail)
          else helper(visited + head, head :: accumulated, tail)
      }
    }

    helper(Set(), Nil, list)
  }

  // Given n groups (whose IDs are from 0 to n-1), generate all possible sequences that only use k groups, and each group appears exactly once in the sequence
  // For example, if n=3 and k=2, then we return 01, 10, 02, 20, 12, 21.
  // As another example, if n=2 and k=2, then we return 01 and 10.
  def kGroupsFromN(k: Int, n: Int): Set[List[Int]] = {
    def helper(used: Set[Int], path: List[Int]): Set[List[Int]] = {
      if (used.size == k) return Set(path) // Not strictly necessary to reverse the path
      Range.apply(0, n).filter(i => !used.contains(i)).flatMap({
        nextId => helper(used + nextId, nextId :: path)
      }).toSet
    }

    helper(Set(), Nil)
  }
}
