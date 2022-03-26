package brbo.backend.verifier.modelchecker

import apron.memory.ObjectManager

object ApronMemoryManager {
  private var abstract0Released: Set[Long] = Set()
  private var linexpr0Released: Set[Long] = Set()
  private var dimchangeReleased: Set[Long] = Set()
  private var texpr0InternReleased: Set[Long] = Set()
  private var mpfrReleased: Set[Long] = Set()
  private var mpzReleased: Set[Long] = Set()
  private var mpqReleased: Set[Long] = Set()
  private var environmentReleased: Set[Long] = Set()
  private var randStateReleased: Set[Long] = Set()
  private var dimpermReleased: Set[Long] = Set()

  private val objectManager = new ObjectManager()

  def releaseMemory(): Unit = {
    objectManager.getAbstract0Map.forEach({
      case (pointer, o) =>
        if (!abstract0Released.contains(pointer)) {
          o.finalize()
          abstract0Released = abstract0Released + pointer
        }
    })
    objectManager.getDimchangeMap.forEach({
      case (pointer, o) =>
        if (!dimchangeReleased.contains(pointer)) {
          o.finalize()
          dimchangeReleased = dimchangeReleased + pointer
        }
    })
    objectManager.getTexpr0InternMap.forEach({
      case (pointer, o) =>
        if (!texpr0InternReleased.contains(pointer)) {
          o.finalize()
          texpr0InternReleased = texpr0InternReleased + pointer
        }
    })
    objectManager.getMpfrMap.forEach({
      case (pointer, o) =>
        if (!mpfrReleased.contains(pointer)) {
          o.finalize()
          mpfrReleased = mpfrReleased + pointer
        }
    })
    objectManager.getMpqMap.forEach({
      case (pointer, o) =>
        if (!mpqReleased.contains(pointer)) {
          o.finalize()
          mpqReleased = mpqReleased + pointer
        }
    })
    objectManager.getMpzMap.forEach({
      case (pointer, o) =>
        if (!mpzReleased.contains(pointer)) {
          o.finalize()
          mpzReleased = mpzReleased + pointer
        }
    })
    objectManager.getEnvironmentMap.forEach({
      case (pointer, o) =>
        if (!environmentReleased.contains(pointer)) {
          o.finalize()
          environmentReleased = environmentReleased + pointer
        }
    })
    objectManager.getDimpermMap.forEach({
      case (pointer, o) =>
        if (!dimpermReleased.contains(pointer)) {
          o.finalize()
          dimpermReleased = dimpermReleased + pointer
        }
    })
    objectManager.getRandStateMap.forEach({
      case (pointer, o) =>
        if (!randStateReleased.contains(pointer)) {
          o.finalize()
          randStateReleased = randStateReleased + pointer
        }
    })
    /*objectManager.getLinexpr0Map.forEach({
      case (pointer, o) =>
        if (!linexpr0Released.contains(pointer)) {
          o.finalize()
          linexpr0Released = linexpr0Released + pointer
        }
    })*/
  }
}
