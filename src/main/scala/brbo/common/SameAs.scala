package brbo.common

trait SameAs {
  /**
   *
   * @param other The other object to compare against
   * @return If the two object are literally the same (i.e., they look exactly the same if printed out)
   */
  def sameAs(other: Any): Boolean
}
