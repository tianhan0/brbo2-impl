package brbo.common

import org.apache.commons.lang3.RandomStringUtils

object RandomString {
  private val MAX_RETRIES = 100

  def generate(avoid: Iterable[String], length: Int = 5): String = {
    var count = 0
    var result = RandomStringUtils.randomAlphabetic(length)
    while (avoid.exists(i => i == result)) {
      if (count > MAX_RETRIES)
        throw new Exception(s"Cannot generate a random string with length `$length` that avoids `$avoid`")
      result = RandomStringUtils.randomAlphabetic(length)
      count = count + 1
    }
    result
  }
}
