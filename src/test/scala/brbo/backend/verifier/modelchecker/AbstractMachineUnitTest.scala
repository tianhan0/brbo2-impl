package brbo.backend.verifier.modelchecker

import org.scalatest.flatspec.AnyFlatSpec

class AbstractMachineUnitTest extends AnyFlatSpec {
  "" should "succeed" in {
    // val machine = new AbstractMachine(???)
  }
}

// Under Octagon / Polka:
// pre-condition: r* <= a
// command: r* = r* + a
// post-condition: ???

// Under Octagon / Polka:
// pre-condition: r* <= a
// command: r* = b
// post-condition: ???

// Under Octagon / Polka:
// pre-condition: r <= a
// command: r = r + 1
// post-condition: ???