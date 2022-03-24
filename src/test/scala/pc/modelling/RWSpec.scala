package pc.modelling

import org.scalatest.FlatSpec
import pc.examples.DiscreteRW
import pc.examples.DiscreteRW.place._
import pc.utils.MSet

class RWSpec extends FlatSpec {

  val rwV = DiscreteRW.rwV
  val statesNum = 20    // Max number of states to verify.

  "Readers and Writers system" should "have at most 1 Writer active at the same time" in {
    rwV.paths(MSet(INITIAL,INITIAL,MUTEX), statesNum).par.foreach(path => {
      assert(DiscreteRW.maxOneWriter(path))
    })
  }

  it should "have no Readers and Writers active at the same time" in {
    rwV.paths(MSet(INITIAL,INITIAL,MUTEX), statesNum).par.foreach(path => {
      assert(DiscreteRW.noReaderAndWriter(path))
    })
  }
}
