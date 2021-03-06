package pc.modelling

import org.scalatest.flatspec.AnyFlatSpec

class SystemChannelSpec extends AnyFlatSpec {
  import pc.examples.SystemChannel, pc.examples.SystemChannel.State.*

  private val ch = SystemChannel.channel

  "System Channel" should "properly identify normal forms" in {
    assert(!ch.normalForm(IDLE))
    assert(ch.normalForm(DONE))
  }

  "System Channel" should "properly draw next states" in {
    assertResult(Set(SEND))(ch.next(IDLE))
    assertResult(Set(SEND, DONE, FAIL))(ch.next(SEND))
  }

  "System Channel" should "properly generate paths" in {
    assert(ch.paths(IDLE,3).contains(List(IDLE, SEND, SEND)))

    assertResult(
      List(List(IDLE, SEND, DONE), List(IDLE, SEND, SEND, DONE))
    )(
      ch.completePathsUpToDepth(IDLE,4).toList
    )

    assert(ch.completePaths(IDLE).contains(
           List(IDLE, SEND, SEND, SEND, FAIL, IDLE, SEND, DONE)))
  }
}
