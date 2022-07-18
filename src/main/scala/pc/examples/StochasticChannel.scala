package pc.examples

import pc.modelling.CTMC

object StochasticChannel extends App {

  enum State:
    case IDLE, SEND, DONE, FAIL

  import State.*

  def stocChannel: CTMC[State] = CTMC.ofTransitions(
    (IDLE,1.0,SEND),
    (SEND,100000.0,SEND),
    (SEND,200000.0,DONE),
    (SEND,100000.0,FAIL),
    (FAIL,100000.0,IDLE),
    (DONE,1.0,DONE)
  )

  // example run
  State.values.foreach(s => println(s.toString + stocChannel.transitions(s).toString()))
}
