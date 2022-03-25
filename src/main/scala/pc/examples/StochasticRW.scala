package pc.examples

import pc.modelling.SPN.toCTMC
import pc.modelling.{CTMCSimulation, SPN}
import pc.utils.MSet

import java.util.Random

// The Guru
object StochasticRW extends App {

  object place extends Enumeration {
    val INITIAL,CHOICE,ASK_READING,ASK_WRITING,MUTEX,READING,WRITING = Value
  }
  import place._
  type Place = place.Value

  val spn = SPN[Place](
    (MSet(INITIAL), _ => 1.0, MSet(CHOICE), MSet()),
    (MSet(CHOICE), _ => 200000, MSet(ASK_READING), MSet()),
    (MSet(CHOICE), _ => 100000, MSet(ASK_WRITING), MSet()),
    (MSet(ASK_READING, MUTEX), _ => 100000, MSet(READING, MUTEX), MSet()),
    (MSet(ASK_WRITING, MUTEX), _ => 100000, MSet(WRITING), MSet(READING)),
    (MSet(READING), m => 0.1 * m(READING), MSet(INITIAL), MSet()),
    (MSet(WRITING), _ => 0.2, MSet(INITIAL, MUTEX), MSet())
  )

  val rwSimulator = CTMCSimulation(toCTMC(spn))
  println(rwSimulator.newSimulationTrace(MSet(MUTEX,INITIAL,INITIAL,INITIAL,INITIAL,INITIAL,INITIAL,INITIAL),new Random)
    .take(30)
    .toList.mkString("\n"))
}
