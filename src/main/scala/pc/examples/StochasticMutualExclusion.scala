package pc.examples

import pc.modelling.{CTMCSimulation, SPN}
import pc.utils.MSet
import java.util.Random

object StochasticMutualExclusion extends App {
  // Specification of my data-type for states
  enum Place:
    case N, T, C
  
  import SPN.*
  import Place.*

  val spn = SPN[Place](
    (MSet(N), _ =>1.0,MSet(T),MSet()),
    (MSet(T), m=>m(T),MSet(C),MSet(C)),
    (MSet(C), _ =>2.0,MSet(),MSet()))


  val rwSimulator = CTMCSimulation(toCTMC(spn))
  println(rwSimulator.newSimulationTrace(MSet(N,N,N,N),new Random)
                    .take(20)
                    .toList.mkString("\n"))
}