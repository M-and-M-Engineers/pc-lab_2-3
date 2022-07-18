package pc.examples

import pc.modelling.PetriNet
import pc.modelling.PetriNet.*
import pc.utils.MSet

object PNMutualExclusion extends App {

  enum Place:
    case N, T, C

  import Place.*

  // DSL-like specification of A Petri Net
  def mutualExclusionSystem = toSystem(PetriNet[Place](
    MSet(N) ~~> MSet(T),
    MSet(T) ~~> MSet(C) ^^^ MSet(C),
    MSet(C) ~~> MSet())
  )

  // example usage
  println(mutualExclusionSystem.paths(MSet(N,N),7).toList.mkString("\n"))
}
