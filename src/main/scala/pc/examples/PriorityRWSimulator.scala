package pc.examples

import pc.modelling.*
import pc.modelling.PetriNet.*
import pc.modelling.PriorityPetriNet
import pc.modelling.PriorityPetriNet.*
import pc.utils.MSet

object PriorityRWSimulator extends App {

  enum Place:
    case INITIAL, CHOICE, ASK_READING, ASK_WRITING, MUTEX, READING, WRITING

  import Place.*

  // Alternative Solution to The Designer (version with priorities)
  def priorityRW: System[MSet[Place]] = PriorityPetriNet.toSystem(PriorityPetriNet[Place](
    MSet(INITIAL) ~~> MSet(CHOICE) --> 10,
    MSet(CHOICE) ~~> MSet(ASK_READING) --> 10,
    MSet(CHOICE) ~~> MSet(ASK_WRITING) --> 10,
    MSet(ASK_READING, MUTEX) ~~> MSet(READING, MUTEX) --> 100,
    (MSet(ASK_WRITING, MUTEX) ~~> MSet(WRITING) ^^^ MSet(READING)) --> 10,
    MSet(READING) ~~> MSet(INITIAL) --> 10,
    MSet(WRITING) ~~> MSet(INITIAL, MUTEX) --> 10
  ))

  priorityRW.paths(MSet(INITIAL,INITIAL,MUTEX), 6).foreach(println(_))
}
