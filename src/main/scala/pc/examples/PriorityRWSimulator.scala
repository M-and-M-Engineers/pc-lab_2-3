package pc.examples

import pc.modelling.PriorityPetriNet
import pc.modelling.PriorityPetriNet._
import pc.utils.MSet

object PriorityRWSimulator extends App {

  object place extends Enumeration {
    val INITIAL,CHOICE,ASK_READING,ASK_WRITING,MUTEX,READING,WRITING = Value
  }
  import place._
  type Place = place.Value

  // Alternative Solution to The Designer (version with priorities)
  def priorityRW = toSystem(PriorityPetriNet[Place](
    MSet(INITIAL) ~~> MSet(CHOICE) --> 10,
    MSet(CHOICE) ~~> MSet(ASK_READING) --> 10,
    MSet(CHOICE) ~~> MSet(ASK_WRITING) --> 10,
    MSet(ASK_READING, MUTEX) ~~> MSet(READING, MUTEX) --> 100,
    (MSet(ASK_WRITING, MUTEX) ~~> MSet(WRITING) ^^^ MSet(READING)) --> 10,
    MSet(READING) ~~> MSet(INITIAL) --> 10,
    MSet(WRITING) ~~> MSet(INITIAL, MUTEX) --> 10
  ))

  priorityRW.paths(MSet(INITIAL,INITIAL,MUTEX), 6).par.foreach(println(_))
}
