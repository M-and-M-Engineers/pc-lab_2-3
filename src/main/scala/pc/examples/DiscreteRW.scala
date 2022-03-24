package pc.examples

import pc.modelling.PetriNet
import pc.modelling.PetriNet._
import pc.utils.MSet

object DiscreteRW {

  object place extends Enumeration {
    val INITIAL,CHOICE,ASK_READING,ASK_WRITING,MUTEX,READING,WRITING = Value
  }
  import place._
  type Place = place.Value

  // The Verifier task
  def rwV = toSystem(PetriNet[Place](
    MSet(INITIAL) ~~> MSet(CHOICE),
    MSet(CHOICE) ~~> MSet(ASK_READING),
    MSet(CHOICE) ~~> MSet(ASK_WRITING),
    MSet(ASK_READING, MUTEX) ~~> MSet(READING, MUTEX),
    MSet(ASK_WRITING, MUTEX) ~~> MSet(WRITING) ^^^ MSet(READING),
    MSet(READING) ~~> MSet(INITIAL),
    MSet(WRITING) ~~> MSet(INITIAL, MUTEX)
  ))

  // The Designer task
  /* Dimostrazione della correttezza: aggiungendo ASK_READING al set di inibizione per la transizione T5, essa non potrà
     mai essere effettuata, proibendo allo scrittore di scrivere nel caso in cui almeno un lettore voglia leggere. Perciò,
     se un lettore richiede la lettura, egli infine leggerà poiché blocca tutti gli scrittori che hanno intenzione
     di scrivere. */
  def rwD = toSystem(PetriNet[Place](
    MSet(INITIAL) ~~> MSet(CHOICE),
    MSet(CHOICE) ~~> MSet(ASK_READING),
    MSet(CHOICE) ~~> MSet(ASK_WRITING),
    MSet(ASK_READING, MUTEX) ~~> MSet(READING, MUTEX),
    MSet(ASK_WRITING, MUTEX) ~~> MSet(WRITING) ^^^ MSet(READING, ASK_READING), // T5
    MSet(READING) ~~> MSet(INITIAL),
    MSet(WRITING) ~~> MSet(INITIAL, MUTEX)
  ))

  def maxOneWriter(path: List[MSet[Place]]) = !path.contains(MSet(WRITING,WRITING))

  def noReaderAndWriter(path: List[MSet[Place]]) = !path.contains(MSet(WRITING, READING))
}
