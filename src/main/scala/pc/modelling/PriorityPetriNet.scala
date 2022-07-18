package pc.modelling

import pc.utils.MSet

import scala.annotation.targetName

// The Artist
object PriorityPetriNet {

  // pre-conditions, effects, inhibition, priority
  type PriorityPetriNet[P] = Set[(MSet[P],MSet[P],MSet[P],Int)]

  // factory of A Priority Petri Net
  def apply[P](transitions: (MSet[P],MSet[P],MSet[P], Int)*): PriorityPetriNet[P] =
    transitions.toSet

  /* Dimostrazione della soluzione proposta:
     Per eseguire soltanto le transizione con la priorità più alta, viene ricercata la priorità più alta tra tutte le
     transizioni effettuabili nello stato attutale. Successivamente vengono filtrate tutte le transizioni effettuabili
     aggiungendo come condizione quella di avere la priorità pari a quella calcolata precedentemente. In questo modo il
     Set ottenuto contiene solo le transizioni aventi la priorità più alta tra quelle disponibili.
   */
  def toPartialFunction[P](pn: PriorityPetriNet[P]): PartialFunction[MSet[P],Set[MSet[P]]] = {
    case m =>
      var maxPriority = Int.MinValue
      val res = for
        (cond, eff, inh, priority) <- pn
        if m disjoined inh
        out <- m extract cond
      yield {
        if priority > maxPriority then
          maxPriority = priority
        (out union eff, priority)
      }

      for (solution, priority) <- res
           if priority == maxPriority
        yield solution
  }

  // factory of A System
  def toSystem[P](pn: PriorityPetriNet[P]): System[MSet[P]] =
    System.ofFunction( toPartialFunction(pn))

  // Syntactic sugar to write transitions as:  MSet(a,b,c) ~~> MSet(d,e) --> priority
  //                                           OR
  //                                           (MSet(a,b,c) ~~> MSet(d,e) ^^^ MSet(f)) --> priority
  // Priority is designed to be mandatory
  extension [P] (self: (MSet[P], MSet[P], MSet[P]))
    @targetName("with priority")
    def -->(z: Int): (MSet[P], MSet[P], MSet[P], Int) = Tuple4(self._1, self._2, self._3, z)
}
