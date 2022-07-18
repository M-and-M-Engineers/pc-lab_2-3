package pc.modelling

import pc.utils.MSet

import scala.annotation.targetName

object PetriNet {
  // pre-conditions, effects, inhibition
  opaque type PetriNet[P] = Set[(MSet[P],MSet[P],MSet[P])]

  // factory of A Petri Net
  def apply[P](transitions: (MSet[P],MSet[P],MSet[P])*): PetriNet[P] =
    transitions.toSet

  def toPartialFunction[P](pn: PetriNet[P]): PartialFunction[MSet[P],Set[MSet[P]]] =
    {case m => for (cond,eff,inh)<-pn
                    if m disjoined inh
                    out <- m extract cond yield out union eff }

  // factory of A System
  def toSystem[P](pn: PetriNet[P]): System[MSet[P]] =
    System.ofFunction( toPartialFunction(pn))

  // Syntactic sugar to write transitions as:  MSet(a,b,c) ~~> MSet(d,e)
  extension [P] (self: MSet[P])
    @targetName("into")
    def ~~>(y: MSet[P]): (MSet[P], MSet[P], MSet[P]) = Tuple3(self, y, MSet[P]())

  extension [P] (self: (MSet[P], MSet[P], MSet[P]))
    @targetName("only if")
    def ^^^(z: MSet[P]): (MSet[P], MSet[P], MSet[P]) = Tuple3(self._1, self._2, z)

}