package pc.modelling

import pc.modelling.PetriNet.PetriNet
import pc.utils.MSet

import scala.collection.mutable

trait ActorSystem[ActorType] {

  def execute(initial: MSet[ActorType]): Unit

  def actorsOf(actorType: ActorType, limit: Int): Set[Actor[ActorType]]

  def actorsOf(actorType: ActorType): Set[Actor[ActorType]]

  // Essendo possibile solo una transizione alla volta, è necessario sapere se il determinato attore può eseguire una delle sue transizioni, bloccando il sistema
  def requireLock(actor: Actor[ActorType]): Boolean

  def releaseLock(): Unit

  def spawnActor(actorType: ActorType): Unit

  def actorConsumed(actorType: ActorType, actor: Actor[ActorType]): Unit
}

/* The Brave:
  Nel nostro sistema, gli attori fungono da token eseguendo le transizioni assegnategli inizialmente.
  Per evitare problemi di corse critiche, solo un attore tra quelli appartenenti alla condizione viene scelto come responsabile della specifica transizione;
  gli altri sono considerati elementi passivi che saranno consumati dall'attore responsabile.
  Perciò ad ogni attore viene assegnato l'insieme delle transizioni di cui sarà responsabile.
  Questo significa che potrebbero esistere attori puramente passivi.
  Ogni attore nel momento in cui viene generato, seleziona casualmente una tra le proprie transizioni.
  Innanzitutto verifica se sono presenti gli altri attori necessari all'esecuzione della transizione (token della condizione)
  e nel caso se non sono presenti token di inibizione.
  Nel momento della verifica della condizione, agli attori coinvolti vengono trattenuti negandogli il permesso di eseguire una qualsiasi propria transizione.
  A questo punto, se le condizione e l'inibizione vengono soddisfatte, l'attore richiede al sistema se può eseguire la transizione.
  Infatti il sistema, per evitare ulteriori corse critiche, permette l'esecuzione di una sola transizione alla volta.
  Una volta ricevuto il permesso, l'attore completa la transizione consumando tutti i token (attori) partecipanti (compreso se stesso)
  e generando i nuovi token (attori) come effetto della transizione.
  Se i criteri di condizione per eseguire la transizione non sono soddisfatti, tutti gli attori partecipanti devono essere rilasciati e
  l'attore deve ripartire dalla scelta di una nuova transizione.
*/
object ActorSystem {

  def apply[ActorType](petriNet: PetriNet[ActorType]): ActorSystem[ActorType] = ActorSystemImpl(petriNet)

  private case class ActorSystemImpl[ActorType](petriNet: PetriNet[ActorType]) extends ActorSystem[ActorType] {

    private val activeActors: mutable.Map[ActorType, mutable.Set[Actor[ActorType]]] = mutable.Map.empty

    // Ogni transizione viene assegnata a un solo attore di quelli appartenenti alla condizione
    private val assignedTransitions: mutable.Map[ActorType, PetriNet[ActorType]] =
      petriNet.map(transition => transition._1.asList.head -> Set(transition)).to(mutable.Map)
    private var lockedBy: Option[Actor[ActorType]] = None

    def execute(initial: MSet[ActorType]): Unit = {
      initial.asList.foreach(token => this.spawnActor(token))
    }

    def actorsOf(actorType: ActorType, limit: Int): Set[Actor[ActorType]] = {
      this.activeActors(actorType).take(limit).toSet
    }

    def actorsOf(actorType: ActorType): Set[Actor[ActorType]] = {
      this.activeActors(actorType).toSet
    }

    def requireLock(actor: Actor[ActorType]): Boolean = {
      val isEmpty = this.lockedBy.isEmpty
      if isEmpty then
        this.lockedBy = Some(actor)
      isEmpty
    }

    def releaseLock(): Unit = this.lockedBy = None // Broadcast a tutti gli attori attivi che possono transitare

    def spawnActor(actorType: ActorType): Unit = {
      val actor: Actor[ActorType] =
        if this.assignedTransitions.contains(actorType) then
          Actor(actorType, this.assignedTransitions(actorType), this)
        else
          Actor(actorType, this)
      if this.activeActors.isDefinedAt(actorType) then
        this.activeActors(actorType).add(actor)
      else
        this.activeActors.put(actorType, mutable.Set(actor))
      actor.execute()
    }

    def actorConsumed(actorType: ActorType, actor: Actor[ActorType]): Unit = this.activeActors(actorType).remove(actor)
  }
}
