package pc.modelling

import pc.modelling.PetriNet.PetriNet
import pc.utils.MSet

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

trait Actor[*] {

  def execute(): Unit
}

object Actor {

  def apply[ActorType](self: ActorType, petriNet: PetriNet[ActorType], system: ActorSystem[ActorType]): Actor[ActorType] = ActorImpl(self, petriNet, system)

  def apply[ActorType](self: ActorType, system: ActorSystem[ActorType]): Actor[ActorType] = ActorImpl(self, Set.empty, system)
  
  private case class ActorImpl[ActorType](private val selfType: ActorType,
                                          private val petriNet: PetriNet[ActorType],
                                          private val system: ActorSystem[ActorType]) extends Actor[ActorType] {

    private val random: Random = Random
    private val chosenTransitionParticipants: mutable.Set[Actor[ActorType]] = mutable.Set.empty

    @tailrec
    final def execute(): Unit = {
      if this.petriNet.nonEmpty then {
        val chosenIndex = this.random.nextInt(this.petriNet.size) // Random choice
        val chosenTransition = this.petriNet.toVector(chosenIndex) // Chosen transition
        val conditionTokensMSet = chosenTransition._1 diff MSet(this.selfType) // Essential tokens required to transit
        val conditionTokensSet = conditionTokensMSet.asList.toSet // As set to avoid actor duplicates
        val inhibitionActors = chosenTransition._3
        // Check if all condition actors are available and all inhibition actors are absent
        if conditionTokensSet.forall(actorType => this.checkConditionToken(conditionTokensMSet.asList.count(_ == actorType), actorType))
          && inhibitionActors.asList.forall(actorType => this.checkInhibition(actorType)) then {

          // Check if can transit
          if this.system.requireLock(this) then {
            println("Execute consumption of all chosen transition actors")
            chosenTransition._2.asList.foreach(place => this.system.spawnActor(place)) // Spawn actors from the effect of the transition
            println("Execute self consumption")
            this.system.actorConsumed(this.selfType, this) // Tell system i'm consumed
          } else
            println("Wait for the system to be unlocked")
        } else {
          // Unlock all locked chosen transition participants 
          this.chosenTransitionParticipants.clear
          this.execute()
        } 
      }
    }

    private def checkConditionToken(requiredAmount: Int, actorType: ActorType): Boolean = {
      val availableActors = this.system.actorsOf(actorType, requiredAmount)
      val isConditionSatisfied = availableActors.size >= requiredAmount
      if isConditionSatisfied then
        // Forbid available actors to perform any transition by themselves
        this.chosenTransitionParticipants ++ availableActors
      isConditionSatisfied
    }

    private def checkInhibition(actorType: ActorType): Boolean = this.system.actorsOf(actorType).nonEmpty

  }

}
