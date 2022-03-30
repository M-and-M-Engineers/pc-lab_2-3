package pc.examples

import pc.modelling.{CTMCSimulation, DAP, DAPGrid}
import pc.utils.{Grids, MSet}

import java.util.Random

// Variante DAPBroadcast con i soli 3 stati A,B,C
object DAPBroadcastABC extends App {
  object place extends Enumeration {
    val A,B,C = Value
  }
  type Place = place.Value
  type ID = (Int,Int)
  import place._
  import DAP._

  val SENDER = MSet(A)
  val MESSAGE = MSet(B, B)
  val BROADCASTER = MSet(B)
  val RECEIVER = MSet(C)
  val REPLIER = MSet(B,C)
  val REPLY = MSet(C, C)

  // Questa variante, dopo la ricezione della risposta, continua a fare broadcast della risposta
  val messagesRules = DAP[Place](
    Rule(SENDER, _ => 1, SENDER, MESSAGE), // invio messaggio
    Rule(MESSAGE, _ => 100, BROADCASTER, MESSAGE), // broadcast dell'invio
    Rule(BROADCASTER union MESSAGE, _ => 10000, BROADCASTER, MSet()), // consumo invio (broadcaster)
    Rule(SENDER union MESSAGE, _ => 10000, SENDER,MSet()), // ignoro invio (sender)
    Rule(SENDER union BROADCASTER, _ => 10000, SENDER, MSet()), // ignoro invio (sender)
    Rule(RECEIVER union MESSAGE, _ => 1000, REPLIER, MSet()), // consumo invio (receiver/broadcaster)
    Rule(BROADCASTER union REPLY, _ => 100, BROADCASTER, REPLY), // broadcast della risposta
    Rule(REPLIER, _ => 1, RECEIVER, REPLY), // invio risposta
    Rule(RECEIVER union REPLY, _ => 10000, REPLY, MSet()), // consumo risposta (receiver)
    Rule(SENDER union REPLY, _ => 10000, SENDER, MSet()), // ricezione risposta
  )

  val messageCTMC = DAP.toCTMC[ID,Place](messagesRules)
  val net = Grids.createRectangularGrid(5,5)
  // an `a` initial on top left (sender) and a `c` (receiver) on 3,4
  val state = State[ID,Place](MSet(Token((0,0),A), Token((3,4), C)),MSet(),net)

  val simulator = CTMCSimulation(messageCTMC)
  simulator.newSimulationTrace(state,new Random).take(450).toList.foreach(
    step => {
      println(step._1) // print time
      println(DAPGrid.simpleGridStateToString(step._2)) // print state -> all tokens
    })

  val runs = 20
  val res = simulator.experiment(state, new Random, runs, state => !state.tokens.matches(MSet(Token((0,0), A), Token((0,0), C), Token((0,0), C))))
  println("Average steps across " + runs + " runs -> " + res._1)
  println("Average time of reply across " + runs + " runs -> " + res._2)
}