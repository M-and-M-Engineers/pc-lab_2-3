package pc.examples

import java.util.Random

import pc.modelling.{CTMCSimulation, DAP, DAPGrid}
import pc.utils.{MSet, Grids}

// Variante DAPBroadcast con diversi tipi di token
object DAPBroadcast extends App {
  object place extends Enumeration {
    val SENDER,MESSAGE,RECEIVER,RESPONSE,MESSAGE_BROADCASTED,RESPONSE_BROADCASTED,END = Value
  }
  type Place = place.Value
  type ID = (Int,Int)
  import place._
  import DAP._

  // Il sistema permette di effettuare l'invio e la ricezione del messaggio una singola volta, dopodiché si blocca
  // in uno stato finale dal quale non viene effettuata alcuna transizione.
  val messagesRules = DAP[Place](
    Rule(MSet(SENDER), _ =>1,MSet(SENDER),MSet(MESSAGE)), // invio messaggio
    Rule(MSet(MESSAGE), _ =>10,MSet(MESSAGE_BROADCASTED),MSet(MESSAGE)), // broadcast dell'invio
    Rule(MSet(MESSAGE,MESSAGE), _ =>10000,MSet(MESSAGE),MSet()), // consumo invio (broadcaster)
    Rule(MSet(MESSAGE,MESSAGE_BROADCASTED), _ =>10000,MSet(MESSAGE_BROADCASTED),MSet()), // consumo invio (broadcaster)
    Rule(MSet(SENDER,MESSAGE), _ =>10000, MSet(SENDER), MSet()), // ignoro invio (sender)
    Rule(MSet(RECEIVER,MESSAGE), _ =>100000, MSet(RECEIVER), MSet(RESPONSE)), // invio risposta
    Rule(MSet(RECEIVER,RESPONSE), _ =>10000, MSet(RECEIVER), MSet()), // ignoro risposta (receiver)
    Rule(MSet(RESPONSE,RESPONSE), _ =>10000, MSet(RESPONSE), MSet()), // consumo risposta (broadcaster)
    Rule(MSet(MESSAGE,RESPONSE), _ =>700, MSet(RESPONSE_BROADCASTED), MSet(RESPONSE)), // broadcast della risposta
    Rule(MSet(MESSAGE_BROADCASTED,RESPONSE), _ =>700, MSet(RESPONSE_BROADCASTED), MSet(RESPONSE)), // broadcast della risposta
    Rule(MSet(RESPONSE_BROADCASTED,RESPONSE), _ =>700, MSet(RESPONSE_BROADCASTED), MSet()), // consumo della risposta
    Rule(MSet(RESPONSE_BROADCASTED,MESSAGE), _ =>700, MSet(RESPONSE_BROADCASTED), MSet()), // consumo della risposta
    Rule(MSet(SENDER,RESPONSE), _ =>100000, MSet(END), MSet()), // ricezione risposta
    Rule(MSet(END,RESPONSE), _ =>100000, MSet(END), MSet()), // ignoro risposta
  )

  val messageCTMC = DAP.toCTMC[ID,Place](messagesRules)
  val net = Grids.createRectangularGrid(5,5)
  // an `a` initial on top left (sender) and a `c` (receiver) on 3,4
  val state = State[ID,Place](MSet(Token((0,0),SENDER), Token((3,4),RECEIVER)),MSet(),net)

  val simulator = CTMCSimulation(messageCTMC)
  val simulatedPath = simulator.newSimulationTrace(state,new Random)
  simulatedPath.take(150).toList.foreach(
    step => {
      println(step._1) // print time
      println(DAPGrid.simpleGridStateToString(step._2)) // print state -> all tokens
    }
  )

  val runs = 20
  val res = simulator.experiment(state, new Random, runs, state => !state.tokens.matches(MSet(Token((0,0), END))))
  println("Average steps across " + runs + " runs -> " + res._1)
  println("Average time of reply across " + runs + " runs -> " + res._2)
}