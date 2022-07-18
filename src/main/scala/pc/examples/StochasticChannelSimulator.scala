package pc.examples

import pc.examples.StochasticChannel.State.*
import pc.modelling.CTMCSimulation

import java.util.Random

// The Simulator
object StochasticChannelSimulator extends App {

  def averageTime(runs: Int) = {
    var time = 0.0
    for _ <- 1 to runs do {
      time += CTMCSimulation(StochasticChannel.stocChannel)
        .newSimulationTrace(IDLE, new Random)
        .dropWhile(tuple => tuple._2 != DONE)
        .head._1
    }
    time/runs
  }

  def failTime(runs: Int) = {
    val percents = Array[Double]()
    var previousTuple = (0.0, IDLE)
    var time = 0.0
    for i <- 1 to runs do {
      val simulation = CTMCSimulation(StochasticChannel.stocChannel)
        .newSimulationTrace(IDLE, new Random)
      val numStatesBeforeDone = simulation.takeWhile(tuple => tuple._2 != DONE).length

      simulation.take(numStatesBeforeDone + 1)
        .foreach(tuple => {
          // Save last FAILed state
          if tuple._2 == FAIL then
            previousTuple = tuple
          // Compute time in FAIL and accumulate (ignoring the first IDLE state)
          else if tuple._2 == IDLE && tuple._1 != 0 then
            time += (tuple._1 - previousTuple._1)
          // Compute the FAIL percentage of this run
          else if tuple._2 == DONE then {
            percents(i - 1) = (time / tuple._1) * 100
            time = 0.0
          }
        })
    }
    percents
  }

  println("Average time: " + averageTime(10) + "\n")
  println("Fail percentage:\n" + failTime(10).toList.map(value => s"$value %").mkString(" \n"))
}
