package pc.modelling

import java.util.Random

import pc.utils.Stochastics

trait CTMCSimulation[S] { self: CTMC[S] =>

  type Trace[A] = Stream[(Double,A)]

  def newSimulationTrace(s0: S, rnd: Random): Trace[S] =
    Stream.iterate( (0.0,s0) ){ case (t,s) =>
      val paths = transitions(s)
      if (paths.isEmpty) (t,s) else {
        val next = Stochastics.cumulative(paths.toList)
        val sumR = next.last._1
        val choice = Stochastics.draw(next)(rnd)
        (t + Math.log(1 / rnd.nextDouble()) / sumR, choice)
      }
    }

  // Experiment that performs N runs to compute average steps and time that are required to satisfy particular condition
  def experiment(initialState: S, rnd: Random, runs: Int, condition: S => Boolean): (Int, Double) = {
    val result = (0 until runs).par.map(_ => {
      val stream = this.newSimulationTrace(initialState, rnd)
      // Counts steps required to satisfy the condition,
      val numberOfSteps = stream.takeWhile(step => condition(step._2)).length + 1
      // Returns number of steps and gets the time after 'numberOfSteps'
      (numberOfSteps, stream.take(numberOfSteps).last._1)
    }).reduce((acc, current) => (acc._1 + current._1, acc._2 + current._2))
    (result._1/runs, result._2/runs)
  }
}

object CTMCSimulation {
  def apply[S](ctmc: CTMC[S]): CTMCSimulation[S] =
    new CTMC[S] with CTMCSimulation[S]{
      override def transitions(s: S) = ctmc.transitions(s)
    }
}

