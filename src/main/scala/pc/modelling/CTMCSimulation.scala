package pc.modelling

import java.util.Random

import pc.utils.Stochastics

trait CTMCSimulation[S] { self: CTMC[S] =>

  type Trace[A] = LazyList[(Double,A)]

  def newSimulationTrace(s0: S, rnd: Random): Trace[S] =
    LazyList.iterate( (0.0,s0) ){ case (t,s) =>
      if transitions(s).isEmpty then (t,s) else {
        val next = Stochastics.cumulative(transitions(s).toList)
        val sumR = next.last._1
        val choice = Stochastics.draw(next)(using rnd)
        (t + Math.log(1 / rnd.nextDouble()) / sumR, choice)
      }
    }
}

object CTMCSimulation {
  def apply[S](ctmc: CTMC[S]): CTMCSimulation[S] =
    new CTMC[S] with CTMCSimulation[S]{
      override def transitions(s: S): Set[(Double, S)] = ctmc.transitions(s)
    }
}

