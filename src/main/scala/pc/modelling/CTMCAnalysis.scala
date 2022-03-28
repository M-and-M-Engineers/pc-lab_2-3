package pc.modelling

import java.util.Random

trait CTMCAnalysis[S] extends CTMCSimulation[S] {
  self: CTMC[S] =>

  type Property[A] = Trace[A] => Boolean

  // globally is simply achieved by equivalence not G x= F not x
  def eventually[A](filt: A => Boolean): Property[A] =
    trace => trace.exists { case (t, a) => filt(a) }

  def globally[A](filter: A => Boolean): Property[A] =
    trace => !trace.exists { case (_, a) => !filter(a) }

  // until: collect all states before the check. Then filters the starting elements with the specified filter
  def until[A](filter: A => Boolean, check: A => Boolean): Property[A] =
    trace => trace.takeWhile(el => !check(el._2)) match {
      case startingStates if trace.length != startingStates.length => startingStates.dropWhile(el => filter(el._2)).isEmpty
      case _ => false
    }

  // next: checks the next state
  def next[A](check: A => Boolean): Property[A] =
    trace => checkStateProperty(trace.take(2).lastOption, check)

  // returns true if property holds on specified optional state otherwise false
  private def checkStateProperty[A](state: Option[(Double, A)], check: A => Boolean): Boolean = state match {
    case Some(value) => check(value._2)
    case None => false
  }

  // checks if last state holds specified property
  def steadyState[A](stateCheck: A => Boolean): Property[A] =
    trace => stateCheck(trace.last._2)

  // takes a property and makes it time bounded by the magics of streams
  def bounded[A](timeBound: Double)(prop: Property[A]): Property[A] =
    trace => prop(trace.takeWhile { case (t, _) => t <= timeBound })

  // a PRISM-like experiment, giving a statistical result (in [0,1])
  def experiment(runs: Int = 10000, prop: Property[S], rnd: Random = new Random, s0: S, timeBound: Double): Double =
    (0 until runs).count(i => bounded(timeBound)(prop)(newSimulationTrace(s0, rnd))).toDouble / runs

  // experiment on long runs to find the steady state probability
  def steadyStateExperiment(steadyStateProperty: S => Boolean, rnd: Random = new Random, s0: S): Double =
    experiment(20000, steadyState(steadyStateProperty), rnd, s0, 100.0)
}

object CTMCAnalysis {
  def apply[S](ctmc: CTMC[S]): CTMCAnalysis[S] = new CTMC[S] with CTMCAnalysis[S]{
    override def transitions(s: S) = ctmc.transitions(s)
  }
}