package pc.utils

// A multiset datatype
trait MSet[A] extends (A => Int) {
  def union(m: MSet[A]): MSet[A]
  def diff(m: MSet[A]): MSet[A]
  def disjoined(m: MSet[A]): Boolean
  def size: Int
  def matches(m: MSet[A]): Boolean
  def extract(m: MSet[A]): Option[MSet[A]]
  def asList: List[A]
  def asMap: Map[A,Int]
  def iterator: Iterator[A]
  def matchIterator: Iterator[(A,MSet[A])]
  def map[B](f: A=>B):MSet[B]
  def flatMap[B](f: A=>MSet[B]):MSet[B]
  def filter(f: A=>Boolean): MSet[A]
  def collect[B](f: PartialFunction[A,B]): MSet[B]
}

// Functional-style helpers/implementation
object MSet {
  // Factories
  def apply[A](l: A*): MSet[A] = new MSetImpl(l.toList)
  def ofList[A](l: List[A]): MSet[A] = new MSetImpl(l)
  def ofMap[A](m: Map[A,Int]): MSet[A] = MSetImpl(m)

  // Hidden reference implementation
  private case class MSetImpl[A](asMap: Map[A,Int]) extends MSet[A] {

    // keeping both representations, as Map and as List
    override val asList: List[A] = asMap.toList.flatMap{case (a,n) => List.fill(n)(a)}

    def this(list: List[A]) = this(list.groupBy(a=>a).map{case (a,n) => (a,n.size)})

    override def apply(v1: A): Int = asMap.getOrElse(v1,0)
    override def union(m: MSet[A]) = new MSetImpl[A](asList ++ m.asList)
    override def diff(m: MSet[A]) = new MSetImpl[A](asList diff m.asList)
    override def disjoined(m: MSet[A]): Boolean = (asList intersect m.asList).isEmpty
    override def size: Int = asList.size
    override def matches(m: MSet[A]): Boolean = extract(m).isDefined
    override def extract(m: MSet[A]): Option[MSet[A]] = Some(this diff m) filter (_.size == size - m.size)
    override def iterator: Iterator[A] = asList.distinct.iterator
    override def matchIterator: Iterator[(A, MSet[A])] = iterator.map(a => (a,extract(MSet(a)).get))
    override def toString = s"{${asList.mkString("|")}}"
    override def map[B](f: A => B) = new MSetImpl[B](asList.map(f))
    override def flatMap[B](f: A => MSet[B]) =
      new MSetImpl[B](asList.flatMap(x => f(x).asList))
    override def filter(f: A => Boolean) =
      new MSetImpl[A](asList.filter(f))
    override def collect[B](f: PartialFunction[A, B]) =
      new MSetImpl[B](asList.collect(f))
  }
}