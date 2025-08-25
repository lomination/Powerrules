package lomination.powerrules.util

import scala.annotation.tailrec
import scala.collection.mutable.Builder
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

extension [A](seq: Seq[A])

  /** Drops the first element of this sequence
    *
    * @return
    *   a new sequence containing all the elements of this one except the first one (equivalent to `this.drop(1)`)
    */
  def dropOnce: Seq[A] = seq.drop(1)

  /** Drops the last element of this sequence
    *
    * @return
    *   a new sequence containing all the elements of this one except the last one (equivalent to `this.dropRight(1)`)
    */
  def dropRightOnce: Seq[A] = seq.dropRight(1)

  /** Splits this sequence at the given element
    *
    * @param elem
    *   the element used a seperator
    * @returns
    *   ```scala
    *   this.split(_ == elem)
    *   ```
    */
  def split(elem: A): Seq[Seq[A]] =
    seq.split(_ == elem)

  /** Splits this sequence at every element that satisfies the given predicate
    *
    * @param pred
    *   a function that returns true when applied to an element of this sequence if the sequence should be splitted at the given element
    *
    * @returns
    *   a new sequence containing part of this seq, determined by the given predicate. If an empty sequence is given, returns an empty sequence
    */
  def split(pred: A => Boolean): Seq[Seq[A]] =
    val (b1, b2) = seq
      .foldLeft[(Builder[Seq[A], Seq[Seq[A]]], Builder[A, Seq[A]])]((Seq.newBuilder, Seq.newBuilder)) {
        case builder -> subBuilder -> a if pred(a) => (builder.addOne(subBuilder.result), Seq.newBuilder)
        case builder -> subBuilder -> a            => (builder, subBuilder.addOne(a))
      }
    b1.addOne(b2.result).result

  /** Applies a function that may fail (`A => Try[B]`) to a given sequence and returns the result as a `Try[Seq[B]]`.
    *
    * @param B
    *   the type of the elements of the resulting sequence.
    * @param f
    *   a function from `A` to `Try[B]`.
    * @return
    *   a try of sequence of B `Try[Seq[B]]`.
    */
  def tryMap[B](f: A => Try[B]): Try[Seq[B]] =
    @tailrec def process(acc: Seq[B], source: Iterator[A]): Try[Seq[B]] =
      if source.isEmpty then Success(acc)
      else
        f(source.next()) match
          case Success(b)         => process(acc :+ b, source)
          case Failure(exception) => Failure(exception)
    process(Seq(), seq.iterator)

extension (string: String)
  inline def i: Regex =
    string.iterator
      .foldLeft(StringBuilder()) {
        case builder -> char if char.isLetter => builder.addAll(s"[${char.toLower}${char.toUpper}]")
        case builder -> char                  => builder.addOne(char)
      }
      .result
      .r
