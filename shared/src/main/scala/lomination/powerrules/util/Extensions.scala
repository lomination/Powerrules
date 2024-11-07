package lomination.powerrules.util

import scala.collection.mutable.Builder
// import scala.util.{Failure, Success, Try}
import scala.util.parsing.input.{NoPosition, Position}
import scala.util.matching.Regex
import scala.annotation.tailrec
// import scala.annotation.tailrec

// def recTry[A, B](iterable: IterableOnce[A])(f: A => Try[B]): Try[Seq[B]] =
//   @tailrec def process(to: Seq[B], from: Iterator[A]) =
//     if from.isEmpty then
//       Success(to)
//     else
//       f(from.next()) match
//         case Success(b) => process(to :+ b, from)
//         case Failure(exception) => Failure(exception)
//   process(Seq(), iterable.iterator)

// extension [A](seq: Seq[Try[A]])
//   /** Transforms this sequence of tries of A into a try of sequence of A */
//   def toTry: Try[Seq[A]] =
//     seq
//       .foldLeft[Try[Builder[A, Seq[A]]]](Success(Seq.newBuilder)) {
//         case Success(l) -> Success(a)   => Success(l.addOne(a))
//         case (failure: Failure[_]) -> _ => failure
//         case _ -> Failure[A](exception) => Failure[Builder[A, Seq[A]]](exception)
//       }
//       .map(_.result)

// @tailrec
// def processOnList[A, B](f: (B, List[A]) => (B, List[A]), acc: B, list: List[A]): B =
//   val (newAcc, next) = f(acc, list)
//   processOnList(f, newAcc, next)

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

extension (string: String)
  inline def i: Regex =
    string.iterator
      .foldLeft(StringBuilder()) {
        case builder -> char if char.isLetter => builder.addAll(s"[${char.toLower}${char.toUpper}]")
        case builder -> char                  => builder.addOne(char)
      }
      .result
      .r
