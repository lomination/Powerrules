package lomination.powerrules.util

import scala.collection.mutable.Builder
// import scala.util.{Failure, Success, Try}
import scala.util.parsing.input.{NoPosition, Position}
import scala.util.matching.Regex
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

extension [A](seq: Seq[A])

  def dropOnce: Seq[A] = seq.drop(1)

  def dropRightOnce: Seq[A] = seq.dropRight(1)

  def split(elem: A): Seq[Seq[A]] =
    seq.split(_ == elem)

  def split(pred: A => Boolean): Seq[Seq[A]] =
    if seq.isEmpty then Seq() else
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
