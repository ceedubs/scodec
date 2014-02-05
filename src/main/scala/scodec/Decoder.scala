package scodec

import scalaz.{ \/, Monad, Monoid }

/** Supports decoding a value of type `A` from a `BitVector`. */
trait Decoder[+A] { self =>

  /**
   * Attempts to decode a value of type `A` from the specified bit vector.
   *
   * @param bits bits to decode
   * @return error if value could not be decoded or the remaining bits and the decoded value
   */
  def decode(bits: BitVector): Error \/ (BitVector, A)

  /** Converts this decoder to a `Decoder[B]` using the supplied `A => B`. */
  def map[B](f: A => B): Decoder[B] = new Decoder[B] {
    def decode(bits: BitVector) = self.decode(bits) map { case (rem, a) => (rem, f(a)) }
  }

  /** Converts this decoder to a `Decoder[B]` using the supplied `A => Decoder[B]`. */
  def flatMap[B](f: A => Decoder[B]): Decoder[B] = new Decoder[B] {
    def decode(bits: BitVector) = self.decode(bits) flatMap { case (rem, a) => f(a).decode(rem) }
  }
}

/** Companion for [[Decoder]]. */
object Decoder {

  /** Provides syntaax for summoning a `Decoder[A]` from implicit scope. */
  def apply[A](implicit dec: Decoder[A]): Decoder[A] = dec

  def point[A](a: => A): Decoder[A] = new Decoder[A] {
    private lazy val value = a
    def decode(bits: BitVector) = \/.right((bits, value))
    override def toString = s"const($value)"
  }

  implicit val monadInstance: Monad[Decoder] = new Monad[Decoder] {
    def point[A](a: => A) = Decoder.point(a)
    def bind[A, B](decoder: Decoder[A])(f: A => Decoder[B]) = decoder.flatMap(f)
  }

  implicit def monoidInstance[A: Monoid]: Monoid[Decoder[A]] = new Monoid[Decoder[A]] {
    def zero = Decoder.point(Monoid[A].zero)
    def append(x: Decoder[A], y: => Decoder[A]) = new Decoder[A] {
      private lazy val yy = y
      def decode(bits: BitVector) = for {
        first <- x.decode(bits)
        second <- yy.decode(first._1)
      } yield (second._1, Monoid[A].append(first._2, second._2))
    }
  }
}
