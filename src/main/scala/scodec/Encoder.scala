package scodec

import scalaz.{ \/, Contravariant }

/** Supports encoding a value of type `A` to a `BitVector`. */
trait Encoder[-A] { self =>

  /**
   * Attempts to encode the specified value in to a bit vector.
   *
   * @param value value to encode
   * @param return error or binary encoding of the value
   */
  def encode(value: A): Error \/ BitVector

  /** Converts this encoder to an `Encoder[B]` using the supplied `B => A`. */
  def contramap[B](f: B => A): Encoder[B] = new Encoder[B] {
    def encode(b: B) = self.encode(f(b))
  }
}

/** Companion for [[Encoder]]. */
object Encoder {

  /** Provides syntaax for summoning an `Encoder[A]` from implicit scope. */
  def apply[A](implicit enc: Encoder[A]): Encoder[A] = enc

  implicit val contravariantInstance: Contravariant[Encoder] = new Contravariant[Encoder] {
    def contramap[A, B](e: Encoder[A])(f: B => A) = e contramap f
  }
}

