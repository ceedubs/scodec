package scodec

import scalaz.{ \/, Profunctor }

/** Generalized codec that allows the type to encode to vary with the type to decode. */
trait GenCodec[-A, +B] extends Encoder[A] with Decoder[B] { self =>

  /** Converts this `GenCodec` to a `GenCodec[A, C]` using the supplied `B => C`. */
  override def map[C](f: B => C): GenCodec[A, C] = new GenCodec[A, C] {
    def encode(a: A) = self.encode(a)
    def decode(bits: BitVector) = self.decode(bits).map { case (rem, b) => (rem, f(b)) }
  }

  /** Converts this `GenCodec` to a `GenCodec[C, B]` using the supplied `C => A`. */
  override def contramap[C](f: C => A): GenCodec[C, B] = new GenCodec[C, B] {
    def encode(c: C) = self.encode(f(c))
    def decode(bits: BitVector) = self.decode(bits)
  }

  /** Converts this generalized codec in to a non-generalized codec assuming `A` and `B` are the same type. */
  final def fuse[AA <: A, BB >: B](implicit ev: BB =:= AA): Codec[AA] = new Codec[AA] {
    def encode(c: AA) = self.encode(c)
    def decode(bits: BitVector) = self.decode(bits) map { case (rem, b) => (rem, ev(b)) }
  }
}

object GenCodec {

  /** Provides syntaax for summoning a `GenCodec[A, B]` from implicit scope. */
  def apply[A, B](implicit gc: GenCodec[A, B]): GenCodec[A, B] = gc

  implicit val profunctorInstance: Profunctor[GenCodec] = new Profunctor[GenCodec] {
    def mapfst[A, B, C](gc: GenCodec[A, B])(f: C => A) = gc contramap f
    def mapsnd[A, B, C](gc: GenCodec[A, B])(f: B => C) = gc map f
  }
}
