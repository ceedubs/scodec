package scodec

import scalaz.\/

trait Encoder[-A] {
  def encode(a: A): Error \/ BitVector
}

trait Decoder[+A] {
  def decode(bits: BitVector): Error \/ (BitVector, A)
}

trait GenCodec[-A, +B] extends Encoder[A] with Decoder[B] { self =>

  final def map[C](f: B => C): GenCodec[A, C] = new GenCodec[A, C] {
    def encode(a: A) = self.encode(a)
    def decode(bits: BitVector) = self.decode(bits).map { case (rem, b) => (rem, f(b)) }
  }

  final def contramap[C](f: C => A): GenCodec[C, B] = new GenCodec[C, B] {
    def encode(c: C) = self.encode(f(c))
    def decode(bits: BitVector) = self.decode(bits)
  }

  final def fuse[AA <: A, BB >: B](implicit ev: BB =:= AA): Codec[AA] = new Codec[AA] {
    def encode(c: AA) = self.encode(c)
    def decode(bits: BitVector) = self.decode(bits) map { case (rem, b) => (rem, ev(b)) }
  }
}


