import scalaz.{ \/, Monoid, StateT }
import shapeless._
import ops.hlist.{Prepend, RightFolder, Init, Last, Length, Split}
import scodec.bits._

/**
 * Combinator library for working with binary data.
 *
 * The primary abstraction of this library is [[Codec]], which provides the ability to encode/decode values to/from binary.
 *
 * There are more general abstractions though, such as [[Encoder]] and [[Decoder]]. There's also [[GenCodec]] which extends
 * both `Encoder` and `Decoder` but allows the types to vary. Given these more general abstractions, a `Codec[A]` can be
 * represented as a `GenCodec[A, A]`.
 *
 * The more general abstractions are important because they allow operations on codecs that would not otherwise be possible.
 * For example, given a `Codec[A]`, mapping a function `A => B` over the codec yields a `GenCodec[A, B]`. Without the
 * more general abstractions, `map` is impossible to define (e.g., how would `codec.map(f).encode(b)` be implemented?).
 * Given a `GenCodec[A, B]`, the encoding functionality can be ignored by treating it as a `Decoder[B]`, or the encoding
 * type can be changed via `contramap`. If after further transformations, the two types to `GenCodec` are equal, we can
 * reconstitue a `Codec` from the `GenCodec` by calling `fuse`.
 *
 * See the [[codecs]] package object for pre-defined codecs for many common data types and combinators for building larger
 * codecs out of smaller ones.
 *
 * For the categorically minded, note the following:
 *  - `Decoder` is a monad
 *  - `Encoder` is a contravariant functor
 *  - `GenCodec` is a profunctor
 *  - `Codec` is an invariant functor
 *
 * Each type has the corresponding Scalaz typeclass defined in its companion object.
 */
package object scodec {

  /** Alias for state/either transformer that simplifies calling decode on a series of codecs, wiring the remaining bit vector of each in to the next entry. */
  type DecodingContext[A] = StateT[({type λ[a] = String \/ a})#λ, BitVector, A]

  implicit val bitVectorMonoidInstance: Monoid[BitVector] = Monoid.instance(_ ++ _, BitVector.empty)

  implicit val byteVectorMonoidInstance: Monoid[ByteVector] = Monoid.instance(_ ++ _, ByteVector.empty)

  /** Provides common operations on a `Codec[HList]`. */
  final implicit class HListCodecEnrichedWithHListSupport[L <: HList](val self: Codec[L]) extends AnyVal {
    import codecs.HListCodec

    /**
     * When called on a `Codec[L]` for some `L <: HList`, returns a new codec representing `Codec[B :: L]`.
     * That is, this operator is a codec-level `HList` prepend operation.
     * @param codec codec to prepend
     * @group hlist
     */
    def ::[B](codec: Codec[B]): Codec[B :: L] = HListCodec.prepend(codec, self)

    /**
     * When called on a `Codec[L]` for some `L <: HList`, returns a new codec that encodes/decodes
     * `B :: L` but only returns `L`.  HList equivalent of `~>`.
     * @group hlist
     */
    def :~>:[B](codec: Codec[B])(implicit ev: Unit =:= B): Codec[L] = codec.dropLeft(self)

    /**
     * When called on a `Codec[L]` for some `L <: HList`, returns a new codec that encodes/decodes
     * the `HList L` followed by a `B`.
     * That is, this operator is a codec-level `HList` append operation.
     * @group hlist
     */
    def :+[B, LB <: HList](codec: Codec[B])(implicit
      prepend: Prepend.Aux[L, B :: HNil, LB],
      init: Init.Aux[LB, L],
      last: Last.Aux[LB, B]
    ): Codec[LB] = HListCodec.append(self, codec)

    /**
     * When called on a `Codec[L]` for some `L <: HList`, returns a new codec the encodes/decodes
     * the `HList K` followed by the `HList L`.
     * @group hlist
     */
    def :::[K <: HList, KL <: HList, KLen <: Nat](k: Codec[K])(implicit
      prepend: Prepend.Aux[K, L, KL],
      lengthK: Length.Aux[K, KLen],
      split: Split.Aux[KL, KLen, (K, L)]
    ): Codec[KL] = HListCodec.concat(k, self)
  }

  /** Provides `HList` related syntax for codecs of any type. */
  final implicit class ValueCodecEnrichedWithHListSupport[A](val self: Codec[A]) extends AnyVal {
    import codecs.HListCodec

    /**
     * When called on a `Codec[A]` where `A` is not a subytpe of `HList`, creates a new codec that encodes/decodes an `HList` of `B :: A :: HNil`.
     * For example, {{{uint8 :: utf8}}} has type `Codec[Int :: String :: HNil]`.
     * @group hlist
     */
    def ::[B](codecB: Codec[B]): Codec[B :: A :: HNil] =
      codecB :: self :: HListCodec.hnilCodec

    /**
     * Creates a new codec that encodes/decodes an `HList` type of `A :: L` given a function `A => Codec[L]`.
     * This allows later parts of an `HList` codec to be dependent on earlier values.
     * @group hlist
     */
    def flatPrepend[L <: HList](f: A => Codec[L]): Codec[A :: L] = HListCodec.flatPrepend(self, f)

    /**
     * Creates a new codec that encodes/decodes an `HList` type of `A :: L` given a function `A => Codec[L]`.
     * This allows later parts of an `HList` codec to be dependent on earlier values.
     * Operator alias for `flatPrepend`.
     * @group hlist
     */
    def >>:~[L <: HList](f: A => Codec[L]): Codec[A :: L] = flatPrepend(f)

    /**
     * Creates a new codec that encodes/decodes an `HList` type of `A :: B :: HNil` given a function `A => Codec[B]`.
     * If `B` is an `HList` type, consider using `flatPrepend` instead, which avoids nested `HLists`.
     * This is the direct `HList` equivalent of `flatZip`.
     * @group hlist
     */
    def flatZipHList[B](f: A => Codec[B]): Codec[A :: B :: HNil] = flatPrepend(f andThen (_.hlist))
  }
}
