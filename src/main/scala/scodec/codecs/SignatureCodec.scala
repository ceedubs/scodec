package scodec
package codecs

import scalaz.{\/, \/-, -\/}

import java.security.{KeyPair, PrivateKey, PublicKey, Signature, SignatureException}
import java.security.cert.Certificate

import scodec.bits.{ BitVector, ByteVector }

/**
 * Represents the ability to create a `java.security.Signature` for use with [[fixedSizeSignature]] and [[variableSizeSignature]].
 * @group crypto
 */
trait SignatureFactory {

  /** Creates a signature initialized for signing. */
  def newSigner: Signature

  /** Creates a signature initialized for verifying. */
  def newVerifier: Signature
}

/**
 * Companion for [[SignatureFactory]].
 * @group crypto
 */
object SignatureFactory {

  /** Creates a signature factory for the specified algorithm using the specified private and public keys. */
  def apply(algorithm: String, privateKey: PrivateKey, publicKey: PublicKey): SignatureFactory =
    new SimpleSignatureFactory(algorithm, privateKey, publicKey)

  /** Creates a signature factory for the specified algorithm using the specified key pair. */
  def apply(algorithm: String, keyPair: KeyPair): SignatureFactory =
    new SimpleSignatureFactory(algorithm, keyPair.getPrivate, keyPair.getPublic)

  /** Creates a signature factory for the specified algorithm using the specified key pair. */
  def apply(algorithm: String, privateKey: PrivateKey, certificate: Certificate): SignatureFactory =
    new SimpleSignatureFactory(algorithm, privateKey, certificate.getPublicKey)

  /** Creates a signature factory that only supports signing for the specified algorithm using the specified private key. */
  def signing(algorithm: String, privateKey: PrivateKey): SignatureFactory =
    new SimpleSignatureFactorySigning(algorithm, privateKey) with SignatureFactory {
      def newVerifier = sys.error("Cannot verify with a signature factory that only supports signing.")
    }

  /** Creates a signature factory that only supports signing for the specified algorithm using the specified public key. */
  def verifying(algorithm: String, publicKey: PublicKey): SignatureFactory =
    new SimpleSignatureFactoryVerifying(algorithm, publicKey) with SignatureFactory {
      def newSigner = sys.error("Cannot sign with a signature factory that only supports verifying.")
    }

  /** Creates a signature factory that only supports signing for the specified algorithm using the specified public key. */
  def verifying(algorithm: String, certificate: Certificate): SignatureFactory =
    verifying(algorithm, certificate.getPublicKey)


  private trait WithSignature {
    protected def algorithm: String
    protected def newSignature: Signature =
      Signature.getInstance(algorithm)
  }

  private trait SignatureFactorySigning extends WithSignature {
    protected def privateKey: PrivateKey
    def newSigner: Signature = {
      val sig = newSignature
      sig.initSign(privateKey)
      sig
    }
  }

  private class SimpleSignatureFactorySigning(
    protected val algorithm: String,
    protected val privateKey: PrivateKey
  ) extends SignatureFactorySigning

  private trait SignatureFactoryVerifying extends WithSignature {
    protected def publicKey: PublicKey
    def newVerifier: Signature = {
     val sig = newSignature
      sig.initVerify(publicKey)
      sig
    }
  }

  private class SimpleSignatureFactoryVerifying(
    protected val algorithm: String,
    protected val publicKey: PublicKey
  ) extends SignatureFactoryVerifying

  private class SimpleSignatureFactory(
    protected val algorithm: String,
    protected val privateKey: PrivateKey,
    protected val publicKey: PublicKey
  ) extends SignatureFactory with SignatureFactorySigning with SignatureFactoryVerifying
}

/** @see [[fixedSizeSignature]] and [[variableSizeSignature]] */
private[codecs] final class SignatureCodec[A](codec: Codec[A], signatureCodec: Codec[BitVector])(implicit signatureFactory: SignatureFactory) extends Codec[A] {
  import Codec._

  override def encode(a: A) = for {
    encoded <- codec.encode(a)
    sig <- sign(encoded)
    encodedSig <- signatureCodec.encode(sig)
  } yield encoded ++ encodedSig

  private def sign(bits: BitVector): String \/ BitVector = {
    try {
      val signature = signatureFactory.newSigner
      signature.update(bits.toByteArray)
      \/-(BitVector(signature.sign))
    } catch {
      case e: SignatureException =>
        -\/("Failed to sign: " + e.getMessage)
    }
  }

  override def decode(buffer: BitVector) = (for {
    initialBits <- DecodingContext.monadState.get
    value <- DecodingContext(codec.decode)
    bitsAfterValueDecoding <- DecodingContext.monadState.get
    valueBits = initialBits take (initialBits.size - bitsAfterValueDecoding.size)
    decodedSig <- DecodingContext(signatureCodec.decode)
    _ <- DecodingContext liftE verify(valueBits.toByteVector, decodedSig.toByteVector)
  } yield value).run(buffer)

  private def verify(data: ByteVector, signatureBytes: ByteVector): String \/ Unit = {
    val verifier = signatureFactory.newVerifier
    verifier.update(data.toArray)
    try {
      if (verifier.verify(signatureBytes.toArray)) {
        \/-(())
      } else {
        -\/("Signature verification failed")
      }
    } catch {
      case e: SignatureException =>
        -\/("Signature verification failed: " + e)
    }
  }

  override def toString = s"signature($codec, $signatureCodec)"
}
