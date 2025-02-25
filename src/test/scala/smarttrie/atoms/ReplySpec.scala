package smarttrie.atoms

import smarttrie.test._
import smarttrie.lang._

class ReplySpec extends Spec with CodecAsserts {
  import Reply._

  "Reply" should "encode/decode" in {
    verifyCodec(Error: Reply)
    verifyCodec(Null: Reply)
    verifyCodec(Data(Value("foo".toUTF8Array)): Reply)
  }
}
