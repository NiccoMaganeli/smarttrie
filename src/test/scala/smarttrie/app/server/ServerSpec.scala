package smarttrie.app.server

import bftsmart.tom.MessageContext
import java.nio.file.{Files, Path}
import smarttrie.atoms._
import smarttrie.io._
import smarttrie.lang._
import smarttrie.test._

class ServerSpec extends Spec {
  import Command._
  import Reply._

  var dataDir: Path = _

  before {
    dataDir = Files.createTempDirectory("smarttrie-log")
  }

  after {
    if (dataDir ne null) {
      IO.cleanDirectory(dataDir)
    }
  }

  check("State.treeMap", State.treeMap)
  check("State.hashMap", State.hashMap)
  check("State.trieMap", State.trieMap)

  def check(name: String, newState: => State): Unit = {
    def aKey = Key("foo".toUTF8Array)
    def aValue = Value("bar".toUTF8Array)
    def bValue = Value("bax".toUTF8Array)

    s"Server($name)" should "add a key" in {
      val server = new Server(newState)
      run(server, Set(aKey, aValue)) shouldBe Null
      run(server, Set(aKey, bValue)) shouldBe Data(aValue) // returns old value
    }

    it should "read a key" in {
      val state = newState
      state.put(aKey, aValue)

      val server = new Server(state)
      run(server, Get(aKey)) shouldBe Data(aValue)
    }

    it should "remove a key" in {
      val state = newState
      state.put(aKey, aValue)
      val server = new Server(state)
      run(server, Remove(aKey)) shouldBe Data(aValue) // return deleted value
      run(server, Remove(aKey)) shouldBe Null
    }

    def run(server: Server, cmd: Command, cid: Int = 0): Reply = {
      val ctx = new MessageContext(
        0,
        0,
        null,
        0,
        0,
        0,
        0,
        null,
        0,
        0,
        0,
        0,
        0,
        cid,
        null,
        null,
        false
      )

      val batch = Array(Codec.encode(cmd))
      val response = server.appExecuteBatch(batch, Array(ctx), false)
      Codec.decode(response(0)).as[Reply]
    }
  }
}
