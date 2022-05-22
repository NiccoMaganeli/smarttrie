package smarttrie.app.server

import bftsmart.statemanagement.strategy.collaborative.CollaborativeStateManager
import bftsmart.statemanagement.StateManager
import bftsmart.tom.server.defaultservices.DefaultRecoverable
import bftsmart.tom.{MessageContext, ServiceReplica}
import org.slf4j.LoggerFactory
import scala.util.control.NonFatal
import smarttrie.atoms._
import smarttrie.io._

object Server {
  def main(args: Array[String]): Unit = {
    val server = new Server(State(args(1)))
    val replica = new ServiceReplica(args(0).toInt, server, server)
    Runtime.getRuntime.addShutdownHook(new Thread(() => {
      replica.kill()
    }))
  }
}

final class Server(
    private[this] val state: State
) extends DefaultRecoverable {
  import Command._
  import Reply._

  private[this] var lastCID = CID.Null
  private[this] val logger = LoggerFactory.getLogger(getClass)

  def appExecuteUnordered(request: Request, ctx: MessageContext): Array[Byte] =
    execute(request)

  def appExecuteBatch(
      batch: Batch,
      ctxs: Array[MessageContext],
      fromConsensus: Boolean
  ): Array[Array[Byte]] = {
    lastCID = CID(ctxs(0).getConsensusId)
    val pending = Array.newBuilder[Request]
    val replies = Array.newBuilder[Array[Byte]]
    replies.sizeHint(batch.length)

    def executePending(): Unit = {
      val entries = pending.result()
      if (entries.nonEmpty) {
        entries foreach { replies += execute(_) }
        pending.clear()
      }
    }

    for (i <- batch.indices) {
      val ctx = ctxs(i)
      val request = batch(i)
      val currentCID = CID(ctx.getConsensusId)
      if (currentCID > lastCID) {
        executePending()
        lastCID = currentCID
      }
      pending += request
    }

    executePending()
    replies.result()
  }

  private def execute(request: Request): Array[Byte] = {
    def execute0(): Array[Byte] = {
      val cmd = Codec.decode(request).as[Command]
      val res = runCommand(cmd)
      Codec.encode(res)
    }

    def runCommand(cmd: Command): Reply = {
      val res = cmd match {
        case Get(k)    => state.get(k)
        case Set(k, v) => state.put(k, v)
        case Remove(k) => state.remove(k)
      }
      res.map(Data).getOrElse(Null)
    }

    try execute0()
    catch {
      case NonFatal(err) =>
        logger.error("Error while executing command", err)
        Codec.encode(Error)
    }
  }

  // Satisfy recoverable interface ------------------------------------------------

  private final class InternalStateManager extends CollaborativeStateManager {
    def tom = tomLayer // expose tom layer so we can control its last seen CID
  }

  private[this] lazy val internalStateManager =
    new InternalStateManager()

  override def getStateManager: StateManager =
    internalStateManager

  def installSnapshot(newState: Array[Byte]): Unit =
    state.installSnapshot(newState)

  def getSnapshot(): Array[Byte] =
    state.getSnapshot()
}
