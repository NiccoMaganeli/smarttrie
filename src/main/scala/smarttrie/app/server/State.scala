package smarttrie.app.server

import java.util.concurrent.ConcurrentHashMap
import java.util.{TreeMap => JTreeMap}
import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters._
import smarttrie.atoms._
import java.io.{
  ByteArrayInputStream,
  ByteArrayOutputStream,
  ObjectInputStream,
  ObjectOutputStream
}

abstract class State(val allowConcurrentSnapshot: Boolean) {
  def get(key: Key): Option[Value]
  def remove(key: Key): Option[Value]
  def put(key: Key, value: Value): Option[Value]
  def foreach(fn: (Key, Value) => Any): Unit
  def clear(): Unit
  def getSnapshot(): Array[Byte]
  def installSnapshot(appState: Array[Byte]): Unit
}

object State {

  def treeMap: State = new TreeMapState()
  def hashMap: State = new HashMapState()
  def trieMap: State = new TrieMapState()

  def apply(name: String): State =
    name match {
      case "tree-map" => treeMap
      case "hash-map" => hashMap
      case "trie-map" => trieMap
      case other =>
        throw new IllegalArgumentException(s"Invalid state name $other")
    }

  private final class TreeMapState extends State(allowConcurrentSnapshot = false) {

    private var state =
      new JTreeMap[Key, Value]()

    def get(key: Key): Option[Value] =
      Option(state.get(key))

    def remove(key: Key): Option[Value] =
      Option(state.remove(key))

    def put(key: Key, value: Value): Option[Value] =
      Option(state.put(key, value))

    def foreach(fn: (Key, Value) => Any): Unit = {
      val it = state.entrySet().iterator()
      while (it.hasNext) {
        val next = it.next()
        fn(next.getKey, next.getValue)
      }
    }

    def clear(): Unit =
      state.clear()

    def getSnapshot(): Array[Byte] = {
      val stream = new ByteArrayOutputStream()
      val oos = new ObjectOutputStream(stream)
      oos.writeObject(state)
      oos.close()
      stream.toByteArray()
    }

    def installSnapshot(newState: Array[Byte]): Unit = {
      val stream = new ObjectInputStream(new ByteArrayInputStream(newState))
      state = stream.readObject.asInstanceOf[JTreeMap[Key, Value]]
      stream.close()
    }

    override def hashCode(): Int = 13 * state.hashCode()

    override def equals(obj: Any): Boolean =
      obj match {
        case other: TreeMapState => state == other.state
        case _                   => false
      }

    override def toString: String =
      state.entrySet().asScala.view.take(1_000).mkString("TreeMap<", ", ", ">")
  }

  private final class HashMapState extends State(allowConcurrentSnapshot = false) {

    private var state =
      new ConcurrentHashMap[Key, Value]()

    def get(key: Key): Option[Value] =
      Option(state.get(key))

    def remove(key: Key): Option[Value] =
      Option(state.remove(key))

    def put(key: Key, value: Value): Option[Value] =
      Option(state.put(key, value))

    def foreach(fn: (Key, Value) => Any): Unit = {
      val it = state.entrySet().iterator()
      while (it.hasNext) {
        val next = it.next()
        fn(next.getKey, next.getValue)
      }
    }

    def getSnapshot(): Array[Byte] = {
      val stream = new ByteArrayOutputStream()
      val oos = new ObjectOutputStream(stream)
      oos.writeObject(state)
      oos.close()
      stream.toByteArray()
    }

    def installSnapshot(newState: Array[Byte]): Unit = {
      val stream = new ObjectInputStream(new ByteArrayInputStream(newState))
      state = stream.readObject.asInstanceOf[ConcurrentHashMap[Key, Value]]
      stream.close()
    }

    def clear(): Unit =
      state.clear()

    override def hashCode(): Int = 7 * state.hashCode()

    override def equals(obj: Any): Boolean =
      obj match {
        case other: HashMapState => state == other.state
        case _                   => false
      }

    override def toString: String =
      state.entrySet().asScala.view.take(1_000).mkString("HashMap<", ", ", ">")
  }

  private final class TrieMapState extends State(allowConcurrentSnapshot = true) {

    private var state =
      TrieMap.empty[Key, Value]

    def get(key: Key): Option[Value] =
      state.get(key)

    def remove(key: Key): Option[Value] =
      state.remove(key)

    def put(key: Key, value: Value): Option[Value] =
      state.put(key, value)

    def foreach(fn: (Key, Value) => Any): Unit = {
      val it = state.readOnlySnapshot().iterator
      while (it.hasNext) {
        val (key, value) = it.next()
        fn(key, value)
      }
    }

    def clear(): Unit =
      state.clear()

    def getSnapshot(): Array[Byte] = {
      val stream = new ByteArrayOutputStream()
      val oos = new ObjectOutputStream(stream)
      oos.writeObject(state.readOnlySnapshot())
      oos.close()
      stream.toByteArray()
    }

    def installSnapshot(newState: Array[Byte]): Unit = {
      val stream = new ObjectInputStream(new ByteArrayInputStream(newState))
      state = stream.readObject.asInstanceOf[TrieMap[Key, Value]]
      stream.close()
    }

    override def hashCode(): Int = 31 * state.hashCode()

    override def equals(obj: Any): Boolean =
      obj match {
        case other: TrieMapState => state == other.state
        case _                   => false
      }

    override def toString: String =
      state.view.take(1_000).mkString("TrieMap<", ", ", ">")
  }
}
