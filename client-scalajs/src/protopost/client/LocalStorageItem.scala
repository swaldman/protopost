package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import com.github.plokhotnyuk.jsoniter_scala.core.{*,given}
import com.github.plokhotnyuk.jsoniter_scala.macros.{*,given}

import protopost.common.api.{*,given}

object LocalStorageItem:

  // really?
  //given JsonValueCodec[String] = JsonCodecMaker.make

  enum Key[T : JsonValueCodec]( val defaultValue : T ):
    case composer                    extends Key[Composer]                                        (Client.DefaultComposer)
    case location                    extends Key[Tab]                                             (Tab.destinationsAndPosts)
    case currentPostIdentifier       extends Key[Option[PostIdentifier]]                          (None)
    case currentPostLocalPostContent extends Key[PostContent]                                     (PostContent.default)
    case recoveredRevisions          extends Key[List[Tuple2[RevisionTimestamp,NewPostRevision]]] (Nil)

  def resetAll() : Unit =
    Key.values.foreach: key =>
      dom.window.localStorage.removeItem(key.toString())

class LocalStorageItem[T : JsonValueCodec](key: LocalStorageItem.Key[T]):
  require( key.defaultValue != null, "The value of a LocalStorageItem cannot be null." )

  private val _var = Var[Option[T]](
    Option(dom.window.localStorage.getItem(key.toString())).map( s => readFromString[T](s) )
  )

  val signal: Signal[T] = _var.signal.map( _.getOrElse(key.defaultValue) )

  def set(value: T): Unit =
    require( value != null, "The value of a LocalStorageItem cannot be null, define an variable of Option type instead." )
    if value == key.defaultValue then
      dom.window.localStorage.removeItem(key.toString())
    else
      dom.window.localStorage.setItem(key.toString(), writeToString[T](value))
    _var.set(Some(value))

  def update( doUpdate : T => T ) = set( doUpdate(this.now()) )

  def now(): T = _var.now().getOrElse( key.defaultValue )


