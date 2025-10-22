package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import com.github.plokhotnyuk.jsoniter_scala.core.{*,given}
import com.github.plokhotnyuk.jsoniter_scala.macros.{*,given}

import protopost.common.PosterId
import protopost.common.api.{*,given}

object LocalStorageItem:

  // really?
  //given JsonValueCodec[String] = JsonCodecMaker.make

  enum Key[T : JsonValueCodec]( val defaultValue : T ):
    case composer                    extends Key[Composer]                                        (Client.DefaultComposer)
    case topPanelLocation            extends Key[TopPanel.Tab]                                    (TopPanel.Tab.destinationsAndPosts)
    case currentPostIdentifier       extends Key[Option[PostIdentifier]]                          (None)
    case currentPostLocalPostContent extends Key[PostContent]                                     (PostContent.default)
    case recoveredRevisions          extends Key[List[UnsavedRevision]]                           (Nil)
    case openDestinations            extends Key[Set[DestinationIdentifier]]                      (Set.empty)
    case lastLoggedInPoster          extends Key[Option[PosterId]]                                (None)
    case externalJsConfig            extends Key[ProtopostExternalJsConfig]                       (ProtopostExternalJsConfig.default)

  /* ick */
  var instances : List[LocalStorageItem[?]] = Nil 

  def resetAll() : Unit =
    // we do want any listeners notified of the change in local storage values
    instances.foreach( _.reset() )
    // we want to make sure that any local storage vars the current user has not constructed are also cleared
    Key.values.foreach: key =>
      dom.window.localStorage.removeItem(key.toString())

class LocalStorageItem[T : JsonValueCodec](key: LocalStorageItem.Key[T]) extends util.laminar.VarLike[T]:
  require( key.defaultValue != null, "The value of a LocalStorageItem cannot be null." )

  LocalStorageItem.instances = this :: LocalStorageItem.instances

  private val _var = Var[Option[T]](
    Option(dom.window.localStorage.getItem(key.toString())).map( s => readFromString[T](s) )
  )

  val signal : Signal[T] = _var.signal.map( _.getOrElse(key.defaultValue) )

  def set(value: T): Unit =
    require( value != null, "The value of a LocalStorageItem cannot be null, define an variable of Option type instead." )
    if value == key.defaultValue then
      dom.window.localStorage.removeItem(key.toString())
    else
      dom.window.localStorage.setItem(key.toString(), writeToString[T](value))
    _var.set(Some(value))

  def reset() : Unit = set(key.defaultValue)

  def update( doUpdate : T => T ) = set( doUpdate(this.now()) )

  def now(): T = _var.now().getOrElse( key.defaultValue )


