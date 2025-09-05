package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import com.github.plokhotnyuk.jsoniter_scala.core.{*,given}
import com.github.plokhotnyuk.jsoniter_scala.macros.{*,given}

object LocalStorageItem:
  enum Key[T : JsonValueCodec]:
    case composer extends Key[Composer]
    case location extends Key[Tab]

class LocalStorageItem[T : JsonValueCodec](key: LocalStorageItem.Key[T], defaultValue : T):
  require( defaultValue != null, "The value of a LocalStorageItem cannot be null." )

  private val _var = Var[Option[T]](
    Option(dom.window.localStorage.getItem(key.toString())).map( s => readFromString[T](s) )
  )

  val signal: Signal[T] = _var.signal.map( _.getOrElse(defaultValue) )

  def set(value: T): Unit =
    require( value != null, "The value of a LocalStorageItem cannot be null, define an variable of Option type instead." )
    if value == defaultValue then
      dom.window.localStorage.removeItem(key.toString())
    else
      dom.window.localStorage.setItem(key.toString(), writeToString[T](value))
    _var.set(Some(value))

  def now(): T = _var.now().getOrElse( defaultValue )


