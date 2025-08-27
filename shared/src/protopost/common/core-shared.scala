package protopost.common

enum Service:
  case protopost, seismic;

enum Protocol( val defaultPort : Int ):
  case http  extends Protocol(80)  // for testing only! auth credentials are sent "in the clear", so only https should be used in production
  case https extends Protocol(443)
