package protopost.common.util

def urlEncode( raw : String ) : String =
  import java.net.URLEncoder
  import java.nio.charset.StandardCharsets
  URLEncoder.encode(raw, StandardCharsets.UTF_8.toString)


