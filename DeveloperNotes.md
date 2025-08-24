# protopost - developer notes

* We've adopted the following basic identifier format for restack service nodes:
  ```plaintext
  <service-name>[<alg>(<crv>)]<pubkey-hex>:<location>
  ```
  where `<location>` is just an `https` URL.
  
  (`http` is supported for testing, but should not be used in production, as authentication tokens are sent in the clear.)
  
  Each node is identified by host and port (if not the protocol default). Any path information identifies some resource on the node.
  
  For now, two services are defined, `protopost` and `seismic`.
  
  Algorithm and curve abbreviations are taken from [JSON Web Algorithms](https://datatracker.ietf.org/doc/html/rfc7518).
  
  Each supported alg-crv is associated with an expected public key format.
  
  Hex strings may optionally begin with the `0x` prefix.

  Supported alg-crvs are:

  * `[ES256(P-256)]`
    - Public keys are in uncompressed format, 65 bytes, beginning with 0x04, concatenated with the 32-byte x value,
      concatenated with the 32-byte y value of the public key point.

* Useful documentation while developing
  - Airstream: ([docs/git](https://github.com/raquo/Airstream))([apidocs](https://javadoc.io/doc/com.raquo/airstream_sjs1_3/latest/index.html))
  - Laminar: ([docs](https://laminar.dev/documentation)) ([apidocs](https://javadoc.io/doc/com.raquo/laminar_sjs1_3/latest/index.html))

