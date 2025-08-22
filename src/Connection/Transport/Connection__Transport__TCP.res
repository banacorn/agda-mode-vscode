open NodeJs.Net
module Error = {
  type t =
    | Timeout(int)
    | OnError(Js.Exn.t)

  let toString = x =>
    switch x {
    | Timeout(timeout) => "Expected to connect within " ++ string_of_int(timeout) ++ "ms"
    | OnError(exn) => Util.JsError.toString(exn)
    }
}

// see if the TCP port is available
let probe = (url: NodeJs.Url.t, ~timeout=1000) => {
  let connection = Promise.make((resolve, _) => {
    // connect and resolve `Ok()` on success
    let socket = NodeJs.Net.TcpSocket.make()

    socket
    ->NodeJs.Net.TcpSocket.connect(~port=url.port, ~host=url.hostname, () => ())
    ->NodeJs.Net.Socket.onConnectOnce(() => {
      // destroy the connection afterwards
      Socket.destroy(socket, ~error=None)->ignore
      resolve(Ok())
    })
    ->NodeJs.Net.Socket.onErrorOnce(exn => {
      resolve(Error(Error.OnError(exn)))
    })
    ->NodeJs.Net.Socket.onTimeoutOnce(() => resolve(Error(Timeout(timeout))))
    ->ignore
  })

  let timeout = async () => {
    await Util.Promise_.setTimeout(timeout)
    Error(Error.Timeout(timeout))
  }

  Promise.race([connection, timeout()])
}
