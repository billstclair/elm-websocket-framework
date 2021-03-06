This directory contains an example of using the `WebSocketFramework` module.

It is a very simple arithmetic server with no state and no games or players.

To run the example in `elm-reactor`:

    cd .../elm-websocket-framework/example
    elm reactor
    
Then aim your browser at http://localhost:8000/Main.elm

It starts out using a local "server", with no communication over the network.

To use it, enter integers for "x" and "y" and click the "Add" or "Multiply" button. The result will be displayed, and a running log of all messages sent and received will be updated.

If you have built and are running the [server](http://package.elm-lang.org/packages/billstclair/elm-websocket-framework-server/latest) example, you can click the "Connect" button to enable communication with its WebSocket. The "Connect" button will change to ""Disconnect", and the "URL" will be disabled for input. The log will say "sock" instead of "recv" for received messages.

The example server code is in the "example" directory of [billstclair/elm-websocket-framework-server](http://package.elm-lang.org/packages/billstclair/elm-websocket-framework-server/latest). Not much to it. I have a more involved example under construction at [billstclair/elm-chat-server](https://github.com/billstclair/elm-chat-server).
