-module(opendata_pubsub_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(cowboy),
    application:start(gproc),
    application:start(opendata_pubsub).

start(_StartType, _StartArgs) ->
    Dispatch = [
            {'_', [
                    {[<<"pubsub">>, '_'], opendata_handler, []},
                    {'_', default_handler, []}
                    ]}
            ],
    cowboy:start_listener(my_http_listener, 100,
                          cowboy_tcp_transport, [{port, 8080}],
                          cowboy_http_protocol, [{dispatch, Dispatch}]
                         ),
    opendata_pubsub_sup:start_link().

stop(_State) ->
    ok.
