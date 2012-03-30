-module(opendata_demohook).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    RandVal = random:uniform(1000000),
    {ok, Req2} = cowboy_http_req:reply(200, [], list_to_binary(integer_to_list(RandVal)), Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.
