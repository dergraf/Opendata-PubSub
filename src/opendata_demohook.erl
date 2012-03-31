-module(opendata_demohook).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {JsonTopics, _} = cowboy_http_req:qs_val(<<"topics">>, Req),
    Topics = jsx:to_term(JsonTopics),
    Response = [{T, random:uniform(1000000)} ||T <- Topics],
    JsonResponse = jsx:to_json(Response),
    {ok, Req2} = cowboy_http_req:reply(200, [], JsonResponse, Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.
