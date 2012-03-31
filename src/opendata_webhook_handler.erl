-module(opendata_webhook_handler).

-behaviour(gen_server).

%% API
-export([start_hook/3,
        stop_hook/1,
        incr_topic_counter/2,
        decr_topic_counter/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {name, hook, topics=ets:new(topics, []), interval}).

%%%===================================================================
%%% API
%%%===================================================================
start_hook(HookName, WebHook, Interval) ->
    gen_server:start({local, list_to_atom(HookName)}, ?MODULE, [HookName, WebHook, Interval], []).

stop_hook(HookName) ->
    gen_server:call(HookName, stop).
incr_topic_counter(HookName, Topic) when is_binary(HookName) ->
    incr_topic_counter(binary_to_list(HookName), Topic);
incr_topic_counter(HookName, Topic) when is_list(HookName) ->
    incr_topic_counter(list_to_atom(HookName), Topic);
incr_topic_counter(HookName, Topic) ->
    gen_server:cast(HookName, {incr_topic_counter, Topic}).

decr_topic_counter(HookName, Topic) when is_binary(HookName) ->
    decr_topic_counter(binary_to_list(HookName), Topic);
decr_topic_counter(HookName, Topic) when is_list(HookName) ->
    decr_topic_counter(list_to_atom(HookName), Topic);
decr_topic_counter(HookName, Topic) ->
    gen_server:cast(HookName, {decr_topic_counter, Topic}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([HookName, WebHook, Interval]) ->
    timer:send_interval(Interval, trigger),
    {ok, #state{name=HookName, hook=WebHook, interval=Interval}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({incr_topic_counter, Topic}, #state{topics=Topics} = State) ->
    case catch ets:update_counter(Topics, Topic, 1) of
        {'EXIT', {badarg, _}} ->
            ets:insert(Topics, {Topic, 1});
        _ ->
            ok
    end,
    {noreply, State};
handle_cast({decr_topic_counter, Topic}, #state{topics=Topics} = State) ->
    case catch ets:update_counter(Topics, Topic, -1) of
        {'EXIT', {badarg, _}} ->
            ok; %% should not happen, but it's ok anyway
        Val when Val =< 0 ->
            ets:delete(Topics, Topic);
        _ ->
            ok
    end,
    {noreply, State}.

handle_info(trigger, #state{name=HookName, hook=Hook, topics=Topics} = State) ->
    ListOfTopics = lists:flatten(ets:match(Topics, {'$1', '_'})),
    Url = list_to_binary([Hook, <<"?topics=">>, jsx:term_to_json(ListOfTopics)]),
    case ibrowse:send_req(binary_to_list(Url), [], get) of
        {ok, _Status, _ResponseHeaders, "[]"} ->
            ok;
        {ok, _Status, _ResponseHeaders, JsonResponseBody} ->
            ResponseBody = jsx:json_to_term(list_to_binary(JsonResponseBody)),
            BHook = list_to_binary(HookName),
            spawn(fun()->
                [opendata_pubsub:publish(BHook, Topic, Data)||{Topic, Data} <- ResponseBody]
            end)
    end,
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
