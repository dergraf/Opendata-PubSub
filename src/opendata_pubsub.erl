-module(opendata_pubsub).
-export([publish/2, subscribe/1, unsubscribe/1]).

publish(Topic, Msg) ->
    Key = {?MODULE, Topic},
    gproc:send({p, l, Key}, {publish, Topic, Msg}).

subscribe(Topic) ->
    Key = {?MODULE, Topic},
    gproc:reg({p, l, Key}).

unsubscribe(Topic) ->
    Key = {?MODULE, Topic},
    gproc:unreg({p, l, Key}).
