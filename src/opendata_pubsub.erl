-module(opendata_pubsub).
-export([publish/3, subscribe/2, unsubscribe/2]).

publish(Service, Topic, Msg) ->
    Key = {?MODULE, Service, Topic},
    gproc:send({p, l, Key}, {publish, Topic, Msg}).

subscribe(Service, Topic) ->
    Key = {?MODULE, Service, Topic},
    gproc:reg({p, l, Key}).

unsubscribe(Service, Topic) ->
    Key = {?MODULE, Service, Topic},
    gproc:unreg({p, l, Key}).
