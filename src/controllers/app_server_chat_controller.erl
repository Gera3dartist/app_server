-module(app_server_chat_controller).
-export([
         index/1,
	     topic/1,
	     subscribe/1
        ]).

index(_NovaReq) ->
    {ok, []}.

topic(#{req := #{method := <<"PUT">>,
                 bindings := #{topic := _Topic}} = Req}) ->
    io:format("~p~n", [Req]),
    {json, <<"Topic!">>}.

subscribe(#{method := <<"POST">>,
            bindings := #{<<"user">> := User}} = Req) ->
    logger:warning(">>USER: ~p", [User]),
    {ok, Data, _} = cowboy_req:read_body(Req),
    #{<<"topic">> := Topic} = json:decode(Data, [maps]),
    nova_pubsub:subscribe(User, Topic),
    {json, <<"Subscribed!">>};
subscribe(Req) ->
    io:format("~p", [Req]),
    {json, <<"default">>}.
