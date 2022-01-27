-module(app_server_chat_controller).
-export([
         index/1,
	     topic/1,
	     subscribe/1,
         topics_list/1
        ]).


index(_NovaReq) ->
    {ok, []}.

topics_list(_NovaReq) ->
    ChatTopics = chat_backend:list_topics(),

    TopicList = case orddict:size(ChatTopics) > 0 of
        true -> [#{ 
            topic => Topic, 
            subscribed =>  [#{ user_id => U } || U <-Subsribed]
            } || {Topic, Subsribed} <- ChatTopics];
        false -> 
            []
        end,
    {json, 200, #{<<"Access-Control-Allow-Origin">> => <<"http://localhost:8000">>}, TopicList}.


topic(#{req := #{method := <<"PUT">>,
                 bindings := #{topic := _Topic}} = Req}) ->
    io:format("~p~n", [_Topic]),
    {json, <<"Topic!">>}.


subscribe(#{method := <<"POST">>,
            bindings := #{<<"userid">> := User}} = Req) ->
    {ok, Data, _} = cowboy_req:read_body(Req),
    #{<<"topic">> := Topic} = json:decode(Data, [maps]),
    chat_backend:subscribe(User, Topic),
    {json, #{<<"status">> => <<"ok">>}};
subscribe(Req) ->
    io:format("~p", [Req]),
    {json, #{<<"status">> => <<"default">>}}.
