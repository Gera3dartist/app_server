-module(app_server_chat_controller_simple).
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
    logger:warning(">>ChatTopics: ~p", [ChatTopics]),

    TopicList = case orddict:size(ChatTopics) > 0 of
        true -> [#{ 
            topic => Topic, 
            subscribed =>  [#{ user_id => U } || U <-Subsribed]
            } || {Topic, Subsribed} <- ChatTopics];
        false -> 
            []
        end,
    logger:warning(">>TopicList: ~p", [TopicList]),
    {json, 200, #{}, TopicList}.


topic(#{req := #{method := <<"PUT">>,
                 bindings := #{topic := _Topic}} = Req}) ->
    io:format("~p~n", [Req]),
    {json, <<"Topic!">>}.

subscribe(#{bindings := #{<<"userid">> := User},
                 json := #{<<"topic">> := Topic}}) ->

    io:format(">>> user and topic: ~p, ~p~n", [User, Topic]),
    chat_backend:subscribe(User, Topic),
    {json, <<"Subscribed!">>};
subscribe(Req) ->
    io:format("~p", [Req]),
    {json, <<"default">>}.
