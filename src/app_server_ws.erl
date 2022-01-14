-module(app_server_ws).

-export([init/1,
	 websocket_init/1,
	 websocket_handle/2,
	 websocket_info/2,
	 terminate/3]).

init(#{req := Req}) ->
    logger:warning(">>initializing chat"),
    #{bindings := UserMap} = Req,
    {ok, UserMap}.

websocket_init(State) ->
    logger:warning(">>websocket_init chat"),
    #{<<"userid">> := User} = State,
    ok = chat_backend:online(User, self()),
    {ok, State}.

websocket_handle({text, Message}, State) ->
    logger:warning(">>websocket_handle chat"),
    Decode = json:decode(Message, [maps]),
    #{<<"userid">> := User} = State,
    #{<<"topic">> := Topic} = Decode,
    Json = json:encode(Decode#{<<"userid">> => User}, 
                       [maps, binary]),
    io:format(">>MESSAGE TO SEND: ~p~n", [Json]),                       
    ok = chat_backend:publish(Topic, Json),
    {ok, State}.

websocket_info(Payload,State) ->
    logger:warning(">>websocket_info chat"),
    {reply, {text, Payload}, State}.

terminate(_, _, State)->
    logger:warning(">>terminate chat"),
    #{<<"userid">> := User} = State,
    ok = chat_backend:offline(User).