-module(chat_backend).
% -compile(export_all).
-export([
    subscribe/2,
    list_topics/0,
    loop/1,
    start_link/0,
    init/0,
    online/2,
	offline/1,
    publish/2,
	user_topics/1
    ]).

-record(state, {topics, online}).

subscribe(User, Topic) ->
	% Ref = erlang:monitor(process, whereis(?MODULE)),
	?MODULE ! {self(), {subscribe, User, Topic}},
	receive
		ok ->
            ok;
		{'DOWN', process, _Pid, Reason} -> 
			{error, Reason}
	after 5000 ->
		{error, timeout}
	end.

user_topics(UserId) ->
	?MODULE ! {self(), {user_topics, UserId}},
	receive
		{ok, Topics} ->
            Topics;
		{'DOWN', process, _Pid, Reason} -> 
			{error, Reason}
	after 5000 ->
		{error, timeout}
	end.

list_topics() -> 
    ?MODULE ! {self(), show_topics},
    receive
		{ok, Topics} ->
			Topics;
		{'DOWN', process, _Pid, Reason} -> 
			{error, Reason}
	after 5000 ->
		{error, timeout}
	end.

online(User, Socket) ->
    ?MODULE ! {self(), {online, User, Socket}},
    receive
		ok -> 
		    ok;
		{'DOWN', process, _Pid, Reason} -> 
			{error, Reason}
	after 5000 ->
		{error, timeout}
	end.

offline(User) ->
    ?MODULE ! {self(), {offline, User}},
    receive
		ok -> 
		    ok;
		{'DOWN', process, _Pid, Reason} -> 
			{error, Reason}
	after 5000 ->
		{error, timeout}
	end.


publish(Topic, Json) ->
    ?MODULE ! {self(), {publish, Topic, Json}},
    receive
		ok -> 
		    ok;
		{'DOWN', process, _Pid, Reason} -> 
			{error, Reason}
	after 5000 ->
		{error, timeout}
	end.


loop(S=#state{}) ->
	receive
        {Pid, show_topics} ->
            io:format("~p~n", ["Handling topics"]),

			% Pid ! {ok, [{"foobar", {"user_id", self()}}]};
			Pid ! {ok, S#state.topics},
            loop(S);

		{Pid, {subscribe, UserId, Topic}} ->
            % circuit breaker - if user is already subscribed - do nothing
            case orddict:find(Topic, S#state.topics) of
                {ok, Users} ->
                    case lists:member(UserId, Users) of
                        true ->
                            Pid ! ok,
                            loop(S);
                        false ->
                            NewTopics = do_subscribe(UserId, Topic, S#state.topics),
                            Pid ! ok,
                            loop(S#state{topics=NewTopics})
                    end;
                error ->
                    NewTopics = do_subscribe(UserId, Topic, S#state.topics),
                    Pid ! ok,
                    loop(S#state{topics=NewTopics})
            end;
		{Pid, {user_topics, UserId}} ->
			UserTopics = orddict:filter(fun (_, UserList) -> filter_topics_by_user(UserId, UserList) end, S#state.topics),
			Pid ! {ok, orddict:fetch_keys(UserTopics)},
			loop(S);

		{Pid, {online, User, Socket}} ->
			%update online users
            OnlineUsers = orddict:store(User, Socket, S#state.online),
            Pid ! ok,
            loop(S#state{online=OnlineUsers});
		{Pid, {offline, User}} ->
			%update online users
            OnlineUsers = orddict:erase(User, S#state.online),
            Pid ! ok,
            loop(S#state{online=OnlineUsers});
        {Pid, {publish, Topic, Body}} ->
            case orddict:find(Topic, S#state.topics) of 
                {ok, Subscribers} ->
                    OnlineUsers = [orddict:fetch(U, S#state.online) || U <- Subscribers, orddict:is_key(U,  S#state.online)],
                    io:format(">>>OnlineUsers: ~p~n", [OnlineUsers]),
                    send_to_clients(Body, OnlineUsers),
                    Pid ! ok,
					loop(S);
                {ok, error} ->
                    io:format("Topic not found: ~p~n", [Topic]),
                    loop(S)
            end;
            
		shutdown -> 
			exit(shutdown);
		code_change ->
			?MODULE:loop(S);
		Unknown -> 
			io:format("Unknown message: ~p~n", [Unknown]),
			loop(S)
	end.


send_to_clients(Msg, Clients) ->
	lists:map(fun(ClientPid) ->  ClientPid ! Msg end, Clients).

init() -> 
    io:format(">>> Initiating chat process : ~n"),
	loop(#state{topics=orddict:new(), online=orddict:new()}).

start() ->
	register(?MODULE, Pid=spawn(?MODULE, init, [])),
	Pid.

start_link() ->
	register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
	Pid.

terminate() ->
	?MODULE ! shutdown.



do_subscribe(UserId, Topic, Topics) ->

    orddict:update(Topic, fun (Old) -> Old ++ [UserId] end, [UserId], Topics).


filter_topics_by_user(UserId, UserList) ->
	case lists:member(UserId, UserList) of 
		true -> 
			true;
		false -> 
			false
	end.