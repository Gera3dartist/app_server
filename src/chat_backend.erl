-module(chat_backend).
-compile(export_all).
% -export([
%     subscribe/2,
%     list_topics/0,
%     loop/1,
%     start_link/0
%     ]).

-record(state, {topics}).

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

list_topics() -> 
    ?MODULE ! {self(), show_topics},
    receive
		{ok, TopicUsers} ->
			TopicUsers;
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
            NewTopics = orddict:update(
                Topic, fun (Old) -> Old ++ [UserId] end, [UserId], S#state.topics),
			Pid ! ok,
			loop(S#state{topics=NewTopics});
		
		{Pid, {online_users, Topic}} ->
			%% start event
            TopicUsers = orddict:update(
                Topic, fun (Old) -> Old ++ [{"some_user", Pid}] end, [{"some_user", Pid}], S#state.topics),
            % TopicUsers = orddict:find(Topic, S#state.topics),
            Pid ! {ok, TopicUsers},
            loop(S);

		% {'DOWN', Ref, process, _Pid, _Reason} -> 
		% 	loop(S#state{topics=orddict:erase(Ref, S#state.topics)});
		shutdown -> 
			exit(shutdown);
		code_change ->
			?MODULE:loop(S);
		Unknown -> 
			io:format("Unknown message: ~p~n", [Unknown]),
			loop(S)
	end.


send_to_clients(Msg, Clients) ->
	orddict:map(fun(_Ref, ClientPid) ->  ClientPid ! Msg end, Clients).


init() -> 
    io:format(">>> Initiating chat process : ~n"),
	loop(#state{topics=orddict:new()}).

start() ->
	register(?MODULE, Pid=spawn(?MODULE, init, [])),
	Pid.

start_link() ->
	register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
	Pid.

terminate() ->
	?MODULE ! shutdown.

