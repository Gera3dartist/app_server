-module(app_server_users_controller).
-export([
         get_users/1,
         get_single_user/1,
         get_user_topics/1
        ]).

get_users(_Req) ->
    SQL = <<"SELECT * FROM public.users">>,
    case pgo:query(SQL, []) of
        #{command := select,
            rows := Rows} -> logger:warning("UsersList: ~p", [Rows]),
                             UserList = [ 
                                 #{ id => Id, first_name => FirstName, last_name => LastName} 
                                || #{id := Id, first_name := FirstName, last_name := LastName} <- Rows],
                             {json, 200, #{<<"Access-Control-Allow-Origin">> => <<"http://localhost:8000">>}, UserList};
        {error, _Error} ->
            {status, 500}
        end.

get_single_user((#{bindings := #{<<"userid">> := UserId}})) -> 
    SQL = <<"SELECT * FROM public.users WHERE id = $1">>,

    case pgo:query(SQL, [list_to_integer(binary_to_list(UserId))]) of
        #{command := select, rows := Row} -> 
            {json, 200, #{<<"Access-Control-Allow-Origin">> => <<"http://localhost:8000">>}, Row};
        {error, _Error} ->
            logger:warning("ERROR >>> : ~p", [_Error]),

            {status, 500}
        end.


get_user_topics((#{bindings := #{<<"userid">> := UserId}})) -> 

    _UserTopics = chat_backend:user_topics(UserId),

    UserTopics = case orddict:size(_UserTopics) > 0 of
        true -> 
            [#{ name => TopicName} || TopicName <- _UserTopics];
        false -> 
            #{}
        end,
    {json, 200, #{<<"Access-Control-Allow-Origin">> => <<"http://localhost:8000">>}, UserTopics}.

