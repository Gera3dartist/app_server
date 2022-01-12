-module(app_server_router).
-behaviour(nova_router).

-export([
         routes/1
        ]).

%% The Environment-variable is defined in your sys.config in {nova, [{environment, Value}]}
routes(_Environment) ->
    [#{prefix => "",
      security => false,
      routes => [
                 {"/", { app_server_chat_controller, index}, #{methods => [get]}},
                 {"/topics", {app_server_chat_controller_simple, topics_list}, #{methods => [get]}},
                 {"/topic/:topic", {app_server_chat_controller_simple, topic}, #{methods => [put]}},
                 {"/users", { app_server_users_controller, get_users}, #{methods => [get]}},
                 {"/users/:userid", { app_server_users_controller, get_single_user}, #{methods => [get]}},
                 {"/user/:userid/subscribe", {app_server_chat_controller_simple, subscribe}, #{methods => [post]} },
	               {"/user/:userid/ws", app_server_ws, #{protocol => ws, idle_timeout => 30000}},

                 {"/assets/[...]", "assets"}
                ]

      }
    ].
