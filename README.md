
1. Framework introduction: 
https://dev.to/taure/routing-m91
2. Framework doc
https://hexdocs.pm/nova/quick-start.html#content
3. APP with db example:
https://github.com/Taure/nova_db_app

4. chat example
https://github.com/novaframework/nova_chat


## In order to build frontend
1. `cd src/views`
2. `elm make src/Index.elm --output ../../priv/assets/main.js`
3. Stop running app: with ctrl + c
4. Run it again `cd app_server && rebar3 nova serve`