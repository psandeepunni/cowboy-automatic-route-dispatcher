-module(cards_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
    {'_', [
    {"/:controller/:action/[:extras]", cards_dealer, []},
    {"/", cards_root_handler, []}
    ]}
  ]),
  cowboy:start_http(my_http_listener, 100, [{port, application:get_env(cards, cowboy_port, 8080)}],
    [{env, [{dispatch, Dispatch}]}]
  ),
  cards_sup:start_link().

stop(_State) ->
    ok.
