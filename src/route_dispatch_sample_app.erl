-module(route_dispatch_sample_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
      {'_', [{"/:controller/:action", route_handler, []}]}
  ]),
  cowboy:start_http(my_http_listener, 100, [{port, 8080}],
      [{env, [{dispatch, Dispatch}]}]
  ),
	route_dispatch_sample_sup:start_link().

stop(_State) ->
	ok.
