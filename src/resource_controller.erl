-module(resource_controller).

-export([actionone/2]).
-export([actiontwo/2]).

actionone(<<"GET">>,Req) ->
  cowboy_req:reply(200,[],<<"GET /resource/actionone">>, Req).

actiontwo(<<"POST">>, Req) ->
  cowboy_req:reply(200,[],<<"POST /resource/actiontwo">>, Req).
