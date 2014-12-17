-module(cards_dealer).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
  {ok, Req, #state{}}.

get_module(Controller) ->
  Part = <<"_controller">>,
  ModuleName = <<Controller/binary,Part/binary>>,
  case binary_to_existing_atom(ModuleName, utf8) of
    {'EXIT', {badarg,_}} ->
      binary_to_atom(ModuleName, utf8);
    _ ->
      binary_to_existing_atom(ModuleName, utf8)
  end.

get_function(Action) ->
  case catch binary_to_existing_atom(Action, utf8) of
    {'EXIT', {badarg,_}} ->
      binary_to_atom(Action,utf8);
    _ ->
      binary_to_existing_atom(Action,utf8)
  end.

handle(Req, State=#state{}) ->
  {Controller,_} = cowboy_req:binding(controller,Req),
  {Action,_} = cowboy_req:binding(action,Req),
  Module = get_module(Controller),
  Func = get_function(Action),
  {Method, Req1} = cowboy_req:method(Req),
  Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, POST, PUT, DELETE, OPTIONS">>, Req1),
  Req3 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req2),
  {Status,Req4} = apply(Module, Func, [Method,Req3]),
  {Status, Req4, State}.

terminate(_Reason, _Req, _State) ->
  ok.
