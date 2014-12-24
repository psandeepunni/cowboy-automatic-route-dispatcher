-module(cards_dealer).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
  {ok, Req, #state{}}.

  
get_module({ModuleName}) ->
  case binary_to_existing_atom(ModuleName, utf8) of
    {'EXIT', {badarg,_}} ->
      binary_to_atom(ModuleName, utf8);
    _ ->
      binary_to_existing_atom(ModuleName, utf8)
  end;
get_module(controller) ->
  get_module({<<"httproot">>});
get_module(Controller) ->
  Part = application:get_env(application:get_application(), handle_tail, <<"controller">>),
  ModuleName = <<Controller/binary,Part/binary>>,
  get_module({ModuleName}).

get_function(Action) ->
  case catch binary_to_existing_atom(Action, utf8) of
    {'EXIT', {badarg,_}} ->
      binary_to_atom(Action,utf8);
    _ ->
      binary_to_existing_atom(Action,utf8)
  end.

response(<<"OPTIONS">>, Req, State) ->
  {Status, Req1} = cowboy_req:reply(200,[{<<"Access-Control-Allow-Headers">>,<<"origin, x-csrftoken, content-type, accept">>}],<<"">>,Req),
  {Status, Req1, State};
response(Method, Req, State) ->
  {Controller,_} = cowboy_req:binding(controller,Req,controller),
  {Action,_} = cowboy_req:binding(action,Req,<<"index">>),
  Module = get_module(Controller),
  Func = get_function(Action),
  {Status, Req1} = apply(Module, Func, [Method,Req]),
  {Status, Req1, State}.

handle(Req, State=#state{}) ->
  {Method, Req1} = cowboy_req:method(Req),
  Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, POST, PUT, DELETE, OPTIONS">>, Req1),
  Req3 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req2),
  response(Method,Req3, State).


terminate(_Reason, _Req, _State) ->
  ok.
