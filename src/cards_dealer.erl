-module(cards_dealer).
-behaviour(cowboy_http_handler).

%% API
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
  {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
  {Method, Req1} = cowboy_req:method(Req),
  {Controller,_} = cowboy_req:binding(controller, Req1),
  {Action,_} = cowboy_req:binding(action, Req1, <<"index">>),
  io:format("Controller : ~p, Action : ~p, Method : ~p~n",[Controller,Action,Method]),
  Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, POST, PUT, DELETE, OPTIONS">>, Req1),
  Req3 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req2),
  {RequestHeaders, Req4} = cowboy_req:header(<<"access-control-request-headers">>, Req3, <<"*">>),
  Req5 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, RequestHeaders, Req4),
  PolicyModuleNameList = access_policy:get(Controller, Action),
  {AccessStatus, Req6, Opts} = access_policy:evaluate(PolicyModuleNameList, Req5),
  cards_res:reply(AccessStatus, {Method, Controller, Action}, Req6, Opts, State).


terminate(_Reason, _Req, _State) ->
  ok.
