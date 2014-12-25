-module(cards_dealer).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
  {ok, Req, #state{}}.

%%%============================================================================
%%% Function : get_module
%%% Params : Controller/binary
%%% Return : Controller/atom
%%%============================================================================

get_module(Controller) ->
  Part = <<"_controller">>,
  ModuleName = <<Controller/binary,Part/binary>>,
  case binary_to_existing_atom(ModuleName, utf8) of
    {'EXIT', {badarg,_}} ->
      binary_to_atom(ModuleName, utf8);
    _ ->
      binary_to_existing_atom(ModuleName, utf8)
  end.

%%%============================================================================
%%% Function : get_function
%%% Params : Action/binary
%%% Return : Action/atom
%%%============================================================================

get_function(Action) ->
  case catch binary_to_existing_atom(Action, utf8) of
    {'EXIT', {badarg,_}} ->
      binary_to_atom(Action,utf8);
    _ ->
      binary_to_existing_atom(Action,utf8)
  end.

%%%==========================================================================================================
%%% Function : response
%%% Params : Status/atom, {Method/binary, Controller/binary, Action/binary}, Req/Term, Opts/Term, State/Term
%%% Return : Action/atom
%%%==========================================================================================================

response(error, _, Req, _, State) ->
  {ok, Req1} = cowboy_req:reply(200,[],<<"you are not authorized to access the api">>, Req),
  {ok, Req1, State};
response(ok, {<<"OPTIONS">>, _, _}, Req, _, State) ->
  {Status, Req1} = cowboy_req:reply(200,[{<<"Access-Control-Allow-Headers">>,<<"origin, x-csrftoken, content-type, accept">>}],<<"">>,Req),
  {Status, Req1, State};
response(ok, {Method, Controller, Action}, Req, Opts, State) ->
  Module = get_module(Controller),
  Func = get_function(Action),
  {Status, Req1} = apply(Module, Func, [Method, Req, Opts]),
  {Status, Req1, State}.

%%%==========================================================================================================
%%% Evaluate & Execute Chained Access Policies
%%% Function : evaluate_access_policy
%%% Params : Status/atom, PolicyList/list, Req/Term, Opts/Term,
%%% Return : Status/atom, Req/Term, Opts/term
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Function : execute_access_policy
%%% Params : Status/atom, Req/Term
%%% Return : Status/atom, Req/Term, Opts/term
%%%==========================================================================================================

evaluate_access_policy(Status,[],Req,Opts) ->
  {Status, Req, Opts};
evaluate_access_policy(error,_,Req, Opts) ->
  {error, Req, Opts};
evaluate_access_policy(ok, [Policy|T], Req, Opts) ->
  {Status, Req1, Opts1} = apply(Policy, execute, [Req]),
  evaluate_access_policy(Status, T, Req1, [Opts1 | Opts]).

execute_access_policy([true], Req) ->
  {ok, Req, []};
execute_access_policy([false], Req) ->
  {error, Req, []};
execute_access_policy(PolicyModuleList, Req) ->
  evaluate_access_policy(ok, PolicyModuleList, Req, []).

%%%==========================================================================================================
%%% Function : get_action_policy
%%% Params : Status/atom, Action/binary, PolicyMap/Term, Default/atom
%%% Return : ModuleName/atom
%%%==========================================================================================================

get_action_policy(error, _, _, GlobalDefault) ->
  GlobalDefault;
get_action_policy(ok, Action, PolicyMap, GlobalDefault) ->
  Result = maps:find(Action, PolicyMap),
  case Result of
    error ->
      maps:get(<<"default">>, PolicyMap, GlobalDefault);
    _ ->
      {_, ActionPolicyModuleList} = Result,
      ActionPolicyModuleList
  end.

%%%==========================================================================================================
%%% Function : get_policies
%%% Params : Controller/binary, Action/binary
%%% Return : ModuleName/atom
%%%==========================================================================================================

get_policies(Controller, Action) ->
  Policies = policies:get(),
  GlobalDefaultPolicy = maps:get(<<"default">>,Policies,[true]),
  Result = maps:find(Controller,Policies),
  {ControllerPolicyFoundStatus, ControllerPolicyMap} =  case Result of
                                                          error -> {error, #{}};
                                                          _ -> Result
                                                        end,
  PolicyModuleNameList = get_action_policy(ControllerPolicyFoundStatus, Action, ControllerPolicyMap, GlobalDefaultPolicy),
  PolicyModuleNameList.

handle(Req, State=#state{}) ->
  {Method, Req1} = cowboy_req:method(Req),
  {Controller,_} = cowboy_req:binding(controller,Req1),
  {Action,_} = cowboy_req:binding(action,Req1),
  Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, POST, PUT, DELETE, OPTIONS">>, Req1),
  Req3 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req2),
  PolicyModuleNameList = get_policies(Controller, Action),
  {AccessStatus, Req4, Opts} = execute_access_policy(PolicyModuleNameList, Req3),
  response(AccessStatus, {Method, Controller, Action}, Req4, Opts, State).


terminate(_Reason, _Req, _State) ->
  ok.
