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

%%--------------------------------------------------------------------
%% @doc Gets/Extracts the Controller Module Name from Request path.
%% @spec get_module(binary()) -> atom()
%% @end
%%--------------------------------------------------------------------

get_module(Controller) ->
  Part = <<"_controller">>,
  ModuleName = <<Controller/binary,Part/binary>>,
  case binary_to_existing_atom(ModuleName, utf8) of
    {'EXIT', {badarg,_}} ->
      binary_to_atom(ModuleName, utf8);
    _ ->
      binary_to_existing_atom(ModuleName, utf8)
  end.

%%--------------------------------------------------------------------
%% @doc Gets/Extracts the Function From Module in Request path.
%% @spec get_function(binary()) -> atom()
%% @end
%%--------------------------------------------------------------------

get_function(Action) ->
  case catch binary_to_existing_atom(Action, utf8) of
    {'EXIT', {badarg,_}} ->
      binary_to_atom(Action,utf8);
    _ ->
      binary_to_existing_atom(Action,utf8)
  end.

%%--------------------------------------------------------------------
%% @doc Responds to an Http/Https request.
%% @spec response(atom(),{binary(),binary(),binary()}, term(), term(), term()) -> {atom(), term(), term()}
%% @end
%%--------------------------------------------------------------------

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

%%--------------------------------------------------------------------
%% @doc Executes a Chain of access policy modules, and returns their results.
%% @spec evaluate_access_policy(atom(), list(), term(), term()) -> {atom(), term(), list()}
%% @end
%%--------------------------------------------------------------------

evaluate_access_policy(Status,[],Req,Opts) ->
  {Status, Req, Opts};
evaluate_access_policy(error,_,Req, Opts) ->
  {error, Req, Opts};
evaluate_access_policy(ok, [Policy|T], Req, Opts) ->
  {Status, Req1, Opts1} = apply(Policy, execute, [Req]),
  evaluate_access_policy(Status, T, Req1, [Opts1 | Opts]).

%%--------------------------------------------------------------------
%% @doc Evaluate & Executes a Chain of access policy modules, and returns their results.
%% @spec evaluate_access_policy(list(), term()) -> {atom(), term(), list()}
%% @end
%%--------------------------------------------------------------------

execute_access_policy([true], Req) ->
  {ok, Req, []};
execute_access_policy([false], Req) ->
  {error, Req, []};
execute_access_policy(PolicyModuleList, Req) ->
  evaluate_access_policy(ok, PolicyModuleList, Req, []).

%%--------------------------------------------------------------------
%% @doc Get the access policy for the action in the route path. If it's not present, then the controller default policy, if declared, is used. Else, Global Access Policy or All Public Access is defined
%% @spec get_action_policy(atom(), binary(), term(), list()) -> list()
%% @end
%%--------------------------------------------------------------------

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

%%--------------------------------------------------------------------
%% @doc Get/Load the Access Policy for the server from the policies module. The Server crashes if no policies module is provided.
%% @spec get_policies(binary(), binary()) -> list()
%% @end
%%--------------------------------------------------------------------

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

%%--------------------------------------------------------------------
%% @doc Cowboy Handler to route the request to the correct Controller/Action, after evaluating the access policy
%% @spec handle(term(), term()) -> {atom(),term(),term()}
%% @end
%%--------------------------------------------------------------------

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
