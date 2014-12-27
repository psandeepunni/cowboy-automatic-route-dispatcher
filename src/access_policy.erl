-module(access_policy).

-export([evaluate/2]).
-export([get/2]).

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
%% @spec get(binary(), binary()) -> list()
%% @end
%%--------------------------------------------------------------------
get(Controller, Action) ->
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
%% @doc Executes a Chain of access policy modules, and returns their results.
%% @spec evaluate_linked_access_policy(atom(), list(), term(), term()) -> {atom(), term(), list()}
%% @end
%%--------------------------------------------------------------------

evaluate_linked_access_policy(Status,[],Req,Opts) ->
  {Status, Req, Opts};
evaluate_linked_access_policy(error,_,Req, Opts) ->
  {error, Req, Opts};
evaluate_linked_access_policy(ok, [Policy|T], Req, Opts) ->
  {Status, Req1, Opts1} = apply(Policy, execute, [Req]),
  evaluate_linked_access_policy(Status, T, Req1,  lists:append(Opts,Opts1)).

%%--------------------------------------------------------------------
%% @doc Evaluate and Executes a Chain of access policy modules, and returns their results.
%% @spec evaluate(list(), term()) -> {atom(), term(), list()}
%% @end
%%--------------------------------------------------------------------
evaluate([true], Req) ->
  {ok, Req, []};
evaluate([false], Req) ->
  {error, Req, []};
evaluate(PolicyModuleList, Req) ->
  evaluate_linked_access_policy(ok, PolicyModuleList, Req, []).
