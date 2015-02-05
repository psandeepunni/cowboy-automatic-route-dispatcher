-module(cards_res).

-export([reply/5]).

%%--------------------------------------------------------------------
%% @doc Gets/Extracts the Controller Module Name from Request path.
%% @spec get_module(binary()) -> atom()
%% @end
%%--------------------------------------------------------------------

get_module({ModuleName}) ->
  case binary_to_existing_atom(ModuleName, utf8) of
    {'EXIT', {badarg,_}} ->
      binary_to_atom(ModuleName, utf8);
    _ ->
      binary_to_existing_atom(ModuleName, utf8)
  end;
get_module(Controller) ->
  Part = application:get_env(cards, handle_tail, <<"_controller">>),
  ModuleName = <<Controller/binary,Part/binary>>,
  get_module({ModuleName}).

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
%% @spec reply(atom(),{binary(),binary(),binary()}, term(), term(), term()) -> {atom(), term(), term()}
%% @end
%%--------------------------------------------------------------------

reply(error, _, Req, _, State) ->
  {ok, Req1} = cowboy_req:reply(200,[],<<"you are not authorized to access the api">>, Req),
  {ok, Req1, State};
reply(ok, {<<"OPTIONS">>, _, _}, Req, _, State) ->
  {Status, Req1} = cowboy_req:reply(200,[{<<"Access-Control-Allow-Headers">>,<<"*">>}],<<"">>,Req),
  {Status, Req1, State};
reply(ok, {Method, Controller, Action}, Req, Opts, State) ->
  Module = get_module(Controller),
  Func = get_function(Action),
  {Status, Req1} = apply(Module, Func, [Method, Req, Opts]),
  {Status, Req1, State}.
