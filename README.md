cowboy-automatic-route-dispatcher-sync (CARDS)
=================================

A Cowboy based simple router with Sync. Cards use a simple filename based convention of defining controllers.

```/``` will translate to ```httproot_controller:index(Method, Req, Options)```
```/:controllerOne/``` will translate to ```controllerOne_controller:index(Method, Req, Options)```
```/:controllerTwo/:actionOne``` will translate to ```controllerTwo_controller:actionOne(Method, Req, Options)```
```/:controllerThree/:actionTwo/[:extras]``` will translate to ```controllerThree_controller.erl:actionTwo(Method, Req, Options)``` with the rest of the URL in ```extras``` binding,
where Method is a bitstring representing the METHOD header of an http request and Req is a cowboy Request Object.

#### Declaring Access Policies

Sample policies.erl 

<code>
-module(policies).

-export([get/0]).

get() ->
  #{
    <<"default">> => [false], %%% OPTIONAL, by default is [true], i.e all APIs are public
    <<"controllerOne">> => #{
      <<"default">> => [false], %%% OPTIONAL, by default is [true], i.e all APIs actions with base path /controllerOne will be public
      <<"actionOne">> => [isOwnerOf]
    },
    <<"controllerThree">> => #{
      <<"getinfo">> => [isLoggedIn]
    }
  }.
</code>

Sample Policy Implementation isOwnerOf.erl

<code>
-module(isOwnerOf).
-behaviour(cards_access_policy).

-export([execute/1]).

execute(Req) ->
    {Status, Value, Req2} = cowboy_req:parse_header(<<"owner">>, Req),
    io:format("Status : ~p, owner : ~p~n",[Status, Value]),
    case Value of
      <<"sandeep">> -> {ok, Req2, [{owner,Value}]};
      _ -> {error, Req2, []}
    end.
</code>

isLoggedIn.erl

<code>
-module(isLoggedIn).
-behaviour(cards_access_policy).

-export([execute/1]).

execute(Req) ->
    {Status, Value, Req2} = cowboy_req:parse_header(<<"loggedin">>, Req),
    io:format("Status : ~p, logged in : ~p~n",[Status, Value]),
    case {Status,Value} of
      {ok, _} -> {ok, Req2, [{permitted,on},{loggedin,Value}]};
      {undefined, undefined} -> {error, Req2, []};
      {undefined, _} -> {ok, Req2, [{permitted,on},{loggedin,Value}]};
      {_,_} -> {error, Req2, []}
    end.
</code>

#### Adding cards to your stack

#### rebar
 > {cards, ".*", {git, "git@github.com:psandeepunni/cowboy-automatic-route-dispatcher.git", {branch, "master"}}}

#### erlang.mk
 > DEPS = cards
 >
 > dep_cards = git https://github.com/psandeepunni/cowboy-automatic-route-dispatcher.git master
