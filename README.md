CARDS (cowboy automatic route dispatcher and sync)
=================================

A  Rest Framework based on rails with Sync. Cards use a simple filename based convention of defining controllers with automatic route binding. Cowboy is used as the internal http/s server.

1. ```/``` will translate to ```httproot_controller:index(Method, Req, Options)```. 
2. ```/:controllerOne/``` will translate to ```controllerOne_controller:index(Method, Req, Options)```.
3. ```/:controllerTwo/:actionOne``` will translate to ```controllerTwo_controller:actionOne(Method, Req, Options)```.
4. ```/:controllerThree/:actionTwo/[:extras]``` will translate to ```controllerThree_controller.erl:actionTwo(Method, Req, Options)``` with the rest of the URL in ```extras``` binding,
where Method is a bitstring representing the METHOD header of an http request and Req is a cowboy Request Object.

#### Defining Controllers

A route controller is an erlang module with the naming convention of name_controller.erl. Adding APIs, functions with arity 3, to this controller module, will result in automatic secondary path binding, such as, /controller_module_name/function_name

Sample Controller Module

1. resource_controller.erl

```erlang
-module(resource_controller).

-export([actionone/3]).
-export([actiontwo/3]).
-export([login/3]).
-export([index/3]).

%% Binds to Path /resource
index(<<"GET">>, Req, Options) -> %% Options is the list of items returned after evaluation of access policy for the API
  cowboy_req:reply(200,[],<<"GET /resource">>,Req).

%% Binds to API endpoint GET /resource/actionone
actionone(<<"GET">>,Req, Options) -> 
  cowboy_req:reply(200,[],<<"GET /resource/actionone">>,Req).

%% Binds to API endpoint POST /resource/login
login(<<"POST">>, Req, Options) -> 
  cowboy_req:reply(200,[],<<"logged in successfully">>, Req).
  
%% Binds to API endpoint PUT /resource/actiontwo
actiontwo(<<"PUT">>, Req, Options) ->
  cowboy_req:reply(200,[],<<"PUT actiontwo">>, Req).
```
Req object is of type cowboy_req:req() type.

#### Declaring Access Policies

Access policies are declared in policies module. 
Currently has support for chained access policies for end points.

Sample policies.erl 

```erlang
-module(policies).

-export([get/0]).

get() ->
  #{
    <<"default">> => [false], %%% OPTIONAL, by default is [true], i.e all APIs are public
    <<"controllerOne">> => #{
      <<"default">> => [false], %%% OPTIONAL, by default is [true], i.e all APIs actions with base path /controllerOne will be public
      <<"actionOne">> => [isOwnerOf, isLoggedIn]
    },
    <<"controllerThree">> => #{
      <<"getinfo">> => [isLoggedIn]
    }
  }.
```

Sample Policy Implementation isOwnerOf.erl

```erlang
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
```

isLoggedIn.erl

```erlang
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
```

#### Adding cards to your stack

#### rebar
 > {cards, ".*", {git, "git@github.com:psandeepunni/cowboy-automatic-route-dispatcher.git", {branch, "master"}}}

#### erlang.mk
 > DEPS = cards
 >
 > dep_cards = git https://github.com/psandeepunni/cowboy-automatic-route-dispatcher.git master
