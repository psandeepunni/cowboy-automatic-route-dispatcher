cowboy-automatic-route-dispatcher-sync (CARDS)
=================================

A Cowboy based simple router with Sync. Cards use a simple filename based convention of defining controllers.

```/``` will translate to ```httproot.erl:index(Method, Req)```
```/:controllerOne/``` will translate to ```controllerOne_controller.erl:index(Method, Req)```
```/:controllerTwo/:actionOne``` will translate to ```controllerTwo_controller.erl:actionOne(Method, Req)```
```/:controllerThree/:actionTwo/[:extras]``` will translate to ```controllerThree_controller.erl:actionTwo(Method, Req)``` with the rest of the URL in ```extras``` binding,
where Method is a bitstring representing the METHOD header of an http request and Req is a cowboy Request Object.


#### Adding cards to your stack

#### rebar
 > {cards, ".*", {git, "git@github.com:psandeepunni/cowboy-automatic-route-dispatcher.git", {branch, "master"}}}

#### erlang.mk
 > DEPS = cards
 >
 > dep_cards = git https://github.com/psandeepunni/cowboy-automatic-route-dispatcher.git master
