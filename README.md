cowboy-automatic-route-dispatcher-sync (CARDS)
=================================

A Cowboy based simple router with Sync. Cards use a simple filename based convention of defining controllers.

```/:controllerOne/:actionOne``` will translate to ```controllerOne_controller.erl:actionOne(Method, Req)```
where Method is a bitstring representing the METHOD header of an http request and Req is a cowboy Request Object.


#### Adding cards to your stack

#### rebar
 > {cards, ".*", {git, "git@github.com:psandeepunni/cowboy-automatic-route-dispatcher.git", {branch, "master"}}}

#### erlang.mk
 > DEPS = cards
 > dep_cards = https://github.com/psandeepunni/cowboy-automatic-route-dispatcher.git master
