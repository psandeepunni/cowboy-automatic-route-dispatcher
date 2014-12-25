-module(cards_access_policy).

-callback execute(Req)
  -> {ok, Req, list()}
  | {error, Req, list()}
  when Req::cowboy_req:req().
