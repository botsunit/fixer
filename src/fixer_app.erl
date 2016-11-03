% @hidden
-module(fixer_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  fixer_sup:start_link().

stop(_State) ->
  ok.
