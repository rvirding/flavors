%%%-------------------------------------------------------------------
%% @doc flavors public API
%% @end
%%%-------------------------------------------------------------------

-module(flavors_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    flavors_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
