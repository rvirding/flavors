%% Copyright (c) 2015 Robert Virding
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% File    : flavors_lib.erl
%% Author  : Robert Virding
%% Purpose : Basic LFE Flavors library functions.

-module(flavors_lib).

-export([mod_name/1,core_name/1]).

%% List set functions
-export([member/2,union/2,intersection/2,subtract/2]).

-include("flavors.hrl").

mod_name(Flav) ->
    list_to_atom(lists:concat([Flav,"-flavor"])).

core_name(Flav) ->
    list_to_atom(lists:concat([Flav,"-flavor-core"])).

%% member(Elem, List) -> Bool.
%% union(List1, List2) -> UList.
%% intersection(List1, List2) -> IList.
%% subtract(List1, List2) -> Slist.
%%  Lists as sets functions.

member(E, L) -> lists:member(E, L).

union(L1, L2) ->
    [ L || L <- L1, not lists:member(L, L2) ] ++ L2.

intersection(L1, L2) ->
    [ L || L <- L1, lists:member(L, L2) ].

subtract(L1, L2) ->
    [ L || L <- L1, not lists:member(L, L2) ].
