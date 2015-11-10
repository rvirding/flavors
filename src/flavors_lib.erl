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

%% List set functions.
-export([member/2,adjoin/2,union/2,intersection/2,'set-difference'/2]).

%% Property list functions.
-export([getf/2,getf/3,putf/3,remf/2,'get-properties'/2]).

-include("flavors.hrl").

mod_name(Flav) ->
    list_to_atom(lists:concat([Flav,"-flavor"])).

core_name(Flav) ->
    list_to_atom(lists:concat([Flav,"-flavor-core"])).

%% member(Elem, List) -> Bool.
%% adjoin(Item, Lis) -> List.
%% union(List1, List2) -> UList.
%% intersection(List1, List2) -> IList.
%% set-difference(List1, List2) -> Slist.
%%  Lists as sets functions.

member(E, L) -> lists:member(E, L).

adjoin(I, L) ->
    case member(I, L) of
	true -> L;
	false -> [I|L]
    end.

union(L1, L2) ->
    [ L || L <- L1, not lists:member(L, L2) ] ++ L2.

intersection(L1, L2) ->
    [ L || L <- L1, lists:member(L, L2) ].

'set-difference'(L1, L2) ->
    [ L || L <- L1, not lists:member(L, L2) ].

%% getf(Plist, Pname[, Default]) -> Value.
%% putf(Plist, Value, Pname) -> Plist.
%% remf(Plist, Pname) -> Plist.
%%  Property list functions.

getf(Plist, Pname) -> getf(Plist, Pname, []).

getf([Pname,Val|_], Pname, _) -> Val;
getf([_,_|Plist], Pname, Def) ->
    getf(Plist, Pname, Def);
getf([], _, Def) -> Def.

putf([Pname,_|Plist], Val, Pname) ->
    [Pname,Val|Plist];
putf([P,V|Plist], Val, Pname) ->
    [P,V|putf(Plist, Val, Pname)];
putf([], Val, Pname) ->
    [Pname,Val].

remf([Pname,_|Plist], Pname) -> Plist;
remf([P,V|Plist], Pname) ->
    [P,V|remf(Plist, Pname)];
remf([], _) -> [].

'get-properties'([P,V|Plist], Pnames) ->
    case member(P, Pnames) of
	true -> {P,V,[P,V|Plist]};
	false -> 'get-properties'(Plist, Pnames)
    end;
'get-properties'([], _) -> {[],[],[]}.
