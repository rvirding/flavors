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

%% File    : vanilla-flavor-core.erl
%% Author  : Robert Virding
%% Purpose : Basic LFE Flavors vanilla flavor definition.

%% This is just simple definition of the vanilla flavor base flavor.
%%
%% (defflavor vanilla () ()
%%    abstract-flavor)
%%
%% (defmethod (vanilla print-self) (stream)
%%   (lfe_io:print stream self))
%%
%% (defmethod (vanilla print-self) ()
%%   (lfe_io:print self))
%%
%% (defmethod (vanilla set) (var val)
%%   (set var val))
%%
%% (endflavor vanilla)

-module('vanilla-flavor-core').

-compile({no_auto_import,[get/0,get/1]}).

-export([name/0,'instance-variables'/0,components/0,options/0]).
-export(['local-instance-variables'/0,
         'gettable-instance-variables'/0,
         'settable-instance-variables'/0,
         'inittable-instance-variables'/0,
	 'init-keywords'/0,
         plist/0]).
-export(['primary-methods'/0,'before-daemons'/0,'after-daemons'/0]).
-export(['primary-method'/3,'before-daemon'/3,'after-daemon'/3]).

name() -> 'vanilla'.

'instance-variables'() -> [].

components() -> [].

options() -> ['abstract-flavor'].

'local-instance-variables'() -> [].

'gettable-instance-variables'() -> [].

'settable-instance-variables'() -> [].

'inittable-instance-variables'() -> [].

'init-keywords'() -> [].

plist() -> [{'abstract-flavor',true}].

%% These must be ordsets.
'primary-methods'() -> [{'print-self',0},{'print-self',1},{set,2}].

'before-daemons'() -> [].

'after-daemons'() -> [].

-compile({nowarn_unused_function, [get/0,get/1,set/2]}).

get() -> erlang:get('instance-variables').

get(Var) ->
    maps:get(Var, erlang:get('instance-variables')).

set(Var, Val) ->
    Ivars0 = erlang:get('instance-variables'),
    Ivars1 = maps:update(Var, Val, Ivars0),
    erlang:put('instance-variables', Ivars1),
    Val.

'primary-method'('print-self', Self, {Stream}) ->
    lfe_io:print(Stream, {Self,get()});
'primary-method'('print-self', Self, {}) ->
    lfe_io:print({Self,get()});
'primary-method'(set, _, {I,V}) ->
    set(I, V);
'primary-method'(M, _, _) ->
    error({'undefined-primary-method','vanilla',M}).

'before-daemon'(M, _, _) ->
    error({'undefined-before-daemon','vanilla',M}).

'after-daemon'(M, _, _) ->
    error({'undefined-after-daemon','vanilla',M}).
