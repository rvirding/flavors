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

%% File    : vanilla-flavor-flavor-core.erl
%% Author  : Robert Virding
%% Purpose : Basic LFE Flavors vanilla-flavor definition.

%% This is just simple definition of the base flavor vanilla-flavor.
%%
%% (defflavor vanilla-flavor () () abstract-flavor)
%%
%% (defmethod (vanilla-flavor print-self) (stream)
%%   (lfe_io:print stream self)
%%   (tuple 'ok self))
%%
%% (defmethod (vanilla-flavor set) (var val)
%%   (tuple 'ok (maps:update self var val)))
%%
%% (endflavor vanilla-flavor)

-module('vanilla-flavor-flavor-core').

-export([name/0,'instance-variables'/0,'normalised-instance-variables'/0,
	 components/0,options/0,'normalised-options'/0,
	 methods/0,daemons/1]).

-export(['primary-method'/3,'before-daemon'/3,'after-daemon'/3]).

name() -> 'vanilla-flavor'.

'instance-variables'() -> [].

'normalised-instance-variables'() -> [].

components() -> [].

options() -> [].

'normalised-options'() ->
    [['gettable-instance-variables'],
     ['settable-instance-variables'],
     ['inittable-instance-variables'],
     ['required-instance-variables'],
     ['required-methods'],
     ['required-flavors'],
     'abstract-flavor'
    ].

methods() -> ['print-self',set].

daemons(before) -> [];
daemons('after') -> [].

'primary-method'('print-self', Self, [Stream]) ->
    lfe_io:print(Stream, Self),
    {ok,Self};
'primary-method'(set, Self, [I,V]) ->
    {ok,maps:update(I, V, Self)};
'primary-method'(M, _, _) ->
    error({'undefined-primary-method','vanilla-flavor',M}).

'before-daemon'(M, _, _) ->
    error({'undefined-before-daemon','vanilla-flavor',M}).

'after-daemon'(M, _, _) ->
    error({'undefined-after-daemon','vanilla-flavor',M}).
