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

%% File    : property-list-mixin-flavor-core.erl
%% Author  : Robert Virding
%% Purpose : Basic LFE Flavors property-list-mixin definition.

%% This is just simple definition of the property-list-mixin flavor.
%%
%% (defflavor property-list-mixin ((property-list ())) ()
%%   settable-instance-variables
%%   abstract-flavor)
%%
%% (defmethod (get) (pname)
%%   (cl:getf (get 'property-list) pname))
%%
%% (defmethod (get) (pname def)
%%   (cl:getf (get 'property-list) pname def))
%%
%% (defmethod (getl) (pnames)
%%   (element 3 (cl:get-properties (get 'property-list) pnames)))
%%
%% (defmethod (putprop) (value pname)
%%   (let ((plist (cl:putf (get 'property-list) value pname)))
%%     (set 'property-list plist)))
%%
%% (defmethod (remprop) (pname)
%%   (let ((plist (cl:remf (get 'property-list) pname)))
%%     (set 'property-list plist)))
%%
%% (endflavor property-list-mixin)

-module('property-list-mixin-flavor-core').

-compile({no_auto_import,[get/0,get/1]}).
-compile({nowarn_unused_function, [get/0,get/1,set/1,set/2]}).

-export([name/0,'instance-variables'/0,components/0,options/0]).
-export(['local-instance-variables'/0,
         'gettable-instance-variables'/0,
         'settable-instance-variables'/0,
         'inittable-instance-variables'/0,
	 'init-keywords'/0,
         plist/0]).
-export(['primary-methods'/0,'before-daemons'/0,'after-daemons'/0]).
-export(['primary-method'/3,'before-daemon'/3,'after-daemon'/3]).

name() -> 'property-list-mixin'.

'instance-variables'() -> [['property-list',[]]].

components() -> [].

options() -> ['settable-instance-variables','abstract-flavor'].

'local-instance-variables'() -> ['property-list'].

'gettable-instance-variables'() -> ['property-list'].

'settable-instance-variables'() -> ['property-list'].

'inittable-instance-variables'() -> ['property-list'].

'init-keywords'() -> [].

plist() -> [{'abstract-flavor',true}].

%% These must be ordsets.
'primary-methods'() ->
    [{'get',1},{'get',2},{'getl',1},{putprop,2},{remprop,2}].

'before-daemons'() -> [].

'after-daemons'() -> [].

get() -> erlang:get('instance-variables').

get(Var) ->
    maps:get(Var, erlang:get('instance-variables')).

set(Ivars) ->
    erlang:put('instance-variables', Ivars).

set(Var, Val) ->
    Ivars0 = erlang:get('instance-variables'),
    Ivars1 = maps:update(Var, Val, Ivars0),
    erlang:put('instance-variables', Ivars1),
    Val.

'primary-method'('get', _, {Pname}) ->
    cl:getf(get('property-list'), Pname);
'primary-method'('get', _, {Pname,Def}) ->
    cl:getf(get('property-list'), Pname, Def);
'primary-method'('getl', _, {Pnames}) ->
    {_,_,Plist} = cl:'get-properties'(get('property-list'), Pnames),
    Plist;
'primary-method'('putprop', _, {Value,Pname}) ->
    Plist = cl:putf(get('property-list'), Value, Pname),
    set('property-list', Plist),
    Plist;
'primary-method'('remprop', _, {Pname}) ->
    Plist = cl:remf(get('property-list'), Pname),
    set('property-list', Plist),
    Plist;
'primary-method'('get-property-list', _, {}) ->
    get('property-list');
'primary-method'('set-property-list', _, {Plist}) ->
    set('property-list', Plist);
'primary-method'(M, _, _) ->
    error({'undefined-primary-method','property-list-mixin',M}).

'before-daemon'(M, _, _) ->
    error({'undefined-before-daemon','property-list-mixin',M}).

'after-daemon'(M, _, _) ->
    error({'undefined-after-daemon','property-list-mixin',M}).
