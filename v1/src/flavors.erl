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

%% File    : flavors.erl
%% Author  : Robert Virding
%% Purpose : Basic LFE Flavors interface.

-module(flavors).

-export(['make-init-plist'/1,'instantiate-flavor'/2,send/3]).

-define(Q(E), [quote,E]).			%We do a lot of quoting.

%% send(Instance, Method, Args) -> {Result,Instance}.

send(#{'*flavor-module*' := Fm}=Self, Meth, Args) ->
    Fm:'combined-method'(Meth, Self, Args);
send(_, _, _) ->
    error(flavor_instance).

%'instantiate-flavor'(Flavor, OptionPlist) -> Instance.

'instantiate-flavor'(Flav, Opts) ->
    Fm = flavors_lib:mod_name(Flav),		%Name of the flavor module
    Fc = flavors_lib:core_name(Flav),		%Name of the flavor core module
    %% Check that flavor module is loaded, otherwise make it and load it.
    erlang:module_loaded(Fm) orelse make_load_module(Flav, Fm, Fc),
    %% Now make the instance.
    make_instance(Flav, Fm, Opts).

'make-init-plist'([V,I|Ilist]) ->
    [?Q(V),I|'make-init-plist'(Ilist)];
'make-init-plist'([]) -> [].

make_instance(Flav, Fm, Opts) ->
    Ivars = Fm:'instance-variables'(),
    Mlist = make_map_list(Ivars, Opts),
    maps:from_list([{'*flavor-module*',Fm}|Mlist]).

make_map_list([[V,I]|Mlist], Opts) ->
    Pair = case plist_get(V, Opts) of
	       {ok,I1} -> {V,I1};
	       error -> {V,lfe_eval:expr(I)}
	   end,
    [Pair|make_map_list(Mlist, Opts)];
make_map_list([], _) -> [].

plist_get(X, [X,V|_]) -> {ok,V};
plist_get(X, [_,_|Plist]) -> plist_get(X, Plist);
plist_get(_, []) -> error.

make_load_module(Flav, Fm, Fc) ->
    Nopts = Fc:'normalised-options'(),
    %% Generate error if abstract flavor.
    lists:member('abstract-flavor', Nopts) andalso
	error({'abstract-flavor',Flav}),
    %% Do we want the vanilla falvor
    Van = case lists:member('no-vanilla-flavor', Nopts) of
	      true -> [];
	      false -> ['vanilla-flavor']
	  end,
    Seq = make_comp_sequence([Flav|Van]),
    Ivars = get_ivars(Seq),
    Meths = get_methods(Seq),
    %%lfe_io:format("m: ~p\n", [Meths]),
    Cmeths = get_comb_methods(Meths, Seq),
    Mod = [defmodule,Fm,
	   [export,[name,0],['instance-variables',0],['component-sequence',0],
	    ['combined-methods',0],['combined-method',3]]],
    Funcs = [[defun,name,[],?Q(Flav)],
	     [defun,'instance-variables',[],?Q(Ivars)],
	     [defun,'component-sequence',[],?Q(Seq)],
	     [defun,'combined-methods',[],?Q(Cmeths)]],
    Combs = combined_methods(Cmeths, Flav),
    Combined = [defun,'combined-method'|Combs],
    Forms = [Mod,Combined|Funcs],
    %%lfe_io:format("~p\n", [Forms]),
    Source = lists:concat([Flav,".lfe"]),
    {ok,_,Binary,_} = lfe_comp:forms(Forms, [report,return,{source,Source}]),
    code:load_binary(Fm, lists:concat([Fm,".lfe"]), Binary).

combined_methods(Cmeths, Flav) ->
    E = 'undefined-method',
    [ combined_method(Cm) || Cm <- Cmeths ] ++
	[[[m,'_',as],[error,[tuple,?Q(E),?Q(Flav),m,[length,as]]]]].

combined_method({M,F,Bs,As}) ->
    Acs = [ after_method(M, Af) || Af <- As ],
    Bcs = [ before_method(M, Bf) || Bf <- Bs ],
    Pfc = flavors_lib:core_name(F),
    Pc = [[tuple,ret,self],[':',Pfc,'primary-method',?Q(M),self,args]],
    [[?Q(M),self,args],
     ['let*',Bcs ++ [Pc] ++ Acs,[tuple,ret,self]]].

before_method(M, Bf) ->
    Bfc = flavors_lib:core_name(Bf),
    [self,[':',Bfc,'before-daemon',?Q(M),self,args]].

after_method(M, Bf) ->
    Bfc = flavors_lib:core_name(Bf),
    [self,[':',Bfc,'after-daemon',?Q(M),self,args]].

%% Make the component sequence.

make_comp_sequence(Seq) ->
    add_comps(Seq, []).

add_comps([F|Fs], Seq0) ->
    Seq1 = case lists:member(F, Seq0) of
	       true -> Seq0;
	       false -> add_comp(F, Seq0)       %Add this flavors components
	   end,
    add_comps(Fs, Seq1);
add_comps([], Seq) -> Seq.

add_comp(F, Seq) ->
    Fc = flavors_lib:core_name(F),	        %Flavor core module name
    Cs = Fc:components(),			%Flavor components
    add_comps(Cs, Seq ++ [F]).			%We come before our components

%% get_instance_vars(Sequence) -> Ivars.

%%  Get the instance variables. Use an ordered set to keep the
%%  variables. The instance variables or sorted!

get_ivars(Seq) -> get_ivars(Seq, ordsets:new()).

get_ivars([F|Fs], Ivars) ->
    Fc = flavors_lib:core_name(F),
    Fvs = Fc:'normalised-instance-variables'(),
    get_ivars(Fs, merge_ivars(Fvs, Ivars));
get_ivars([], Ivars) -> Ivars.

merge_ivars([[V,I]|Ivs], Ivars) ->
    case lfe_lib:assoc(V, Ivars) of
	[] -> merge_ivars(Ivs, ordsets:add_element([V,I], Ivars));
	_ -> merge_ivars(Ivs, Ivars)
    end;
merge_ivars([], Ivars) -> Ivars.

%% Get the methods.

get_methods(Seq) -> get_methods(Seq, []).

get_methods([F|Fs], Meths0) ->
    Fms = get_flavor_methods(F),		%Flavor methods
    Meths1 = add_methods(Fms, F, Meths0),
    get_methods(Fs, Meths1);
get_methods([], Meths) -> Meths.

get_flavor_methods(F) ->
    Fc = flavors_lib:core_name(F),
    Os = Fc:'normalised-options'(),
    Ms = Fc:methods(),
    [_|Gs] = lfe_lib:assoc('gettable-instance-variables', Os),
    [_|Ss] = lfe_lib:assoc('settable-instance-variables', Os),
    Ms ++ Gs ++ [ list_to_atom(lists:concat(["set-",I])) || I <- Ss ].

add_methods([Fm|Fms], Flav, Meths) ->
    case lists:keymember(Fm, 1, Meths) of
	true ->
	    add_methods(Fms, Flav, Meths);
	false ->
	    add_methods(Fms, Flav, Meths ++ [{Fm,Flav}])
    end;
add_methods([], _, Meths) -> Meths.

%% Get the combined methods.

get_comb_methods([M|Ms], Seq) ->
    Cm = get_comb_method(M, Seq),
    [Cm|get_comb_methods(Ms, Seq)];
get_comb_methods([], _) -> [].

get_comb_method({M,Flav}, Seq) ->
    {Bds,Ads} = get_daemons(M, Seq, [], []),
    {M,Flav,Bds,Ads}.

get_daemons(M, [F|Fs], Bds0, Ads0) ->
    Fc = flavors_lib:core_name(F),	        %Flavor core module name
    Bds1 = case lists:member(M, Fc:daemons(before)) of
	       true -> [F|Bds0];
	       false -> Bds0
	   end,
    Ads1 = case lists:member(M, Fc:daemons('after')) of
	       true -> [F|Ads0];
	       false -> Ads0
	   end,
    get_daemons(M, Fs, Bds1, Ads1);
get_daemons(_, [], Bds, Ads) ->
    {lists:reverse(Bds),Ads}.
