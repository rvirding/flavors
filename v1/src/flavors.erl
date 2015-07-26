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

-export(['make-instance'/2,send/3]).

-define(Q(E), [quote,E]).			%We do a lot of quoting.

%% send(Instance, Method, Args) -> {Result,Instance}.

send(#{'*flavor-module*' := Fm}=Self, Meth, Args) ->
    Fm:'combined-method'(Meth, Self, Args);
send(_, _, _) ->
    error(flavor_instance).

%'make-instance'(Flavor, Options) -> Instance.

'make-instance'(Name, Opts) ->
    Fm = flavors_lib:mod_name(Name),		%Name of the flavor module
    %% Check that flavor module is loaded, otherwise make it and load it.
    erlang:module_loaded(Fm) orelse make_load_module(Name, Fm),
    %% Now make the instance.
    do_make_instance(Fm, Opts).

do_make_instance(Fm, _) ->
    Ivars = Fm:'instance-variables'(),
    maps:from_list([{'*flavor-module*',Fm}|[{V,undefined} || V <- Ivars ]]).

make_load_module(Flav, Fm) ->
    Seq = make_comp_sequence([Flav,'vanilla-flavor']),
    Ivars = get_ivars(Seq),
    Meths = get_methods(Seq),
    Cmeths = get_comb_methods(Meths, Seq),
    Mod = [defmodule,Fm,
	   [export,[name,0],['instance-variables',0],
	    ['combined-methods',0],['combined-method',3]]],
    Funcs = [[defun,name,[],?Q(Fm)],
	     [defun,'instance-variables',[],?Q(Ivars)],
	     [defun,'combined-methods',[],?Q(Cmeths)]],
    Combs = combined_methods(Cmeths, Flav),
    Combined = [defun,'combined-method'|Combs],
    Forms = [Mod,Combined|Funcs],
    lfe_io:format("~p\n", [Forms]),
    {ok,_,Binary} = lfe_comp:forms(Forms),
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
    case lists:member(F, Seq0) of
	true ->
	    add_comps(Fs, Seq0);
	false ->
	    Seq1 = add_comp(F, Seq0),		%Add this flavors components
	    add_comps(Fs, Seq1)
    end;
add_comps([], Seq) -> Seq.

add_comp(F, Seq) ->
    Fc = flavors_lib:core_name(F),	        %Flavor core module name
    Cs = Fc:components(),			%Flavor components
    add_comps(Cs, Seq ++ [F]).			%We come before our components

%% Get the instance variables. Use an ordered set to keep the variables.

get_ivars(Seq) -> get_ivars(Seq, ordsets:new()).

get_ivars([F|Fs], Ivars) ->
    Fc = flavors_lib:core_name(F),
    Fvs = ordsets:to_list(Fc:'instance-variables'()),
    get_ivars(Fs, ordsets:union(Fvs, Ivars));
get_ivars([], Ivars) -> Ivars.

%% Get the methods.

get_methods(Seq) -> get_methods(Seq, []).

get_methods([F|Fs], Meths0) ->
    Fms = get_flavor_methods(F),		%Flavor methods
    Meths1 = add_methods(Fms, F, Meths0),
    get_methods(Fs, Meths1);
get_methods([], Meths) -> Meths.

get_flavor_methods(F) ->
    Fc = flavors_lib:core_name(F),
    Os = Fc:options(),
    Ms = Fc:methods(),
    Is = Fc:'instance-variables'(),
    Gs = case lists:member('gettable-instance-variables', Os) of
	     true -> Is;
	     false -> []
	 end,
    Ss = case lists:member('settable-instance-variables', Os) of
	     true -> [ list_to_atom(lists:concat(["set-",I])) || I <- Is ];
	     false -> []
	 end,
    Ms ++ Gs ++ Ss.    

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
