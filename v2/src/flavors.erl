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

-export(['instantiate-flavor'/2,send/3]).

-include("flavors.hrl").

-define(DBG_PRINT(Format, Args), ok).
%%-define(DBG_PRINT(Format, Args), lfe_io:format(Format, Args)).

%% send(Instance, Method, Args) -> {Result,Instance}.
%%  Send a method and its arguments to be evaluated by flavor
%%  instance. Any errors occuring in the instance are sent back here
%%  and resignaled here. This seems more reasonable than crashing the
%%  instance.

send(#'flavor-instance'{instance=Pid}, Meth, Args) ->
    case flavors_instance:send(Pid, Meth, Args) of
        {ok,Res} -> Res;
        {error,Error} -> error(Error);          %Resignal error
        {exit,Exit} -> exit(Exit);              %Resignal exit
        {throw,Value} -> throw(Value)           %Rethrow value
    end;
send(_, _, _) ->
    error(flavor_instance).

%% 'instantiate-flavor'(Flavor, OptionPlist) -> Instance.
%%  Make a flavor instance using the Plist. If the flavor module is
%%  not loaded then first create, compile and load it. No file is ever
%%  created.

'instantiate-flavor'(Flav, Opts) ->
    Fm = flavors_lib:mod_name(Flav),            %Name of the flavor module
    Fc = flavors_lib:core_name(Flav),           %Name of the flavor core module
    %% Check that flavor module is loaded, otherwise make it and load it.
    erlang:module_loaded(Fm) orelse make_load_module(Flav, Fm, Fc),
    %% Now make the instance.
    {ok,Inst} = flavors_instance:start(Flav, Fm, Opts),
    #'flavor-instance'{flavor=Flav,
                       flavor_mod=Fm,
                       instance=Inst}.

%% make_load_module(Flavor, FlavorModule, FlavorCore) -> {module,FlavorModule}.
%%  Build a flavor module, compile it and finally load it. This module
%%  is always completely in memory.

make_load_module(Flav, Fm, Fc) ->
    Plist = Fc:plist(),                         %This is an orddict
    %% Generate error if abstract flavor.
    orddict:is_key('abstract-flavor', Plist) andalso
        error({'abstract-flavor',Flav}),
    %% Do we want the vanilla flavor?
    Vanilla = case orddict:is_key('no-vanilla-flavor', Plist) of
                  true -> [];
                  false -> ['vanilla-flavor']
              end,
    Seq = make_comp_sequence([Flav|Vanilla]),
    %% Check for required instance variables, methods and flavors.
    check_required_ivars(Seq, Flav),
    check_required_methods(Seq, Flav),
    check_required_flavors(Seq, Flav),
    %% Define the flavor module.
    Ivars = get_instance_vars(Seq),
    Methods = get_methods(Seq),
    ?DBG_PRINT("m: ~p\n", [Methods]),
    Cmethods = get_combined_methods(Methods, Seq),
    Mod = [defmodule,Fm,
           [export,
            [name,0],
            ['instance-variables',0],
            ['component-sequence',0],
            ['combined-methods',0],
            ['combined-method',3]]],
    Funcs = [[defun,name,[],?Q(Flav)],
             [defun,'instance-variables',[],?Q(Ivars)],
             [defun,'component-sequence',[],?Q(Seq)],
             [defun,'combined-methods',[],?Q(Cmethods)]],
    Cclauses = combined_method_clauses(Cmethods, Flav),
    Combined = [defun,'combined-method'|Cclauses],
    Forms = [Mod,Combined|Funcs],
    ?DBG_PRINT("~p\n", [Forms]),
    load_module(Forms, Flav, Fm).

load_module(Forms, Flav, Fm) ->
    Source = lists:concat([Flav,".lfe"]),
    %% Old and new style module compilation.
    case lfe_comp:forms(Forms, [report,return,{source,Source}]) of
        {ok,Bins,_} ->                          %New style
            Load = fun ({ok,M,Bin,_}) ->
                           Bfm = flavors_lib:mod_name(M),
                           code:load_binary(Fm, lists:concat([Bfm,".lfe"]), Bin)
                   end,
            lists:foreach(Load, Bins);
        {ok,_,Binary,_} ->                      %Old style
            code:load_binary(Fm, lists:concat([Fm,".lfe"]), Binary)
    end.

%% make_comp_sequence(Flavors) -> Sequence.
%%  Make the component sequence. The resultant component sequence is a
%%  list of #(flavor flavor-core) and is always kept in order so all
%%  new flavors are added to the end of the sequence list.

make_comp_sequence(Seq) ->
    add_comps(Seq, []).

add_comps([F|Fs], Seq0) ->
    Seq1 = case lists:keymember(F, 1, Seq0) of  %List of #(flav flav-core)
               true -> Seq0;
               false -> add_comp(F, Seq0)       %Add this flavors components
           end,
    add_comps(Fs, Seq1);
add_comps([], Seq) -> Seq.

add_comp(F, Seq) ->
    Fc = flavors_lib:core_name(F),              %Flavor core module name
    Cs = Fc:components(),                       %Flavor components
    add_comps(Cs, Seq ++ [{F,Fc}]).             %We come before our components

%% check_required_ivars(Seq) -> ok.
%% check_required_methods(Seq) -> ok.
%% check_required_flavors(Seq) -> ok.
%%  Check the required instance-variables/methods/flavors are defined.

check_required_ivars(Seq, Flav) ->
    Vars = get_vars(Seq),
    Req = get_required_ivars(Seq),
    case ordsets:subtract(Req, Vars) of
        [] -> ok;
        Vs -> error({'required-instance-variables',Flav,Vs})
    end.

get_vars(Seq) -> get_vars(Seq, ordsets:new()).

get_vars([{_,Fc}|Fs], Vars) ->
    Fvs = Fc:'local-instance-variables'(),      %This is an ordset
    get_vars(Fs, ordsets:union(Fvs, Vars));
get_vars([], Vars) -> Vars.

check_required_methods(Seq, Flav) ->
    Meths = get_meths(Seq),
    Req = get_required_methods(Seq),
    case ordsets:subtract(Req, Meths) of
        [] -> ok;
        Ms -> error({'required-methods',Flav,Ms})
    end.

get_meths(Seq) -> get_meths(Seq, ordsets:new()).

get_meths([{_,Fc}|Fs], Meths) ->
    Ms = Fc:'primary-methods'(),                %This is an ordset
    get_meths(Fs, ordsets:union(Ms, Meths));
get_meths([], Meths) -> Meths.

check_required_flavors(Seq, Flav) ->
    Flavs = get_flavs(Seq),
    Req = get_required_flavors(Seq),
    case ordsets:subtract(Req, Flavs) of
        [] -> ok;
        Fs -> error({'required-methods',Flav,Fs})
    end.

get_flavs(Seq) ->
    lists:map(fun ({F,_}) -> F end, Seq).

%% get_required_ivars(Sequence) -> InstanceVars.
%% get_required_methods(Sequence) -> Methods.
%% get_required_flavors(Sequence) -> Flavors.
%%  Get the required instance-variables/methods/flavors from the
%%  components. These have all been saved in the same way on the
%%  plist.

get_required_ivars(Seq) ->
    get_required(Seq, 'required-instance-variables').

get_required_methods(Seq) ->
    get_required(Seq, 'required-methods').

get_required_flavors(Seq) ->
    get_required(Seq, 'required-flavors').

get_required(Seq, What) ->
    Req = fun ({_,Fc}, Flavs) ->
                  Plist = Fc:plist(),           %This is an orddict
                  case orddict:find(What, Plist) of
                      {ok,Vs} -> ordsets:union(ordsets:from_list(Vs), Flavs);
                      error -> Flavs
                  end
          end,
    lists:foldl(Req, ordsets:new(), Seq).

%% get_instance_vars(Sequence) -> Ivars.
%%  Get the instance variables. Use an ordered set to keep the
%%  variables. The instance variables are sorted! We have to add them
%%  from the rear so that variable initialisation from earlier flavors
%%  take precedence.

get_instance_vars(Seq) ->
    %% From the rear!
    lists:foldr(fun merge_instance_vars/2, orddict:new(), Seq).

merge_instance_vars({_,Fc}, Vars) ->
    Fvs = Fc:'instance-variables'(),
    lists:foldl(fun ([V,I], Vs) -> orddict:store(V, I, Vs);
                    (V, Vs) -> orddict:store(V, ?Q(undefined), Vs)
                end, Vars, Fvs).

%% get_methods(Sequence) -> Methods.
%%  Get the methods. The resultant Methods is a list of #(method
%%  flavor) and is kept in order so new methods are added to the end
%%  of the method list.

get_methods(Seq) -> get_methods(Seq, []).

get_methods([{F,Fc}|Fs], Meths0) ->
    Fms = get_flavor_methods(Fc),               %Flavor methods
    Meths1 = add_methods(Fms, F, Meths0),
    get_methods(Fs, Meths1);
get_methods([], Meths) -> Meths.

get_flavor_methods(Fc) ->
    Ms = Fc:'primary-methods'(),                %User defined methods
    Gs = Fc:'gettable-instance-variables'(),
    Ss = Fc:'settable-instance-variables'(),
    Ms ++ Gs ++ [ list_to_atom(lists:concat(["set-",I])) || I <- Ss ].

add_methods([Fm|Fms], Flav, Meths0) ->
    Meths1 = case lists:keymember(Fm, 1, Meths0) of
                 true -> Meths0;
                 false -> Meths0 ++ [{Fm,Flav}]
             end,
    add_methods(Fms, Flav, Meths1);
add_methods([], _, Meths) -> Meths.

%% get_combined_methods(Methods, Seq) -> CombMethods.
%%  Get the combined methods returning the method, flavor and
%%  before/after daemons for each method.

get_combined_methods([M|Ms], Seq) ->
    Cm = get_combined_method(M, Seq),
    [Cm|get_combined_methods(Ms, Seq)];
get_combined_methods([], _) -> [].

get_combined_method({M,Flav}, Seq) ->
    {Bds,Ads} = get_daemons(M, Seq, [], []),
    {M,Flav,Bds,Ads}.

get_daemons(M, [{F,Fc}|Fs], Bds0, Ads0) ->
    Bds1 = case lists:member(M, Fc:'before-daemons'()) of
               true -> [F|Bds0];
               false -> Bds0
           end,
    Ads1 = case lists:member(M, Fc:'after-daemons'()) of
               true -> [F|Ads0];
               false -> Ads0
           end,
    get_daemons(M, Fs, Bds1, Ads1);
get_daemons(_, [], Bds, Ads) ->
    {lists:reverse(Bds),Ads}.                   %Get the order right

%% combined_method_clauses(CombinedMeths, Flavor) -> Clauses.
%%  Get the combined method clauses for a flavor. The clause for each
%%  method will first call the before daemons, then the primary clause
%%  and finally the after daemons.

combined_method_clauses(Cmeths, Flav) ->
    E = 'undefined-method',
    [ combined_method_clause(Cm) || Cm <- Cmeths ] ++
        [[[m,'_',as],[error,[tuple,?Q(E),?Q(Flav),m,[length,as]]]]].

combined_method_clause({M,F,Bs,As}) ->
    Bcs = [ call_before_method(M, Bf) || Bf <- Bs ],
    Acs = [ call_after_method(M, Af) || Af <- As ],
    Pfc = flavors_lib:core_name(F),
    Pc = [':',Pfc,'primary-method',?Q(M),self,args],
    PAs = if Acs =:= [] -> [Pc];                %Primary and after daemons
             true -> [[prog1,Pc|Acs]]
          end,
    [[?Q(M),self,args] | Bcs ++ PAs].

call_before_method(M, Bf) ->
    Bfc = flavors_lib:core_name(Bf),
    [':',Bfc,'before-daemon',?Q(M),self,args].

call_after_method(M, Af) ->
    Afc = flavors_lib:core_name(Af),
    [':',Afc,'after-daemon',?Q(M),self,args].
