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

%% File    : flavors_comp.erl
%% Author  : Robert Virding
%% Purpose : Basic LFE Flavors compiler.

-module(flavors_comp).

-export([defflavor/4,defmethod/2,endflavor/1]).

-define(Q(E), [quote,E]).                       %We do a lot of quoting

%% The flavor record.
-record(flavor, {name,                          %Flavor name
                 ivars=[],                      %Local instance variables
                 comps=[],                      %Components
                 options=[],
                 %% Derived data.
                 local_vars=[],                 %Local instance variables
                 gettables=[],                  %Gettable, settable, inittable
                 settables=[],
                 inittables=[],
                 req_ivars=[],                  %Required flavors
                 req_meths=[],                  %Required methods
                 req_flavs=[],                  %Required instance vars
                 plist=[],                      %Other esoteric things
                 %%noptions=[],                   %Normalised options
                 methods=[],                    %Primary methods and daemons
                 daemons=[]}).

%% Options arguments we collect.
-record(collect, {vars=[],comps=[],
                  gets=[],sets=[],inits=[],
                  reqi=[],reqm=[],reqf=[],plist=[]}).

defflavor(Name, IVars, Comps, Opts) ->
    %% Check arguments, generate error if faulty.
    check_name(Name),
    Vars = check_instance_vars(Name, IVars),    %Variable names
    check_components(Name, Comps),
    %% Now collect everything.
    C0 = #collect{vars=Vars,comps=Comps},
    C1 = parse_options(Opts, Name, Vars, C0),
    %% Settable instance variables are also gettable and inittable.
    Sets = C1#collect.sets,
    C2 = C1#collect{gets=ordsets:union(Sets, C1#collect.gets)},
    C3 = C2#collect{inits=ordsets:union(Sets, C2#collect.inits)},
    %% Now we have everything so save it in the flavor record.
    Fl = #flavor{name=Name,
                 ivars=IVars,
                 comps=Comps,
                 options=Opts,
                 %% Our processed data.
                 gettables=C3#collect.gets,
                 settables=C3#collect.sets,
                 inittables=C3#collect.inits,
                 req_ivars=C3#collect.reqi,
                 req_meths=C3#collect.reqm,
                 req_flavs=C3#collect.reqf,
                 plist=C3#collect.plist
                },
    lfe_io:format("~p\n", [Fl]),                %Debug print
    put(flavor_core, Fl),
    [progn].

%% check_name(Name) -> true.
%% check_instance_vars(Name, Vars) -> [VarName].
%% check_components(Name, Components) -> true.

check_name(Name) ->
    is_atom(Name) orelse error({'illegal-flavor',Name}).

check_instance_vars(Name, Vars) ->
    Check = fun (V, Ivs) when is_atom(V) -> ordsets:add_element(V, Ivs);
                ([V,_], Ivs) when is_atom(V) -> ordsets:add_element(V, Ivs);
                (V, _) -> error({'illegal-instance-var',Name,V})
            end,
    lists:foldl(Check, ordsets:new(), Vars).

check_components(Name, Comps) ->
    lfe_lib:is_symb_list(Comps) orelse error({'illegal-component',Name}).

%% parse_options(Options, Name, VarNames, Collect) -> Collect.

parse_options(Opts, Name, Ivars, Coll) ->
    Each = fun ([O|Args], C) -> parse_option(O, Args, Name, Ivars, C);
               (O, C) -> parse_option(O, [], Name, Ivars, C)
           end,
    lists:foldl(Each, Coll, Opts).

parse_option(Opt, Args, Name, Ivars, C) ->
    case Opt of
        %% Handle the instance variables.
        'gettable-instance-variables' ->
            validate_instance_vars(Opt, Args, Name, Ivars),
            C#collect{gets=args_or_ivars(Args, Ivars)};
        'settable-instance-variables' ->
            validate_instance_vars(Opt, Args, Name, Ivars),
            C#collect{sets=args_or_ivars(Args, Ivars)};
        'inittable-instance-variables' ->
            validate_instance_vars(Opt, Args, Name, Ivars),
            C#collect{inits=args_or_ivars(Args, Ivars)};
        %% Handle the required things.
        'required-instance-variables' ->
            C#collect{plist=orddict:store(Opt, Args, C#collect.plist)};
            %%C#collect{reqi=Args};
        'required-methods' ->
            C#collect{plist=orddict:store(Opt, Args, C#collect.plist)};
            %%C#collect{reqm=Args};
        'required-flavors' ->
            C#collect{plist=orddict:store(Opt, Args, C#collect.plist)};
            %%C#collect{reqf=Args};
        %% Now for the rest which we accept.
        'no-vanilla-flavor' ->
            C#collect{plist=orddict:store(Opt, true, C#collect.plist)};
            %%C#collect{plist=[Opt|C#collect.plist]};
        'abstract-flavor' ->
            C#collect{plist=orddict:store(Opt, true, C#collect.plist)};
            %%C#collect{plist=[Opt|C#collect.plist]};
        _Other ->
            error({'illegal-option',Name,Opt})
    end.

validate_instance_vars(Opt, Args, Name, Ivars) ->
    Aset = ordsets:from_list(Args),
    case ordsets:subtract(Aset, Ivars) of
        [] -> Aset;
        Unknown -> error({'unknown-instance-vars',Name,Opt,Unknown})
    end.

args_or_ivars([], Ivars) -> Ivars;              %This is already an ordset
args_or_ivars(Args, _) -> Args.

defmethod(Method, Def) ->
    Fl0 = get(flavor_core),
    Fl1 = defmethod(Method, Def, Fl0),
    put(flavor_core, Fl1),
    [progn].

defmethod([Flav,Meth], Def, #flavor{name=Flav,methods=Ms}=Fl) ->
    check_method(Flav, Meth, Def),
    Fl#flavor{methods=Ms ++ [{Meth,Def}]};
defmethod([Flav,Daemon,Meth], Def, #flavor{name=Flav,daemons=Ds}=Fl) ->
    check_daemon(Flav, Meth, Daemon, Def),
    Fl#flavor{daemons=Ds ++ [{Meth,Daemon,Def}]};
defmethod(_, _, #flavor{name=Name}) ->
    error({'illegal-method',Name}).

check_method(_, _, _) ->    
    ok.

check_daemon(_, _, before, _) -> ok;
check_daemon(_, _, 'after', _) -> ok;
check_daemon(Flav, _, D, _) -> error({'illegal-daemon-type',Flav,D}).

%% endflavor(FlavorName) -> [progn].
%%  This builds the actual flavor definition from the definition and
%%  the modules.

endflavor(Name) ->
    Fl = erase(flavor_core),                    %Get and erase flavor_core
    lfe_io:format("~p\n", [Fl]),                %Debug print
    Cname = flavors_lib:core_name(Fl#flavor.name),
    Mod = [defmodule,Cname,
           [export,                             %Arguments to defflavor
            [name,0],
            ['instance-variables',0],
            [components,0],
            [options,0],
            [methods,0],[daemons,1]],
           [export,                             %Parse options
            ['gettable-instance-variables',0],
            ['settable-instance-variables',0],
            ['inittable-instance-variables',0],
            ['plist',0]],
           %%[export,
           %% ['normalised-instance-variables',0],
           %% ['normalised-options',0]],
           [export,                             %Methods
            ['primary-method',3],
            ['before-daemon',3],
            ['after-daemon',3]]],
    Funcs = [[defun,name,[],?Q(Name)],
             [defun,'instance-variables',[],?Q(Fl#flavor.ivars)],
             [defun,components,[],?Q(Fl#flavor.comps)],
             [defun,options,[],?Q(Fl#flavor.options)],
             [defun,'gettable-instance-variables',[],?Q(Fl#flavor.gettables)],
             [defun,'settable-instance-variables',[],?Q(Fl#flavor.settables)],
             [defun,'inittable-instance-variables',[],?Q(Fl#flavor.inittables)],
             [defun,'plist',[],?Q(Fl#flavor.plist)],
             %%[defun,'normalised-instance-variables',[],?Q(Fl#flavor.nvars)],
             %%[defun,'normalised-options',[],?Q(Fl#flavor.noptions)],
             [defun,methods,[],
              ?Q([ M || {M,_} <- Fl#flavor.methods ])],
             [defun,daemons,
              [[?Q(before)],?Q([ D || {D,before,_} <- Fl#flavor.daemons])],
              [[?Q('after')],?Q([ D || {D,'after',_} <- Fl#flavor.daemons])]]],
    Methods = methods(Fl#flavor.methods, Name),
    Gets = gettable(Fl),
    Sets = settable(Fl),
    Primary = [defun,'primary-method'|Gets ++ Sets ++ Methods],
    Befs = daemons(Fl#flavor.daemons, before, Name),
    Before = [defun,'before-daemon'|Befs],
    Afts = daemons(Fl#flavor.daemons, 'after', Name),
    After = [defun,'after-daemon'|Afts],
    Forms = [Mod,Primary,Before,After|Funcs],
    lfe_io:format("~p\n", [Forms]),             %Debug print
    Source = lists:concat([Fl#flavor.name,".lfe"]),
    {ok,_,Binary} = lfe_comp:forms(Forms, [verbose,report,{source,Source}]),
    file:write_file(lists:concat([Cname,".beam"]), Binary),
    %%Ret = lfe_comp:forms(Forms, [to_core,verbose,report,{source,Source}]),
    %%file:write_file(lists:concat([Cname,".beam"]), io_lib:format("~p\n", [Ret])),
    [progn].

methods(Ms, Flav) ->
    E = 'undefined-primary-method',
    lists:foldr(fun (M, Mcs) -> method(M, Mcs) end,
                [[[m,'_','_'],[error,[tuple,?Q(E),?Q(Flav),m]]]],
                Ms).

method({M,[As|Body]=Cs}, Mcs) ->
    %% Check whether it is traditional or matching form.
    case lfe_lib:is_symb_list(As) of
        true ->
            [method_clause(M, As, Body)|Mcs];
        false ->
            [ method_clause(M, As, Body) || [As|Body] <- Cs ] ++ Mcs
    end.

method_clause(M, [], Body) -> [[?Q(M),self,[]] | Body];
method_clause(M, As, Body) -> [[?Q(M),self,[list|As]] | Body].

gettable(#flavor{gettables=Gs}) ->
    Get = fun (Var) ->
                  B = [[tuple,[mref,self,?Q(Var)],self]],
                  method_clause(Var, [], B)
          end,
    [ Get(Var) || Var <- Gs ].

settable(#flavor{settables=Ss}) ->
    Set = fun (Var) ->
                  M = list_to_atom(lists:concat(["set-",Var])),
                  B = [[tuple,?Q(ok),[mupd,self,?Q(Var),val]]],
                  method_clause(M, [val], B)
          end,
    [ Set(Var) || Var <- Ss ].

daemons(Ds, Daemon, Flav) ->
    E = list_to_atom(lists:concat(["undefined-",Daemon,"-daemon"])),
    [ method_clause(M, As, Body) || {M,D,[As|Body]} <- Ds, D =:= Daemon ] ++
        [[[m,'_','_'],[error,[tuple,?Q(E),?Q(Flav),m]]]].
