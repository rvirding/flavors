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

%% File    : flavors_compile.erl
%% Author  : Robert Virding
%% Purpose : Basic LFE Flavors compiler.

-module(flavors_compile).

-export([defflavor/4,defmethod/2,endflavor/1]).

-include("flavors.hrl").

-define(DBG_PRINT(Format, Args), ok).
%%-define(DBG_PRINT(Format, Args), lfe_io:format(Format, Args)).

%% The flavor record.
-record(flavor, {name,                          %Flavor name
                 ivars=[],                      %Local instance variables
                 comps=[],                      %Components
                 options=[],
                 %% Derived data, these are all list sets.
                 locals=[],                     %Local instance variables
                 gettables=[],                  %Gettable, settable, inittable
                 settables=[],
                 inittables=[],
                 initkeywords=[],               %Valid init-plist keywords
                 plist=[],                      %Other esoteric things
                 %% Required instance variables/methods/flavors.
                 req_ivars=[],
                 req_meths=[],
                 req_flavs=[],
                 %% The collected methods and daemons.
                 methods=[],
                 daemons=[]}).

%% Options arguments we collect.
-record(collect, {vars=[],comps=[],
                  gets=[],sets=[],inits=[],ikeys=[],
                  reqi=[],reqm=[],reqf=[],plist=[]}).

%% defflavor(Name, InstanceVariables, Components, Options) ->
%%     ModuleDefinition.
%%  Return base flavor module definition. This intended to be run as a
%%  macro and together with defmethod and endflavor expands to code
%%  which defines the flavor core.

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
    C2 = C1#collect{gets=cl:union(Sets, C1#collect.gets)},
    C3 = C2#collect{inits=cl:union(Sets, C2#collect.inits)},
    %% Now we have everything so save it in the flavor record.
    Fl = #flavor{name=Name,                     %Call arguments
                 ivars=IVars,
                 comps=Comps,
                 options=Opts,
                 locals=C3#collect.vars,        %Processed data
                 gettables=C3#collect.gets,
                 settables=C3#collect.sets,
                 inittables=C3#collect.inits,
                 initkeywords=C3#collect.ikeys,
                 req_ivars=C3#collect.reqi,
                 req_meths=C3#collect.reqm,
                 req_flavs=C3#collect.reqf,
                 plist=C3#collect.plist
                },
    ?DBG_PRINT("df 1: ~p\n", [Fl]),
    Cname = flavors_lib:core_name(Fl#flavor.name),
    %% The flavor module definition,
    Mod = [defmodule,Cname,
           [export,                             %Arguments to defflavor
            [name,0],
            ['instance-variables',0],
            [components,0],
            [options,0]],
           [export,                             %Parse options, ordered
            ['local-instance-variables',0],
            ['gettable-instance-variables',0],
            ['settable-instance-variables',0],
            ['inittable-instance-variables',0],
            ['init-keywords',0],
            ['plist',0]],
           [export,                             %Methods
            ['primary-methods',0],
            ['before-daemons',0],
            ['after-daemons',0]],
           [export,                             %Method functions
            ['primary-method',3],
            ['before-daemon',3],
            ['after-daemon',3]],
	   %% Inofrm compiler of predefined getters and setters.
	   [compile,
	    {no_auto_import,[{get,0},{get,1}]},
	    {nowarn_unused_function,[{get,0},{get,1},{set,1},{set,2}]}]
	  ],
    %% The standard flavor functions.
    Funcs = [[defun,name,[],?Q(Name)],
             [defun,'instance-variables',[],?Q(Fl#flavor.ivars)],
             [defun,components,[],?Q(Fl#flavor.comps)],
             [defun,options,[],?Q(Fl#flavor.options)],
             [defun,'local-instance-variables',[],?Q(Fl#flavor.locals)],
             [defun,'gettable-instance-variables',[],?Q(Fl#flavor.gettables)],
             [defun,'settable-instance-variables',[],?Q(Fl#flavor.settables)],
             [defun,'inittable-instance-variables',[],?Q(Fl#flavor.inittables)],
             [defun,'init-keywords',[],?Q(Fl#flavor.initkeywords)],
             [defun,'plist',[],?Q(Fl#flavor.plist)],
             %% Getting and setting instance variables. Internal functions.
             [defun,get,[],
              [':',erlang,get,?Q('instance-variables')]],
             [defun,get,[var],
              [':',maps,get,var,[':',erlang,get,?Q('instance-variables')]]],
             [defun,set,[ivars],
              [':',erlang,put,?Q('instance-variables'),ivars]],
             [defun,set,[var,val],
              ['let*',[[ivars,[':',erlang,get,?Q('instance-variables')]],
                       [ivars,[':',maps,update,var,val,ivars]]],
               [':',erlang,put,?Q('instance-variables'),ivars],
               val]]
            ],
    ?DBG_PRINT("df 2: ~p\n", [[Mod|Funcs]]),
    erlang:put('flavor-core', Fl),              %Save the flavor info
    %% Return the flavor definition and standard functions.
    [progn,Mod|Funcs].

%% check_name(Name) -> true.
%% check_instance_vars(Name, Vars) -> [VarName].
%% check_components(Name, Components) -> true.

check_name(Name) ->
    is_atom(Name) orelse error({'illegal-flavor',Name}).

check_instance_vars(Name, Vars) ->
    Check = fun (V, Ivs) when is_atom(V) -> cl:adjoin(V, Ivs);
                ([V,_], Ivs) when is_atom(V) -> cl:adjoin(V, Ivs);
                (V, _) -> error({'illegal-instance-var',Name,V})
            end,
    lists:foldl(Check, [], Vars).

check_components(Name, Comps) ->
    lfe_lib:is_symb_list(Comps) orelse error({'illegal-component',Name}).

%% parse_options(Options, Name, VarNames, Collect) -> Collect.

parse_options(Opts, Name, Vars, Coll) ->
    Each = fun ([O|Args], C) -> parse_option(O, Args, Name, Vars, C);
               (O, C) -> parse_option(O, [], Name, Vars, C)
           end,
    lists:foldl(Each, Coll, Opts).

parse_option(Opt, Args, Name, Vars, C) ->
    case Opt of
        %% Handle the instance variables.
        'gettable-instance-variables' ->
            Gs = validate_instance_vars(Opt, Args, Name, Vars),
            C#collect{gets=Gs};
        'settable-instance-variables' ->
            Ss = validate_instance_vars(Opt, Args, Name, Vars),
            C#collect{sets=Ss};
        'inittable-instance-variables' ->
            Is = validate_instance_vars(Opt, Args, Name, Vars),
            C#collect{inits=Is};
        %% Handle the init-plist.
        'init-keywords' ->
            C#collect{ikeys=Args};
        %% Handle the required things.
        'required-instance-variables' ->
            Is = validate_required_instance_vars(Opt, Args, Name),
            C#collect{plist=orddict:store(Opt, Is, C#collect.plist)};
            %%C#collect{reqi=Args};
        'required-methods' ->
            Ms = validate_required_methods(Opt, Args, Name),
            C#collect{plist=orddict:store(Opt, Ms, C#collect.plist)};
            %%C#collect{reqm=Args};
        'required-flavors' ->
            Fs = validate_required_flavors(Opt, Args, Name),
            C#collect{plist=orddict:store(Opt, Fs, C#collect.plist)};
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

%% validate_instance_vars(Opt, Args, Name, Vars) -> ArgsSet.
%%  Return an ordered set of the arguments to the option or all the
%%  variables if no arguments have been given.

validate_instance_vars(_, [], _, Vars) -> Vars;
validate_instance_vars(Opt, Args, Name, Vars) -> 
    case cl:'set-difference'(Args, Vars) of
        [] -> Args;
        Unknown -> error({'unknown-instance-variables',Name,Opt,Unknown})
    end.

%% validate_required_instance_vars(Opt, Args, Name) -> Required.
%% validate_required_flavors(Opt, Args, Name) -> Required.
%%  Return an ordered set of variables/flavors.

validate_required_instance_vars(_Opt, Args, Name) ->
    Req = fun require_symbol/2,
    validate_required('illegal-required-variables', Req, Args, Name).

validate_required_flavors(_Opt, Args, Name) ->
    Req = fun require_symbol/2,
    validate_required('illegal-required-flavors', Req, Args, Name).

require_symbol(A, {Rs,Es}) when is_atom(A) ->
    {cl:adjoin(A, Rs),Es};
require_symbol(A, {Rs,Es}) -> {Rs,[A|Es]}.

%% validate_required_methods(Opt, Args, Name) -> Methods.
%%  Return an ordered set of name/arity tuples.

validate_required_methods(_Opt, Args, Name) ->
    Req = fun require_method/2,
    validate_required('illegal-required-methods', Req, Args, Name).

require_method([M,Ar], {Rs,Es}) when is_atom(M), is_integer(Ar), Ar >= 0 ->
    {cl:adjoin({M,Ar}, Rs),Es};
require_method(A, {Rs,Es}) -> {Rs,[A|Es]}.

validate_required(Error, Required, Args, Name) ->
    case lists:foldl(Required, {[],[]}, Args) of
        {Reqs,[]} -> Reqs;
        {_,Es} -> error({Error,Name,Es})
    end.

%% defmethod(Method, Def) -> [progn].
%%  Save a method definition for later processing.

defmethod(Method, Def) ->
    case erlang:get('flavor-core') of
        undefined -> error('undefined-flavor');
        Fl0 ->
	    Name = Fl0#flavor.name,
            Fl1 = ?CATCH(defmethod(Method, Def, Fl0),
                         error({'illegal-method',Name,Method})),
            erlang:put('flavor-core', Fl1),
            [progn]
    end.

method_arity([As|_]) ->
    case lfe_lib:is_symb_list(As) of
        true -> length(As);                     %No clauses
        false -> length(hd(As))                 %Look at args of first clause
    end.

defmethod([Meth], Def, #flavor{name=Flav,methods=Ms}=Fl) ->
    check_method(Flav, Meth, Def),
    Ar = method_arity(Def),
    Fl#flavor{methods=Ms ++ [{{Meth,Ar},Def}]};
defmethod([Meth,Daemon], Def, #flavor{name=Flav,daemons=Ds}=Fl) ->
    check_daemon(Flav, Meth, Daemon, Def),
    Ar = method_arity(Def),
    Fl#flavor{daemons=Ds ++ [{{Meth,Ar},Daemon,Def}]}.

check_method(_, Meth, _) when is_atom(Meth) -> ok.

check_daemon(_, _, before, _) -> ok;
check_daemon(_, _, 'after', _) -> ok;
check_daemon(Flav, _, D, _) -> error({'illegal-daemon-type',Flav,D}).

%% endflavor(FlavorName) -> [progn].
%%  Return the flavor method definitions. This intended to be run as a
%%  macro and together with defflavor and defmethod expands to code
%%  which defines the flavor core.

endflavor(Name) ->
    Fl = erlang:erase('flavor-core'),           %Get and erase flavor-core
    ((Fl =:= undefined) orelse (Name =/= Fl#flavor.name))
	andalso error({'illegal-flavor',Name}),
    ?DBG_PRINT("ef 1: ~p\n", [Fl]),
    %% The defined method/daemon listing functions.
    Ms = get_methods(Fl#flavor.methods),
    Bs = get_daemons(before, Fl#flavor.daemons),
    As = get_daemons('after', Fl#flavor.daemons),
    ?DBG_PRINT("ef 2: ~p\n", [{Ms,Bs,As}]),
    Funcs = [[defun,'primary-methods',[],?Q(Ms)],
             [defun,'before-daemons',[],?Q(Bs)],
             [defun,'after-daemons',[],?Q(As)]],
    %% Build the primary method function.
    Methods = method_clauses(Fl),
    Gets = gettable_clauses(Fl, Ms),
    Sets = settable_clauses(Fl, Ms),
    Defs = default_clauses(Fl, Ms),
    Perror = primary_error_clause(Fl),
    Primary = [defun,'primary-method'|
               Gets ++ Sets ++ Methods ++ Defs ++ [Perror]],
    %% Build the before and after daemon functions.
    Befs = daemon_clauses(Fl, before),
    Before = [defun,'before-daemon'|Befs],
    Afts = daemon_clauses(Fl, 'after'),
    After = [defun,'after-daemon'|Afts],
    ?DBG_PRINT("ef 3: ~p\n", [Funcs ++ [Primary,Before,After]]),
    %% Return the flavor functions to be compiled as a macro expansion.
    [progn|Funcs ++ [Primary,Before,After]].

    %% Source = lists:concat([Fl#flavor.name,".lfe"]),
    %% {ok,_,Binary} = lfe_comp:forms(Forms, [verbose,report,{source,Source}]),
    %% file:write_file(lists:concat([Cname,".beam"]), Binary),

get_methods(Ds) ->
    lists:foldl(fun ({M,_}, Ms) -> cl:adjoin(M, Ms) end, [], Ds).

get_daemons(Type, Ds) ->
    lists:foldl(fun ({D,T1,_}, Ts) when T1 =:= Type ->
                        cl:adjoin(D, Ts);
                    (_, Ts) -> Ts
                end, [], Ds).

primary_error_clause(#flavor{name=Name}) ->
    E = 'undefined-primary-method',
    [[m,'_','_'],[error,[tuple,?Q(E),?Q(Name),m]]].

method_clauses(#flavor{methods=Ms}) ->
    lists:foldr(fun (M, Mcs) -> method_clause(M, Mcs) end, [], Ms).

method_clause({{M,_},[Args|Body]=Cs}, Mcs) ->
    %% Check whether it is traditional or matching form.
    case lfe_lib:is_symb_list(Args) of
        true ->
            [method_clause(M, Args, Body)|Mcs];
        false ->
            [ method_clause(M, As, B) || [As|B] <- Cs ] ++ Mcs
    end.

method_clause(M, [], Body) -> [[?Q(M),self,{}] | Body];
method_clause(M, As, Body) -> [[?Q(M),self,[tuple|As]] | Body].

%% gettable_clauses(Flavor, DefMethods) -> Clauses.
%% settable_clauses(Flavor, DefMethods) -> Clauses.
%%  Generate clauses for the gettable and settable variables. Skip
%%  those already explicitly defined as methods.

gettable_clauses(#flavor{gettables=Gs}, Meths) ->
    Get = fun (Var) ->
                  B = [[get,?Q(Var)]],
                  method_clause(Var, [], B)
          end,
    [ Get(Var) || Var <- Gs, not cl:member({Var,0}, Meths) ].

settable_clauses(#flavor{settables=Ss}, Meths) ->
    MVs = [ {list_to_atom(lists:concat(["set-",Var])),Var} || Var <- Ss ],
    Set = fun (M, Var) ->
                  B = [[set,?Q(Var),val]],
                  method_clause(M, [val], B)
          end,
    [ Set(M, Var) || {M,Var} <- MVs, not cl:member({M,1}, Meths) ].

%% default_clauses(Flavor, DefMethods) -> Clauses.

default_clauses(_, Meths) ->
    Def = fun (M, As, B) -> method_clause(M, As, B) end,
    DMs = [{init,1,[plist],[?Q(ok)]},
           {terminate,0,[],[?Q(ok)]}],
    [ Def(M, As, B) || {M,Ar,As,B} <- DMs,
                       not cl:member({M,Ar}, Meths) ].

%% daemon_clauses(Flavor, Daemon, Daemons) -> Clauses

daemon_clauses(#flavor{daemons=Ds,name=Name}, Daemon) ->
    E = list_to_atom(lists:concat(["undefined-",Daemon,"-daemon"])),
    [ method_clause(M, As, Body) || {{M,_},D,[As|Body]} <- Ds, D =:= Daemon ] ++
        [[[m,'_','_'],[error,[tuple,?Q(E),?Q(Name),m]]]].
