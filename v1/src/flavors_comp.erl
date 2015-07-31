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

-define(Q(E), [quote,E]).			%We do a lot of quoting

-record(flavor, {name,ivars=[],nvars=[],comps=[],options=[],
		 noptions=[],			%Normalised options
		 methods=[],daemons=[]}).

%% Options arguments we collect.
-record(collect, {gets=[],sets=[],inits=[],reqi=[],reqm=[],reqf=[],rest=[]}).

defflavor(Name, Vars, Comps, Opts) ->
    %% Check arguments, generate error if faulty.
    check_name(Name),
    {Ivars,Nvars} = check_vars(Name, Vars),
    check_comps(Name, Comps),
    Fl = #flavor{name=Name,
		 ivars=Ivars,
		 nvars=Nvars,
		 comps=Comps,
		 options=Opts},
    check_options(Fl),				%Need more stuff here
    Coll = collect_options(Fl, #collect{}),
    Nopts = normalise_options(Coll),
    put(flavor_core, Fl#flavor{noptions=Nopts}),
    [progn].

check_name(Name) ->
    is_atom(Name) orelse error({illegal_flavor,Name}).

check_vars(Name, Vars) ->
    Check = fun (V, {Ivars,Nvars}) when is_atom(V) ->
		    {[V|Ivars],[[V,?Q(undefined)]|Nvars]};
		([V,I], {Ivars,Nvars}) when is_atom(V) ->
		    {[V|Ivars],[[V,I]|Nvars]};
		(V, _) -> error({illegal_instance_var,Name,V})
	    end,
    lists:foldr(Check, {[],[]}, Vars).

check_comps(Name, Comps) ->
    lfe_lib:is_symb_list(Comps) orelse error({illegal_component,Name}).

check_options(#flavor{name=Name,ivars=Ivs,options=Opts}) ->
    lists:foreach(fun (O) ->
			  check_option(O, Ivs)
			      orelse error({illegal_option,Name,O})
	      end, Opts).

check_option(['gettable-instance-variables'|Vs], Vars) -> subset(Vs, Vars);
check_option(['settable-instance-variables'|Vs], Vars) -> subset(Vs, Vars);
check_option(['inittable-instance-variables'|Vs], Vars) -> subset(Vs, Vars);
check_option(['required-instance-variables'|Vs], _) -> lfe_lib:is_symb_list(Vs);
check_option(['required-methods'|Ms], _) -> lfe_lib:is_symb_list(Ms);
check_option(['required-flavors'|Fs], _) -> lfe_lib:is_symb_list(Fs);
check_option('gettable-instance-variables', _) -> true;
check_option('settable-instance-variables', _) -> true;
check_option('inittable-instance-variables', _) -> true;
check_option('no-vanilla-flavor', _) -> true;
check_option('abstract-flavor', _) -> true;
check_option(_, _) -> false.

collect_options(#flavor{options=Opts}=Fl, Coll) ->
    lists:foldl(fun (O, C) -> collect_option(O, Fl, C) end, Coll, Opts).

collect_option('gettable-instance-variables', Fl, C) ->
    C#collect{gets=Fl#flavor.ivars};
collect_option('settable-instance-variables', Fl, C) ->
    C#collect{sets=Fl#flavor.ivars};
collect_option('inittable-instance-variables', Fl, C) ->
    C#collect{inits=Fl#flavor.ivars};
collect_option(['gettable-instance-variables'|Vs], _, C) ->
    C#collect{gets=Vs};
collect_option(['settable-instance-variables'|Vs], _, C) ->
    C#collect{sets=Vs};
collect_option(['inittable-instance-variables'|Vs], _, C) ->
    C#collect{inits=Vs};
collect_option(['required-instance-variables'|Vs], _, C) ->
    C#collect{reqi=Vs};
collect_option(['required-methods'|Ms], _, C) ->
    C#collect{reqm=Ms};
collect_option(['required-flavors'|Fs], _, C) ->
    C#collect{reqf=Fs};
collect_option(O, _, #collect{rest=R}=C) ->
    C#collect{rest=R ++ [O]}.

normalise_options(Coll) ->
    [['gettable-instance-variables'|Coll#collect.gets],
     ['settable-instance-variables'|Coll#collect.sets],
     ['inittable-instance-variables'|Coll#collect.inits],
     ['required-instance-variables'|Coll#collect.reqi],
     ['required-methods'|Coll#collect.reqm],
     ['required-flavors'|Coll#collect.reqf]
     | Coll#collect.rest].

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
    error({illegal_method,Name}).

check_method(_, _, _) ->    
    ok.

check_daemon(_, _, before, _) -> ok;
check_daemon(_, _, 'after', _) -> ok;
check_daemon(Flav, _, D, _) -> error({illegal_daemon,Flav,D}).

%% endflavor(FlavorName) -> [progn].
%%  This builds the actual flavor definition from the definition and
%%  the modules.

endflavor(Name) ->
    Fl = erase(flavor_core),			%Get and erase flavor_core
    %%lfe_io:format("e: ~p\n", [Fl]),
    Cname = flavors_lib:core_name(Fl#flavor.name),
    Mod = [defmodule,Cname,
	   [export,[name,0],
	    ['instance-variables',0],['normalised-instance-variables',0],
	    [components,0],
	    [options,0],['normalised-options',0],
	    [methods,0],[daemons,1]],
	   [export,['primary-method',3],['before-daemon',3],['after-daemon',3]]],
    Funcs = [[defun,name,[],?Q(Name)],
	     [defun,'instance-variables',[],?Q(Fl#flavor.ivars)],
	     [defun,'normalised-instance-variables',[],?Q(Fl#flavor.nvars)],
	     [defun,components,[],?Q(Fl#flavor.comps)],
	     [defun,options,[],?Q(Fl#flavor.options)],
	     [defun,'normalised-options',[],?Q(Fl#flavor.noptions)],
	     [defun,methods,[],
	      ?Q([ M || {M,_} <- Fl#flavor.methods ])],
	     [defun,daemons,
	      [[?Q(before)],?Q([ D || {D,before,_} <- Fl#flavor.daemons])],
	      [[?Q('after')],?Q([ D || {D,'after',_} <- Fl#flavor.daemons])]]],
    Meths = methods(Fl#flavor.methods, Name),
    Gets = gettable(Fl),
    Sets = settable(Fl),
    Primary = [defun,'primary-method'|Gets ++ Sets ++ Meths],
    Befs = daemons(Fl#flavor.daemons, before, Name),
    Before = [defun,'before-daemon'|Befs],
    Afts = daemons(Fl#flavor.daemons, 'after', Name),
    After = [defun,'after-daemon'|Afts],
    Forms = [Mod,Primary,Before,After|Funcs],
    %%lfe_io:format("~p\n~p\n", [Fl,Forms]),
    Source = lists:concat([Fl#flavor.name,".lfe"]),
    {ok,_,Binary} = lfe_comp:forms(Forms, [verbose,report,{source,Source}]),
    file:write_file(lists:concat([Cname,".beam"]), Binary),
    [progn].

methods(Ms, Flav) ->
    E = 'undefined-primary-method',
    lists:foldr(fun (M, Mcs) -> method(M, Mcs) end,
		[[[m,'_','_'],[error,[tuple,?Q(E),?Q(Flav),m]]]],
		Ms).

method({M,[As|Body]=Cs}, Mcs) ->
    %% Check whether it is traditional or matching form.
    case lfe_lib:is_symb_list(As) of
	true -> [method_clause(M, As, Body)|Mcs];
	false ->
	    [ method_clause(M, As, Body) || [As|Body] <- Cs ] ++ Mcs
    end.

method_clause(M, [], Body) -> [[?Q(M),self,[]] | Body];
method_clause(M, As, Body) -> [[?Q(M),self,[list|As]] | Body].

gettable(#flavor{noptions=Opts}) ->
    Get = fun (Var) ->
		  B = [[tuple,[mref,self,?Q(Var)],self]],
		  method_clause(Var, [], B)
	  end,
    case lfe_lib:assoc('gettable-instance-variables', Opts) of
	[_|Gs] -> [ Get(Var) || Var <- Gs ];
	false -> []
    end.

settable(#flavor{noptions=Opts}) ->
    Set = fun (Var) ->
		  M = list_to_atom(lists:concat(["set-",Var])),
		  B = [[tuple,?Q(ok),[mupd,self,?Q(Var),val]]],
		  method_clause(M, [val], B)
	  end,
    case lfe_lib:assoc('settable-instance-variables', Opts) of
	[_|Ss] -> [ Set(Var) || Var <- Ss ];
	[] -> []
    end.

daemons(Ds, Daemon, Flav) ->
    E = list_to_atom(lists:concat(["undefined-",Daemon,"-daemon"])),
    [ method_clause(M, As, Body) || {M,D,[As|Body]} <- Ds, D =:= Daemon ] ++
	[[[m,'_','_'],[error,[tuple,?Q(E),?Q(Flav),m]]]].

subset(Vs, Vars) ->
    lists:all(fun (V) -> lists:member(V, Vars) end, Vs).
