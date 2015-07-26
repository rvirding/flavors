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

-record(flavor, {name,vars=[],comps=[],options=[],methods=[],daemons=[]}).

defflavor(Name, Vars, Comps, Opts) ->
    %% Check arguments, generate error if faulty.
    check_name(Name),
    check_vars(Name, Vars),
    check_comps(Name, Comps),
    check_options(Name, Opts),
    Fl = #flavor{name=Name,
		 vars=Vars,
		 comps=Comps,
		 options=Opts},
    put(flavor_core, Fl),
    [progn].

check_name(Name) ->
    is_atom(Name) orelse error({illegal_flavor,Name}).

check_vars(Name, Vars) ->
    Check = fun (V) when is_atom(V) -> true;
		%%([V,_]) when is_atom(V) -> true;
		(_) -> false
	    end,
    lists:all(Check, Vars) orelse error({illegal_instance_var,Name}).

check_comps(Name, Comps) ->
    lists:all(fun (C) -> is_atom(C) end, Comps)
	orelse error({illegal_component,Name}).

check_options(Name, Opts) ->
    lists:all(fun (O) -> option(O) end, Opts)
	orelse error({illegal_option,Name}).

option('gettable-instance-variables') -> true;
option('settable-instance-variables') -> true;
option('inittable-instance-variables') -> true;
option(_) -> false.

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

endflavor(Name) ->
    Fl = erase(flavor_core),			%Get and erase flavor_core
    Cname = flavors_lib:core_name(Fl#flavor.name),
    Mod = [defmodule,Cname,
	   [export,[name,0],['instance-variables',0],
	    [components,0],[options,0],[methods,0],[daemons,1]],
	   [export,['primary-method',3],['before-daemon',3],['after-daemon',3]]],
    Funcs = [[defun,name,[],?Q(Name)],
	     [defun,'instance-variables',[],?Q(Fl#flavor.vars)],
	     [defun,components,[],?Q(Fl#flavor.comps)],
	     [defun,options,[],?Q(Fl#flavor.options)],
	     [defun,methods,[],
	      ?Q([ M || {M,_} <- Fl#flavor.methods ])],
	     [defun,daemons,
	      [[?Q(before)],?Q([ D || {D,before,_} <- Fl#flavor.daemons])],
	      [[?Q('after')],?Q([ D || {D,'after',_} <- Fl#flavor.daemons])]]],
    Meths = methods(Fl#flavor.methods, Name),
    Gets = gettable(Fl),
    Sets = settable(Fl),
    Method = [defun,'primary-method'|Gets ++ Sets ++ Meths],
    Befs = daemons(Fl#flavor.daemons, before, Name),
    Before = [defun,'before-daemon'|Befs],
    Afts = daemons(Fl#flavor.daemons, 'after', Name),
    After = [defun,'after-daemon'|Afts],
    Forms = [Mod,Method,Before,After|Funcs],
    lfe_io:format("~p\n", [Forms]),
    {ok,_,Binary} = lfe_comp:forms(Forms),
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

gettable(#flavor{vars=Vars,options=Opts}) ->
    Get = fun (Var) ->
		  B = [[tuple,[mref,self,?Q(Var)],self]],
		  method_clause(Var, [], B)
	  end,
    case lists:member('gettable-instance-variables', Opts) of
	true -> [ Get(Var) || Var <- Vars ];
	false -> []
    end.

settable(#flavor{vars=Vars,options=Opts}) ->
    Set = fun (Var) ->
		  M = list_to_atom(lists:concat(["set-",Var])),
		  B = [[tuple,?Q(ok),[mupd,self,?Q(Var),val]]],
		  method_clause(M, [val], B)
	  end,
    case lists:member('settable-instance-variables', Opts) of
	true -> [ Set(Var) || Var <- Vars ];
	false -> []
    end.

daemons(Ds, Daemon, Flav) ->
    E = list_to_atom(lists:concat(["undefined-",Daemon,"-daemon"])),
    [ method_clause(M, As, Body) || {M,D,[As|Body]} <- Ds, D =:= Daemon ] ++
	[[[m,'_','_'],[error,[tuple,?Q(E),?Q(Flav),m]]]].
