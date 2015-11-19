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

%% File    : flavors_instance.erl
%% Author  : Robert Virding
%% Purpose : Basic LFE Flavors instance process.

%% We run a gen_server to manage an flavor instance. This may be a bit
%% overkill but it is the easiest way for the time being. When we
%% evaluate a method we catch errors/exits/throws and send them back
%% to the caller and generate errors there. So errors will not crash
%% the flaovr instance but be signalled at the caller. I think this is
%% a reasonable handling of errors.
%%
%% An interesting question is whether to signal exits as exits and
%% throw throw in the caller, or just signal them as errors? Now they
%% are all errors which is probably good enough.

-module(flavors_instance).

-behaviour(gen_server).

-export([start/3,start_link/3,stop/1]).
-export([send/3]).

-export([init/1,terminate/2,code_change/3,
         handle_call/3,handle_cast/2,handle_info/2]).

-include("flavors.hrl").

-record(state, {name,fm,self,ivars=none}).

%% Management API.
start(Flav, Fm, Opts) ->
    gen_server:start(?MODULE, {Flav,Fm,Opts}, []).

start_link(Flav, Fm, Opts) ->
    gen_server:start_link(?MODULE, {Flav,Fm,Opts}, []).

stop(Ins) ->
    gen_server:cast(Ins, stop).

%% User API.
send(Ins, Meth, Args) ->
    gen_server:call(Ins, {send,Meth,Args}).

%% Behaviour callbacks.
init({Flav,Fm,Opts}) ->
    Ivars = Fm:'instance-variables'(),
    Ikeys = Fm:'init-keywords'(),
    %% Catch errors and return {stop,_} for better error value.
    try
	check_init_keywords(Opts, Ivars, Ikeys, Flav),
	Mlist = make_map_list(Ivars, Opts),
	Imap = maps:from_list(Mlist),
	erlang:put('instance-variables', Imap),
	Self = #'flavor-instance'{flavor=Flav,
				  flavor_mod=Fm,
				  instance=self()},
	%% Send ourselves init.
	Fm:'combined-method'(init, Self, {Opts}),
	{ok,#state{name=Flav,fm=Fm,self=Self}}
    catch
	throw:_ -> {stop,nocatch};
	_:E -> {stop,E}				%Exit and error
    end.

check_init_keywords([O,_|Opts], Ivars, Ikeys, Name) ->
    case cl:member(O, Ikeys) orelse
	lists:keymember(O, 1, Ivars) of
	true ->
	    check_init_keywords(Opts, Ivars, Ikeys, Name);
	false -> error({'init-keywords',Name,O})
    end;
check_init_keywords([], _, _, _) -> ok.

make_map_list([{V,I}|Mlist], Opts) ->
    Pair = case cl:getf(Opts, V) of
	       [] -> {V,lfe_eval:expr(I)};
               I1 -> {V,I1}
           end,
    [Pair|make_map_list(Mlist, Opts)];
make_map_list([], _) -> [].

terminate(_, St) ->
    send_method(terminate, {}, St).             %Send ourselves terminate

handle_call({send,terminate,{}}, _, St) ->      %The true terminate
    {stop,normal,{ok,ok},St};
handle_call({send,Meth,Args}, _, St) ->
    Reply = send_method(Meth, Args, St),
    {reply,Reply,St}.

send_method(Meth, Args, #state{fm=Fm,self=Self}) ->
    %% Catch errors, exits and throws and signal in the caller.
    try
        Result = Fm:'combined-method'(Meth, Self, Args),
        {ok,Result}
    catch                                       %Catch and return
        error:Error -> {error,Error};
        exit:Exit -> {error,Exit};
        throw:Thrown -> {throw,Thrown}
    end.

handle_cast(stop, St) ->
    {stop,normal,St}.

%% Unused callbacks.
handle_info(_, St) ->
    {noreply,St}.

code_change(_, St, _) ->
    {ok,St}.
