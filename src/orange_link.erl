%% Copyright 2009-2010 Nicolas R Dufour.
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
%%
%% @author Nicolas R Dufour <nrdufour@gmail.com>
%% @copyright 2009-2010 Nicolas R Dufour.

-module(orange_link).
-behavior(gen_server).
-author('Nicolas R Dufour <nrdufour@gmail.com>').
-include("adt.hrl").

%% API exports
-export([create/3, create/4, hibern/3, awake/3, destroy/3, resur/3, purge/3, start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create(FromClassName, ToClassName, LinkName) ->
    gen_server:call(?MODULE, {create, FromClassName, ToClassName, LinkName}).

create(FromClassName, ToClassName, LinkName, Extra) ->
    gen_server:call(?MODULE, {create, FromClassName, ToClassName, LinkName, Extra}).

hibern(FromClassName, ToClassName, LinkName) ->
    gen_server:call(?MODULE, {hibern, FromClassName, ToClassName, LinkName}).

awake(FromClassName, ToClassName, LinkName) ->
    gen_server:call(?MODULE, {awake, FromClassName, ToClassName, LinkName}).

destroy(FromClassName, ToClassName, LinkName) ->
    gen_server:call(?MODULE, {destroy, FromClassName, ToClassName, LinkName}).

resur(FromClassName, ToClassName, LinkName) ->
    gen_server:call(?MODULE, {resur, FromClassName, ToClassName, LinkName}).

purge(FromClassName, ToClassName, LinkName) ->
    gen_server:call(?MODULE, {purge, FromClassName, ToClassName, LinkName}).

init([]) ->
    process_flag(trap_exit, true),
    io:format("~p starting~n", [?MODULE]),
    {ok, []}.

handle_call({create, FromClassName, ToClassName, LinkName}, _From, State) ->
    {reply, orange_executor:do_create(link, [FromClassName, ToClassName, LinkName]), State};
handle_call({hibern, FromClassName, ToClassName, LinkName}, _From, State) ->
    {reply, orange_executor:do_hibern(link, [FromClassName, ToClassName, LinkName]), State};
handle_call({awake, FromClassName, ToClassName, LinkName}, _From, State) ->
    {reply, orange_executor:do_awake(link, [FromClassName, ToClassName, LinkName]), State};
handle_call({destroy, FromClassName, ToClassName, LinkName}, _From, State) ->
    {reply, orange_executor:do_destroy(link, [FromClassName, ToClassName, LinkName]), State};
handle_call({resur, FromClassName, ToClassName, LinkName}, _From, State) ->
    {reply, orange_executor:do_resur(link, [FromClassName, ToClassName, LinkName]), State};
handle_call(_, _From, State) ->
    {reply, not_yet_implemented, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) ->
    io:format("~p stopping~n", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% internal API ==============================================================

