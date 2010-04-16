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

-module(orange_server).
-behavior(gen_server).
-author('Nicolas R Dufour <nrdufour@gmail.com>').

-export([execute/4, start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

execute(Operation, Type, Names, Extra) ->
    gen_server:call(?MODULE, {execute, {Operation, Type, Names, Extra}}).

init([]) ->
    %% Note we must set trap_exit = true if we
    %% want terminate/2 to be called when the application
    %% is stopped
    process_flag(trap_exit, true),
    io:format("~p starting~n", [?MODULE]),
    {ok, 0}.

handle_call({execute, {Operation, Type, Names, Extra}}, _From, N) ->
    {reply, execute_operation(Operation, Type, Names, Extra), N+1}.

handle_cast(_Msg, N) -> {noreply, N}.

handle_info(_Info, N) -> {noreply, N}.

terminate(_Reason, _N) ->
    io:format("~p stopping~n", [?MODULE]),
    ok.

code_change(_OldVsn, N, _Extra) -> {ok, N}.

execute_operation(Operation, class, Names, _Extra) ->
    [ClassName] = Names,
    orange_class:execute(Operation, ClassName);

execute_operation(Operation, attribute, Names, Extra) ->
    orange_attribute:execute(Operation, Names, Extra);

execute_operation(Operation, link, Names, Extra) ->
    orange_link:execute(Operation, Names, Extra);

execute_operation(Operation, object, Names, Extra) ->
    orange_object:execute(Operation, Names, Extra).

