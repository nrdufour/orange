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

-module(orange).
-behavior(application).
-author('Nicolas R Dufour <nrdufour@gmail.com>').

%% Application API
-export([start/2, stop/1]).

%% User API
-export([start/0]).

%%

%% @doc starts the application w/ crypto.
start(_Type, StartArgs) ->
    case start_apps([crypto, sasl]) of
        ok ->
            orange_sup:start_link(StartArgs);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc stops the application.
stop(_State) ->
    ok.

%%

%% @doc starts the application using the framework
start() ->
    application:start(orange).

%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Piece of code written by Benoît Chesneau <benoitc@e-engura.org>
%% @doc starts any dependencies.
start_apps([]) ->
    ok;
start_apps([App|Rest]) ->
    case application:start(App) of
    ok ->
       start_apps(Rest);
    {error, {already_started, App}} ->
       start_apps(Rest);
    {error, _Reason} ->
       {error, {app_would_not_start, App}}
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
