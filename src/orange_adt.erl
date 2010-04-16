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

%% @type adtOperation() = create | hibern | awake | destroy | resur | purge
%% @type adtState() = alive | frozen | destroyed | none

-module(orange_adt).
-author('Nicolas R Dufour <nrdufour@gmail.com>').
-include("adt.hrl").

-export([new_adt/2, new_state_after/2, is_ready_for/2]).

%% @spec new_adt(adt_type(), names()) -> adt()
%% @doc create a branch adt stucture w/ state = alive.
new_adt(class, [ClassName]) ->
    Meta = #meta{type = class, names = [ClassName], state = alive},
    #adt{meta = Meta, data = []};
new_adt(attribute, [ClassName, AttributeName]) ->
    Meta = #meta{type = attribute, names = [ClassName, AttributeName], state = alive},
    #adt{meta = Meta, data = []};
new_adt(link, [FromClassName, ToClassName, LinkName]) ->
    Meta = #meta{type = link, names = [FromClassName, ToClassName, LinkName], state = alive},
    #adt{meta = Meta, data = []};
new_adt(object, [ClassName, ObjectName]) ->
    Meta = #meta{type = object, names = [ClassName, ObjectName], state = alive},
    #adt{meta = Meta, data = []};
new_adt(_,_) ->
    throw(not_supported_type).

%% @spec new_state_after(adtOperation(), adtState()) -> adtState()
%% @doc returns the ADT state following a given operation.
new_state_after(Operation, State) ->
    case {Operation, State} of
        {create, none}   -> alive;
        {hibern, alive}    -> frozen;
        {awake, frozen}    -> alive;
        {destroy, alive}   -> destroyed;
        {resur, destroyed} -> alive;
        {purge, destroyed} -> none;
        {_, _}             -> wrong_state
    end.

%% @spec is_ready_for(adtOperation(), adtState()) -> boolean()
%% @doc returns true if you can perform a given operation on an Adt
%% with the given state.
is_ready_for(Operation, State) ->
    new_state_after(Operation, State) /= wrong_state.

%%
