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

-module(orange_query).
-author('Nicolas R Dufour <nrdufour@gmail.com>').

-export([find_all_attributes/1, find_all_objects/1, find_all_links/1]).

%% @doc returns all attributes belonging to the given class.
find_all_attributes(ClassName) ->
    [].

%% @doc returns all objects belonging to the given class.
find_all_objects(ClassName) ->
    [].

%% @doc returns all links originating/targeting from/to the given class.
find_all_links(ClassName) ->
    [].

%%
