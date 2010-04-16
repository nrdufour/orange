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

-module(orange_executor).
-author('Nicolas R Dufour <nrdufour@gmail.com>').
-include("adt.hrl").

%% API exports
-export([do_create/2, do_hibern/2, do_awake/2, do_destroy/2, do_resur/2, do_purge/2, find_related/2]).

%% @doc creates an new ADT of type Type w/ the names Names.
do_create(Type, Names) ->
    do_it(create, Type, Names).

%% @doc hibernates an ADT (frozen state).
do_hibern(Type, Names) ->
    do_it(hibern, Type, Names).

%% @doc awakes a previously frozen ADT.
do_awake(Type, Names) ->
    do_it(awake, Type, Names).

%% @doc destroys an alive ADT.
do_destroy(Type, Names) ->
    do_it(destroy, Type, Names).

%% @doc resurects a previously destroyed ADT.
do_resur(Type, Names) ->
    do_it(resur, Type, Names).

%% @doc definitely remove a destroyed ADT from the repository.
do_purge(Type, Names) ->
    do_it(purge, Type, Names).

%% @doc find any related ADTs according to its type.
find_related(class, [ClassName]) ->
    %% must return attributes, objects and links!
    Attributes = orange_query:find_all_attributes(ClassName),
    Objects    = orange_query:find_all_objects(ClassName),
    Links      = orange_query:find_all_links(ClassName),
    lists:append([Attributes, Objects, Links]);
find_related(attribute, [_ClassName, _AttributeName]) ->
    %% should return object attributes later on
    [];
find_related(link, [_FromClassName, _ToClassName, _LinkName]) ->
    %% should return object links later on
    [];
find_related(object, [_ClassName, _ObjectName]) ->
    %% should return both object attributes and links later on
    [];
find_related(_, _) ->
    throw(wrong_type).

%%% --------------------------------------------------------------------------

%% @doc returns true if the 'Class' ADT state is 'alive'. 
is_class_alive(ClassName) ->
    ClassAdt = orange_storage_server:load(class, [ClassName]),
    case ClassAdt of
        not_found -> false;
        _         -> Meta = ClassAdt#adt.meta,
		     (Meta#meta.type =:= class) and (Meta#meta.state =:= alive)
    end.

%% @doc returns true if all parents exist and are alive (according to the type).
are_adt_parents_valid(class, _Names) ->
    true;
are_adt_parents_valid(Type, Names) when Type == attribute; Type == object ->
    [ClassName, _Name] = Names,
    is_class_alive(ClassName);
are_adt_parents_valid(link, Names) ->
    [FromClassName, ToClassName, _LinkName] = Names,
    is_class_alive(FromClassName) and is_class_alive(ToClassName);
are_adt_parents_valid(_Type, _Names) ->
    throw(invalid_type).

%% @doc check if the given adt is ready for the Operation.
check_adt_state(Type, Names, Operation) ->
    AreParentsValid = are_adt_parents_valid(Type, Names),
    case AreParentsValid of
        false ->
            {error, invalid_parents};
	_     ->
            Adt = orange_storage_server:load(Type, Names),
            case Operation of
                create ->
                    case Adt of
                        not_found -> {adt, not_found, alive};
                        _         -> {error, already_exists}
                    end;
                _      ->
                    case Adt of
                        not_found -> {error, not_found};
                        _         -> 
                            Meta = Adt#adt.meta,
                            NextState = orange_adt:new_state_after(Operation, Meta#meta.state),
                            case NextState of
                                wrong_state -> {error, wrong_state};
                                _           -> {adt, Adt, NextState}
                            end
                    end
            end
    end.

%% @doc performs an operation on an Adt.
do_it(Operation, Type, Names) ->
    Check = check_adt_state(Type, Names, Operation),
    case Check of
        {error, _Reason} -> Check;
	{adt, Adt, NextState} ->
            case Operation of
                create  ->
                    NewAdt = orange_adt:new_adt(Type, Names),
                    orange_storage_server:store(Type, Names, NewAdt),
		    {ok, alive};
                hibern ->
                    Meta = Adt#adt.meta,
		    UpdatedMeta = Meta#meta{state = NextState},
		    UpdatedAdt = Adt#adt{meta = UpdatedMeta},
		    orange_storage_server:store(Type, Names, UpdatedAdt),
		    {ok, frozen};
                awake   ->
                    Meta = Adt#adt.meta,
		    UpdatedMeta = Meta#meta{state = NextState},
		    UpdatedAdt = Adt#adt{meta = UpdatedMeta},
		    orange_storage_server:store(Type, Names, UpdatedAdt),
		    {ok, alive};
                destroy ->
                    Meta = Adt#adt.meta,
		    UpdatedMeta = Meta#meta{state = NextState},
		    UpdatedAdt = Adt#adt{meta = UpdatedMeta},
		    orange_storage_server:store(Type, Names, UpdatedAdt),
		    {ok, destroyed};
                resur   ->
                    Meta = Adt#adt.meta,
		    UpdatedMeta = Meta#meta{state = NextState},
		    UpdatedAdt = Adt#adt{meta = UpdatedMeta},
		    orange_storage_server:store(Type, Names, UpdatedAdt),
		    {ok, alive};
                purge   ->
		    {error, not_yet_implemented};
                _       ->
		    {error, wrong_operation}
            end
    end.

%%

