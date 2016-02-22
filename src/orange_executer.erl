%% Copyright 2009-2016 Nicolas R Dufour.
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
%% @copyright 2009-2016 Nicolas R Dufour.

-module(orange_executer).
-behaviour(gen_server).

-author('Nicolas R Dufour <nrdufour@gmail.com>').

-include("adt.hrl").

%% API.
-export([start_link/0]).
-export([execute/3]).

-export([is_valid_op/1]).
-export([is_valid_type/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
}).

-define(VALID_OPS, [create_op, hibern_op, awake_op, destroy_op, resur_op, purge_op]).
-define(VALID_TYPES, [class_t, attribute_t, link_t, object_t]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

execute(Op, Type, Names) ->
	gen_server:call(?MODULE, {exec, Op, Type, Names}).

is_valid_op(Op) ->
	lists:member(Op, ?VALID_OPS).

is_valid_type(Type) ->
	lists:member(Type, ?VALID_TYPES).

%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call({exec, Op, Type, Names}, _From, State) ->
	IsValidOp = is_valid_op(Op),
	IsValidType = is_valid_type(Type),
	case {IsValidOp, IsValidType} of
		{false, true} ->
			{reply, {error, wrong_op}, State};
		{true, false} ->
			{reply, {error, wrong_type}, State};
		{false, false} ->
			{reply, {error, wrong_o_n_t}, State};
		{true, true} ->
			Reply = do_it(Op, Type, Names),
			{reply, Reply, State}
	end;
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal

%% @doc returns true if the 'Class' ADT state is 'alive'. 
is_class_alive(ClassName) ->
	ClassAdt = orange_storage_server:load(class_t, [ClassName]),
	io:format("ClassAdt:~p~n", [ClassAdt]),
	case ClassAdt of
		not_found -> false;
		_         -> Meta = ClassAdt#adt.meta,
			(Meta#meta.type =:= class_t) and (Meta#meta.state =:= alive)
	end.

%% @doc returns true if all parents exist and are alive (according to the type).
are_adt_parents_valid(class_t, _Names) ->
	true;
are_adt_parents_valid(Type, Names) when Type == attribute_t; Type == object_t ->
	[ClassName, _Name] = Names,
	is_class_alive(ClassName);
are_adt_parents_valid(link_t, Names) ->
	[FromClassName, ToClassName, _LinkName] = Names,
	is_class_alive(FromClassName) and is_class_alive(ToClassName);
are_adt_parents_valid(_Type, _Names) ->
	throw(invalid_type).

%% @doc check if the given adt is ready for the Operation.
check_adt_state(Type, Names, Operation) ->
	AreParentsValid = are_adt_parents_valid(Type, Names),
	io:format("AreParentsValid: ~p~n", [AreParentsValid]),
	case AreParentsValid of
		false ->
			{error, invalid_parents};
		_     ->
			Adt = orange_storage_server:load(Type, Names),
			io:format("loading from storage on T:~p/N:~p -> ~p~n", [Type, Names, Adt]),
			case Operation of
				create_op ->
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
	io:format("Start do_it w/ O:~p, T:~p, N:~p~n", [Operation, Type, Names]),
	Check = check_adt_state(Type, Names, Operation),
	io:format("do_it after check w/ O:~p, T:~p, N:~p -> Check:~p~n", [Operation, Type, Names, Check]),
	case Check of
		{error, _Reason} -> Check;
		{adt, Adt, NextState} ->
			case Operation of
				create_op  ->
					NewAdt = orange_adt:new_adt(Type, Names),
					orange_storage_server:store(Type, Names, NewAdt),
					{ok, alive};
				hibern_op ->
					Meta = Adt#adt.meta,
					UpdatedMeta = Meta#meta{state = NextState},
					UpdatedAdt = Adt#adt{meta = UpdatedMeta},
					orange_storage_server:store(Type, Names, UpdatedAdt),
					{ok, frozen};
				awake_op   ->
					Meta = Adt#adt.meta,
					UpdatedMeta = Meta#meta{state = NextState},
					UpdatedAdt = Adt#adt{meta = UpdatedMeta},
					orange_storage_server:store(Type, Names, UpdatedAdt),
					{ok, alive};
				destroy_op ->
					Meta = Adt#adt.meta,
					UpdatedMeta = Meta#meta{state = NextState},
					UpdatedAdt = Adt#adt{meta = UpdatedMeta},
					orange_storage_server:store(Type, Names, UpdatedAdt),
					{ok, destroyed};
				resur_op   ->
					Meta = Adt#adt.meta,
					UpdatedMeta = Meta#meta{state = NextState},
					UpdatedAdt = Adt#adt{meta = UpdatedMeta},
					orange_storage_server:store(Type, Names, UpdatedAdt),
					{ok, alive};
				purge_op   ->
					{error, not_yet_implemented};
				_       ->
					{error, wrong_operation}
			end
	end.
