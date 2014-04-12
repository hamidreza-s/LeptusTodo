-module(todo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("todo_record.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

	%% mnesia
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(todo, [
		{attributes, record_info(fields, todo)},
		{disc_copies, [node()]}
	]),

	%% leptus
	leptus:start_listener(http, [{'_', [{todo_handler, undef}]}]),

	%% dtl
	application:start(dtl),

	%% supervisor
	todo_sup:start_link().

stop(_State) ->
	ok.
