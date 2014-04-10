-module(todo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("src/todo_record.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

	%% mnesia
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(todo, [{attributes, record_info(fields, todo)}]),

	%% leptus
	leptus:start_http([{handlers, [{todo_handler, state}]}]),

	%% dtl
	application:start(dtl),

	%% supervisor
	todo_sup:start_link().

stop(_State) ->
	ok.
