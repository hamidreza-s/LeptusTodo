-module(todo_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Include todo record
-include("todo_record.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

   %% Start mnesia database in current node
   %% which is nonode@nohost
   mnesia:create_schema([node()]),
   mnesia:start(),
   
   %% Create mnesia table based on todo record
   %% which is defined in src/todo_records.hrl
   mnesia:create_table(todo, [
      {attributes, record_info(fields, todo)},
      {disc_copies, [node()]} %% disc_copies means persistent
   ]),

   %% Define static directory for application
   Opts = [{static_dir, {'_', {priv_dir, ?MODULE, "templates"}}}],
   
   %% Start Leptus listener and set it to route every requests
   %% to src/todo_handler.erl
   leptus:start_listener(http, [{'_', [{todo_handler, undef}]}], Opts).

stop(_State) ->
   ok.
