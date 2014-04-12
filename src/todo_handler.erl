-module(todo_handler).
-compile({parse_transform, leptus_pt}).

%% leptus callbacks
-export([init/3]).
-export([terminate/4]).

%% leptus routes
-export([get/3]).
-export([post/3]).
-export([put/3]).
-export([delete/3]).

%% record
-include("todo_record.hrl").

%% @todo: add static route

%% Start
init(_Route, _Req, State) ->
	{ok, State}.

%% Main
get("/", _Req, State) ->
	{ok, Body} = dtl:render("index.html", [
		{title, <<"Todo List">>}
	]),
	{200, {html, Body}, State};

%% List
get("/todos", _Req, State) ->
	{200, {json, [{<<"get">>,<<"todos">>}]}, State};

%% Retrieve
get("/todo/:id", _Req, State) ->
	{200, {json, [{<<"get">>,<<"todo">>}]}, State}.

%% Create
post("/todo", Req, State) ->
	Post = leptus_req:body_qs(Req),
	io:format("post: ~p~n", [Post]),

	{<<"id">>, Id} = lists:keyfind(<<"id">>, 1, Post),
	{<<"content">>, Content} = lists:keyfind(<<"content">>, 1, Post),
	{<<"priority">>, Priority} = lists:keyfind(<<"priority">>, 1, Post),
	{<<"status">>, Status} = lists:keyfind(<<"status">>, 1, Post),

	Write = fun() ->
		Todo = #todo{
			id = Id,
			content = Content,
			priority = Priority,
			status = Status
		},
		mnesia:write(Todo)
	end,

	mnesia:transaction(Write),

	{200, {json, Post}, State}.

%% Update
put("/todo/:id", _Req, State) ->
	{200, {json, [{<<"put">>,<<"todo">>}]}, State}.

%% Delete
delete("/todo/:id", _Req, State) ->
	{200, {json, [{<<"delete">>,<<"todo">>}]}, State}.

%% End
terminate(_Reason, _Route, _Req, _State) ->
	ok.
