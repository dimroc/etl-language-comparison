-module(mapper).

-export([map/1]).

-define(SEARCH, <<"knicks">>).

map(InputDir) ->
	case file:list_dir(InputDir) of
		{ok, Filenames} ->
			async([filename:join(InputDir, Filename) || Filename <- Filenames], 0);
		{error, Reason} ->
			erlang:error(Reason)
	end.

async(Files, N) ->
	async(Files, parser:new(?SEARCH), N).

async([File | Files], Parser, N) ->
	case filelib:is_file(File) of
		false ->
			async(Files, Parser, N);
		true ->
			spawn_link(map_actor, map, [self(), Parser, File]),
			async(Files, N+1)
	end;
async([], _Parser, N) ->
	await(N, dict:new()).

await(0, Acc) ->
	Acc;
await(N, Acc) ->
	receive
		{ok, Mappings} ->
			await(N-1, dict:merge(fun merger/3, Mappings, Acc))
	end.

merger(_Key, Value1, Value2) ->
	Value1 + Value2.
