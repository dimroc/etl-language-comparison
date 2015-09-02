-module(mapreduce).

-export([main/1]).
-export([mode/0]).
-export([start/0]).

main(_) ->
	Mappings = mapper:map("../../tmp/tweets/"),
	ok = reducer:reduce(Mappings, ["../../tmp/erlang_", atom_to_list(mode()), "_output"]),
	erlang:halt(0).

mode() ->
	case os:getenv("MAPREDUCE") of
		"binary" ->
			binary;
		"regex" ->
			regex;
		"unsafe" ->
			unsafe;
		_ ->
			regex
	end.

start() ->
	main([]).
