-module(map_actor).

-export([map/2]).

map(Parent, File) ->
	case file:read_file(File) of
		{ok, Binary} ->
			Parent ! {ok, tweet_parser:parse(Binary, dict:new())};
		{error, Reason} ->
			erlang:error(Reason)
	end.
