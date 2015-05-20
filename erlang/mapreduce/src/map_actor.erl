-module(map_actor).

-include("parser.hrl").

-export([map/3]).

map(Parent, Parser=#parser{mode=unsafe}, File) ->
	case file:read_file(File) of
		{ok, Binary} ->
			Parent ! {ok, parser:parse(Binary, Parser, dict:new())};
		{error, Reason} ->
			erlang:error(Reason)
	end;
map(Parent, Parser, File) ->
	case file:open(File, [raw, read_ahead, binary]) of
		{ok, Device} ->
			Parent ! {ok, read_lines(Device, Parser, dict:new())};
		{error, Reason} ->
			erlang:error(Reason)
	end.

read_lines(Device, Parser, Acc) ->
	case file:read_line(Device) of
		{ok, Line} ->
			read_lines(Device, Parser, parser:parse(Line, Parser, Acc));
		eof ->
			Acc;
		{error, Reason} ->
			erlang:error(Reason)
	end.
