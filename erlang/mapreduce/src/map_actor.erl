-module(map_actor).

-export([map/2]).

-define(SEARCH, <<"knicks">>).

map(Parent, File) ->
	case file:open(File, [raw, read_ahead, binary]) of
		{ok, Device} ->
			Parent ! {ok, read_lines(Device, dict:new())};
		{error, Reason} ->
			erlang:error(Reason)
	end.

read_lines(Device, Acc) ->
	case re:compile(?SEARCH, [caseless]) of
		{ok, Regex} ->
			read_lines(Device, Regex, binary:compile_pattern(<< $\t >>), Acc);
		{error, Reason} ->
			erlang:error(Reason)
	end.

read_lines(Device, Regex, Tab, Acc) ->
	case file:read_line(Device) of
		{ok, Data} ->
			read_lines(Device, Regex, Tab, parse(Data, Regex, Tab, Acc));
		eof ->
			Acc;
		{error, Reason} ->
			erlang:error(Reason)
	end.

parse(Line, Regex, Tab, Acc) ->
	case re:run(Line, Regex, [{capture, none}]) of
		match ->
			case binary:match(Line, Tab) of
				{A, _} ->
					case binary:match(Line, Tab, [{scope, {A+1, byte_size(Line)-A-1}}]) of
						{B, _} ->
							Hood = binary:part(Line, A+1, B-A-1),
							dict:update_counter(Hood, 1, Acc);
						_ ->
							erlang:error(badarg)
					end;
				_ ->
					erlang:error(badarg)
			end;
		_ ->
			Acc
	end.
