-module(parser).

-include("parser.hrl").

-export([new/1]).
-export([parse/3]).

new(Text) ->
	case mapreduce:mode() of
		Mode=unsafe ->
			Newline = binary:compile_pattern(<< $\n >>),
			Pattern = binary:compile_pattern(permute_case(Text)),
			#parser{mode=Mode, newline=Newline, pattern=Pattern};
		Mode=binary ->
			Pattern = binary:compile_pattern(permute_case(Text)),
			#parser{mode=Mode, pattern=Pattern};
		Mode=regex ->
			case re:compile(Text, [caseless]) of
				{ok, Pattern} ->
					#parser{mode=Mode, pattern=Pattern};
				{error, Reason} ->
					erlang:error(Reason)
			end
	end.

parse(Binary, #parser{mode=unsafe, newline=Newline, pattern=Pattern, tab=Tab}, Acc) ->
	parse_unsafe(Binary, Newline, Tab, Pattern, Acc);
parse(Line, #parser{mode=binary, pattern=Pattern, tab=Tab}, Acc) ->
	case binary:match(Line, Pattern) of
		{Pos, _} ->
			case binary:match(Line, Tab) of
				{A, _} ->
					case binary:match(Line, Tab, [{scope, {A+1, Pos-A-1}}]) of
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
	end;
parse(Line, #parser{mode=regex, pattern=Pattern, tab=Tab}, Acc) ->
	case re:run(Line, Pattern, [{capture, none}]) of
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

parse_unsafe(Binary, Newline, Tab, Pattern, Acc) ->
	case binary:match(Binary, Newline) of
		{Pos, _} ->
			case binary:match(Binary, Pattern, [{scope, {0, Pos}}]) of
				{_, _} ->
					case binary:match(Binary, Tab) of
						{A, _} ->
							case binary:match(Binary, Tab, [{scope, {A+1, Pos-A-1}}]) of
								{B, _} ->
									Hood = binary:part(Binary, A+1, B-A-1),
									parse_unsafe(binary:part(Binary, Pos+1, byte_size(Binary)-Pos-1), Newline, Tab, Pattern, dict:update_counter(Hood, 1, Acc));
								_ ->
									erlang:error(badarg)
							end;
						_ ->
							erlang:error(badarg)
					end;
				_ ->
					parse_unsafe(binary:part(Binary, Pos+1, byte_size(Binary)-Pos-1), Newline, Tab, Pattern, Acc)
			end;
		_ ->
			Acc
	end.

permute_case(Text) ->
	List = unicode:characters_to_list(Text),
	permute_case(List, 0, 0, 1 bsl length(List), [], []).

permute_case(_, L, _, L, _, Acc) ->
	lists:reverse(Acc);
permute_case([H | T], I, J, L, Word, Acc) when (J band 1) =/= 0 ->
	[C] = string:to_upper([H]),
	permute_case(T, I, J bsr 1, L, [C | Word], Acc);
permute_case([H | T], I, J, L, Word, Acc) ->
	[C] = string:to_lower([H]),
	permute_case(T, I, J bsr 1, L, [C | Word], Acc);
permute_case([], I, _, L, Word, Acc) ->
	List = lists:reverse(Word),
	J = I+1,
	permute_case(List, J, J, L, [], [unicode:characters_to_binary(List) | Acc]).
