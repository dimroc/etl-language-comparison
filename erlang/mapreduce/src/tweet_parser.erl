-module(tweet_parser).

-export([parse/2]).

parse(<<>>, Acc) ->
	Acc;
parse(Binary, Acc) ->
	parse_tweet(Binary, Acc).

parse_tweet(<< $\t, Rest/binary >>, Acc) ->
	parse_tweet_hood(Rest, <<>>, Acc);
parse_tweet(<< _, Rest/binary >>, Acc) ->
	parse_tweet(Rest, Acc);
parse_tweet(<<>>, Acc) ->
	Acc.

parse_tweet_hood(<< $\t, Rest/binary >>, Hood, Acc) ->
	parse_next_tab(Rest, Hood, Acc);
parse_tweet_hood(<< $\n, Rest/binary >>, _Hood, Acc) ->
	parse(Rest, Acc);
parse_tweet_hood(<< C, Rest/binary >>, Hood, Acc) ->
	parse_tweet_hood(Rest, << Hood/binary, C >>, Acc);
parse_tweet_hood(<<>>, _Hood, Acc) ->
	Acc.

parse_next_tab(<< $\t, Rest/binary >>, Hood, Acc) ->
	parse_tweet_message(Rest, Hood, Acc);
parse_next_tab(<< $\n, Rest/binary >>, _Hood, Acc) ->
	parse(Rest, Acc);
parse_next_tab(<< _, Rest/binary >>, Hood, Acc) ->
	parse_next_tab(Rest, Hood, Acc);
parse_next_tab(<<>>, _Hood, Acc) ->
	Acc.

parse_tweet_message(<< K, Rest/binary >>, Hood, Acc)
		when (K =:= $K orelse K =:= $k) ->
	parse_tweet_message_match(Rest, Hood, Acc);
parse_tweet_message(<< $\n, Rest/binary >>, _Hood, Acc) ->
	parse(Rest, Acc);
parse_tweet_message(<< _, Rest/binary >>, Hood, Acc) ->
	parse_tweet_message(Rest, Hood, Acc);
parse_tweet_message(<<>>, _Hood, Acc) ->
	Acc.

parse_tweet_message_match(<< N, I, C, K, S, Rest/binary >>, Hood, Acc)
		when (N =:= $N orelse N =:= $n)
		andalso (I =:= $I orelse I =:= $i)
		andalso (C =:= $C orelse C =:= $c)
		andalso (K =:= $K orelse K =:= $k)
		andalso (S =:= $S orelse S =:= $s) ->
	parse_next_line(Rest, dict:update_counter(Hood, 1, Acc));
parse_tweet_message_match(Rest, Hood, Acc) ->
	parse_tweet_message(Rest, Hood, Acc).

parse_next_line(<< $\n, Rest/binary >>, Acc) ->
	parse(Rest, Acc);
parse_next_line(<< _, Rest/binary >>, Acc) ->
	parse_next_line(Rest, Acc);
parse_next_line(<<>>, Acc) ->
	Acc.
