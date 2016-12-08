-module(day1).
-export([main/1]).
-import(ets, [insert/2, member/2, new/2]).

main(Input) when length(Input) < 1->
	io:format("Too few arguments.~n"),
	usage();
main(Input) when length(Input) =:= 1 ->
	parse(hd(Input));
main(Input) when length(Input) > 1 ->
	io:format("Too many arguments.~n"),
	usage().

usage() ->
	io:format("Usage: escript ~s INPUT~n", [?FILE]),
	io:format("Example: escript ~s \"$(cat input)\"~n", [?FILE]).

parse(Input) ->
	ets:new(visited, [set, named_table]),
	Operations = string:tokens(Input, ", "),
	parse_operations(north, Operations, 0, 0).

parse_operations(_, [], X, Y) ->
	io:format("We end up ~B blocks away.~n", [abs(X) + abs(Y)]);
parse_operations(PreviousHeading, [Operation | Rest], X, Y) ->
	{Direction, Steps} = parse_operation(Operation),
	Heading = next_heading(PreviousHeading, Direction),
	case ets:member(visited, found_hq) of
		true -> found_hq;
		false ->
			mark_path(Heading, X, Y, Steps)
	end,
	apply_steps(Heading, Steps, Rest, X, Y).

mark_path(north, X, Y, Steps) ->
	[mark_step(X, Y + Increment) || Increment <- lists:seq(1, Steps)],
	ets:insert(visited, {{X, Y}, position});
mark_path(west, X, Y, Steps) ->
	[mark_step(X - Increment, Y) || Increment <- lists:seq(1, Steps)],
	ets:insert(visited, {{X, Y}, position});
mark_path(south, X, Y, Steps) ->
	[mark_step(X, Y - Increment) || Increment <- lists:seq(1, Steps)],
	ets:insert(visited, {{X, Y}, position});
mark_path(east, X, Y, Steps) ->
	[mark_step(X + Increment, Y) || Increment <- lists:seq(1, Steps)],
	ets:insert(visited, {{X, Y}, position}).

mark_step(X, Y) ->
	case ets:member(visited, {X, Y}) of
		true ->
			ets:insert(visited, {found_hq, position}),
			io:format("Already visited (~B, ~B), ~B steps away.~n", [X, Y, abs(X) + abs(Y)]);
		false -> new
	end,
	ets:insert(visited, {{X, Y}, position}).

parse_operation([Direction | Steps]) when Direction =:= $R; Direction =:= $L ->
	{Direction, list_to_integer(Steps)};
parse_operation(_) ->
	io:format("Invalid syntax. Check your input.~n"),
	halt(1).

apply_steps(north, Steps, Operations, X, Y) ->
	parse_operations(north, Operations, X, Y + Steps);
apply_steps(west, Steps, Operations, X, Y) ->
	parse_operations(west, Operations, X - Steps, Y);
apply_steps(south, Steps, Operations, X, Y) ->
	parse_operations(south, Operations, X, Y - Steps);
apply_steps(east, Steps, Operations, X, Y) ->
	parse_operations(east, Operations, X + Steps, Y).

next_heading(north, $R) ->
	east;
next_heading(north, $L) ->
	west;
next_heading(west, $R) ->
	north;
next_heading(west, $L) ->
	south;
next_heading(south, $R) ->
	west;
next_heading(south, $L) ->
	east;
next_heading(east, $R) ->
	south;
next_heading(east, $L) ->
	north.
