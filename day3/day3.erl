-module(day3).
-export([main/1]).

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
	Triangles = string:tokens(Input, "\n"),
	PossibleSum = lists:foldl(
		fun(Triangle, Sum) ->
			Sides = string:tokens(Triangle, " "),
			[A, B, C] = parse_sides(Sides),
			case valid_triangle(A, B, C) of
				true -> Sum + 1;
				false -> Sum
			end
		end,
		0,
		Triangles
	),
	io:format("Reading horizontally, there are ~B possible triangles.~n", [PossibleSum]),

	extract_triangles(Triangles, 0),

	ok.

parse_sides(Sides) when length(Sides) =:= 3 ->
	[A, B, C] = Sides,
	[list_to_integer(A), list_to_integer(B), list_to_integer(C)];
parse_sides(_) ->
	io:format("Invalid horizontal format. Check your input.~n"),
	exit(1).

valid_triangle(A, B, C) ->
	AB = A + B,
	AC = A + C,
	BC = B + C,

	case {AB > C, AC > B, BC > A} of
		{true, true, true} -> true;
		_ -> false
	end.

extract_triangles([T1, T2, T3 | Rest], Sum) ->
	[A1, A2, A3] = parse_sides(string:tokens(T1, " ")),
	[B1, B2, B3] = parse_sides(string:tokens(T2, " ")),
	[C1, C2, C3] = parse_sides(string:tokens(T3, " ")),
	Triangles = [[A1, B1, C1], [A2, B2, C2], [A3, B3, C3]],
	ValidatedTrianglesCount = lists:foldl(
		fun(Triangle, PartialSum) ->
			[A, B, C] = Triangle,
			case valid_triangle(A, B, C) of
				true -> PartialSum + 1;
				false -> PartialSum
			end
		end,
		0,
		Triangles
	),
	extract_triangles(Rest, Sum + ValidatedTrianglesCount);
extract_triangles([], Sum) ->
	io:format("Reading vertically, there are ~B possible triangles.", [Sum]);
extract_triangles(Rest, _) when length(Rest) < 3 ->
	io:format("Invalid vertical format. Check your input.~n"),
	exit(1).
