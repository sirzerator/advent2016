-module(day2).
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
	io:format("The ideal code is "),
	Operations = string:tokens(Input, "\n"),
	LastKey = lists:foldl(
		fun(Operation, Key) ->
			follow_sequence(
				fun(NextStep, Number) ->
					ideal(NextStep, Number)
				end,
				Key,
				Operation
			)
		end,
		5,
		Operations
	),
	io:format(".~n"),

	io:format("The actual code is "),
	lists:foldl(
		fun(Operation, Key) ->
			follow_sequence(
				fun(NextStep, Number) ->
					actual(NextStep, Number)
				end,
				Key,
				Operation
			)
		end,
		LastKey,
		Operations
	),
	io:format(".~n").

follow_sequence(Fun, Key, Steps) ->
	EndKey = lists:foldl(
		Fun,
		Key,
		Steps
	),
	output_key(EndKey),
	EndKey.

output_key(Key) when Key < 10 ->
	io:format("~B", [Key]);
output_key(Key) ->
	io:format("~c", [Key]).

ideal($U, Number) when Number > 3 ->
	Number - 3;
ideal($D, Number) when Number < 7 ->
	Number + 3;
ideal($L, Number) ->
	case (Number - 1) rem 3 of
		0 -> Number;
		_ -> Number - 1
	end;
ideal($R, Number) ->
	case (Number + 1) rem 3 of
		1 -> Number;
		_ -> Number + 1
	end;
ideal(_, Number) ->
	Number.

actual(Instruction, 1) ->
	case Instruction of
		$D -> 3;
		_ -> 1
	end;
actual(Instruction, 2) ->
	case Instruction of
		$D -> 6;
		$R -> 3;
		_ -> 2
	end;
actual(Instruction, 3) ->
	case Instruction of
		$D -> 7;
		$L -> 2;
		$U -> 1;
		$R -> 4;
		_ -> 3
	end;
actual(Instruction, 4) ->
	case Instruction of
		$D -> 8;
		$L -> 3;
		_ -> 4
	end;
actual(Instruction, 5) ->
	case Instruction of
		$R -> 6;
		_ -> 5
	end;
actual(Instruction, 6) ->
	case Instruction of
		$D -> $A;
		$L -> 5;
		$U -> 2;
		$R -> 7;
		_ -> 6
	end;
actual(Instruction, 7) ->
	case Instruction of
		$D -> $B;
		$L -> 6;
		$U -> 3;
		$R -> 8;
		_ -> 7
	end;
actual(Instruction, 8) ->
	case Instruction of
		$D -> $C;
		$L -> 7;
		$U -> 4;
		$R -> 9;
		_ -> 8
	end;
actual(Instruction, 9) ->
	case Instruction of
		$L -> 8;
		_ -> 9
	end;
actual(Instruction, $A) ->
	case Instruction of
		$U -> 6;
		$R -> $B;
		_ -> $A
	end;
actual(Instruction, $B) ->
	case Instruction of
		$D -> $D;
		$L -> $A;
		$U -> 7;
		$R -> $C;
		_ -> $B
	end;
actual(Instruction, $C) ->
	case Instruction of
		$L -> $B;
		$U -> 8;
		_ -> $C
	end;
actual(Instruction, $D) ->
	case Instruction of
		$U -> $B;
		_ -> $D
	end.
