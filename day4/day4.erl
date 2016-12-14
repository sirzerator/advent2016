-module(day4).
-export([main/1]).
-import(maps, [put/3, get/2, get/3, put/3, new/0, to_list/1, without/2]).

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
	RoomIdents = string:tokens(Input, "\n"),
	ValidRoomsSum = lists:foldl(
		fun(RoomIdent, Sum) ->
			case validate_room_name(RoomIdent) of
				{valid, SectorId} -> Sum + SectorId;
				{invalid, _} -> Sum
			end
		end,
		0,
		RoomIdents
	),
	io:format("Sector IDs of all valid rooms, when summed, give ~B .~n", [ValidRoomsSum]),
	io:format("Just `grep` for North Pole objects.~n").

validate_room_name(RoomIdent) ->
	RoomNameParts = string:tokens(RoomIdent, "-"),
	{Map, SectorId, Checksum} = parse_room_name(RoomNameParts),
	case validate_checksum(Map, Checksum) of
		true ->
			IntegerSectorId = list_to_integer(SectorId),
			output_real_name(RoomNameParts, IntegerSectorId),
			{valid, IntegerSectorId};
		false -> {invalid, 0}
	end.

output_real_name([NamePart, _], SectorId) ->
	output_real_name_part(NamePart, SectorId),
	io:format("~p~n", [SectorId]);
output_real_name([NamePart | Rest], SectorId) ->
	output_real_name_part(NamePart, SectorId),
	output_real_name(Rest, SectorId).

output_real_name_part(NamePart, SectorId) ->
	lists:foreach(
		fun(L) ->
			case L of
				$[ -> io:format(" ");
				$] -> io:format(" ");
				_ -> io:format("~c", [((L - $a) + SectorId) rem 26 + $a])
			end
		end,
		NamePart
	),
	io:format(" ").

parse_room_name(RoomNameParts) ->
	Map = maps:new(),
	fill_letters_map(Map, RoomNameParts).

fill_letters_map(Map, [Ident, Tail]) ->
	FinalMap = count_letters(Map, Ident),
	{SectorId, Checksum} = parse_room_name_tail(Tail),
	{FinalMap, SectorId, Checksum};
fill_letters_map(Map, [NamePart | Rest]) ->
	NewMap = count_letters(Map, NamePart),
	fill_letters_map(NewMap, Rest).

count_letters(Map, NamePart) ->
	lists:foldl(
		fun(Letter, IMap) ->
			CurrentValue = maps:get(Letter, IMap, 0),
			maps:put(Letter, CurrentValue + 1, IMap)
		end,
		Map,
		NamePart
	).

parse_room_name_tail(Tail) ->
	parse_room_name_tail(Tail, "", "").

parse_room_name_tail([H, N | Rest], SectorId, Checksum) ->
	case N of
		$[ -> parse_checksum(Rest, SectorId ++ [H], Checksum);
		_ -> parse_room_name_tail(Rest, SectorId ++ [H] ++ [N], Checksum)
	end.

parse_checksum([H, _], SectorId, Checksum) ->
	{SectorId, Checksum ++ [H]};
parse_checksum([H | Rest], SectorId, Checksum) ->
	parse_checksum(Rest, SectorId, Checksum ++ [H]).

validate_checksum(Map, Checksum) ->
	validate_checksum(Map, Checksum, true).

validate_checksum(_, _, false) ->
	false;
validate_checksum(_, [], true) ->
	true;
validate_checksum(Map, [Letter | Rest], _) ->
	LetterCount = maps:get(Letter, Map, 0),
	MapWithout = maps:without([Letter], Map),
	NewResult = validate_checksum_part(MapWithout, Letter, LetterCount),
	validate_checksum(MapWithout, Rest, NewResult).

validate_checksum_part(Map, Letter, LetterCount) ->
	Keys = maps:keys(Map),
	try
		lists:all(
			fun(L) ->
				LCount = maps:get(L, Map),
				LetterCount > LCount orelse ((LetterCount =:= LCount) and (Letter < L))
			end,
			Keys
		)
	catch
		error:{badkey, _} -> false
	end.
