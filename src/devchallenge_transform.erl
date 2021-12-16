-module(devchallenge_transform).

-ifdef(TEST).
-compile(export_all).
-else.
-export([transform/1]).
-endif.

-spec transform(Data) -> {ok, Result} | {error, Reason}
	when Data :: term(),
	     Result :: term(),
	     Reason :: term().
transform(#{<<"address">> := #{<<"values">> := Values}}) ->
	case list_sum(Values) of
		error ->
			{error, invalid_values};
		LSum ->
			{ok, #{<<"result">> => digit_sum(LSum)}}
	end;
transform(_) ->
	{ok, #{<<"result">> => 0}}.

%% Helper function to calculate the sum of all items in a list of integers.
%% Returns either the sum or the atom `error` if a non-integer
%% item is encountered.
-spec list_sum(IntList) -> ListSum | 'error'
	when IntList :: [integer()],
	     ListSum :: integer().
list_sum(L) ->
	list_sum(L, 0).

list_sum([], Acc) ->
	Acc;
list_sum([N|More], Acc) when is_integer(N) ->
	list_sum(More, Acc + N);
list_sum(_, _) ->
	error.

%% Helper function to calculate the digit sum of an integer.
-spec digit_sum(Int) -> DigitSum
	when Int :: non_neg_integer(),
	     DigitSum :: non_neg_integer();
    (Int) -> DigitSum
	when Int :: neg_integer(),
	     DigitSum :: neg_integer().
digit_sum(N) when N >= 0 ->
	digit_sum(N, 0);
digit_sum(N) ->
	-digit_sum(-N, 0).

digit_sum(0, Acc) ->
	Acc;
digit_sum(N, Acc) ->
	digit_sum(N div 10, Acc + (N rem 10)).
