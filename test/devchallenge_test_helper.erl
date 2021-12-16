-module(devchallenge_test_helper).

-export([digit_sum/1]).

digit_sum(Int) ->
	lists:foldr(
		fun
			($-, Acc) -> Acc * -1;
			(D, Acc) -> (D - $0) + Acc
		end,
		0,
		integer_to_list(Int)
	).
