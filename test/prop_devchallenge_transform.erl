-module(prop_devchallenge_transform).

-include_lib("proper/include/proper.hrl").

prop_listsum() ->
	?FORALL(
		List,
		oneof(
			[
				list(integer()), % list all integers (valid)
				list(term()), % list of anything (maybe valid if all integers)
				list(?SUCHTHAT(X, term(), not is_integer(X))) % list of all non-integers (invalid)
			]
		),
		begin
			case lists:all(fun is_integer/1, List) of
				true ->
					Res = devchallenge_transform:list_sum(List),
					is_integer(Res) andalso Res =:= lists:sum(List);
				false ->
					error =:= devchallenge_transform:list_sum(List)
			end
		end
	).

prop_digitsum_valid() ->
	?FORALL(
		Int,
		integer(),
		begin
			Res = devchallenge_transform:digit_sum(Int),
			Exp = devchallenge_test_helper:digit_sum(Int),
			Res =:= Exp
		end
	).

prop_digitsum_invalid() ->
	?FORALL(
		NonInt,
		?SUCHTHAT(X, term(), not is_integer(X)),
		begin
			ok =/= catch begin devchallenge_transform:digit_sum(NonInt), ok end
		end
	).

prop_transform_valid() ->
	?FORALL(
		List,
		list(integer()),
		begin
			Exp = devchallenge_test_helper:digit_sum(lists:sum(List)),
			{ok, #{<<"result">> => Exp}} =:= devchallenge_transform:transform(#{<<"address">> => #{<<"values">> => List}})
		end
	).

prop_transform_missingkeys() ->
	{ok, #{<<"result">> => 0}} =:= devchallenge_transform:transform(#{<<"foo">> => #{<<"values">> => [1]}})
	andalso
	{ok, #{<<"result">> => 0}} =:= devchallenge_transform:transform(#{<<"address">> => #{<<"bar">> => [1]}})
	andalso
	{ok, #{<<"result">> => 0}} =:= devchallenge_transform:transform(#{<<"foo">> => #{<<"bar">> => [1]}}).

prop_transform_invalid1() ->
	?FORALL(
		List,
		?SUCHTHAT(L, list(term()), not lists:all(fun is_integer/1, L)),
		begin
			{error, invalid_values} =:= devchallenge_transform:transform(#{<<"address">> => #{<<"values">> => List}})
		end
	).

prop_transform_invalid2() ->
	?FORALL(
		NonList,
		?SUCHTHAT(T, term(), not is_list(T)),
		begin
			{error, invalid_values} =:= devchallenge_transform:transform(#{<<"address">> => #{<<"values">> => NonList}})
		end
	).
