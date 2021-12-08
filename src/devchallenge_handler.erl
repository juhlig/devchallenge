-module(devchallenge_handler).

-ifdef(TEST).
-compile(export_all).
-else.
-export([init/2, allowed_methods/2, content_types_accepted/2, handle_request/2]).
-endif.

%% Handler init.
%% Switch over to cowboy_rest.
-spec init(Req, State) -> {'cowboy_rest', Req, State}
	when Req :: cowboy_req:req(),
	     State :: term().
init(Req, State) ->
	{cowboy_rest, Req, State}.

%% Allowed methods.
%% Only POST is allowed.
-spec allowed_methods(Req, State) -> {Allowed, Req, State}
	when Req :: cowboy_req:req(),
	     State :: term(),
	     Allowed :: [Method],
	     Method :: binary().
allowed_methods(Req, State) ->
	{[<<"POST">>], Req, State}.

%% Register callbacks.
%% application/json --> handle_request
-spec content_types_accepted(Req, State) -> {Callbacks, Req, State}
	when Req :: cowboy_req:req(),
	     State :: term(),
	     Callbacks :: [Callback],
	     Callback :: {MimeType, Function},
	     MimeType :: binary(),
	     Function :: atom().
content_types_accepted(Req, State) ->
	{[{<<"application/json">>, handle_request}], Req, State}.

%% Request callback.
%% Replies with...
%%    * {"result": R} if the given JSON is valid and contains a
%%      list of only integers in the "address" > "values" key,
%%      with R being the digit sum of the sum of said integers.
%%    * {"result". 0} if the given JSON is valid but does not
%%      contain the "address" > "values" or "address" keys.
%%    * Error code 400 if the JSON is invalid, or if the value
%%      of the "address" > "values" key is not a list or the list
%%      contains at least one non-integer.
-spec handle_request(Req0, State) -> {boolean(), Req1, State}
	when Req0 :: cowboy_req:req(),
	     State :: term(),
	     Req1 :: cowboy_req:req().
handle_request(Req0, State) ->
	{ok, Body, Req1} = read_full_body(Req0),
	try
		jsx:decode(Body, [return_maps, {labels, binary}])
	of
		#{<<"address">> := #{<<"values">> := Values}} ->
			case list_sum(Values) of
				error ->
					{false, Req1, State};
				LSum ->
					DSum = digit_sum(LSum),
					Req2 = cowboy_req:set_resp_body(jsx:encode(#{<<"result">> => DSum}), Req1),
					{true, Req2, State}
			end;
		_ ->
			Req2 = cowboy_req:set_resp_body(jsx:encode(#{<<"result">> => 0}), Req1),
			{true, Req2, State}
	catch
		_:_ ->
			{false, Req1, State}
	end.

%% Helper function to read the full request body.
-spec read_full_body(Req0) -> {'ok', Body, Req1}
	when Req0 :: cowboy_req:req(),
	     Body :: binary(),
             Req1 :: cowboy_req:req().
read_full_body(Req) ->
	read_full_body(cowboy_req:read_body(Req), <<>>).

read_full_body({more, Data, Req1}, Acc) ->
	read_full_body(cowboy_req:read_body(Req1), <<Acc/binary, Data/binary>>);
read_full_body({ok, Data, Req1}, Acc) ->
	{ok, <<Acc/binary, Data/binary>>, Req1}.

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
