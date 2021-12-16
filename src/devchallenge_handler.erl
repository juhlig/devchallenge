-module(devchallenge_handler).

-export([init/2, allowed_methods/2, content_types_accepted/2, handle_request/2]).

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
		Json = jsx:decode(Body, [return_maps, {labels, binary}]),
		devchallenge_transform:transform(Json)
	of
		{ok, Res} ->
			Req2 = cowboy_req:set_resp_body(jsx:encode(Res), Req1),
			{true, Req2, State};
		{error, _} ->
			{false, Req1, State}
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
