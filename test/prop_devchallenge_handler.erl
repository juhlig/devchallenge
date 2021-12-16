-module(prop_devchallenge_handler).

-include_lib("proper/include/proper.hrl").

prop_request_valid() ->
	?SETUP(
		fun() ->
			application:ensure_all_started(devchallenge),
			fun () -> application:stop(devchallenge) end
		end,
		?FORALL(
			IntList,
			list(integer()),
			begin
				Exp = integer_to_binary(devchallenge_test_helper:digit_sum(lists:sum(IntList))),
				Json = jsx:encode(#{<<"address">> => #{<<"values">> => IntList}}),
				{ok, Sock} = gen_tcp:connect("localhost", 8080, [{active, false}, binary]),
				ok = gen_tcp:send(Sock, build_request(Json)),
				{ok, <<"HTTP/1.1 200 OK\r\n", Data/binary>>} = gen_tcp:recv(Sock, 0),
				<<"{\"result\":", Exp:(byte_size(Exp))/binary, "}">> = get_body(Data),
				ok = gen_tcp:close(Sock),
				true
			end
		)
	).

prop_request_incomplete() ->
	application:ensure_all_started(devchallenge),
	Json = jsx:encode(#{}),
	{ok, Sock} = gen_tcp:connect("localhost", 8080, [{active, false}, binary]),
	ok = gen_tcp:send(Sock, build_request(Json)),
	{ok, <<"HTTP/1.1 200 OK\r\n", Data/binary>>} = gen_tcp:recv(Sock, 0),
	<<"{\"result\":0}">> = get_body(Data),
	ok = gen_tcp:close(Sock),
	application:stop(devchallenge),
	true.

prop_request_invalid1() ->
	application:ensure_all_started(devchallenge),
	{ok, Sock} = gen_tcp:connect("localhost", 8080, [{active, false}, binary]),
	ok = gen_tcp:send(Sock, build_request(<<0>>)),
	{ok, <<"HTTP/1.1 400 Bad Request\r\n", _/binary>>} = gen_tcp:recv(Sock, 0),
	ok = gen_tcp:close(Sock),
	application:stop(devchallenge),
	true.

prop_request_invalid2() ->
	?SETUP(
		fun() ->
			application:ensure_all_started(devchallenge),
			fun () -> application:stop(devchallenge) end
		end,
		?FORALL(
			Data,
			oneof([binary(), ?SUCHTHAT(L, non_empty(list(oneof([binary(), integer()]))), not lists:all(fun is_integer/1, L))]),
			begin
				Json = jsx:encode(#{<<"address">> => #{<<"values">> => Data}}),
				{ok, Sock} = gen_tcp:connect("localhost", 8080, [{active, false}, binary]),
				ok = gen_tcp:send(Sock, build_request(Json)),
				{ok, <<"HTTP/1.1 400 Bad Request\r\n", _/binary>>} = gen_tcp:recv(Sock, 0),
				ok = gen_tcp:close(Sock),
				true
			end
		)
	).

build_request(Data) ->
	<<"POST / HTTP/1.1\r\n"
	  "Host: localhost\r\n"
	  "Accept: */*\r\n"
	  "Content-Type: application/json\r\n"
	  "Content-Length: ", (integer_to_binary(byte_size(Data)))/binary, "\r\n"
	  "\r\n",
	  Data/binary>>.

get_body(<<>>) ->
	<<>>;
get_body(<<"\r\n\r\n", Body/binary>>) ->
	Body;
get_body(<<_, More/binary>>) ->
	get_body(More).
