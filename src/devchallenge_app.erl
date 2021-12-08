-module(devchallenge_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Port = list_to_integer(os:getenv("PORT", "8080")),
	Dispatch = cowboy_router:compile(
		[{'_', [{"/", devchallenge_handler, []}]}]
	),
	{ok, _} = cowboy:start_clear(
		devchallenge_listener,
		#{socket_opts => [{port, Port}]},
		#{env => #{dispatch => Dispatch}}
	),
	devchallenge_sup:start_link().

stop(_State) ->
	catch cowboy:stop_listener(devchallenge_listener),
	ok.
