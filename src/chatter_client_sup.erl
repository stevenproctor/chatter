-module(chatter_client_sup).

-behavior(supervisor).

-export([start_link/0, attach_client/1, detach_client/1]).

-export([init/1]).

start_link() ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).

init([]) ->
    client_store:init(),
    {ok, {{simple_one_for_one, 10, 3600},
        [{chatter_client, {chatter_client, start_link, []},
            transient, 2000, worker, [chatter_client]}]}}.

attach_client(Client) ->
    case client_store:find_client(Client) of
        {ok, _Pid} ->
            {error, connected};
        _NotAttached ->
            supervisor:start_child(?MODULE, [Client])
    end.

detach_client(Client) ->
    case client_store:find_client(Client) of
        {ok, Pid} ->
            supervisor:terminate_child(?MODULE, Pid);
        _NotAttached ->
            {error, disconnected}
    end.
