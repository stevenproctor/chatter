-module(client_store).

-export([
         init/0,
         insert/2,
         find_client/1]).

-define(TABLE_ID, ?MODULE).

init() ->
    ets:new(?TABLE_ID, [public, named_table]),
    ok.

insert(Pid, Client) ->
    ets:insert(?TABLE_ID, {Client, Pid}).

find_client(Client) ->
    case ets:lookup(?TABLE_ID, Client) of
        [{Client, Pid}] ->
            {ok, Pid};
        [] -> {error, not_found}
    end.
