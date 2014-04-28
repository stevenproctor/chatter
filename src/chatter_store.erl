-module(chatter_store).

-export([
         init/0,
         insert/2,
         find/1]).

-define(TABLE_ID, ?MODULE).

init() ->
    ets:new(?TABLE_ID, [public, named_table]),
    ok.

insert(Pid, RoomName) ->
    ets:insert(?TABLE_ID, {RoomName, Pid}).

find(RoomName) ->
    case ets:lookup(?TABLE_ID, RoomName) of
        [{RoomName, Pid}] ->
            {ok, Pid};
        [] -> {error, not_found}
    end.
