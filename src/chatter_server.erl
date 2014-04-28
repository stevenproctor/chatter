-module(chatter_server).

-export([
         start/0,
         connect/1,
         join/2]).

-define(SERVER, ?MODULE).

start() ->
    chatter_store:init(),
    {ok, {}}.

connect(ChatHandle) ->
    chatter_client:start(ChatHandle).

join(User, RoomName) ->
    {ok, ChatRoom} = fetch_room(RoomName),
    {ok, HandlerId} = chatter_room:join(ChatRoom, User),
    {ok, ChatRoom, HandlerId}.

%%
%% Private Functions
%%

fetch_room(RoomName) ->
    case chatter_store:find(RoomName) of
        {ok, Pid} ->
            {ok, Pid};
        {error, _} ->
            {ok, Pid} = chatter_room:create(RoomName),
            chatter_store:insert(Pid, RoomName),
            {ok, Pid}
    end.
