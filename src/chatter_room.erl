-module(chatter_room).

-export([create/1,
         join/2,
         leave/3,
         send/4]).

-define(SERVER, ?MODULE).

create(ChatRoomName) ->
    ChatRoom = {?SERVER, ChatRoomName},
    gen_event:start({global, ChatRoom}).

join(RoomPid, User) ->
    HandlerId = {chatter_event, make_ref()},
    Now = calendar:universal_time(),
    gen_event:notify(RoomPid, {user_joined, {User, Now}}),
    gen_event:add_sup_handler(RoomPid, HandlerId, []),
    {ok, HandlerId}.

leave(RoomPid, Handler, User) ->
    gen_event:delete_handler(RoomPid, Handler, []),
    Now = calendar:universal_time(),
    gen_event:notify(RoomPid, {user_left, {User, Now}}).

send(RoomPid, User, Message, Timestamp) ->
    gen_event:notify(RoomPid, {message_from, {User, Message, Timestamp}}).

