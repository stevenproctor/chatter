-module(chatter_client).

-behavior(gen_fsm).

-export([start_link/1, join/2, leave/1, send/2]).

-export([init/1, handle_event/3,
         handle_info/3, handle_sync_event/4,
         code_change/4, terminate/3]).

-export([disconnected/2, chatting/2]).

-record(state, {handle, chat_room, event_handler}).

start_link(Handle) ->
    gen_fsm:start_link(?MODULE, [Handle], []).

init([Handle]) ->
    Self = self(),
    client_store:insert(Self, Handle),
    {ok, disconnected, #state{handle=Handle}}.

join(Pid, ChatRoomName) ->
    gen_fsm:send_event(Pid, {join, ChatRoomName}).

leave(Pid) ->
    gen_fsm:send_event(Pid, leave_chat).

send(Pid, Message) ->
    gen_fsm:send_event(Pid, {send, Message} ).

disconnected({join, ChatRoomName}, State=#state{handle=Handle}) ->
    {ok, ChatRoom, EventHandler} = chatter_server:join(Handle, ChatRoomName),
    {next_state, chatting, State#state{chat_room=ChatRoom, event_handler=EventHandler}}.

chatting(leave_chat, State=#state{handle=Handle, chat_room=ChatRoom, event_handler=EventHandler}) ->
    chatter_room:leave(ChatRoom, EventHandler, Handle),
    {next_state, disconnected, State#state{chat_room=nil}};
chatting({send, Message}, State=#state{handle=Handle, chat_room=ChatRoom}) ->
    Now = calendar:universal_time(),
    chatter_room:send(ChatRoom, Handle, Message, Now),
    {next_state, chatting, State}.

handle_event(_Event, StateName, Data) ->
    {next_state, StateName, Data}.

handle_sync_event(_Event, _From, StateName, Data) ->
    {next_state, StateName, Data}.

handle_info(_Info, StateName, Data) ->
    {next_state, StateName, Data}.


terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVersion, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
