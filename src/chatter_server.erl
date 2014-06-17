-module(chatter_server).

-behavior(gen_server).

-export([
         connect/1,
         join/2]).

-export([
         init/1,
         start_link/0,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).

init(_Args) ->
    chatter_store:init(),
    {ok, {}}.

connect(ChatHandle) ->
    gen_server:call(?SERVER, {connect, ChatHandle}).

join(User, RoomName) ->
    gen_server:call(?SERVER, {join, User, RoomName}).



handle_call({connect, ChatHandle}, _From, State) ->
    Reply = chatter_client:start(ChatHandle),
    {reply, Reply, State};
handle_call({join, User, RoomName}, _From, State) ->
    {ok, ChatRoom} = fetch_room(RoomName),
    {ok, HandlerId} = chatter_room:join(ChatRoom, User),
    {reply, {ok, ChatRoom, HandlerId}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

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
