-module(chatter).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         list_chats/0,
         create_chat/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call({list_chats}, _From, State) ->
    Chats = State,
    {reply, Chats, State};
handle_call({create_chat, ChatName}, _From, State) ->
    Chats = [ChatName] ++ State,
    {reply, ok, Chats};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Public API Function Definitions
%% ------------------------------------------------------------------

list_chats() ->
    gen_server:call(?SERVER, {list_chats}).

create_chat(ChatName) ->
    gen_server:call(?SERVER, {create_chat, ChatName}).


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

