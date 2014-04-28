-module(chatter_event).

-behavior(gen_event).

-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, code_change/3, terminate/2]).

init([]) ->
    {ok, []}.

handle_event({message_from, {User, Message, Timestamp}}, State) ->
    io:format("[~p] ~p: ~p~n", [Timestamp, User, Message]),
    {ok, State};
handle_event({user_joined, {User, Timestamp}}, State) ->
    io:format("[~p] ~p joined the chat~n", [Timestamp, User]),
    {ok, State};
handle_event({user_left, {User, Timestamp}}, State) ->
    io:format("[~p] ~p left the chat~n", [Timestamp, User]),
    {ok, State}.

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
