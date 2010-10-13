-module(http_appender).

-include("../include/log4erl.hrl").

-behaviour(gen_event).
%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-define(DEFAULT_URI, "http://localhost/log").
-define(DEFAULT_TIMEOUT, 100).

init({conf, Conf}) ->
    Level = proplists:get_value(level, Conf, all),
    Uri = proplists:get_value(uri, Conf, ?DEFAULT_URI),
    Timeout = proplists:get_value(timeout, Conf, ?DEFAULT_TIMEOUT),
    Format = proplists:get_value(format, Conf, ?DEFAULT_FORMAT),
    {ok, Form} = log_formatter:parse(Format),
    State = #http_appender{
      level = Level,
      format = Form,
      uri = Uri,
      timeout = Timeout
     },
    {ok, State}.

handle_event({change_level, Level}, State) ->
    State2 = State#http_appender{level = Level},
    ?LOG2("Changed level to ~p~n",[Level]),
    {ok, State2};
handle_event({log,LLog}, State) ->
    ?LOG2("handl_event:log = ~p~n",[LLog]),
    do_log(LLog, State),
    {ok, State}.

handle_call({change_format, Format}, State) ->
    ?LOG2("Old State in console_appender is ~p~n",[State]),
    {ok, Tokens} = log_formatter:parse(Format),
    ?LOG2("Adding format of ~p~n",[Tokens]),
    S2 = State#http_appender{format=Tokens},
    {ok, ok, S2};
handle_call({change_level, Level}, State) ->
    State2 = State#http_appender{level = Level},
    ?LOG2("Changed level to ~p~n",[Level]),
    {ok, ok, State2};
handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_log(#log{level = L} = Log, #http_appender{level=Level} = State) ->
    ToLog = log4erl_utils:to_log(L, Level),
    case ToLog of
        true ->
            Uri = State#http_appender.uri,
            Timeout = State#http_appender.timeout,
            Format = State#http_appender.format,
            Msg = log_formatter:format(Log, Format),
            Who = node(),
            do_send(Uri, Timeout, {Who, L, Msg});
        false ->
            ok
    end.

do_send(Uri, Timeout, {Who, Level, Msg})
  when is_atom(Who), is_atom(Level) ->
    httpc:request(post,
                  {Uri, [], "text/plain", io_lib:format("~p: ~p", [Who, Msg])},
                  [{timeout, Timeout}], []).
