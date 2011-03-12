-module(sendmail_appender).
-author("Seven Du <dujinfang at gmail dot com>").

-include("../include/log4erl.hrl").

-record(sendmail_appender, {level=?DEFAULT_LEVEL, location, args, msg_opts}).

-define(LOCATION, "/usr/sbin/sendmail").
-define(ARGS, "-i -t").

-behaviour(gen_event).
%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
	 handle_info/2, terminate/2, code_change/3]).

-define(DEFAULT_TITLE, "log").
-define(DEFAULT_PORT, 25).

init({conf, Conf}) when is_list(Conf) ->
	Level = proplists:get_value(level, Conf),
	Location = proplists:get_value(location, Conf, ?LOCATION),
	Args = proplists:get_value(args, Conf, ?ARGS),
	MRes = msg_conf(Conf),
	init({Level, Location, Args, MRes});
%% Location = List
%% Args = List
%% MsgInfo = {From, To, Title, Msg}
%%         | {To, Title, Msg}
%%         | {To, Msg}
init({Level, Location, Args, MsgInfo}) ->
	MsgI = check_opts(get_msg_opts(MsgInfo)),
	State = #sendmail_appender{level=Level, location=Location, args=Args, msg_opts=MsgI},
	{ok, State};
init(File) when is_list(File) ->
	case file:consult(File) of
	{error, Reason} ->
		error_logger:error_msg("sendmail_appender: couldn't consult Conf file~n"),
		{error, file:format_error(Reason)};
	{ok, [Terms]} ->
		init(Terms)
	end.

handle_event({change_level, Level}, State) ->
	State2 = State#sendmail_appender{level = Level},
	?LOG2("Changed level to ~p~n",[Level]),
	{ok, State2};
handle_event({log,LLog}, State) ->
	?LOG2("handl_event:log = ~p~n",[LLog]),
	do_log(LLog, State),
	{ok, State}.

handle_call({change_format, Format}, State) ->
	?LOG2("Old State in sendmail_appender is ~p~n",[State]),
	{ok, Tokens} = log_formatter:parse(Format),
	?LOG2("Adding format of ~p~n",[Tokens]),
	S = State#sendmail_appender.msg_opts,
	S2 = S#msg_opts{msg=Tokens},
	State2 = State#sendmail_appender{msg_opts=S2},
	{ok, ok, State2};
handle_call({change_level, Level}, State) ->
	State2 = State#sendmail_appender{level = Level},
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

do_log(#log{level = L} = Log, #sendmail_appender{level=Level} = State) ->
	ToLog = log4erl_utils:to_log(L, Level),
	case ToLog of
	true ->
		MsgOpts = State#sendmail_appender.msg_opts,

		From = MsgOpts#msg_opts.from,
		To = MsgOpts#msg_opts.to,
		Title = MsgOpts#msg_opts.title,
		Msg = log_formatter:format(Log, MsgOpts#msg_opts.msg),
		M = email_msg:simp_msg(From, To, Title, Msg),
		Command = State#sendmail_appender.location ++ " " ++ State#sendmail_appender.args,

		Port = open_port({spawn, Command}, [stream, exit_status, use_stdio,
			stderr_to_stdout, in, out, eof]),
		port_command(Port, M),
		port_close(Port);
	false ->
		ok
	end.


get_msg_opts({From, To, Title, Msg}) ->
	{ok, Mtokens} = log_formatter:parse(Msg),
	#msg_opts{from=From, to=To, title=Title, msg=Mtokens};
get_msg_opts({To, Title, Msg}) ->
	{ok, Mtokens} = log_formatter:parse(Msg),
	#msg_opts{to=To, title=Title, msg=Mtokens};
get_msg_opts({To, Msg}) ->
	{ok, Mtokens} = log_formatter:parse(Msg),
	#msg_opts{to=To, msg=Mtokens};
get_msg_opts(E) ->
	{error, E}.

check_opts(Opts) ->
	case Opts of
	{error, E} ->
		?LOG2("error in getting opts with param ~p~n",[Opts]),
		throw({sendmail_appender_opts, E});
	R ->
		R
	end.

msg_conf(Conf) ->
	To = proplists:get_value(to, Conf),
	Msg = proplists:get_value(msg, Conf),

	case {To, Msg} of
		{undefined, _} ->
			throw(sendmail_to_null);
		{_, undefined} ->
			throw(sendmail_msg_null);
		{_,_} ->
			ok
	end,

	Title = proplists:get_value(title, Conf),
	From = proplists:get_value(from, Conf),

	case {Title, From} of
	{undefined, _} ->
		{To, Msg};
	{_, undefined} ->
		{To, Title, Msg};
	{_, _} ->
		{From, To, Title, Msg}
	end.
