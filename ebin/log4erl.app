%% This is the application resource file (.app file) for the 'base'
%% application.
{application, log4erl,
[{description, "Logger for erlang in the spirit of Log4J"},
 {vsn, "0.9.0"},
 {modules, [console_appender,
	dummy_appender,
	email_msg,
	error_logger_log4erl_h,
	file_appender,
	log4erl_conf,
	log4erl,
	log4erl_lex,
	log4erl_parser,
	log4erl_sup,
	log4erl_utils,
	log_filter_codegen,
	log_filter,
	log_formatter,
	logger_guard,
	log_manager,
	mochinum,
	smtp_appender,
	sendmail_appender,
	smtp_fsm,
	syslog_appender,
	xml_appender]},
 {registered,[log4erl]},
 {applications, [kernel,stdlib]},
 {mod, {log4erl,[]}},
 {start_phases, []}
]}.


