%%% File    : vftpd.erl
%%% Author  : Zhengji Li <foldl@outlook.com>
%%% Purpose : "Virutal" FTP SERVER (RFC 765) (must update to 959)

%%% Based on ftpd.erl developed by <tony@RIOJA>

-module(vftpd).
-author('zhengji.li@gmail.com').

-define(LARGE_FILE_SIZE, (3 * 1024 * 1024 * 1024)).

-compile(export_all).
-export([start/1, start/0]).
-export([init/3, control/2]).

-import(lists, [reverse/1, map/2, append/1, foreach/2]).

-include_lib("kernel/include/file.hrl").

-define(FTPD_PORT, 21).

%% ftpd state record
-record(state,
	{
	  ftp_port = ?FTPD_PORT,            %% port that ftpd listens on
	  tcp_opts = [{active, false}, {nodelay, true}] %% gen_tcp options
	 }).

%% connection state record
%%
%% ust is the user state
%% invalid   - user is undefined
%% ident     - user is identified
%% valid
%%
-record(cstate,
	{
	  ust = invalid,              %% internal state
	  user = "",
	  password = "",
	  account = "",
	  wd = "",                     %% current working directory
	  structure = file,            %% file(F), record(R), page(P)
	  mode = stream,               %% stream(S), block(B), compressed(C)
	  type = {ascii,nonprint,8},   %% ascii(A), 
	  def_data_port,               %% default data port
	  data_port = undefined,       %% set by port 
	  listen = undefined           %% listen socket for pasv
	 }).

-define(CRNL, "\r\n").
-define(R_OKAY, 200).

start() ->
    start([]).

start(Opts) ->
    Tag = make_ref(),
    _Pid = spawn_link(?MODULE, init, [self(), Tag, Opts]),
    receive
	    {Tag, {ok, {Address, Port}}} -> 
            io:format("~p started on ~p, port ~p~n", [?MODULE, Address, Port]),
            ok
    end.

stop() ->
    call(stop).

call(Req) ->
    call(?MODULE, Req).
call(Srv, Req) ->
    Tag = make_ref(),
    Srv ! {call, self(), Tag, Req},
    receive
	{Tag, Reply} ->
	    Reply
    end.

reply(Pid, Tag, Reply) ->
    Pid ! {Tag,Reply}.

init(Pid, Tag, Opts) ->
    case catch register(?MODULE, self()) of
	true ->
	    case options(Opts, #state{}) of
		{ok, St} ->
		    case gen_tcp:listen(St#state.ftp_port, St#state.tcp_opts) of
                {ok, Listen} ->
                    {ok, {Address, Port}} = inet:sockname(Listen),
                    process_flag(trap_exit, true),
                    reply(Pid, Tag, {ok, {Address, Port}}),
                    server(Listen, St, 
                       spawn_link(?MODULE, control,
                              [self(), Listen]));
                Error -> reply(Pid, Tag, Error)
		    end; 
		Error -> reply(Pid,Tag,Error)
	    end;
	{'EXIT', _} ->
	    reply(Pid, Tag, {error, already_started});
	Error ->
	    reply(Pid, Tag, Error)
    end.

is_ip(X) when is_tuple(X), size(X) == 4 ->
    lists:all(fun (E) -> (E >= 0) and (E =< 255) end, tuple_to_list(X));
is_ip(X) when is_tuple(X), size(X) == 8 ->
    lists:all(fun (E) -> (E >= 0) and (E =< 65535) end, tuple_to_list(X));
is_ip(_) -> false.

-define(is_ip4(X), size(X)==4, 
		   (element(1,X) bor element(2,X) bor 
		    element(3,X) bor element(4,X)) band (bnot 255) == 0).

-define(is_ip6(X), size(X)==4, 
		   (element(1,X) bor element(2,X) bor 
		    element(3,X) bor element(4,X)) band (bnot 255) == 0).

%%
%% Valid options are:
%%  {port,P}            -- the ftpd listen port other than ?DEFAULT_FTPD_PORT
%%  {ip,Addr}           -- ip address to bind to {0,0,0,0} is the default
%%
options([Opt | Opts], St) ->
    case Opt of
        {port, P} when P > 0, P < 65536 ->
            options(Opts, St#state{ftp_port = P});
        {ip, IP} when is_tuple(IP) ->
            case is_ip(IP) of
                true -> options(Opts, St#state{tcp_opts = [{ip, IP} | St#state.tcp_opts]});
                _ -> {error, {bad_option, Opt}}
            end;
        {ip, IP} when is_list(IP) ->
            case inet_parse:address(IP) of
                {ok, Addr} -> options(Opts, St#state{tcp_opts = [{ip, Addr} | St#state.tcp_opts]});
                _ -> {error, {bad_option, Opt}}
            end;
        _ ->
            {error, {bad_option, Opt}}
    end;
options([], St) ->
    {ok, St}.

%%
%% Server loop
%%
server(Listen, St, Accept) ->
    receive
        {call, From, Tag, Request} ->
            case handle_call(Request, St) of
                {reply, Reply, St1} ->
                    reply(From, Tag, Reply),
                    server(Listen, St1, Accept);
                {noreply, St1} ->
                    reply(From, Tag, noreply),
                    server(Listen, St1, Accept);
                {stop, _St1} ->
                    reply(From, Tag, stop)
            end;
        {accepted, Accept} ->
            unlink(Accept),
            server(Listen, St, spawn_link(?MODULE, control, [self(), Listen]));
        {'EXIT', Accept, _Reason} ->
            server(Listen, St, spawn_link(?MODULE, control, [self(), Listen]));
        Other ->
            io:format("vftpd: got ~p~n", [Other]),
	        server(Listen, St, Accept)
    end.

handle_call(stop, St) ->
    {stop, St};
handle_call(Req, St) ->
    {reply, {bad_request, Req}, St}.

%%
%% Control channel setup
%%
control(Srv, Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, S} ->
            Srv ! {accepted, self()},
            control_init(Srv, S);
        _Error ->
            exit(bad_accept)
    end.

%% 
%% Control channel init
%%
control_init(_Srv, Ctl) ->
    case inet:peername(Ctl) of
	{ok, {Addr, Port}} ->
	    ctl_loop_init(Ctl, {Addr, Port - 1});
	{error,Err} ->
	    io:format("vftpd: error in inet:peername ~p~n",[Err]),
	    gen_tcp:close(Ctl)
    end.

ctl_loop_init(Ctl, DefaultDataPort) ->
    {ok, Name} = inet:gethostname(),
    rmsend(Ctl, 220, 
        Name ++ " Erlang Virtual FTP server 1.0 ready.",
	   ["The following commands are recognized (* =>'s unimplemented).",
        "  USER    PORT    STOR    MSAM*   RNTO    NLST    MKD     CDUP",
	    "  PASS    PASV    APPE    MRSQ*   ABOR    SITE    XMKD    XCUP",
	    "  ACCT*   TYPE    MLFL*   MRCP*   DELE    SYST    RMD     STOU",
	    "  SMNT*   STRU    MAIL*   ALLO    CWD     STAT    XRMD    SIZE",
	    "  REIN*   MODE    MSND*   REST    XCWD    HELP    PWD     MDTM",
	    "  QUIT    RETR    MSOM*   RNFR    LIST    NOOP    XPWD",
        "Every operation is *virtual*, feel FREE to do anything."],
	   "Direct comments to zhengji.li@gmail.com."),
    ctl_loop(Ctl, #cstate{data_port = DefaultDataPort,
			              def_data_port = DefaultDataPort}, []).

ctl_loop(Ctl, St, Buf) ->
    case ctl_line(Ctl, Buf) of
        {ok, Line, Buf1} ->
            case ctl_parse(Line) of
                {Fun, Args} ->
                    case catch Fun(Args, Ctl, St) of
                        failed -> ctl_loop(Ctl, St, Buf1);
                        quit -> true;
                        init -> ctl_loop_init(Ctl, St#cstate.def_data_port);
                        St1 when is_record(St1, cstate) ->
                            ctl_loop(Ctl, St1, Buf1);
                        _ -> %% Crash etc
                            rsend(Ctl, 501, "argument error: " ++ Line),
                            ctl_loop(Ctl, St, Buf1)
                    end;
                error ->
                    rsend(Ctl, 500, "syntax error: " ++ Line),
                    ctl_loop(Ctl, St, Buf1)
            end;
        {error,closed} ->
            true
    end.

%% parse a command and arguments
%% must be case insensitive on commands and type letters but
%% sensitive on path/user 
%% 
ctl_parse([L1, L2, L3 | T]) ->
    C1 = alpha(L1),
    C2 = alpha(L2),
    C3 = alpha(L3),
    case T of
        [] ->
            ctl_parse(list_to_atom([C1, C2, C3]), []);
        [$  | Arg] ->
            ctl_parse(list_to_atom([C1, C2, C3]), Arg);
        [C4] ->
            ctl_parse(list_to_atom([C1, C2, C3, alpha(C4)]), []);
        [C4, $  | Arg] ->
            ctl_parse(list_to_atom([C1, C2, C3, alpha(C4)]), Arg);
        _ -> error
    end;
ctl_parse(_) -> error.

ctl_parse(user, Arg) -> {fun user/3, Arg};
ctl_parse(pass, Arg) -> {fun pass/3, Arg};
ctl_parse(acct, Arg) -> {fun cni/3, Arg};
ctl_parse(cwd, Arg) -> {fun cwd/3, Arg};
ctl_parse(cdup, Arg) -> {fun cdup/3, Arg};
ctl_parse(smnt, Arg) -> {fun cni/3, Arg};
ctl_parse(quit, Arg) -> {fun quit/3, Arg};
ctl_parse(rein, Arg) -> {fun rein/3, Arg};
ctl_parse(port, Arg) -> {fun port/3, Arg};
ctl_parse(pasv, Arg) -> {fun pasv/3, Arg};
ctl_parse(type, Arg) -> {fun type/3, Arg};
ctl_parse(stru, Arg) -> {fun stru/3, Arg};
ctl_parse(mode, Arg) -> {fun mode/3, Arg};
ctl_parse(retr, Arg) -> {fun retr/3, Arg};
ctl_parse(stor, Arg) -> {fun stor/3, Arg};
ctl_parse(stou, Arg) -> {fun cni/3, Arg};
ctl_parse(appe, Arg) -> {fun cni/3, Arg};
ctl_parse(allo, Arg) -> {fun cni/3, Arg};
ctl_parse(rest, Arg) -> {fun cni/3, Arg};
ctl_parse(rnfr, Arg) -> {fun cni/3, Arg};
ctl_parse(rnto, Arg) -> {fun cni/3, Arg};
ctl_parse(abor, Arg) -> {fun cni/3, Arg};
ctl_parse(dele, Arg) -> {fun dele/3, Arg};
ctl_parse(rmd, Arg)  -> {fun rmd/3, Arg};
ctl_parse(xrmd, Arg) -> {fun rmd/3, Arg};
ctl_parse(pwd, Arg)  -> {fun pwd/3, Arg};
ctl_parse(xpwd, Arg) -> {fun pwd/3, Arg};
ctl_parse(mkd, Arg)  -> {fun mkd/3, Arg};
ctl_parse(xmkd, Arg) -> {fun mkd/3, Arg};
ctl_parse(list, Arg) -> {fun lst/3, {list, Arg}};
ctl_parse(nlst, Arg) -> {fun lst/3, {nlst, Arg}};
ctl_parse(site, Arg) -> {fun cni/3, Arg};
ctl_parse(syst, Arg) -> {fun syst/3, Arg};
ctl_parse(stat, Arg) -> {fun cni/3, Arg};
ctl_parse(help, Arg) -> {fun help/3, Arg};
ctl_parse(noop, Arg) -> {fun noop/3, Arg};
ctl_parse(Cmd, Arg) ->  {fun cbad/3, {Cmd, Arg}}.

%% Commands
%% Reply wiht {ok, NewState}
%% or {error, Code}
%%
user(Name, S, St) ->
    rsend(S, 331),
    St#cstate { ust = ident, user = Name, wd = "/" }.

pass(Password, S, St) ->
    assert_ident(S, St),
    rsend(S, 230, "User " ++ St#cstate.user ++ " logged in, proceed"),
    St#cstate { password = Password, ust = valid }.

%% Change working directory we must keep an absoulte path (emulated
%% so that symbolic links are transparent).
cwd(Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    Dir = rel_name(Arg, St#cstate.wd),
    rsend(Ctl, 250, "new directory \"" ++ abs_name(Dir) ++ "\""),
    St#cstate { wd = Dir }.

mkd(Arg, S, St) ->
    assert_valid(S, St),
    DirR = rel_name(Arg, St#cstate.wd),
    DirA = abs_name(DirR),
    rsend(S, 257, " \"" ++ DirA ++ "\" directory created"),
    St.

dele(Arg, S, St) ->
    assert_valid(S, St),
    FileR = rel_name(Arg, St#cstate.wd),
    FileA = abs_name(FileR),
    rsend(S, 250, "\"" ++ FileA ++ "\" deleted"),
    St. 

rmd(Arg, S, St) ->
    assert_valid(S, St),
    DirR = rel_name(Arg, St#cstate.wd),
    DirA = abs_name(DirR),
    rsend(S, 250, " \"" ++ DirA ++ "\" removed"),
    St.    
    
%% Change to parent directory
cdup(_Arg, S, St) ->
    assert_valid(S, St),
    DirR = rel_name("..", St#cstate.wd),
    DirA = abs_name(DirR),
    rsend(S, 250, "directory changed to \"" ++ DirA ++ "\""),
    St#cstate{wd = DirR}.

pwd(_Arg, S, St) ->
    assert_valid(S, St),
    rsend(S, 257, "\"" ++ abs_name(St#cstate.wd) ++ "\""),
    St.

quit(_, S, _St) ->
    rsend(S, 221),
    gen_tcp:close(S),
    quit.

noop(_, S, St) ->
    rsend(S, 200),
    St.

mode(Arg, S, St) ->
    assert_valid(S, St),
    Mode = case alpha(hd(Arg)) of
	       $s -> stream;
	       $b -> block;
	       $c -> compressed
	   end,
    rsend(S, 200, "new mode " ++ atom_to_list(Mode)),
    St#cstate{mode = Mode}.

stru(Arg, S, St) ->    
    assert_valid(S, St),
    Stru = case alpha(hd(Arg)) of
	       $f -> file;
	       $r -> record;
	       $p -> page;
           _ -> rsend(S, 504), throw(St)
	   end,
    rsend(S, 200, "new file structure " ++ atom_to_list(Stru)),
    St#cstate{structure = Stru}.    

syst(_Arg, S, St) ->
    rsend(S,200,"unix"),
    St.

type(Arg, S, St) ->
    assert_valid(S, St),
    Type = case alpha(hd(Arg)) of
	       $i -> {image, nonprint, 8};
	       $a -> {ascii, nonprint, 8};
	       _ -> rsend(S, 504), throw(St)
	   end,
    rsend(S,200,"new type " ++ atom_to_list(element(1, Type))),
    St#cstate{type = Type}.
		    
rein(_, _S, St) ->
    close_listen(St),
    init.

pasv(_Arg, S, St) ->
    assert_valid(S, St),
    St1 = close_listen(St),
    {ok, {Addr, _}} = inet:sockname(S),
    case gen_tcp:listen(0, [{active, false}, binary, {ip, Addr}]) of
        {ok, L} ->
            {ok, {_, Port}} = inet:sockname(L),
            rsend(S, 227, "Entering Passive Mode (" ++ format_address(Addr, Port) ++ ")."),
            St1#cstate {listen = L};
        {error,Err} ->
            rsend(S, 425, erl_posix_msg:message(Err)),
            St1
    end.

port(Arg, S, St) ->
    assert_valid(S, St),
    St1 = close_listen(St),
    {ok, AddrPort} = parse_address(Arg),
    rsend(S,200),
    St1#cstate{data_port = AddrPort, listen = undefined}.

lst({T, _Arg}, Ctl, St) ->
    assert_valid(Ctl, St),
    {S, St1} = open_data(Ctl, St),
    dir_list(Ctl, S, St#cstate.wd, T),
    gen_tcp:close(S),
    St1.
    
%%
%% store file from data connection onto file given by Arg
%%
stor(_Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    {S, St1} = open_data(Ctl, St),
    case recv_file(S, 1024, 0) of
        {ok, Count} ->
            rsend(Ctl, 226, "closing data connection, recived " ++ 
              integer_to_list(Count) ++ " bytes");

        {error, Err} ->
            rsend(Ctl, 226, "closing data connection, aborted"),
            rsend(Ctl, 550, "error " ++ erl_posix_msg:message(Err))
    end,
    gen_tcp:close(S),
    St1.

-define(DATA_FOO, <<
        <<1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0>>/binary,
        <<1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1>>/binary,
        <<0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1>>/binary,
        <<0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1>>/binary,
        <<0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0>>/binary,
        <<0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0>>/binary,
        <<0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0>>/binary,
        <<0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0>>/binary>>).

build_chunk() ->
    <<?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary,
      ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary,
      ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary,
      ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary,
      ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary,
      ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary,
      ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary,
      ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary>>.

%%
%% retrive a file over a data connection file name is given by Arg
%%
retr(_Arg, Ctl, St) ->
    assert_valid(Ctl, St),
    {S,St1} = open_data(Ctl, St),
    Chunk = build_chunk(),
    case send_file(Chunk, ?LARGE_FILE_SIZE div size(Chunk), S) of
        {ok, _} ->
            rsend(Ctl, 226, "closing data connection, sent " ++ integer_to_list(?LARGE_FILE_SIZE) ++ " bytes");
        {error, Err} ->
            rsend(Ctl, 226, "closing data connection, aborted"),
            rsend(Ctl, 550, "error " ++ erl_posix_msg:message(Err))
    end,
    gen_tcp:close(S),
    St1.

%% command not implemented
cni(_, S, St) ->
    rsend(S, 502),    
    St.

%% help
help(_, S, St) ->
    rmsend(S, 214,
	   "The following commands are recognized (* =>'s unimplemented).",
	   ["  USER    PORT    STOR    MSAM*   RNTO    NLST    MKD     CDUP",
	    "  PASS    PASV    APPE*   MRSQ*   ABOR    SITE    XMKD    XCUP",
	    "  ACCT*   TYPE    MLFL*   MRCP*   DELE    SYST    RMD     STOU",
	    "  SMNT*   STRU    MAIL*   ALLO    CWD     STAT    XRMD    SIZE",
	    "  REIN    MODE    MSND*   REST    XCWD    HELP    PWD     MDTM",
	    "  QUIT    RETR    MSOM*   RNFR    LIST    NOOP    XPWD",
        "Every operation is *virtual*, feel FREE to do anything." ],
	   "Direct comments to zhengji.li@gmail.com."),
    St.

%% bad/unkown command
cbad({Cmd, Arg}, S, St) ->
    rsend(S, 500, "command not understood " ++ atom_to_list(Cmd) ++ " " ++ Arg),
    St.

 %% Send a single line standard message
rsend(S, Code) ->
    gen_tcp:send(S, [rstr(Code) ++ ?CRNL]).

%% Send a single line reply with CRNL
rsend(S, Code, Msg) when is_integer(Code) ->
    gen_tcp:send(S, [integer_to_list(Code), " ", Msg, ?CRNL]).

%% send a multi line reply
rmsend(S, Code, Msg1, Lines, Msg2) ->
    gen_tcp:send(S, [integer_to_list(Code),"-", Msg1, ?CRNL,
		     [[" ", M, ?CRNL] || M <- Lines],
		     integer_to_list(Code)," ", Msg2, ?CRNL]).

%% check that a user has logged in and report errors
assert_valid(S, St) ->
    case St#cstate.ust of
	invalid -> rsend(S, 530), throw(failed);
	ident ->  rsend(S, 331), throw(failed);
	valid -> true
    end.

assert_ident(S, St) ->
    case St#cstate.ust of
	invalid -> rsend(S, 530), throw(failed);
	ident ->   true;
	valid ->   rsend(S, 503), throw(failed)
    end.
    
%% return lower letter space or ?		 
alpha(X) when X >= $A, X =< $Z -> (X-$A)+$a;
alpha(X) when X >= $a, X =< $z -> X;
alpha(X) when X == $  -> X;
alpha(_X) -> $?.

ctl_line(S, Buf) ->
    case split_line(Buf) of
        more ->
            case gen_tcp:recv(S, 0) of
                {ok, Cs} -> ctl_line(S, Buf ++ Cs);
                Error -> Error
            end;
        Done -> Done
    end.

%% split a line after CRLF
split_line(Cs) ->
    split_line(Cs, []).

split_line([$\r, $\n|Cs], Buf) ->
    {ok, reverse(Buf), Cs};
split_line([X | Cs], Buf) ->
    split_line(Cs, [X | Buf]);
split_line([], _) ->
    more.

%% Standard reply strings and theier meaning
%%
%%
rstr(110) -> "110 MARK yyyy = mmmm";             %% ARGS
rstr(120) -> "120 Service ready in nnn minutes.";  %% ARG
rstr(125) -> "125 Data connection alredy open; transfere starting.";
rstr(150) -> "150 File status okay; about to open data connection.";
rstr(200) -> "200 Command okay.";
rstr(202) -> "202 Command not implemented, superfluos at this site.";
rstr(211) -> "211 System status, or system help reply.";
rstr(212) -> "212 Directory status.";
rstr(213) -> "213 File status.";
rstr(214) -> "214 Help message.";     %% ADD HELP
rstr(215) -> "215 NAME system type";  %% set NAME
rstr(220) -> "220 Service ready for user.";
rstr(221) -> "221 Service closing control connection.";
rstr(225) -> "225 Data connection open; no transfere in progress";    
rstr(226) -> "226 Closing data connection.";  %% ADD INFO
rstr(227) -> "227 Entering Passive Mode (h1,h2,h3,h4,p1,p2).";  %% ARGS
rstr(230) -> "230 User logged in, proceed.";
rstr(250) -> "250 Requested file action okay, completed.";
rstr(257) -> "257 PATHNAME created.";  %% ARG
rstr(331) -> "331 User name okay, need password.";
rstr(332) -> "332 Need account for login.";
rstr(350) -> "350 Requested file action pending further information.";
rstr(421) -> "421 Service not available, closing control connection.";
rstr(425) -> "425 Can't open data connection.";
rstr(426) -> "426 Connection closed; transfere aborted.";
rstr(450) -> "450 Requested file action not taken.";
rstr(451) -> "451 Requested action not taken: local error in processing.";
rstr(452) -> "452 Requested action not taken.";
rstr(500) -> "500 Syntax error, command unrecognized.";  %% ADD INFO
rstr(501) -> "501 Syntax error in paramters or arguments.";
rstr(502) -> "502 Command not implemented.";
rstr(503) -> "503 Bad sequence of commands.";
rstr(504) -> "504 Command not implemented for that parameter.";
rstr(530) -> "530 Not logged in.";
rstr(532) -> "532 Need account for storing files.";
rstr(550) -> "550 Requested action not taken.";
rstr(551) -> "551 Requested action aborted: page type unkown.";
rstr(552) -> "552 Requested file action aborted.";
rstr(553) -> "553 Requested action not taken.".

%%
%% Open data connection
%%
open_data(Ctl, St) ->
    rsend(Ctl, 150),
    if St#cstate.listen =/= undefined ->
           case gen_tcp:accept(St#cstate.listen) of
               {ok, S} ->
                   gen_tcp:close(St#cstate.listen),
                   {S, St#cstate{listen = undefined}};
               {error,Err} ->
                   open_data_err(Ctl, Err)
           end;
       true ->
            {Addr, Port} = St#cstate.data_port,
            case gen_tcp:connect(Addr, Port, [{active, false}, binary]) of
            {ok, S} ->
                {S, St};
            {error, Err} ->
                open_data_err(Ctl, Err)
            end
    end.

open_data_err(Ctl,Err) ->
    rsend(Ctl, 421, "Can't open data connection " ++ inet:format_error(Err)),
    throw(failed).

close_listen(#cstate{listen = Listen} = St) when Listen =/= undefined ->
	gen_tcp:close(Listen),
	St#cstate{listen = undefined};
close_listen(St) -> St.

%% Send file data over a socket
send_file(_Chunk, 0, _S) -> {ok, 0};
send_file(Chunk, Count, S) ->
    case gen_tcp:send(S, Chunk) of
        ok -> send_file(Chunk, Count - 1, S);
        Error -> Error
    end.

%% Receive file data over a socket
recv_file(S, Chunk, Count) ->
    case gen_tcp:recv(S, 0) of
        {error, closed} -> 
            {ok, Count};
        {ok, Data} ->
            recv_file(S, Chunk, Count + size(Data));
        Error -> Error
    end.    

%% file mode is binary or text
file_mode(St) ->
    case St#cstate.type of
	{ascii,_,_} -> [];
	{image,_,_} -> [binary]
    end.
	    
%% parse address on form:
%% d1,d2,d3,d4,p1,p2   => {{d1,d2,d3,d4}, port} -- ipv4
%% h1,h2,...,h32,p1,p2 => {{n1,n2,..,n8}, port} -- ipv6
%%
parse_address(Str) ->
    paddr(Str, 0, []).

paddr([X | Xs], N, Acc) when X >= $0, X =< $9 -> paddr(Xs, N * 10 + (X - $0), Acc);
paddr([X | Xs], N, Acc) when X >= $A, X =< $F -> paddr(Xs, N * 10 + (X - $A) + 10, Acc);
paddr([X | Xs], N, Acc) when X >= $a, X =< $f -> paddr(Xs, N * 10 + (X - $a) + 10, Acc);
paddr([$, | Xs], N, Acc) -> paddr(Xs, 0, [N | Acc]);
paddr([], P2, [P1, D4, D3, D2, D1]) -> {ok, {{D1, D2, D3, D4}, P1*256 + P2}};
paddr([], P2, [P1 | As]) when length(As) == 32 ->
    case addr6(As, []) of
	    {ok, Addr} -> {ok, {Addr, P1 * 256 + P2}};
	    error -> error
    end;
paddr(_, _, _) -> error.

addr6([H4, H3, H2, H1|Addr],Acc) when H4 < 16, H3 < 16, H2 < 16, H1 < 16 ->
    addr6(Addr, [H4 + H3 * 16 + H2 * 256 + H1 * 4096 |Acc]);
addr6([], Acc) -> {ok, list_to_tuple(Acc)};
addr6(_, _) -> error.

format_address({A,B,C,D}, Port) ->
    integer_to_list(A) ++ "," ++ integer_to_list(B) ++ "," ++
    integer_to_list(C) ++ "," ++ integer_to_list(D) ++ "," ++
    integer_to_list(Port div 256) ++ "," ++ integer_to_list(Port rem 256);
format_address({N1,N2,N3,N4,N5,N6,N7,N8},Port) ->
    h4(N1) ++ "," ++ h4(N2) ++ "," ++ h4(N3) ++ "," ++ h4(N4) ++ "," ++
    h4(N5) ++ "," ++ h4(N6) ++ "," ++ h4(N7) ++ "," ++ h4(N8) ++ "," ++
	integer_to_list(Port div 256) ++ "," ++ integer_to_list(Port rem 256).

h4(N) ->
    [hx(N bsr 12), $,, hx(N bsr 8), $,, hx(N bsr 4), $,, hx(N)].

hx(N) ->
    N1 = N band 16#f,
    if N1 < 10 -> N1 + $0; true -> (N1 - 10) + $A end.

%%
%% Compose file/directory names
%%
rel_name(Name, Wd) ->
    case filename:pathtype(Name) of
        relative ->
            rel_path(filename:join(Wd, Name));
        absolute ->
            rel_path(Name);
        volumerelative ->
            rel_path(filename:join(Wd, Name))
    end.
%%
%% We sometime need a simulated root, then call abs_name
%%
abs_name(Name) ->
    filename:join("/", Name).

%%
%% rel_path returns a relative path i.e remove
%% and root or volume relative start components
%%
rel_path(Path) ->
    rel_path(filename:split(Path), []).

rel_path([], []) -> "";

%% remove absolute or volume relative stuff
rel_path([Root | Path], RP) ->
    case filename:pathtype(Root) of
        relative -> rpath(Path, [Root | RP]);
        _ -> 
            rpath(Path, RP)
    end.

rpath([".." | P], [_ | RP]) ->  rpath(P, RP);
rpath(["." | P], RP) -> rpath(P, RP);
rpath([F | P], RP) -> rpath(P, [F | RP]);
rpath([], []) -> "";
rpath([], RP) -> filename:join(reverse(RP)).

%%
%% Generate a directory listing
%% should normally go to the socket
%%
dir_list(Ctl, S, Dir, Type) ->
    L = [integer_to_list(N) ++ ".dat" || N <- lists:seq(10, 99)],
    List = case length(Dir) =< 1 of 
        true ->
            ["upload", "download" | L];
        _ -> L
    end,
    foreach(fun(E) when Type == nlst -> gen_tcp:send(S, E ++ ?CRNL);
               (E) when Type == list -> gen_tcp:send(S, list_info(E) ++ ?CRNL)
            end,
      List),
    rsend(Ctl, 226).

file_info([X | _T]) when X >= $0, X =< $9 -> 
    "-rw-rw-rw-  1     0     0 3221225472 Feb  1 12:34";
file_info(_) ->
    "drwxrwxrwx  4     0     0          0 Feb  1 12:34".

list_info(File) -> 
    file_info(File) ++ " " ++ File.

