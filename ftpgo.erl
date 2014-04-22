-module(ftpgo).

-compile([export_all]).

-define(MIN_LARGE_FILE_SIZE, (100 * 1024 * 1024)).
-define(UPLOAD_CHUNK_COUNT, (1024 * 120)).

server(local)           -> [{server, "127.0.0.1"}, {account, {"lte", "lte"}}, {dir, {"/down", "/upload"}}, {jobs, 2}];
server(X)               -> throw({unkown, X}).

-record(config, 
    {mode,
     server,
     port,
     dir,
     account,
     jobs,
     fn}).

gen_opt(ServerConfig, Direction) ->
    {Dd, Du} = case proplists:get_value(dir, ServerConfig, {"/", "/"}) of
        {X, Y} -> {X, Y};
        _ -> {"/", "/"}
    end,
    #config{
        mode = Direction,
        server = get_server(ServerConfig),
        port = get_port(ServerConfig),
        dir = case Direction of dl -> Dd; _ -> Du end,
        account = get_account(ServerConfig),
        jobs = get_jobs(ServerConfig),
        fn = get_fn(ServerConfig)}.

dl(Server, Jobs) when is_atom(Server) ->
    dl(gen_opt([{jobs, Jobs} | server(Server)], dl)).

dl(Server) when is_atom(Server) ->
    dl(gen_opt(server(Server), dl));
dl(Config) when erlang:is_record(Config, config) ->
    Fn = case Config#config.fn of
            "" -> 
                io:format("Inspecting FTP server for a large file...", []),
                find_file(Config);
            X -> X
        end,
    io:format("Large file found: ~ts~n~n", [Fn]),
    supervisor:start_link({local, ftpgo_download}, ?MODULE, Config#config{fn = Fn}).

ul(Server) when is_atom(Server) ->
    ul(gen_opt(server(Server), ul));
ul(Config) when erlang:is_record(Config, config) ->
    supervisor:start_link({local, ftpgo_upload}, ?MODULE, Config).

ul(Server, Jobs) when is_atom(Server) ->
    ul(gen_opt([{jobs, Jobs} | server(Server)], ul)).

stop() ->
    stop(dl),
    stop(ul).

stop(dl) ->
    exit(whereis(ftpgo_download), shutdown);
stop(ul) ->
    exit(whereis(ftpgo_upload), shutdown).

stop0(A) ->
    case get(A) of
        L when is_list(L) ->
            [P ! kill || P <- L];
        _ -> ok
    end,
    erase(A).

get_server(Opts) ->
    proplists:get_value(server, Opts, {127,0,0,1}).

get_port(Opts) ->
    proplists:get_value(port, Opts, 21).

get_account(Opts) ->
    proplists:get_value(account, Opts, {"anonymous", ""}).

get_fn(Opts) ->
    proplists:get_value(fn, Opts, "").

get_dir(Opts) ->
    proplists:get_value(dir, Opts, "/").

get_jobs(Opts) ->
    proplists:get_value(jobs, Opts, 20).

download(Config) ->
    Pid0 = spawn_link(fun () ->
        {ok, Pid} = login(Config),
        Fn = Config#config.fn,
        ok = ftp:cd(Pid, filename:dirname(Fn)),
        ok = ftp:type(Pid, binary),
        download(Pid, filename:basename(Fn))
    end),
    {ok, Pid0}.

download(Pid, Fn) ->
    case ftp:recv_chunk_start(Pid, Fn) of
        ok -> ok = recv(Pid)
    end,
    download(Pid, Fn).

recv(Pid) ->
    case ftp:recv_chunk(Pid) of
        {ok, _} -> recv(Pid);
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

upload(Config) ->
    Pid = spawn_link(fun () ->  
                {ok, Pid} = login(Config),
                ok = ftp:cd(Pid, Config#config.dir),
                ok = ftp:type(Pid, binary),
                upload0(Pid, Config#config.fn)
        end),
    {ok, Pid}.

-define(DATA_FOO, <<
        <<0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0>>/binary,
        <<0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0>>/binary,
        <<0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0>>/binary,
        <<0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0>>/binary,
        <<0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0>>/binary,
        <<0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>/binary,
        <<0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>/binary,
        <<0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>/binary>>).

upload0(Pid, Fn) ->
    Data = <<?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary,
             ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary,
             ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary,
             ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary,
             ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary,
             ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary,
             ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary,
             ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary, ?DATA_FOO/binary>>,
    ftp:delete(Pid, Fn),
    case ftp:send_chunk_start(Pid, Fn) of
        ok -> ok = send(Pid, Data, ?UPLOAD_CHUNK_COUNT)
    end,
    upload0(Pid, Fn).

send(Pid, _Chunk, 0) ->
    ftp:send_chunk_end(Pid);
send(Pid, Chunk, N) ->
    case ftp:send_chunk(Pid, Chunk) of
        ok -> send(Pid, Chunk, N - 1);
        {error, Reason} ->
           ftp:send_chunk_end(Pid),
           {error, Reason}
    end.

login(Config) ->
    case ftp:open(Config#config.server, [{timeout, 5000}, {port, Config#config.port}]) of
        {ok, Pid} ->
            {N, P} = Config#config.account,
            ok = ftp:user(Pid, N, P),
            {ok, Pid};
        {error, ehost} ->
            receive
            after 500 ->
                login(Config)
            end
    end.

find_file(Config) ->
    {ok, Pid} = login(Config),
    case (catch find_file(Pid, Config#config.dir)) of
        {ok, {F, _S}} ->
            ftp:close(Pid),
            F;
        _ ->
            ftp:close(Pid),
            error_logger:error_msg("cann't find a proper file~n"),
            throw(error)
    end.

find_file(Pid, Path) ->
    io:format("searching in path: ~p~n", [Path]),
    {SubDirs, Files} = ls(Pid, Path),
    case lists:sort(fun ({_N1, S1}, {_N2, S2}) -> S1 >= S2 end, Files) of
        [{F, S} | _] when S >= ?MIN_LARGE_FILE_SIZE ->
            throw({ok, {filename:join([Path, F]), S}});
        _ -> 
            [find_file(Pid, filename:join([Path, Dir])) || Dir <- SubDirs]
    end.

ls(Pid, Path) ->
    {ok, L} = ftp:ls(Pid, Path),
    Lst = string:tokens(L, "\r\n"),
    %io:format("ls: ~p~n", [Lst]),
    lists:foldl(fun ({d, Name}, {SubDirs, Files}) ->
                {[Name | SubDirs], Files};
                    ({f, Size, Name}, {SubDirs, Files}) ->
                {SubDirs, [{Name, Size} | Files]};
            (_, Acc) -> Acc
        end, {[], []}, [(catch parse_line(X)) || X <- Lst]).

parse_line_unix(L) ->
    {[FFlag | _Attr], T0} = first_word(L),
    {Type, T1} = first_word(T0),
    {_Uid, T2} = first_word(T1),
    {_Gid, T3} = first_word(T2),
    {Size, T4} = first_word(T3),
    {_Month, T5} = first_word(T4),
    {_Day, T6} = first_word(T5),
    {_TimeYear, T7} = first_word(T6),
    Name = string:strip(T7, left, $ ),
    case Type of
        "4" -> {d, Name};
        "1" -> 
            case {FFlag, list_to_integer(Size)} of
                {$d, 0} -> {d, Name};
                {_, NSize} when Size > 0 -> {f, NSize, Name};
                _ -> {f, 0, Name}
            end
    end.

% 06-12-12  02:08PM       <DIR>          folder
% 06-27-11  02:56PM             70057984 sam_09_07
parse_line_dos1(L) ->
    {_Date, T0} = first_word(L),
    {Time, T1} = first_word(T0),
    {Size, T2} = first_word(T1),
    Name = string:strip(T2, left, $ ),
    case lists:reverse(Time) of
        [$M, $P | _] -> ok;
        [$M, $A | _] -> ok
    end,
    case Size of
        "<DIR>" -> {d, Name};
        _ -> {f, list_to_integer(Size), Name}
    end.

% 04/09/2012  05:21 PM               438 SciTE.session
% 09/18/2012  10:27 AM    <DIR>          Searches
parse_line_dos2(L) ->
    {_Date, T0} = first_word(L),
    {_Time, T1} = first_word(T0),
    T2 = case first_word(T1) of
        {"AM", XX} -> XX;
        {"PM", XX} -> XX
    end,
    {Size, T3} = first_word(T2),
    Name = string:strip(T3, left, $ ),
    case Size of
        "<DIR>" -> {d, Name};
        _ -> {f, list_to_integer(Size), Name}
    end.

parse_line(L) ->
    %io:format("~ts~n", [L]),
    Fs = [fun parse_line_unix/1, fun parse_line_dos1/1, fun parse_line_dos2/1],
    lists:foldl(fun (F, {'EXIT', _}) -> catch F(L);
                    (_F, R) -> R end,
        {'EXIT', 0}, Fs).

first_word([$  | T]) ->
    first_word(T);
first_word([C | T]) ->
    first_word(T, [C]).

first_word([$  | T], Acc) ->
    {lists:reverse(Acc), T};
first_word([X | T], Acc) -> first_word(T, [X | Acc]).

% supervisor callback
init(#config{mode = dl} = Config) ->
    ChildSpec = [{I, {?MODULE, download, [Config]}, 
            permanent, brutal_kill, worker, [?MODULE, ftp]} || I <- lists:seq(1, Config#config.jobs)],
    {ok,{{one_for_one, 50, 100}, ChildSpec}};
init(#config{mode = ul} = Config) ->
    ChildSpec = [{I, {
                        ?MODULE, 
                        upload, 
                        [Config#config{fn = lists:flatten(io_lib:format("UL~2.10.0B", [I]))}]
                     }, 
                  permanent, brutal_kill, worker, [?MODULE, ftp]} || I <- lists:seq(1, Config#config.jobs)],
    {ok,{{one_for_one, 50, 100}, ChildSpec}}.

