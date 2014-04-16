-module(millionaires).

%% An implementation of Yao's solution to the [Millionaires' Problem](http://en.wikipedia.org/wiki/Yao's_Millionaires'_Problem).
%% 
%% Usage:
%% 
%% 1. Start two nodes.
%% 1. On the node that has more computing power (this is named alice): millionaires:connect(node of bob).
%% 1. Input wealth on each node by millionaires:input(). (io:get\_password() is used here)
%% 
%% Node:
%% 
%% Supported weatlth range is (1, MAX\_VALUE). Set MAX\_VALUE up to 50000, it could be solved within half an hour.
%%
%% Acknowledge: Prime generation code (c) Joe Armstrong with some modifications.

-compile([export_all]).

-define(MAX_VALUE, 1000).
-define(BITS_N,    128).

-record(m_alice,
    {
        i,
        'Ea',
        'Da',
        z,
        p,
        bob
    }).

-record(m_bob,
    {
        j,
        x,
        'Ea',
        alice
    }).

start(alice, NodeBob) -> spawn0(alice, #m_alice{bob = NodeBob});
start(bob, NodeAlice) -> spawn0(bob, #m_bob{alice = NodeAlice}).

spawn0(Who, State) ->
    show_logo(),
    spawn(
        fun() ->
                store_pid(Who, self()),
                io:format(user, "Type \"~s:input().\" to input your wealth secretly.~n", [?MODULE]),
                loop(Who, State)
        end),
    ok.

connect(NodeBob) ->
    start(alice, NodeBob),
    ok = rpc:call(NodeBob, ?MODULE, start, [bob, erlang:node()]),
    ok.

recv_msg(Who, Msg) ->
    Pid = get_pid(Who),
    true = is_pid(Pid),
    Pid ! Msg.

get_pid(Who) ->
    case (catch ets:lookup(?MODULE, Who)) of
        [{Who, Pid}] -> Pid;
        _ -> error
    end.

store_pid(Who, Pid) ->
    case lists:member(?MODULE, ets:all()) of
        false -> 
            ets:new(?MODULE, [set, named_table, public]);
        _ -> ok
    end,
    ets:insert(?MODULE, [{me, Who}, {Who, Pid}]).

wealth(Wealth) ->
    case (catch ets:lookup(?MODULE, me)) of
        [{me, Who}] -> wealth(Who, Wealth);
        _ -> error
    end.

wealth(Who, Wealth) when (1 < Wealth) and (Wealth < ?MAX_VALUE) ->
    recv_msg(Who,{wealth, Wealth});
wealth(_Who, _Wealth) -> exit(bad_wealth).

show_logo() ->
    io:format(user, "~n====================================================================", []),
    io:format(user, "~nWelcome to the Ultimate Solution of the Yao's Millionaires' Problem!", []),
    io:format(user, "~nThis version is compiled to support wealth range (1, ~p).", [?MAX_VALUE]),
    io:format(user, "~n====================================================================~n", []).

input() ->
    Who = case (catch ets:lookup(?MODULE, me)) of
        [{me, X}] -> X;
        _ -> '?'
    end,
    wealth(req_wealth(Who)),
    ok.

req_wealth(Who) ->
    io:format(user, "Please input your (~s's) wealth:", [Who]),
    S = io:get_password(),
    list_to_integer(S).

loop(alice, State) ->
    receive
        {final, R} ->
            show_result(R);
        Msg ->
            case alice(Msg, State) of
                {noreply, State10} ->
                    loop(alice, State10);
                {R, State10} ->
                    rpc:cast(State10#m_alice.bob, ?MODULE, recv_msg, [bob, R]),
                    loop(alice, State10);
                _ -> ok
            end
    end;
loop(bob, State) ->
    receive
        Msg ->
            case bob(Msg, State) of
                {noreply, State10} ->
                    loop(bob, State10);
                {{final, R}, State10} ->
                    show_result(R),
                    rpc:cast(State10#m_bob.alice, ?MODULE, recv_msg, [alice, {final, R}]);
                {R, State10} ->
                    rpc:cast(State10#m_bob.alice, ?MODULE, recv_msg, [alice, R]),
                    loop(bob, State10);
                _ -> ok
            end
    end.

alice({wealth, Wealth}, State) ->
    {Da, Ea} = long_time_fun(fun make_d_e/1, ?BITS_N, "Creating keys..."),
    {{'Ea', Ea}, State#m_alice{i = Wealth, 'Da' = Da}}; 
alice({kj1, Kj1}, #m_alice{'Da' = Da, i = I} = State) ->
    Y = long_time_fun(fun (_) -> pmap(fun(U) -> Da(Kj1 + U) end, lists:seq(0, ?MAX_VALUE - 1), cpu_num()) end, 0, "Calculating list Y..."),
    {P, Z} = long_time_fun(fun calc_p_z/1, Y, "Searching for a prime..."),
    {Zl, Zr} = lists:split(I, Z),
    {{pz, {P, Zl ++ [(Zri + 1) rem P || Zri <- Zr]}}, State#m_alice{p = P, z = Z}};
alice({ekj1, Kj1}, #m_alice{'Da' = Da, i = I} = State) ->
    Y = [Da(Kj1 + U) || U <- lists:seq(0, ?MAX_VALUE - 1)],
    {P, Z} = long_time_fun(fun calc_p_z/1, Y, "Searching for a prime..."),
    {Zl, Zr} = lists:split(I - 1, Z),
    [Zt | T] = Zr,
    {{epz, {P, [(Zi + 1) rem P || Zi <- Zl] ++ [Zt] ++ [(Zi + 1) rem P || Zi <- T]}}, State#m_alice{p = P, z = Z}}.

bob({wealth, Wealth}, State) ->
    io:format(user, "Comparing...~n", []),
    State10 = State#m_bob{'j' = Wealth},
    try_kj1(kj1, State10);
bob({'Ea', Ea}, State) ->
    State10 = State#m_bob{'Ea' = Ea},
    try_kj1(kj1, State10);
bob({epz, {P, Z}}, #m_bob{x = X, j = J} = State) ->
    Xp = X rem P,
    R = case lists:nth(J, Z) of
        Xp -> 
            '=';
        _ -> 
            '>'
    end,
    {{final, R}, State};
bob({pz, {P, Z}}, #m_bob{x = X, j = J} = State) ->
    Xp = X rem P,
    case lists:nth(J, Z) of
        Xp -> 
            try_kj1(ekj1, State);
        _ -> 
            {{final, '<'}, State}
    end.

long_time_fun(F, Param, Msg) ->
    io:format(user, Msg, []),
    R = F(Param),
    io:format(user, "Done.~n", []),
    R.

show_result(R) ->
    {MeAlice, MeBob} = case (catch ets:lookup(?MODULE, me)) of
        [{me, alice}] -> {" (You)", ""};
        [{me, bob}] -> {"", " (You)"};
        _ -> {"", ""}
    end,
    io:format(user, "~n================================================", []),
    io:format(user, "~nWealth comparison result: Alice~s ~s Bob~s", [MeAlice, R, MeBob]),
    io:format(user, "~n================================================~n", []).

try_kj1(Msg, #m_bob{j = J, 'Ea' = Ea} = State) when is_number(J) and is_function(Ea) ->
    X = Ea(make(?BITS_N)),
    K = Ea(X),
    {{Msg, K - J + 1}, State#m_bob{x = X}};
try_kj1(_Msg, State) -> {noreply, State}.

calc_p_z(Y) ->
    random_search(fun () -> calc_p_z0(Y) end, cpu_num()).

calc_p_z0(Y) ->
    P = make_prime(?BITS_N div 2),
    Z = [Yi rem P || Yi <- Y],
    case check_z(Z) of
        true -> {P, Z};
        _ -> calc_p_z0(Y)
    end.

make_d_e(BLen) ->
    P = make_prime(BLen),
    Q = make_prime(BLen),
    case P == Q of
        false ->
            {E, N, D} = gen_rsa_key(P, Q),
            %io:format("{E, D, N}:~p~n", [{E, D, N}]),
            Da = fun (V) ->
                    crypto:bytes_to_integer(crypto:mod_pow(V, D, N))
                end,
            Ea = fun (V) ->
                    crypto:bytes_to_integer(crypto:mod_pow(V, E, N))
                end,
            {Da, Ea};
        _ -> make_d_e(BLen)
    end.

check_z([]) -> true;
check_z([Z0 | T]) ->
    case lists:any(fun (X) -> abs(X - Z0) =< 1 end, T) of
        true -> false;
        _ -> check_z(T)
    end.

gen_rsa_key(P, Q) ->
    N = P * Q,
    Fi = (P - 1) * (Q - 1),
    E = gen_e(Fi),
    D = modulus_inverse(E, Fi),
    {E, N, D}.

gen_e(N) ->
    gen_e(1, N, 15, 15). % start from 2^16 + 1

gen_e(Base, N, _Msl, _Shift) when Base > N -> exit(error);
gen_e(Base, N, Msl, Shift) ->
    V = Base + (1 bsl Shift),
    if 
        V > N ->
            gen_e(1 + (Base bsl 1), N, Msl + 1, Msl + 1);
        true ->
            case gcd(V, N) of
                1 -> V;
                _ -> gen_e(Base, N, Msl, Shift + 1)
            end
    end.

gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

modulus_inverse(A, N) ->
    T = modulus_inverse(0, N, 1, A),
    if T < 0 -> T + N; true -> T end.

modulus_inverse(_T, R, _NewT, 0) when R > 1 -> exit(not_invertible);
modulus_inverse(T, _R, _NewT, 0) -> T;
modulus_inverse(T, R, NewT, NewR)  ->
    Q = R div NewR,
    modulus_inverse(NewT, NewR, T - Q * NewT, R - Q * NewR).

%% make a prime with at least K binary bits
%% Here we use 'Bertrand's postulate, is that for every N > 3,
%% there is a prime P satisfying N < P < 2N - 2
%% This was proved by Tchebychef in 1850 (Erdos improved this proof
%% in 1932)
%% Origial author: Joe Armstrong
%% Modifed
make_prime(K) when K > 3 ->
    N = make(K),
    P1 = make_prime(N - 3, N), % 
    P1.

make_prime(0, _) ->
    exit(impossible);
make_prime(K, P) ->
    %io:format(".", []),
    case is_prime(P) of
	    true  -> P;
	    false -> make_prime(K - 1, P + 1)
    end.

%% make(N) -> a random integer with N bits.
make(N) -> 
    binary:decode_unsigned(crypto:strong_rand_bytes((N + 7) div 8)) band (1 bsl N - 1).

%% Fermat's little theorem says that if 
%% N is a prime and if A < N then
%% A^N mod N = A

%S tag3
is_prime(D) ->
    is_prime(D, 100).

is_prime(D, Ntests) ->
    N = size(binary:encode_unsigned(D)) * 8 - 8,
    is_prime(Ntests, D, N).

is_prime(0, _, _) -> true;
is_prime(Ntest, N, Len) ->
    K = random:uniform(Len),
    %% A is a random number less than N 
    A = make(K),
    case crypto:bytes_to_integer(crypto:mod_pow(A,N,N)) of
		A -> is_prime(Ntest-1,N,Len);
		_T -> 
            false
    end.

parts(L, N, Acc) ->
    case length(L) > N of
        true ->
            {L1, L2} = lists:split(N, L),
            parts(L2, N, [L1 | Acc]);
        _ -> 
            lists:reverse([L | Acc])
    end.

pmap(F, L, Workers) ->
    N = (length(L) + Workers - 1) div Workers,
    Ls = parts(L, N, []),
    lists:concat(pmap(fun (ALst) -> [(catch F(X)) || X <- ALst] end, Ls)).

pmap(F, L) -> 
    S = self(), 
    Pids = lists:map(fun(I) -> spawn(fun() -> do_fun(S, F, I) end) end, L), 
    gather(Pids). 

gather([H | T]) -> 
    receive 
        {H, Result} -> [Result | gather(T)] 
    end; 
gather([]) -> 
    []. 

do_fun(Parent, F, I) -> Parent ! {self(), (catch F(I))}.

do_fun2(Parent, F) -> Parent ! {self(), (catch F())}.

random_search(F, Workers) ->
    S = self(), 
    Ref = make_ref(),
    process_flag(trap_exit, true),
    Fun = fun () -> 
        process_flag(trap_exit, true),
        S10 = self(),
        Pids = lists:map(fun(_I) -> spawn(fun() -> do_fun2(S10, F) end) end, lists:seq(1, Workers)), 
        receive
            {_Pid, R} ->
                [X ! kill || X <- Pids],
                S ! {Ref, R}
        end
    end,
    spawn(Fun),
    receive 
        {Ref, R} -> R
    end.

cpu_num() ->
    case erlang:system_info(logical_processors_available) of
        N when is_integer(N) and (N > 0) -> N;
        _ -> 1
    end.
