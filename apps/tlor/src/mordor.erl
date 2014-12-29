-module(mordor).
-author('junjiemars@gmail.com').

-include("tlor.hrl").


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([restart_session/1, send_packet/2]).
-export([login/4, login/5]).
-export([sleep/1]).
-export([info/2]).


%% ------------------------------------------------------------------
%% Function Definitions 
%% ------------------------------------------------------------------

restart_session(Session) ->
    exmpp_session:stop(Session),
    exmpp_session:start().

send_packet(Session, Packet) ->
    try 
        R = exmpp_session:send_packet(Session, Packet),
        {ok, R}
    catch 
        exit:Reason -> {exit, Reason};
        error:Reason -> {error, Reason}
    end.

login(Session, Who, Passwd, Resource) -> 
    [U, N] = string:tokens(Who, "@"),
    J = exmpp_jid:make(U, N, Resource),
    exmpp_session:auth_basic_digest(Session, J, Passwd),

    {ok, H} = application:get_env(?A, host_ipv4),
    {ok, P} = application:get_env(?A, host_port),

    {ok, _StreamId} = exmpp_session:connect_TCP(Session, H, P),
    {ok, J} = exmpp_session:login(Session).
    
login(Session, Who, Passwd, Resource, Autoreg) -> 
    [U, N] = string:tokens(Who, "@"),
    J = exmpp_jid:make(U, N, Resource),
    exmpp_session:auth_basic_digest(Session, J, Passwd),

    {ok, H} = application:get_env(?A, host_ipv4),
    {ok, P} = application:get_env(?A, host_port),

    try exmpp_session:connect_TCP(Session, H, P) of
        {ok, _StreamId} -> {ok, J}
    catch
        _:Econnect -> {error, Econnect}
    end,

    try exmpp_session:login(Session) of
       _ -> {ok, J}
    catch
        throw:Reason ->
            case Reason of
                    {auth_error,'not-authorized'} ->
                                case Autoreg of
                                    true ->
                                        exmpp_session:register_account(
                                          Session, Passwd),
                                        {ok, J};
                                    false -> {error, Reason}
                                end;
                        _  -> {error, Reason}
            end;
        _:Reason -> {error, Reason} %% exit:Reason/error:Reason
    end.

sleep(T) ->
	receive
	    after T -> true
	end.

info(Code, Resource) ->
	if 
		"Hell@" =:= Code -> 
			Env = application:get_all_env(),
    		{ok, Resource, Env};
		true -> {error, Resource, Code}
	end.

