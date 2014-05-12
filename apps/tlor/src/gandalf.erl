-module(gandalf).
-behaviour(gen_server).
-author('junjiemars@gmail.com').

-define(SERVER, ?MODULE).
-define(RESOURCE, "gandalf").
-define(NS_PUBSUB_W3ATOM, 'http://www.w3.org/2005/Atom').

-include("tlor.hrl").
%%-include_lib("exmpp/include/exmpp.hrl").

-record(received_packet,
        {
          packet_type, % message, iq, presence
          type_attr,   % depend on packet. Example: set, get, subscribe, etc
          from,        % JID
          id,          % Packet ID
          queryns,     % IQ only: Namespace of the query
          raw_packet   % raw exmpp record
        }).



%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([register/2, disable/1]).
-export([disco_info/2, disco_info/3, disco_items/2, disco_items/3]).
-export([create_node/3, delete_node/3, node_config/3]).
-export([publish/5, subscribe/3, unsubscribe/2, unsubscribe/4, subscriptions/2]).
-export([sayto/4]).
-export([roster/2, add_roster/5]).
-export([info/1]).


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

register(Who, Passwd) -> gen_server:call(?MODULE, {register, Who, Passwd}).
disable(Who) -> gen_server:call(?MODULE, {disable, Who}).

disco_info(Who, Passwd) -> gen_server:call(?MODULE, {disco_info, Who, Passwd}).
disco_info(Who, Passwd, Node) -> gen_server:call(?MODULE, {disco_info, Who, Passwd, Node}).
disco_items(Who, Passwd) -> gen_server:call(?MODULE, {disco_items, Who, Passwd}).
disco_items(Who, Passwd, Node) -> gen_server:call(?MODULE, {disco_items, Who, Passwd, Node}).

create_node(Who, Passwd, Node) -> gen_server:call(?MODULE, {create_node, Who, Passwd, Node}).
delete_node(Who, Passwd, Node) -> gen_server:call(?MODULE, {delete_node, Who, Passwd, Node}).
node_config(Who, Passwd, Node) -> gen_server:call(?MODULE,{node_config, Who, Passwd, Node}).

publish(Who, Passwd, Node, Type, Subject) -> gen_server:call(?MODULE, {publish, Who, Passwd, Node, Type, Subject}).
subscribe(Who, Passwd, Node) -> gen_server:call(?MODULE, {subscribe, Who, Passwd, Node}).
unsubscribe(Who, Passwd) -> gen_server:call(?MODULE, {unsubscribe, Who, Passwd}).
unsubscribe(Who, Passwd, Node, Subid) -> gen_server:call(?MODULE, {unsubscribe, Who, Passwd, Node, Subid}).
subscriptions(Who, Passwd) -> gen_server:call(?MODULE, {subscriptions, Who, Passwd}).

sayto(Who, Passwd, Whom, What) -> gen_server:call(?MODULE, {sayto, Who, Passwd, Whom, What}).

roster(Who, Passwd) -> gen_server:call(?MODULE, {roster, Who, Passwd}).
add_roster(Who, Passwd, Whom, Group, Nick) -> gen_server:call(?MODULE, {add_roster, Who, Passwd, Whom, Group, Nick}).

info(Code) -> gen_server:call(?MODULE, {info, Code}).
%%login(Who, Passwd, Register) -> gen_server:call(?MODULE, {login, Who, Passwd, Register}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    application:start(exmpp),
    {ok, exmpp_session:start()}.

handle_call({register, Who, Passwd}, _From, Session) ->
    case login(Session, Who, Passwd, true) of 
        {ok, R} -> {reply, {ok, R}, restart_session(Session)};
        {_,  E} -> {reply, {error, E}, restart_session(Session)}
    end;

handle_call({disable, Who}, _From, Session) ->
    R = exmpp_client_register:get_registration_fields(Who),
    %%R = exmpp_client_register:remove_account(Who),
    %%io:format("#before send_packet~n"),
    exmpp_session:send_packet(Session, R),
    %%io:format("#after send_packet~n"),
    %%exmpp_session:stop(Session),
    {reply, R, Session};

handle_call({disco_info, Who, Passwd}, _From, Session) ->
    case login(Session, Who, Passwd) of
		{ok, J} -> 
			{ok, S} = application:get_env(pubsub_service),
		    I = exmpp_client_disco:info(S),
			P = exmpp_stanza:set_sender(I, J),

		    case send_packet(Session, P) of
				{ok, _} ->
				    receive 
				        #received_packet{packet_type=iq, raw_packet=_Packet} ->
							{reply, {ok, ?DBG_RET(Session, {send_packet, Session, P, _Packet})}, restart_session(Session)}
				    end;
				E -> ?DBG_OUT({E,now()}),{reply, ?DBG_RET(E, {E, {send_packet, Session, P}}), restart_session(Session)}
			end;
		E -> {reply, E, restart_session(Session)}
	end;

handle_call({disco_info, Who, Passwd, Node}, _From, Session) ->
    case login(Session, Who, Passwd) of
		{ok, J} -> 
			{ok, S} = application:get_env(pubsub_service),
		    I = exmpp_client_disco:info(S, Node),
			P = exmpp_stanza:set_sender(I, J),

		    case send_packet(Session, P) of
				{ok, _} ->
				    receive 
				        #received_packet{packet_type=iq, raw_packet=_Packet} ->
							{reply, {ok, ?DBG_RET(Session, {send_packet, Session, P, Node, _Packet})}, restart_session(Session)}
				    end;
				E -> ?DBG_OUT({E,now()}),{reply, ?DBG_RET(E, {E, {send_packet, Session, P, Node}}), restart_session(Session)}
			end;
		E -> {reply, E, restart_session(Session)}
	end;

handle_call({disco_items, Who, Passwd}, _From, Session) ->
    case login(Session, Who, Passwd) of
		{ok, J} ->
			{ok, S} = application:get_env(pubsub_service),
		    I = exmpp_client_disco:items(S),
			P = exmpp_stanza:set_sender(I, J),
		    case send_packet(Session, P) of
				{ok, _} ->
		    		receive
		    		    #received_packet{packet_type=iq, raw_packet=_Packet} ->
		    		       {reply, {ok, ?DBG_RET(Session, {send_packet, Session, P, _Packet})}, restart_session(Session)} 
		    		end;
				E -> ?DBG_OUT({E, now()}), {reply, ?DBG_RET(E, {E, {send_packet, Session, P}}), restart_session(Session)}
			end;
		E -> {reply, E, restart_session(Session)}
	end;

handle_call({disco_items, Who, Passwd, Node}, _From, Session) ->
    case login(Session, Who, Passwd) of
		{ok, J} ->
			{ok, S} = application:get_env(pubsub_service),
		    I = exmpp_client_disco:items(S, Node),
			P = exmpp_stanza:set_sender(I, J),
		    case send_packet(Session, P) of
				{ok, _} ->
		    		receive
		    		    #received_packet{packet_type=iq, raw_packet=_Packet} ->
		    		       {reply, {ok, ?DBG_RET(Session, {send_packet, Session, P, Node, _Packet})}, restart_session(Session)} 
		    		end;
				E -> ?DBG_OUT({E, now()}), {reply, ?DBG_RET(E, {E, {send_packet, Session, P, Node}}), restart_session(Session)}
			end;
		E -> {reply, E, restart_session(Session)}
	end;


handle_call({create_node, Who, Passwd, Node}, _From, Session) ->
    case login(Session, Who, Passwd) of
        {ok, _} ->
			{ok, S} = application:get_env(pubsub_service),
            P = exmpp_client_pubsub:create_node(S, Node), 
            case send_packet(Session, P) of
                {ok, _} ->
                    receive 
                        #received_packet{packet_type=iq, raw_packet=_Packet} ->
                            {reply, {ok, ?DBG_RET(Session, {send_packet, Session, P, _Packet})}, restart_session(Session)}
                    end;
                E -> {reply, ?DBG_RET(E, {E, {send_packet, Session, P}}), restart_session(Session)}
            end;
        E -> {reply, E, restart_session(Session)}
    end;

handle_call({delete_node, Who, Passwd, Node}, _From, Session) ->
    case login(Session, Who, Passwd) of
        {ok, _} ->
			{ok, S} = application:get_env(pubsub_service),
            P = exmpp_client_pubsub:delete_node(S, Node),
            case send_packet(Session, P) of
                {ok, _} ->
                    receive
                        #received_packet{packet_type=iq, raw_packet=_Packet} ->
                            {reply, {ok, ?DBG_RET(Session, {send_packet, Session, P, _Packet})}, restart_session(Session)}
                    end;
                E -> {reply, ?DBG_RET(E, {E, {send_packet, Session, P}}), restart_session(Session)}
            end;
        E -> {reply, E, restart_session(Session)}
    end;

handle_call({node_config, Who, Passwd, Node}, _From, Session) ->
    {ok, _J} = login(Session, Who, Passwd),
    node_default_options(Session, "pubsub.localhost"),
    X = exmpp_client_pubsub:get_node_configuration("pubsub.localhost", Node),
    exmpp_session:send_packet(Session, X),
    receive
        _Response = #received_packet{packet_type=iq, raw_packet=Packet} ->
            io:format("## ok!!!~n")
    end,
    exmpp_session:stop(Session),
    {reply, Packet, exmpp_session:start()};

handle_call({publish, Who, Passwd, Node, Type, Subject}, _From, Session) ->
    case  login(Session, Who, Passwd) of
        {ok, J} ->
            E = exmpp_xml:element(?NS_PUBSUB_W3ATOM, "entry"),
            T = exmpp_xml:append_cdata(exmpp_xml:element("type"), unicode:characters_to_binary(Type)),
            S = exmpp_xml:append_cdata(exmpp_xml:element("subject"), unicode:characters_to_binary(Subject)),
            Item = exmpp_xml:append_children(E, [T, S]),

			{ok, H} = application:get_env(pubsub_service),
            X = exmpp_client_pubsub:publish(H, Node, Item),
            P = exmpp_stanza:set_sender(X, J),

            case send_packet(Session, P) of
                {ok, _} ->  
                    receive 
                        #received_packet{packet_type=iq, raw_packet=_Packet} ->
                           {reply, {ok, ?DBG_RET(Session, {send_packet, Session, P, _Packet})}, restart_session(Session)}
                    end;
                {_, E} -> {reply, ?DBG_RET({error, E}, {E, {send_packet, Session, P}}), restart_session(Session)}
            end;
        E -> {reply, E, restart_session(Session)}
    end;

handle_call({subscribe, Who, Passwd, Node}, _From, Session) ->
    case login(Session, Who, Passwd) of
        {ok, _} ->
            {ok, S} = application:get_env(pubsub_service),
            P = exmpp_client_pubsub:subscribe(Who, S, Node),

            case send_packet(Session, P) of
                {ok, _} ->
                    receive
                        #received_packet{packet_type=iq, raw_packet=_Packet} ->
                        	{reply, {ok, ?DBG_RET(Session, {send_packet, Session, P})}, restart_session(Session)}
                    end;
                E -> {reply, ?DBG_RET(E, {E, {send_packet, Session, P}}), restart_session(Session)}
            end;
        E -> {reply, E, restart_session(Session)}
    end;

handle_call({unsubscribe, Who, Passwd}, _From, Session) ->
	case subscriptions(Who, Passwd, _From, Session) of
		{ok, R} -> 
			S = exmpp_xml:get_element(exmpp_xml:get_element(R, "pubsub"), "subscriptions"),
			exmpp_xml:foreach(
				fun(_, C) -> 
					Jid = binary_to_list(exmpp_xml:get_attribute(C, <<"jid">>, undefined)),
					Sid = binary_to_list(exmpp_xml:get_attribute(C, <<"subid">>, undefined)),
					Node = binary_to_list(exmpp_xml:get_attribute(C, <<"node">>, undefine)),
					?DBG_OUT("jid:~p sid:~p node:~p", [Jid, Sid, Node]),
					if Jid =:= Who ->
						{ok, _P} = unsubscribe(Who, Passwd, Node, Sid, _From, Session),
						?DBG_OUT("unsubscribe:~p", exmpp_xml:document_to_iolist(_P))
					end
				end,
			S),	
			{reply, {ok, R}, restart_session(Session)};
		E -> {reply, E, restart_session(Session)}
	end;	

handle_call({unsubscribe, Who, Passwd, Node, Subid}, _From, Session) ->
    case login(Session, Who, Passwd) of
        {ok, _} ->
			case unsubscribe(Who, Passwd, Node, Subid, _From, Session) of
				{ok, _P} -> 
                	{reply, {ok, ?DBG_RET(Session, {unsubscribe, Session, Node, Subid, _P})}, restart_session(Session)};
                E -> {reply, ?DBG_RET(E, {E, {unsubscribe, Session, Node, Subid}}), restart_session(Session)}
            end;
        E -> {reply, E, restart_session(Session)}
    end;

handle_call({subscriptions, Who, Passwd}, _From, Session) ->
    case login(Session, Who, Passwd) of
		{ok, J} ->
			{ok, N} = application:get_env(pubsub_service),
		    S = exmpp_client_pubsub:get_subscriptions(N),
			P = exmpp_stanza:set_sender(S, J),

		    case send_packet(Session, P) of
				{ok, _} ->
				    receive
				        #received_packet{packet_type=iq, raw_packet=_Packet} ->
							{reply, {ok, ?DBG_RET(Session, {send_packet, Session, P, _Packet})}, restart_session(Session)}
				    end;
				E -> ?DBG_OUT({E, now()}), {reply, {E, {send_packet, Session, P}}, restart_session(Session)}
			end;
		E -> {reply, E, restart_session(Session)}
	end;

handle_call({sayto, Who, Passwd, Whom, What}, _From, Session) ->
    case login(Session, Who, Passwd) of 
        {ok, U} -> 
            C = exmpp_message:chat(unicode:characters_to_binary(What)),
            P = exmpp_stanza:set_recipient(exmpp_stanza:set_sender(C, U), Whom),
            R = send_packet(Session, P),
			sleep(150),
            {reply, ?DBG_RET(R, {send_packet, Session, P, R}), restart_session(Session)}; 
        E ->
            {reply, E, restart_session(Session)}
    end;

handle_call({roster, Who, Passwd}, _From, Session) ->
     case login(Session, Who, Passwd) of 
        {ok, U} -> 
            G = exmpp_client_roster:get_roster(),
            P = exmpp_stanza:set_sender(G, U),
            case send_packet(Session, P) of
                {ok, _} ->
                    receive 
                        #received_packet{packet_type=iq, raw_packet=_R} ->
                            {reply, {ok, ?DBG_RET(Session, {send_packet, Session, P, _R})}, restart_session(Session)}
                    end;
                E -> ?DBG_RET(E, {reply, {E, {send_packet, Session, P}}, restart_session(Session)})
            end;
        E ->
            {reply, E, restart_session(Session)}
    end;

handle_call({add_roster, Who, Passwd, Whom, Group, Nick}, _From, Session) ->
     case login(Session, Who, Passwd) of 
        {ok, U} -> 
            G = exmpp_client_roster:set_item(Whom, unicode:characters_to_binary(Group), unicode:characters_to_binary(Nick)),
            P = exmpp_stanza:set_sender(G, U),
            case send_packet(Session, P) of
                {ok, _} ->
                    receive 
                        #received_packet{packet_type=iq, raw_packet=_R} ->
                            {reply, {ok, ?DBG_RET(Session, {send_packet, Session, P, _R})}, restart_session(Session)}
                    end;
                E -> ?DBG_RET(E, {reply, {E, {send_packet, Session, P}}, restart_session(Session)})
            end;
        E ->
            {reply, E, restart_session(Session)}
    end;

handle_call({info, Code}, _From, Session) ->
	if 
		"Hell@" =:= Code -> 
			Env = application:get_all_env(),
    		{reply, {ok, Env}, Session};
		true -> {reply, {error, Code}, Session}
	end;

handle_call({login, Who, Passwd, Register}, _From, Session) ->
    R = login(Session, Who, Passwd, Register),
    {reply, R, restart_session(Session)};

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
%% Internal Function Definitions
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
    
login(Session, Who, Passwd) -> 
    login(Session, Who, Passwd, false).

login(Session, Who, Passwd, Register) -> 
    [U, N] = string:tokens(Who, "@"),
    J = exmpp_jid:make(U, N, ?RESOURCE),
    exmpp_session:auth_basic_digest(Session, J, Passwd),

    {ok, H} = application:get_env(host_ipv4),
    {ok, P} = application:get_env(host_port),
    {ok, _StreamId} = exmpp_session:connect_TCP(Session, H, P),

    try exmpp_session:login(Session) of
       _ -> {ok, J}
    catch
        throw:Reason ->
            case Register of
                true ->
                    exmpp_session:register_account(Session, Passwd),
                    {ok, J};
                false -> {error, Reason}
            end;
        exit:Reason -> exit(Reason);
        error:Reason -> error(Reason)
    end.

subscriptions(Who, Passwd, _From, Session) ->
    case login(Session, Who, Passwd) of
		{ok, J} ->
			{ok, N} = application:get_env(pubsub_service),
		    S = exmpp_client_pubsub:get_subscriptions(N),
			P = exmpp_stanza:set_sender(S, J),

		    case send_packet(Session, P) of
				{ok, _} ->
				    receive
				        #received_packet{packet_type=iq, raw_packet=Packet} -> {ok, Packet}
				    end;
				{_, E} -> E
			end;
		E -> E
	end.

unsubscribe(Who, _Passwd, Node, Subid, _From, Session) ->
	{ok, S} = application:get_env(pubsub_service),
	U = exmpp_client_pubsub:unsubscribe(Who, S, Node),
	Pubsub = exmpp_xml:get_element(U, "pubsub"),
	Unsub = exmpp_xml:set_attribute(exmpp_xml:get_element(Pubsub, "unsubscribe"), <<"subid">>, Subid),
	Npubsub = exmpp_xml:append_child(exmpp_xml:remove_element(Pubsub, "unsubscribe"), Unsub),
	P = exmpp_xml:append_child(exmpp_xml:remove_element(U, "pubsub"), Npubsub),

	case send_packet(Session, P) of
	    {ok, _} ->
	        receive
	            #received_packet{packet_type=iq, raw_packet=Packet} -> {ok, Packet}
	        end;
	    E -> E
	end.

node_default_options(Session, Service) ->
    X = exmpp_client_pubsub:get_default_configuration(Service),
    io:format("### default config options:~n~p~n", [X]),
    exmpp_session:send_packet(Session, X),
    receive
        _Reponse = #received_packet{packet_type=iq, raw_packet=Packet} ->
            io:format("## default config:~n~p~n", [Packet])
    end,
    Packet.

sleep(T) ->
	receive
	after T -> true
	end.
