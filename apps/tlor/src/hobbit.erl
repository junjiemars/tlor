-module(hobbit).
-behaviour(gen_server).
-author('junjiemars@gmail.com').

-define(SERVER, ?MODULE).
-define(RESOURCE, "hobbit").
-define(NS_PUBSUB_W3ATOM, 'http://www.w3.org/2005/Atom').

-include("tlor.hrl").

-import(mordor, [sleep/1, 
                 login/5, 
                 restart_session/1, 
                 send_packet/2,
                 info/2
        ]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([disco_info/2, disco_info/3, disco_items/2, disco_items/3]).
-export([create_node/3, delete_node/3, node_config/3, config_node/5]).
-export([info/1, build_node_config/2]).


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

disco_info(Who, Passwd) -> gen_server:call(?MODULE, {disco_info, Who, Passwd}).
disco_info(Who, Passwd, Node) -> gen_server:call(?MODULE, {disco_info, Who, Passwd, Node}).
disco_items(Who, Passwd) -> gen_server:call(?MODULE, {disco_items, Who, Passwd}).
disco_items(Who, Passwd, Node) -> gen_server:call(?MODULE, {disco_items, Who, Passwd, Node}).

create_node(Who, Passwd, Node) -> gen_server:call(?MODULE, {create_node, Who, Passwd, Node}).
delete_node(Who, Passwd, Node) -> gen_server:call(?MODULE, {delete_node, Who, Passwd, Node}).
node_config(Who, Passwd, Node) -> gen_server:call(?MODULE, {node_config, Who, Passwd, Node}).
config_node(Who, Passwd, Node, Option, Optarg) -> gen_server:call(?MODULE, {config_node, Who, Passwd, Node, Option, Optarg}).

info(Code) -> gen_server:call(?MODULE, {info, Code}).
build_node_config(Service, Node) -> gen_server:call(?MODULE, {build_node_config, Service, Node}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    application:start(exmpp),
    {ok, exmpp_session:start()}.

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
    case login(Session, Who, Passwd) of
        {ok, J} -> 
            {ok, S} = application:get_env(pubsub_service),
            N = exmpp_client_pubsub:get_node_configuration(S, Node),
            P = exmpp_stanza:set_sender(N, J),
            case send_packet(Session, P) of
                {ok, _} ->
                    receive
                        #received_packet{packet_type=iq, raw_packet=_R} ->
                            {reply, {ok, ?DBG_RET(Session, {send_packet, Session, P, _R})}, restart_session(Session)}
                    end;
                E -> {reply, ?DBG_RET(E, {E, {send_packet, Session, P}}), restart_session(Session)}
            end;
        E -> {reply, E, restart_session(Session)}
    end;

handle_call({config_node, Who, Passwd, _Node, _Option, _Optarg}, _From, Session) ->
    case login(Session, Who, Passwd) of
        {ok, _J} ->
            {ok, _S} = application:get_env(pubsub_service),
            X0 = exmpp_xml:element("jabberd:x:data", "x"),
            Xtype = exmpp_xml:set_attribute(X0, exmpp_xml:attribute(<<"type">>, "submit")),
            F0 = exmpp_xml:element("field"),
            Fvar0 = exmpp_xml:set_attribute(F0, exmpp_xml:attribute(<<"var">>, "FORM_TYPE")),
            Ftype0 = exmpp_xml:set_attribute(Fvar0, exmpp_xml:attribute(<<"type">>, "hidden")),
            Vf0 = exmpp_xml:set_cdata(exmpp_xml:element("value"), 
                unicode:characters_to_binary("http://jabber.org/protocol/pubsub#node_config")),
            _V0 = exmpp_xml:append_child(Ftype0, Vf0),

            F1 = exmpp_xml:element("field"),
            Fvar1 = exmpp_xml:set_attribute(F1, exmpp_xml:attribute(<<"var">>, "pubsub#send_last_published_item")),
            Fval1 = exmpp_xml:set_cdata(exmpp_xml:element("value"), unicode:characters_to_binary("on_sub")),
            Fp1 = exmpp_xml:append_child(Fvar1, Fval1),

            C = exmpp_xml:append_children(Xtype, [_V0, Fp1]),
            N = exmpp_client_pubsub:set_node_configuration(_S, _Node, C),
            P = exmpp_stanza:set_sender(N, _J),
            io:format("##~s~n", exmpp_xml:document_to_iolist(P)),
            case send_packet(Session, P) of
               {ok, _} -> 
                    receive
                        #received_packet{packet_type=iq, raw_packet=_R} ->
                            {reply, {ok, ?DBG_RET(Session, {send_packet, Session, P, _R})}, restart_session(Session)}
                    end;
                E -> {reply, ?DBG_RET(E, {E, {send_packet, Session, P}}), restart_session(Session)}
            end;
        E -> {reply, E, restart_session(Session)}
    end;

handle_call({info, Code}, _From, Session) ->
    {reply, info(Code, ?RESOURCE), Session};

handle_call({build_node_config, Service, Node}, _From, Session) ->
    V = exmpp_xml:set_cdata(exmpp_xml:element(value), "on_sub"),
    F = exmpp_xml:set_children(exmpp_xml:element(field), [V]),
    X = exmpp_xml:set_children(exmpp_xml:element("jabber:x:data", "x"), [F]),
    Y = exmpp_xml:remove_attribute(X, "xmlns"),
    P = exmpp_client_pubsub:set_node_configuration(Service, Node, Y),
    io:format("##~s~n", exmpp_xml:document_to_iolist(P)),
    {reply, P, Session};


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

login(Session, Who, Passwd) ->
    login(Session, Who, Passwd, ?RESOURCE, false).


