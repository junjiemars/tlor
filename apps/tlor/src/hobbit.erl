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

-record(xmlel, {ns, declared_ns, name, attrs=[], children=[]}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([disco_info/2, disco_info/3, disco_items/2, disco_items/3]).
-export([create_node/3, delete_node/3, node_config/3, config_node/5]).
-export([info/1, find_element/3, find_element_by_attr/4]).


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
							_DbgRet = ?DBG_RET(Session, 
								{send_packet, Session, P, _Packet}),
                            {reply, {ok, _DbgRet}, restart_session(Session)}
                    end;
                E -> 
					_DbgRet = ?DBG_RET({error, E}, 
						{E, {send_packet, Session, P}}) ,
					{reply, _DbgRet, restart_session(Session)}
            end;
        E -> {reply, {error, E}, restart_session(Session)}
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
			case request_node_config(J, Node, Session) of 
				{ok, P, R} -> 
					{reply, ?DBG_RET(Session, {node_config, Session, P, R}), 
						restart_session(Session)};
				{E, P} -> {reply, ?DBG_RET(E, {node_config, E, P}), 
					restart_session(Session)}
			end;
		E -> {reply, E, restart_session(Session)}
    end;

handle_call({config_node, Who, Passwd, Node, _Option, _Optarg}, _From, Session) ->
    case login(Session, Who, Passwd) of
       {ok, J} ->
			case request_node_config(J, Node, Session) of 
				{ok, N} -> {reply, {ok, N}, restart_session(Session)};
				E -> {reply, E, restart_session(Session)}
			end;
       E -> {reply, E, restart_session(Session)}
    end;

handle_call({info, Code}, _From, Session) ->
    {reply, info(Code, ?RESOURCE), Session};

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

request_node_config(Jid, Node, Session) ->
    {ok, S} = application:get_env(pubsub_service),
    N = exmpp_client_pubsub:get_node_configuration(S, Node),
    P = exmpp_stanza:set_sender(N, Jid),
    case send_packet(Session, P) of
        {ok, _} ->
            receive
                #received_packet{packet_type=iq, raw_packet=R} -> {ok, R}
            end;
        E -> E
    end.


find_element([], _NS, _Name) ->
    undefined;

find_element([Node|_Tail], NS, Name) ->
    case exmpp_xml:element_matches(Node, NS, Name) of
        true -> Node;
        false -> 
            #xmlel{children=N} = Node,
            find_element(N, NS, Name)
    end.

find_element_by_attr([], _Name, _Attr, _Val) ->
    undefined;

find_element_by_attr([Node|Tail], Name, Attr, Val) ->
    case exmpp_xml:get_name_as_atom(Node) of
        Name -> case exmpp_xml:get_attribute(Node, Attr, undefined) of 
                    Val -> Node;
                    _ -> find_element_by_attr(Tail, Name, Attr, Val)
                end;
        _ -> find_element_by_attr(Tail, Name, Attr, Val)
    end.

