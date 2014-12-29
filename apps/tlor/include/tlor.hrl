
-define(A, tlor).

-ifdef(tlor_debug).
-define(DBG_OUT(A), io:format("#DBG_OUT(~p:~p:~p)~n", [?MODULE, ?LINE, A])).
-define(DBG_OUT(F, A), io:format("#DBG_OUT(~p:~p:~p) :"++F++"~n", [self(), ?MODULE, ?LINE]++A)).
-define(DBG_RET(R, D), D).
-else.
-define(DBG_OUT(A), void).
-define(DBG_OUT(F, A), void).
-define(DBG_RET(R, D), R).
-endif.

-record(received_packet,
        {
          packet_type, % message, iq, presence
          type_attr,   % depend on packet. Example: set, get, subscribe, etc
          from,        % JID
          id,          % Packet ID
          queryns,     % IQ only: Namespace of the query
          raw_packet   % raw exmpp record
        }).


