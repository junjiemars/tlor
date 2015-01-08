%% macros, records definitions
-define(A, tlor).
-define(NS_PUBSUB_W3ATOM, 'http://www.w3.org/2005/Atom').

-record(received_packet,
        {
          packet_type, % message, iq, presence
          type_attr,   % depend on packet. Example: set, get, subscribe, etc
          from,        % JID
          id,          % Packet ID
          queryns,     % IQ only: Namespace of the query
          raw_packet   % raw exmpp record
        }).


