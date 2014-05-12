
-ifdef(tlor_debug).
-define(DBG_OUT(A), io:format("#DBG_OUT(~p:~p:~p)~n", [?MODULE, ?LINE, A])).
-define(DBG_OUT(F, A), io:format("#DBG_OUT(~p:~p:~p) :"++F++"~n", [self(), ?MODULE, ?LINE]++A)).
-define(DBG_RET(R, D), D).
-else.
-define(DBG_OUT(A), void).
-define(DBG_OUT(F, A), void).
-define(DBG_RET(R, D), R).
-endif.

