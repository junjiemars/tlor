-module(dc).
-author('junjiemars@gmail.com').
-export([join/1]).

join(Node) ->
    application:stop(ejabberd),
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start(),
    mnesia:change_config(extra_db_nodes, [Node]),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    sync_node(),
    application:start(ejabberd).

sync_node() ->
    [{Tb, mnesia:add_table_copy(Tb, node(), Type)} 
    || {Tb, [{_Node, Type}]} <- [{T, mnesia:table_info(T, where_to_commit)}
    || T <- mnesia:system_info(tables)]].

