-record(logic_clusters, {node, ccu, ccu_limit}).

-record(ws_globals, {key, value}).

-record(ws_expire_globals, {key, value, expire_at}).

-record(ws_sessions, {user_id,
                      node,
                      pid,
                      expire_at}).
