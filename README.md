Usage:
```erlang
$ erl
1> c(peer).
ok
2> c(peer_manager).
ok
3> c(prefix_store).
ok
4> peer_manager ! {add_peer, "global", "192.168.1.1", 41075, 30, <<10,0,0,0>>}. % "vrf", "remote_ip", local_as, hold_time, <<router_id>>
5> prefix_store:show('peer_store_global_192.168.1.1'). % show number of routes from this peer
6> prefix_store:show('peer_store_global_192.168.1.1', verbose). % show every route from this peer
7> prefix_store:show('peer_store_global_192.168.1.1', {<<>>, 0}). % show default route
8> prefix_store:show('peer_store_global_192.168.1.1', {<<192,168,1:1>>, 17}). % show 192.168.128.0/17
```
