Usage:
```erlang
$ erl
> c(log).
> c(peer).
> c(process_manager).
> process_manager:start().
> c(peer_manager).
> peer_manager:init().
> c(prefix_store).
> c(vrf_store).
> vrf_store:start("global").
> peer_manager ! {add_peer, "global", "192.168.1.1", 41075, 30, <<10,0,0,0>>}. % "vrf", "remote_ip", local_as, hold_time, <<router_id>>
> prefix_store:show('peer_store_global_192.168.1.1'). % show the number of routes from this peer
> prefix_store:show('peer_store_global_192.168.1.1', verbose). % show every route from this peer
> prefix_store:show('peer_store_global_192.168.1.1', {<<>>, 0}). % show the default route
> prefix_store:show('peer_store_global_192.168.1.1', {<<192,168,128>>, 17}). % show 192.168.128.0/17
> vrf_store:show("global", {<<10,100,55>>,24}).
```
