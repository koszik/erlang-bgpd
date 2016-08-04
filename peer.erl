-module(peer).
-define(BGP_HEADER_SIZE, 19).
-define(BGP_MARKER, -1:16/unit:8).
-define(AS_TRANS, 23456).
-record(peer, {state, sock, hold_time, last_message_received, local_as, remote_as, local_id, remote_id, prefix_store_pid, remote_ip, vrf, aslength = 2}). 
-export([connect/5, keepalive/1, send_update/3, send_update/4]).
-export([read_header/1]).
-include("peer.hrl").

mod(X,Y) -> (X rem Y + Y) rem Y.

%%%%%%%%%%%%%%% UPDATE operations
update(_Operation, Peer, <<>>, _Attributes) -> Peer;
update(Operation, Peer, <<Length:8, Prefix:Length/bitstring, Rest/bitstring>>, Attributes) when Operation == announce ; Operation == withdraw ->
    Unused_Length = mod(8-Length, 8),
    <<_Unused:Unused_Length/bitstring, NewRest/binary>> = Rest,
    PaddedPrefix = <<Prefix/bitstring, 0:Unused_Length>>,
    Peer#peer.prefix_store_pid ! {Operation, {PaddedPrefix, Length}, Attributes},
    log:debug("~w~n", [[Attributes, Operation, PaddedPrefix, Length]]),
    update(Operation, Peer, NewRest, Attributes).


make_prefixlist({Prefix, Length}) ->
    <<Length:8, Prefix/binary>>.

unfold_ases(_Peer, <<>>) ->
    [];
unfold_ases(Peer, <<AS:16, Rest/binary>>) when Peer#peer.aslength == 2 ->
    [AS|unfold_ases(Peer, Rest)];
unfold_ases(Peer, <<AS:32, Rest/binary>>) when Peer#peer.aslength == 4 ->
    [AS|unfold_ases(Peer, Rest)].


fold_ases(_Peer, []) ->
    <<>>;
fold_ases(Peer, [AS|Rest]) when Peer#peer.aslength == 2 ->
    if AS >  65535 -> <<?AS_TRANS:16, (fold_ases(Peer, Rest))/binary>>;
       AS >= 65535 -> <<AS:16, (fold_ases(Peer, Rest))/binary>>
    end;
fold_ases(Peer, [AS|Rest]) when Peer#peer.aslength == 4 ->
    <<AS:32, (fold_ases(Peer, Rest))/binary>>.

decode_as_path(Peer, Type, Value, Rest) ->
    Type_atom = case Type of
		  1 -> as_set;
		  2 -> as_sequence
		end,
    [{Type_atom, unfold_ases(Peer, Value)}|decode_as_path(Peer, Rest)].
decode_as_path(_Peer, <<>>) ->
    [];
decode_as_path(Peer, <<Type:8, Length:8, Value:Length/binary-unit:16, Rest/binary>>) when Peer#peer.aslength == 2 ->
    decode_as_path(Peer, Type, Value, Rest);
decode_as_path(Peer, <<Type:8, Length:8, Value:Length/binary-unit:32, Rest/binary>>) when Peer#peer.aslength == 4 ->
    decode_as_path(Peer, Type, Value, Rest).

encode_as_path(_Peer, []) ->
    <<>>;
encode_as_path(Peer, [{Type_atom, ASList}|Rest]) ->
    Type = case Type_atom of
	    as_set -> 1;
	    as_sequence -> 2
    end,
    Value = fold_ases(Peer, ASList),
    Length = trunc(byte_size(Value) / Peer#peer.aslength),
    <<Type:8, Length:8, Value/binary, (encode_as_path(Peer, Rest))/binary>>.



decode_communities(<<>>) -> [];
decode_communities(<<C:32, Rest/binary>>) -> [C|decode_communities(Rest)].
decode_community(C) ->
    lists:sort(decode_communities(C)).



-define(PATH_ATTRIBUTE, << Optional:1, Transitive:1, Partial:1, Extended_Length:1, _Unused:4, Type_Code:8, ELength:Extended_Length/unit:8, Length:8, DataRest/binary >>).
encode_origin(_Peer, Attrib) ->
    Type_Code = 1,
    Optional = 0, Transitive = 1, Partial = 0, _Unused = 0,
    Extended_Length = 0, ELength = 0, Length = 1,
    DataRest = case Attrib#attrib.origin of
	    igp -> <<0>>;
	    egp -> <<1>>;
	    incomplete -> <<2>>
    end,
    ?PATH_ATTRIBUTE.
encode_aspath(Peer, Attrib) ->
    Type_Code = 2,
    Optional = 0, Transitive = 1, Partial = 0, _Unused = 0,
    Path = if Peer#peer.remote_as == Peer#peer.local_as -> Attrib#attrib.as_path;
	      Peer#peer.remote_as /= Peer#peer.local_as -> [{as_sequence, [Peer#peer.local_as]}|Attrib#attrib.as_path]
	   end,
    DataRest = encode_as_path(Peer, Path),
    Len = byte_size(DataRest),
    if Len  > 255 ->  Extended_Length = 1, ELength = Len / 256, Length = Len rem 256;
       Len =< 255 ->  Extended_Length = 0, ELength = 0, Length = Len
    end,
    ?PATH_ATTRIBUTE.
encode_nexthop(_Peer, Attrib) ->
    DataRest = Attrib#attrib.next_hop,
    Type_Code = 3,
    Optional = 0, Transitive = 1, Partial = 0, _Unused = 0,
    Extended_Length = 0, ELength = 0, Length = byte_size(DataRest),
    ?PATH_ATTRIBUTE.
encode_localpref(Peer, _Attrib) when Peer#peer.local_as /= Peer#peer.remote_as ->
    <<>>;
encode_localpref(Peer, Attrib) when Peer#peer.local_as == Peer#peer.remote_as ->
    Type_Code = 5,
    Optional = 0, Transitive = 1, Partial = 0, _Unused = 0,
    Extended_Length = 0, ELength = 0, Length = 4,
    DataRest = <<(Attrib#attrib.local_pref):32>>,
    ?PATH_ATTRIBUTE.
encode_community(_Peer, Attrib) when Attrib#attrib.community == undefined ->
    <<>>;
encode_community(_Peer, Attrib) ->
    DataRest = lists:foldl(fun(C, Acc) -> <<Acc/binary, C:32>> end, <<>>, Attrib#attrib.community),
    Type_Code = 8,
    Optional = 1, Transitive = 1, Partial = Attrib#attrib.community_partial, _Unused = 0,
    Len = byte_size(DataRest),
    if Len  > 255 ->  Extended_Length = 1, ELength = Len / 256, Length = Len rem 256;
       Len =< 255 ->  Extended_Length = 0, ELength = 0, Length = Len
    end,
    ?PATH_ATTRIBUTE.

-record(flags, {optional, transitive, partial, ebgp}).
% ORIGIN
type_code(_Peer, Attrib, _Flags, 1, <<Origin:8>>) -> %when Flags==#flags{optional = 0, transitive = 1, partial = 0} ->
    Origin_atom = case Origin of
		    0 -> igp;
		    1 -> egp;
		    2 -> incomplete
		end,
    Attrib#attrib{origin = Origin_atom};
% AS_PATH
type_code(Peer, Attrib, _Flags, 2, ASPath) -> % when Flags==#flags{optional = 0, transitive = 1, partial = 0} ->
    Attrib#attrib{as_path = decode_as_path(Peer, ASPath)};
% NEXTHOP
type_code(_Peer, Attrib, _Flags, 3, NextHop) -> % whenFlags==#flags{optional = 0, transitive = 1, partial = 0} ->
    Attrib#attrib{next_hop = NextHop};
% MULTI_EXIT_DISC
type_code(_Peer, Attrib, _Flags, 4, <<MED:32>>) -> % whenFlags==#flags{optional = 1, transitive = 0, partial = 0}  -> 
    Attrib#attrib{med = MED};
% LOCAL_PREF
type_code(_Peer, Attrib, _Flags, 5, <<LocalPref:32>>) -> % when Flags==#flags{optional = 0, transitive = 1, partial = 0, ebgp = false} ->
    Attrib#attrib{local_pref = LocalPref};
% ATOMIC_AGGREGATE
type_code(_Peer, Attrib, _Flags, 6, <<>>) -> % whenFlags==#flags{optional = 0, transitive = 1, partial = 0} ->
    Attrib#attrib{atomic_aggregate = true};
% AGGREGATOR
type_code(Peer, Attrib, Flags, 7, <<AS:16, IP:32>>) when Peer#peer.aslength == 2 -> % whenFlags==#flags{optional = 1, transitive = 1}  ->
    Attrib#attrib{aggregator = {AS, IP}, aggregator_partial = Flags#flags.partial};
type_code(Peer, Attrib, Flags, 7, <<AS:32, IP:32>>) when Peer#peer.aslength == 4 -> % whenFlags==#flags{optional = 1, transitive = 1}  ->
    Attrib#attrib{aggregator = {AS, IP}, aggregator_partial = Flags#flags.partial};
% COMMUNITY
type_code(_Peer, Attrib, Flags, 8, Community) -> % whenFlags==#flags{optional = 1, transitive = 1}  ->
    Attrib#attrib{community = decode_community(Community), community_partial = Flags#flags.partial};


type_code(_Peer, Attrib, Flags, Code, Data) when Flags#flags.optional == 1, Flags#flags.transitive == 1 -> %Flags==#flags{optional = 1, transitive = 1} ->
    log:debug("Unknown transitive attribute: ~p ~w~n", [Code, Data]),
    Attrib#attrib{unknown = [{Code, Data}|Attrib#attrib.unknown]};
type_code(_Peer, _Attrib, Flags, Code, Data) when Flags#flags.optional == 0 -> %Flags==#flags{optional = 0, transitive = 1, partial = 0} ->
    log:err("Unknown well-known transitive attribute: ~p ~w~n", [Code, Data]),
    exit(unknown_attribute);
%type_code(_Peer, Attrib, Flags, Code, Data) when Flags==#flags{optional = 0, transitive = 0} ->
%    log:err("Well-known attribute w/o transitive: ~p ~p~n", [Code, Data]),
%    exit(bad_attribute);
type_code(_Peer, Attrib, Flags, Code, Data) when Flags#flags.partial == 0 ->
    log:debug("Unknown non-transitive attribute: ~p ~w~n", [Code, Data]),
    Attrib;
type_code(_Peer, Attrib, Flags, Code, Data) ->
    log:err("Error in attribute: ~w ~w ~w ~w", [Attrib, Flags, Code, Data]),
    exit(bad_attribute).

encode_path_attributes(Peer, Attrib) ->
    <<
	(encode_origin(Peer, Attrib))/binary,
	(encode_aspath(Peer, Attrib))/binary,
	(encode_nexthop(Peer, Attrib))/binary,
	(encode_localpref(Peer, Attrib))/binary,
	(encode_community(Peer, Attrib))/binary
    >>.

decode_path_attributes(_Peer, <<>>, Attrib) ->
    Attrib;
decode_path_attributes(Peer, ?PATH_ATTRIBUTE, Attrib) ->
    Real_Length = ELength * 256 + Length,
    <<Data:Real_Length/binary, Rest/binary>> = DataRest,
    Flags = #flags{optional=Optional, transitive=Transitive, partial=Partial, ebgp=Peer#peer.local_as /= Peer#peer.remote_as},
    log:debug("attribute: optional:~p, transitive:~p, partial:~p, type:~p, data:~w~n", [Optional, Transitive, Partial, Type_Code, Data]),
    decode_path_attributes(Peer, Rest, type_code(Peer, Attrib, Flags, Type_Code, Data)).
decode_path_attributes(Peer, Path_Attribute) ->
    Attrib = decode_path_attributes(Peer, Path_Attribute, #attrib{}),
% TODO: if ibgp, check for localpref
    if Attrib#attrib.origin == unknown; Attrib#attrib.as_path == unknown; Attrib#attrib.next_hop == unknown -> log:err("mandatory attribute missing: ~w~n", [Attrib]), exit(bad_attribute);
	true -> Attrib
    end.

%%%%%%% MESSAGES parsing

% Multiprotocol Extensions for BGP-4 [RFC2858]
decode_capability(Peer, 1, <<AFI:16, _Reserved:8, SAFI:8>>) ->
    log:info("MP-BGP: AFI: ~p, SAFI: ~p~n", [AFI, SAFI]),
    Peer;
% Route Refresh Capability for BGP-4 [RFC2918]
decode_capability(Peer, 2, <<>>) ->
    log:info("Route refresh supported~n", []),
    Peer;
% Support for 4-octet AS number capability [RFC6793]
decode_capability(Peer, 65, <<AS:32>>) ->
    log:info("Remote 32-bit AS: ~p~n", [AS]),
    Peer#peer{aslength=4, remote_as=AS};
decode_capability(Peer, Type, Value) ->
    log:info("unsupported capability: ~p ~w~n", [Type, Value]),
    Peer.

decode_capabilities(Peer, <<>>) ->
    Peer;
decode_capabilities(Peer, <<Type:8, Length:8, Value:Length/binary, Rest/binary>>) ->
    decode_capabilities(decode_capability(Peer, Type, Value), Rest).

decode_optional_parameters(Peer, <<>>) ->
    Peer;
decode_optional_parameters(Peer, <<2:8, Length:8, Value:Length/binary, Rest/binary>>) ->
    decode_optional_parameters(decode_capabilities(Peer, Value), Rest).

-define(OPEN_MESSAGE, << Version:8, AS:16, Hold_time:16, ID:4/binary, Optional_parameters_length:8, Optional_parameters:Optional_parameters_length/binary >>).
-define(UPDATE_MESSAGE,
<< Withdrawn_Routes_Length:16, Withdrawn_Routes:Withdrawn_Routes_Length/binary,
   Total_Path_Attribute_Length:16, Path_Attributes:Total_Path_Attribute_Length/binary,
   Network_Layer_Reachability_Information/binary >>).

% OPEN
parse_message(Peer, 1, ?OPEN_MESSAGE) when Peer#peer.state == init, Version == 4, Hold_time == 0 ; Hold_time >= 3 ->
    NewPeer = Peer#peer{state=open, hold_time = min(Peer#peer.hold_time, Hold_time), remote_as = AS, remote_id = ID},
    if NewPeer#peer.hold_time /= 0 -> spawn_link(?MODULE, keepalive, [NewPeer]) end,
    send_keepalive(NewPeer),
    log:info("~p: OPEN: remote AS: ~p, remote ID: ~w, proposed hold time: ~p, agreed hold time: ~p~n", [self(), AS, ID, Hold_time, NewPeer#peer.hold_time]),
    Newest = decode_optional_parameters(NewPeer, Optional_parameters),
    peer_manager ! {peer_state, Newest},
    Peer#peer.prefix_store_pid ! {peer_state, Newest},
    Newest;
% UPDATE
parse_message(Peer, 2, ?UPDATE_MESSAGE) when Peer#peer.state == established -> % Total_Path_Attribute_Length == 0 -> Network_Layer_Reachability_Information == <<>>
    WPeer = update(withdraw, Peer, Withdrawn_Routes, []),
    Attributes = decode_path_attributes(Peer, Path_Attributes),
    update(announce, WPeer, Network_Layer_Reachability_Information, Attributes#attrib{ebgp = Peer#peer.local_as /= Peer#peer.remote_as, received_at=now(), router_id = Peer#peer.remote_id});
% NOTIFICATION
parse_message(_Peer, 3, <<Error_code:8, Error_subcode:8, Data/binary>>) ->
    log:err("got NOTIFICATION: ~p/~p, ~w~n", [Error_code, Error_subcode, Data]),
    exit(notification);
% KEEPALIVE
parse_message(Peer, 4, <<>>) when Peer#peer.state /= init ->
    Peer#peer{state = case Peer#peer.state of
      open -> established;
      X -> X
    end}.


send_update(update, Peer, Prefix, Attribs) ->
    Path_Attributes = encode_path_attributes(Peer, Attribs),
    Network_Layer_Reachability_Information = make_prefixlist(Prefix),
    Withdrawn_Routes_Length = 0, Withdrawn_Routes = <<>>,
    Total_Path_Attribute_Length = byte_size(Path_Attributes),
    send_message(Peer, 2, ?UPDATE_MESSAGE).


send_update(withdraw, Peer, Prefix) ->
    Path_Attributes = <<>>,
    Total_Path_Attribute_Length = 0,
    Network_Layer_Reachability_Information = <<>>,
    Withdrawn_Routes = make_prefixlist(Prefix),
    Withdrawn_Routes_Length = byte_size(Withdrawn_Routes),
    send_message(Peer, 2, ?UPDATE_MESSAGE).

%%%%%%%%%%%%%% HEADER level functions

% ?? parse_header(Peer, << ?BGP_MARKER, Length:16, Type:8 >>) when Length =< 4096 ->
parse_header(Peer, << Marker:16/binary, Length:16, Type:8 >>) when Marker == <<?BGP_MARKER>>, Length =< 4096 ->
    Read_Length = Length - ?BGP_HEADER_SIZE,
    {ok, Data} = if Read_Length > 0 -> gen_tcp:recv(Peer#peer.sock, Read_Length);
		    true -> {ok, <<>>}
		 end,
    log:debug("header: type: ~p, data: ~w~n", [Type, Data]),
    parse_message(Peer, Type, Data).

read_header(Peer) ->
    {ok, Header} = gen_tcp:recv(Peer#peer.sock, ?BGP_HEADER_SIZE),
    ?MODULE:read_header(parse_header(Peer#peer{last_message_received=now()}, Header)).

%%%%%%%%%%%%%%% MESSAGE level functions

send_message(Peer, Type, Data) ->
    Length = byte_size(Data) + ?BGP_HEADER_SIZE,
    Packet = <<?BGP_MARKER, Length:16, Type:8, Data/binary>>,
    log:debug("send data: ~w~n", [Packet]),
    ok = gen_tcp:send(Peer#peer.sock, Packet).

send_open(Peer) ->
    Version = 4, Hold_time = Peer#peer.hold_time, ID = Peer#peer.local_id,
    AS = case Peer#peer.local_as of A when A =< 65536 -> A; _ -> ?AS_TRANS end,
    AS4_cap = <<65, 4, (Peer#peer.local_as):32>>,
    Optional_parameters = <<2, 6, AS4_cap/binary>>,
    Optional_parameters_length = byte_size(Optional_parameters),
    Open = ?OPEN_MESSAGE,
    send_message(Peer, 1, Open).


send_keepalive(Peer) ->
    send_message(Peer, 4, <<>>).

%%% keepalive PROCESS
keepalive(Peer) ->
    send_keepalive(Peer),
    % check if received keepalive is recent enough?
    receive after Peer#peer.hold_time * 333 ->
	keepalive(Peer)
    end.
%%%

connect(VRF, PeerAddress, AS, Hold_Time, ID) ->
    ok = process_manager:register({peer, {VRF, PeerAddress}}),
    {ok, Sock} = gen_tcp:connect(PeerAddress, 179, [binary, {active, false}, {packet, raw}]),
    Peer_skel = #peer{sock=Sock, state=init, vrf = VRF, local_as = AS, hold_time = Hold_Time, local_id = ID, remote_ip = PeerAddress},
    StorePid = spawn_link(prefix_store, prefix_store, [VRF]),
    ok = process_manager:register({prefix_store, {VRF, PeerAddress}}, StorePid),
    Peer = Peer_skel#peer{prefix_store_pid = StorePid},
    vrf_store:new_peer(VRF, StorePid),
    send_open(Peer),
    read_header(Peer).
