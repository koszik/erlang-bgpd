-module(peer).
-define(HOST, "88.151.96.253").
-define(BGP_HEADER_SIZE, 19).
-define(HOLD_TIME, 30).
-define(AS, 41075).
-define(ID, 12345678).
-define(BGP_MARKER, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255).
-define(PEER_RECORD, state, sock, hold_time, last_message_received, local_as, remote_as, local_id, remote_id, prefix_store_pid).
-record(peer, {?PEER_RECORD}). 
-export([connect/1, start_client/0, keepalive/1]).
-include("peer.hrl").

mod(X,Y) -> (X rem Y + Y) rem Y.

%% TODO:
%%  - check for all mandatory attribs



%%%%%%%%%%%%%%% UPDATE operations
update(_Operation, Peer, <<>>, _Attributes) -> Peer;
update(Operation, Peer, <<Length:8, Prefix:(Length)/bitstring, Rest/bitstring>>, Attributes) when Operation == announce ; Operation == withdraw ->
    Unused_Length = mod(8-Length, 8),
    <<_Unused:(Unused_Length)/bitstring, NewRest/binary>> = Rest,
    Peer#peer.prefix_store_pid ! {Operation, Prefix, Length, Attributes},
%    io:format("~p ~p ~p/~p~n", [Attributes, Operation, Prefix, Length]),
    update(Operation, Peer, NewRest, Attributes).


unfold_ases(<<>>) ->
    [];
unfold_ases(<<AS:16, Rest/binary>>) ->
    [AS|unfold_ases(Rest)].

decode_as_path(<<>>) ->
    [];
decode_as_path(<<Type:8, Length:8, Value:(Length)/binary-unit:16, Rest/binary>>) when Type == 1; Type == 2 ->
    Type_atom = case Type of
		  1 -> as_set;
		  2 -> as_sequence
		end,
    [{Type_atom, unfold_ases(Value)}|decode_as_path(Rest)].


-define(MANDATORY_ATTRIBUTES, [origin, as_path, next_hop]).
%-define(ATTRIB_RECORD, optional, transitive, partial, type, data, type_decoded, data_decoded, received_at).
%-record(attrib, {?ATTRIB_RECORD}).


%% ?? shall we check for transitive/partial for mandatory attribs?

% ORIGIN
type_code(Attrib, 1, <<Origin:8>>) ->
    Origin_atom = case Origin of
		    0 -> igp;
		    1 -> egp;
		    2 -> incomplete
		end,
    Attrib#attrib{optional = 0, transitive = 1, partial = 0, type_decoded = origin, data_decoded = Origin_atom, origin = Origin_atom};
% AS_PATH
type_code(Attrib, 2, ASPath) ->
    Attrib#attrib{optional = 0, transitive = 1, partial = 0, type_decoded = as_path, data_decoded = decode_as_path(ASPath), as_path = decode_as_path(ASPath)};
% NEXTHOP
type_code(Attrib, 3, NextHop) ->
    Attrib#attrib{optional = 0, transitive = 1, partial = 0, type_decoded = next_hop, data_decoded = NextHop, next_hop = NextHop};
% MULTI_EXIT_DISC
type_code(Attrib, 4, <<MED:32>>) ->
    Attrib#attrib{optional = 1, transitive = 0, partial = 0,  type_decoded = med, data_decoded = MED, med = MED};
% LOCAL_PREF
type_code(Attrib, 5, <<LocalPref:32>>) -> % ignore if from ebgp
    Attrib#attrib{optional = 0, partial = 0, type_decoded = local_pref, data_decoded = LocalPref, local_pref = LocalPref};
% ATOMIC_AGGREGATE
type_code(Attrib, 6, <<>>) ->
    Attrib#attrib{optional = 0, partial = 0, type_decoded = atomic_aggregate, atomic_aggregate = true};
% AGGREGATOR
type_code(Attrib, 7, <<AS:16, IP:32>>) ->
    Attrib#attrib{optional = 1, transitive = 1, type_decoded = aggregator, data_decoded = {AS, IP}, aggregator = {AS, IP}};
% unknown
type_code(Attrib, _Code, _Data) when Attrib#attrib.transitive == 1 ->
%    io:format("Unknown transitive attribute: ~p ~p~n", [Code, Data]),
    Attrib#attrib{partial = 1, type_decoded = "unknown"};
type_code(_Attrib, _Code, _Data) ->
%    io:format("Unknown non-transitive attribute: ~p ~p~n", [Code, Data]),
    [].


% 	Unused=0 %% when sending
-define(PATH_ATTRIBUTE, << Optional:1, Transitive:1, Partial:1, Extended_Length:1, _Unused:4, Type_Code:8, ELength:(Extended_Length)/binary, Length:8, Rest/binary >>).
decode_path_attributes(_Peer, <<>>, Attrib) ->
    Attrib;
decode_path_attributes(Peer, ?PATH_ATTRIBUTE, Attrib)
    when % well_known -> transitive = 1
        Partial==0 ; Transitive==1  ->
    Real_Length = case ELength of <<>> -> Length; 1 -> ELength * 8 + Length end,
    <<Data:(Real_Length)/binary, Rest2/binary>> = Rest,
%    Attrib_skel = #attrib{optional=Optional, transitive=Transitive, partial=Partial, type=Type_Code, data=Data},
%    io:format("in : optional:~p, transitive:~p, partial:~p, type:~p, data:~p~n", [Attrib_skel#attrib.optional, Attrib_skel#attrib.transitive, Attrib_skel#attrib.partial, Attrib_skel#attrib.type, Attrib_skel#attrib.data]),
    NewAttrib = type_code(Attrib, Type_Code, Data),
    case NewAttrib of
	[] -> decode_path_attributes(Peer, Rest2, Attrib);
	X -> %io:format("out: optional:~p, transitive:~p, partial:~p, type_dec:~p, data_dec:~p~n", [Attrib#attrib.optional, Attrib#attrib.transitive, Attrib#attrib.partial, Attrib#attrib.type_decoded, Attrib#attrib.data_decoded]),
	 decode_path_attributes(Peer, Rest2, NewAttrib)
    end.
decode_path_attributes(Peer, Path_Attribute) ->
    decode_path_attributes(Peer, Path_Attribute, #attrib{}).

%%%%%%% MESSAGES parsing
-define(OPEN_MESSAGE, << Version:8, AS:16, Hold_time:16, ID:32, Optional_parameters_length:8, Optional_parameters/binary >>).
-define(UPDATE_MESSAGE,
<< Withdrawn_Routes_Length:16, Withdrawn_Routes:(Withdrawn_Routes_Length)/binary,
   Total_Path_Attribute_Length:16, Path_Attributes:(Total_Path_Attribute_Length)/binary,
   Network_Layer_Reachability_Information/binary >>).


% OPEN
parse_message(Peer, 1, ?OPEN_MESSAGE) when Peer#peer.state == init, Version == 4, Hold_time == 0 ; Hold_time >= 3, byte_size(Optional_parameters) >= Optional_parameters_length ->
    NewPeer = Peer#peer{state=open, hold_time = min(Peer#peer.hold_time, Hold_time), remote_as = AS, remote_id = ID},
    if NewPeer#peer.hold_time /= 0 -> spawn_link(?MODULE, keepalive, [NewPeer]) end,
    NewPeer;
% UPDATE
parse_message(Peer, 2, ?UPDATE_MESSAGE) when Peer#peer.state == established -> % Total_Path_Attribute_Length == 0 -> Network_Layer_Reachability_Information == <<>>
    WPeer = update(withdraw, Peer, Withdrawn_Routes, []),
    Attributes = decode_path_attributes(Peer, Path_Attributes),
    update(announce, WPeer, Network_Layer_Reachability_Information, Attributes);
% NOTIFICATION
parse_message(Peer, 3, <<Error_code:8, Error_subcode:8, Data/binary>>) ->
    io:format("got NOTIFICATION: ~p/~p, ~p~n", [Error_code, Error_subcode, Data]),
    exit();
% KEEPALIVE
parse_message(Peer, 4, <<>>) when Peer#peer.state /= init ->
    Peer#peer{state = case Peer#peer.state of
      open -> established;
      X -> X
    end}.


%%%%%%%%%%%%%% HEADER level functions

parse_header(Peer, << Marker:16/binary, Length:16, Type:8 >>) when Marker == <<?BGP_MARKER>>, Length >= ?BGP_HEADER_SIZE, Length =< 4096, Type >= 1, Type =< 4 ->
    Read_Length = Length - ?BGP_HEADER_SIZE,
    {ok, Data} = if Read_Length > 0 -> gen_tcp:recv(Peer#peer.sock, Read_Length);
		    true -> {ok, <<>>}
		 end,
%    io:format("type: ~p, data: ~p~n", [Type, Data]),
    parse_message(Peer, Type, Data).


read_header(Peer) ->
    {ok, Header} = gen_tcp:recv(Peer#peer.sock, ?BGP_HEADER_SIZE),
    read_header(parse_header(Peer#peer{last_message_received=now()}, Header)).

%%%%%%%%%%%%%%% MESSAGE level functions

send_message(Peer, Type, Data) ->
    Length = byte_size(Data) + ?BGP_HEADER_SIZE,
    Marker = <<?BGP_MARKER>>,
    Packet = <<Marker/binary, Length:16, Type:8, Data/binary>>,
%    io:format("send data: ~p~n", [Packet]),
    ok = gen_tcp:send(Peer#peer.sock, Packet),
    Peer.



send_open(Peer) ->
    Version = 4, AS = ?AS, Hold_time = ?HOLD_TIME, ID = ?ID, Optional_parameters_length = 0, Optional_parameters = <<>>,
    Open = ?OPEN_MESSAGE,
    send_message(Peer#peer{local_as = AS, hold_time = Hold_time, local_id = ID}, 1, Open).


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

connect(PeerAddress) ->
    {ok, Sock} = gen_tcp:connect(PeerAddress, 179, [binary, {active, false}, {packet, raw}]),
    Peer = #peer{sock=Sock, state=init, prefix_store_pid = spawn_link(prefix_store, prefix_store, [])},
    NewPeer = send_open(Peer),
    send_keepalive(NewPeer),
    read_header(NewPeer).


start_client() ->
    spawn(?MODULE, connect, [?HOST]).

