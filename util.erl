-module(util).
-export([prefix_to_list/2, ip_to_list/2]).

ip_to_list_v4([], 0, Acc) -> Acc;
ip_to_list_v4([], 4, _Acc) -> "0.0.0.0";
ip_to_list_v4([], N, Acc) -> ip_to_list_v4([], N-1, Acc++".0");
ip_to_list_v4([N|Rest], 4, _Acc) -> ip_to_list_v4(Rest, 3, integer_to_list(N));
ip_to_list_v4([N|Rest], Rem, Acc) -> ip_to_list_v4(Rest, Rem-1, Acc++"."++integer_to_list(N)).

% 1 = ipv4
ip_to_list(1, Prefix) ->
    ip_to_list_v4(binary:bin_to_list(Prefix), 4, "").

prefix_to_list(AFI, {Prefix, Length}) ->
    ip_to_list(AFI, Prefix) ++ "/"++integer_to_list(Length).
