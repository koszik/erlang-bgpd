% Compare two routes
-module(bestpath).
-export([compare_routes/3]).
-include("peer.hrl").


get_origin_code(Origin) ->
    case Origin of
	igp -> 0;
	egp -> 1;
	incomplete -> 2
    end.

bigger(First, Second) ->
    if First > Second -> first;
       First < Second -> second;
       true    -> equal
    end.

smaller(First, Second) ->
    if First > Second -> second;
       First < Second -> first;
       true    -> equal
    end.


% prefer lower neighbor router ID
compare_router_id(_Config, Attrib1, Attrib2) ->
    case smaller(Attrib1#attrib.router_id, Attrib2#attrib.router_id) of
	equal -> log:err("exactly equal routes! ~w~n", [[Attrib1, Attrib2]]), first;
	R -> R
    end.

% prefer older route
compare_older_ebgp(Config, Attrib1, Attrib2) when Attrib1#attrib.ebgp, Attrib2#attrib.ebgp ->
    case smaller(Attrib1#attrib.received_at, Attrib2#attrib.received_at) of
	equal -> compare_router_id(Config, Attrib1, Attrib2);
	R -> R
    end;
compare_older_ebgp(Config, Attrib1, Attrib2) ->
    compare_router_id(Config, Attrib1, Attrib2).

% prefer closest igp neighbor - not implemented
compare_metric(Config, Attrib1, Attrib2) ->
    compare_older_ebgp(Config, Attrib1, Attrib2).

% prefer ebgp over ibgp  (ebgp = true | unknown)
compare_ebgp(Config, Attrib1, Attrib2) ->
    case smaller(Attrib1#attrib.ebgp, Attrib2#attrib.ebgp) of
	equal -> compare_metric(Config, Attrib1, Attrib2);
	R -> R
    end.

% lower MED; TODO: compare only if from same ebgp nbr?
compare_med(Config, Attrib1, Attrib2) ->
    if Attrib1#attrib.med == unknkown ; Attrib2#attrib.med == unknown -> compare_ebgp(Config, Attrib1, Attrib2);
	true -> case smaller(Attrib1#attrib.med, Attrib2#attrib.med) of
		    equal -> compare_ebgp(Config, Attrib1, Attrib2);
		    R -> R
		end
    end.

% lower origin
compare_origin(Config, Attrib1, Attrib2) ->
    case smaller(get_origin_code(Attrib1#attrib.origin), get_origin_code(Attrib2#attrib.origin)) of
	equal -> compare_med(Config, Attrib1, Attrib2);
	R -> R
    end.

as_path_length({as_sequence, S}, Acc) -> Acc + length(S);
as_path_length({as_set, _}, Acc) -> Acc + 1.
as_path_length(undefined) -> -1;
as_path_length(ASPath) -> lists:foldl(fun as_path_length/2, 0, ASPath).

% prefer locally originated routes; prefer shorter paths
compare_as_path(Config, Attrib1, Attrib2) ->
    case smaller(as_path_length(Attrib1#attrib.as_path), as_path_length(Attrib2#attrib.as_path)) of
	equal -> compare_origin(Config, Attrib1, Attrib2);
	R -> R
    end.

% higher local_pref
compare_local_pref(Config, Attrib1, Attrib2) ->
    if Attrib1#attrib.local_pref == unknkown ; Attrib2#attrib.local_pref == unknown -> compare_as_path(Config, Attrib1, Attrib2);
       true -> case bigger(Attrib1#attrib.local_pref, Attrib2#attrib.local_pref) of
		    equal -> compare_as_path(Config, Attrib1, Attrib2);
		    R -> R
		end
    end.

% higher weight
compare_weight(Config, Attrib1, Attrib2) ->
    case bigger(Attrib1#attrib.weight, Attrib2#attrib.weight) of
	equal -> compare_local_pref(Config, Attrib1, Attrib2);
	R -> R
    end.


compare_routes(Config, Attrib1, Attrib2) ->
    compare_weight(Config, Attrib1, Attrib2).

