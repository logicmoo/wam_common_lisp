

/*************************************************************************************
 * shortest_paths.pl
 *
 * 17/4/00, Neil Smith
 *
 * Uses Dijkstra's algorithm for finding the shortest paths between nodes on a graph
 * Will work on both directed and undirected graphs
 *
 * shortest_paths(+Start, +AdjacencyPred, -Paths)
 *   true if Paths is the set of all shortest paths, starting at Start, on the graph
 *   defined by AdjacenyPred
 *
 * all_pairs_shortest_paths(AdjacencyPred, -Paths)
 *   true if Paths is the set of all shortest paths in the graph defined by AdjacenyPred
 *
 * Paths are defined as a list of 4-tuples: 
 *       path(Start, End, Cost, Nodes)
 *   where Nodes is the list of nodes on the path, in reverse order, including the Start
 *   and End
 * 
 * Note that the graphs are not defined here: instead, they must be passed a predicate that
 * is used with call/4: 
 *       call(+AdjacenyPred, ?Node1, ?Node2, ?Cost)
 * AdjacenyPred should enumerate through all directly adjacent nodes in the graph.  
 * It should be able to generate all neighbours of a given Node1 (call(+, +, ?, ?)),
 * and all all nodes in a graph (call(+, -, ?, ?)).
 * 
 *************************************************************************************/

:- module(shortest_paths,
      [ shortest_paths/3,
	all_pairs_shortest_paths/2
      ]).


:- ensure_loaded((utils_higher_order)).		% for filter/3, call/4, foldl/4
%:- ensure_loaded((utils_oset)).	% for insert_merge/4
:- ensure_loaded((utils_set)).		% for select/3


shortest_paths(From, Adjacency, Paths):-
	shortest_paths([path(From, From, 0, [From])], Adjacency, [], Paths).

% shortest_paths(OpenNodes, Adjacency, ClosedNodes0, ClosedNodes).
% each 'node' is the best-so-far path (closed nodes are the best possible),
%   a tuple of
%      path(Start, End, Cost, [Vertex])
%   where [Vertex] is the path (in reverse order) from Start to End with Cost
% OpenNodes are kept in sorted order by Cost

shortest_paths([], _, Paths, Paths).
shortest_paths([path(Start, Current, Cost, Path)|OpenNodes0], Adjacency, ShortestPaths0, ShortestPaths):-
	one (	setof(EdgeCost - Neighbour, call(Adjacency, Current, Neighbour, EdgeCost), CostedNeighbours0)
	    ;	CostedNeighbours0 = []  ),
	filter(CostedNeighbours0, [_ - X]^(\+ member(path(Start, X, _, _), ShortestPaths0)), CostedNeighbours),
	relax(CostedNeighbours, Start, Current, Cost, Path, OpenNodes0, OpenNodes),
	shortest_paths(OpenNodes, Adjacency, [path(Start, Current, Cost, Path)|ShortestPaths0], ShortestPaths).


relax([], _, _, _, _, Open, Open).
relax([Cost - Neighbour | CostedNeighbours], Start, Current, PathCost, Path, OpenNodes0, OpenNodes):-
	NewPathCost is PathCost + Cost,
	(	select(path(Start, Neighbour, OtherCost, _OtherPath), OpenNodes0, RemainingOpen)
	->	(	NewPathCost < OtherCost
			->	insert_merge(RemainingOpen, path(Start, Neighbour, NewPathCost, [Neighbour|Path]), 
					compare_paths_by_cost, OpenNodes1)
			;	OpenNodes1 = OpenNodes0	)
	;	insert_merge(OpenNodes0, path(Start, Neighbour, NewPathCost, [Neighbour|Path]), 
			compare_paths_by_cost, OpenNodes1)	),
	relax(CostedNeighbours, Start, Current, PathCost, Path, OpenNodes1, OpenNodes).


compare_paths_by_cost(Rel, path(_, _, Cost1, _), path(_, _, Cost2, _)):- 
	compare(Rel, Cost1, Cost2).


all_pairs_shortest_paths(Adjacency, AllPaths):-
	setof(Node, Node2^Cost^call(Adjacency, Node, Node2, Cost), Nodes),
	foldl(Nodes, 
		[ExistingPaths, Node, Paths]^
			(shortest_paths([path(Node, Node, 0, [Node])], Adjacency, ExistingPaths, Paths)), 
		[], 
		AllPaths).
