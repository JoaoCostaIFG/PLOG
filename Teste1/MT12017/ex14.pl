:-use_module(library(lists)).

make_tree(id, TE, TD, tree(id, TE, TD)).

tree_getId(tree(id, _, _), id).
tree_getTE(tree(_, TE, _), TE).
tree_getTD(tree(_, _, TD), TD).

tree_setId(tree(_, TE, TD), id, tree(id, TE, TD)).

dendo(Dendo) :-
  Dendo = make_tree(1, make_tree(2, make_tree(5, make_tree(7, make_tree(8, 'australia', make_tree(9, make_tree(10, 'stahelena', 'anguila'), 'georgiadosul')), 'reinounido'), make_tree(6, 'servia', 'franca')), make_tree(3, make_tree(4, 'niger', 'india'), 'irlanda')), 'brasil').

distance(Node1, Node2, Dendograma, Dist) :-
  nodesFromRoot(Node1, Dendograma, NodePath1),
  nodesFromRoot(Node2, Dendograma, NodePath2),

nodesFromRoot(Node, Dendograma, Ret) :-
.
