:-use_module(library(clpfd)).
:-use_module(library(lists)).

%dados
tree(2, 1).
tree(4, 2).
tree(2, 3).
tree(4, 4).
tree(6, 4).
tree(1, 5).
tree(2, 5).
tree(4, 6).

column(1, 3).
column(6, 1).

row(1, 2).
row(6, 2).


size(6).

%___________________________________________________
display(_, Y, S):-
	Y == S,
	nl.
	
display(X, Y, S):-
	X == S,
	X2 is 1,
	Y2 is Y + 1,
	nl, nl,
	display(X2, Y2, S).
	
display(X,Y,S):-
	tree(X,Y),
	write(' T '),
	X2 is X + 1,
	display(X2, Y, S).
	
display(X, Y, S):-
	write(' _ '), 
	X2 is X + 1,
	display(X2, Y, S).
	
display:-
	size(S),
	SS is S + 1,
	display(1,1,SS).
	
dis(L, 0):-
	nl,
	dis(L, 6).
dis([C|R], N):-
	write(C),
	write('  '),
	N2 is N - 1,
	dis(R, N2).
dis([], _).

	
%___________________________________________________
exactly(_, [], 0).
exactly(Number, [L|Ls], N):-
	Number #= L #<=> A,
	N #= M+A,
	exactly(Number, Ls, M).

equals(L):-
	findall(X-Y, tree(X, Y), Res),
	length(Res, NT),
	exactly(1, L, NT).
	
isIn(X, Y):-
	X > 0,
	Y > 0,
	size(S),
	X =< S,
	Y =< S.
	
nTentsTreeAux(X, Y, L, Res):-
	isIn(X, Y),
	Res = [X-Y|L].
	
nTentsTreeAux(_, _, L, Res):-
	Res = L.
	
nTentsTree(L, X, Y):-
	Xe is X + 1,
	nTentsTreeAux(Xe, Y, [], Res1),
	Xw is X - 1,
	nTentsTreeAux(Xw, Y, Res1, Res2),
	Yn is Y - 1,
	nTentsTreeAux(X, Yn, Res2, Res3),
	Ys is Y + 1,
	nTentsTreeAux(X, Ys, Res3, Res4),
	nTentsTreeRestriction(L, Res4).
	
nTentsTreeRestriction(L, [X1-Y1,X2-Y2]):-
	coordsToIndice(X1, Y1, I1),
	element(I1, L, C1),
	coordsToIndice(X2, Y2, I2),
	element(I2, L, C2),
	sum([C1, C2], #>, 0).
	
nTentsTreeRestriction(L, [X1-Y1,X2-Y2,X3-Y3]):-
	coordsToIndice(X1, Y1, I1),
	element(I1, L, C1),
	coordsToIndice(X2, Y2, I2),
	element(I2, L, C2),
	coordsToIndice(X3, Y3, I3),
	element(I3, L, C3),
	sum([C1, C2, C3], #>, 0).
	
nTentsTreeRestriction(L, [X1-Y1,X2-Y2,X3-Y3,X4-Y4]):-
	coordsToIndice(X1, Y1, I1),
	element(I1, L, C1),
	coordsToIndice(X2, Y2, I2),
	element(I2, L, C2),
	coordsToIndice(X3, Y3, I3),
	element(I3, L, C3),
	coordsToIndice(X4, Y4, I4),
	element(I4, L, C4),
	sum([C1, C2, C3, C4], #>, 0).
	
	
allTrees(_, []).

allTrees(L, [X-Y|Res]):-
	nTentsTree(L, X, Y),
	allTrees(L, Res).
	
	
indiceToCoords(I, X, Y):-
	size(S),
	Yz is div(I, S),
	Y is Yz + 1,
	X is mod(I, S).
	
coordsToIndice(X, Y, I):-
	size(S),
	Yz is Y - 1,
	Iz is Yz * S,
	I is Iz + X.
	

	
%____________________________________________________
tents:-
	size(S),
	SS is S * S,
	length(L, SS),
	domain(L, 0, 1),
	equals(L),
	/*
	nTentsTree(L, 2, 1),
	nTentsTree(L, 4, 2),
	nTentsTree(L, 2, 3),
	nTentsTree(L, 4, 4),
	nTentsTree(L, 6, 4),
	nTentsTree(L, 1, 5),
	nTentsTree(L, 2, 5),
	nTentsTree(L, 4, 6),
	*/
	findall(X-Y, tree(X, Y), Trees),
	allTrees(L, Trees),
	labeling([], L),
	dis(L, 6),
	write(L).
	
	