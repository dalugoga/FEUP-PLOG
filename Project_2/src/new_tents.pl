:-use_module(library(clpfd)).
:-use_module(library(lists)).
:-use_module(library(sets)).
:-use_module(library(between)).

%dados
/*
%exemple 1

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
*/

/*
%exemple 2

tree(3, 1).
tree(6, 1).
tree(1, 2).
tree(2, 3).
tree(5, 3).
tree(4, 5).
tree(1, 6).
tree(4, 6).
tree(7, 7).

column(1, 1).
column(2, 2).
column(3, 1).
column(4, 2).
column(5, 1).
column(6, 1).
column(7, 1).

row(1, 2).
row(2, 1).
row(3, 1).
row(4, 1).
row(5, 1).
row(6, 1).
row(7, 2).

size(7).
*/


%exemple 3

tree(3, 1).
tree(5, 1).
tree(10, 1).
tree(2, 2).
tree(11, 2).
tree(7, 3).
tree(12, 3).
tree(1, 4).
tree(3, 4).
tree(9, 4).
tree(11, 5).
tree(2, 7).
tree(6, 7).
tree(9, 7).
tree(12, 7).
tree(6, 8).
tree(8, 8).
tree(11, 8).
tree(4, 9).
tree(8, 9).
tree(12, 9).
tree(1, 10).
tree(6, 10).
tree(11, 10).
tree(3, 11).
tree(8, 11).
tree(11, 11).
tree(4, 12).

column(1, 3).
column(2, 1).
column(3, 4).
column(4, 0).
column(5, 3).
column(6, 3).
column(7, 1).
column(8, 2).
column(9, 3).
column(10, 2).
column(11, 1).
column(12, 5).

row(1, 2).
row(2, 3).
row(3, 2).
row(4, 1).
row(5, 3).
row(6, 2).
row(7, 1).
row(8, 4).
row(9, 2).
row(10, 4).
row(11, 1).
row(12, 3).

size(12).




%___________________________________________________

writeRowClue(Y):-
	row(Y, Value),
	write(Value).
	
writeRowClue(_).


writeColumnClue(X, SS):-
	X == SS.

writeColumnClue(X, SS):-
	column(X, Value),
	write('  '),
	write(Value),
	write(' '),
	X1 is X + 1,
	writeColumnClue(X1, SS).
	
writeColumnClue(X, SS):-
	write('    '),
	X1 is X + 1,
	writeColumnClue(X1, SS).

	
display(_, _, Y, SS):-
	Y == SS.

display(L, X, Y, SS):-
	X == SS,
	size(S),
	Y1 is Y + 1,
	write('| '),
	writeRowClue(Y),
	writeDivision(S),
	display(L, 1, Y1, SS).
	
display(L, X, Y, SS):-
	tree(X, Y),
	write('|'),
	write(' T '),
	X1 is X + 1,
	display(L, X1, Y, SS).
	
display(L, X, Y, SS):-
	coordsToIndice(X, Y, I),
	nth1(I, L, Value),
	Value == 1,
	write('|'),
	write('/l'),
	write(''\''),
	X1 is X + 1,
	display(L, X1, Y, SS).
	
display(L, X, Y, SS):-
	coordsToIndice(X, Y, I),
	nth1(I, L, Value),
	Value == 0,
	write('|'),
	write(' '),
	write('  '),
	X1 is X + 1,
	display(L, X1, Y, SS).
	

writeDivisionNTimes(0).
	
writeDivisionNTimes(N):-
	write('---+'),
	N1 is N - 1,
	writeDivisionNTimes(N1).
	
writeDivision(N):-
	nl,
	write('+'),
	writeDivisionNTimes(N),
	nl.
	
displayBoard(L):-
	size(S),
	SS is S + 1,
	writeDivision(S),
	display(L, 1, 1, SS),
	writeColumnClue(1, SS),
	nl, nl, nl.
	
displaySolvedBoard(L):-
	nl,
	write('Solved Board'), 
	nl,
	displayBoard(L).
	
displayEmptyBoard:-
	nl,nl,
	write('Board to solve'), 
	nl,
	size(S),
	SSS is S * S,
	findall(0, between(1, SSS, _), L),
	displayBoard(L).
	
	
	

%___________________________________________________
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
	
%FIRST RESTRICTION
exactly(_, [], 0).
exactly(Number, [L|Ls], N):-
	Number #= L #<=> A,
	N #= M+A,
	exactly(Number, Ls, M).

equals(L):-
	findall(X-Y, tree(X, Y), Res),
	length(Res, NT),
	exactly(1, L, NT).
	
%SECOND RESTRICTION
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

	
%THIRD RESTRICTION
searchRow(_, X, _, Lt, Lc, SS):-
	X == SS,
	Lc = Lt.
	
searchRow(L, X, Y, Lt, Lc, SS):-
	coordsToIndice(X, Y, Index),
	element(Index, L, C),
	Xn is X + 1,
	searchRow(L, Xn, Y, [C|Lt], Lc, SS).

searchColumn(_, _, Y, Lt, Lc, SS):-
	Y == SS,
	Lc = Lt.
	
searchColumn(L, X, Y, Lt, Lc, SS):-
	coordsToIndice(X, Y, Index),
	element(Index, L, C),
	Yn is Y + 1,
	searchColumn(L, X, Yn, [C|Lt], Lc, SS).
	
searchAllRows(_, [], _).

searchAllRows(L, [Row-Value|Lrows], SS):-
	searchRow(L, 1, Row, [], Lc, SS),
	sum(Lc, #=, Value),
	searchAllRows(L, Lrows, SS).
	

searchAllColumns(_, [], _).

searchAllColumns(L, [Column-Value|Lcols], SS):-
	searchColumn(L, Column, 1, [], Lc, SS),
	sum(Lc, #=, Value),
	searchAllColumns(L, Lcols, SS).
	
gridValuesRestriction(L):-
	size(S),
	SS is S + 1,
	findall(Row-Value, row(Row, Value), Rows),
	findall(Column-Value, column(Column, Value), Columns),
	searchAllRows(L, Rows, SS),
	searchAllColumns(L, Columns, SS).
	
	
%FOURTH RESTRICTION
search2x2(_, _, Y, S):-
	Y == S.
	
search2x2(L, X, Y, S):-
	X == S,
	Y1 is Y + 1,
	search2x2(L, 1, Y1, S).

search2x2(L, X, Y, S):-
	Y1 is Y + 1,
	X1 is X + 1,
	coordsToIndice(X, Y, I1),
	coordsToIndice(X1, Y, I2),
	coordsToIndice(X, Y1, I3),
	coordsToIndice(X1, Y1, I4),
	element(I1, L, C1),
	element(I2, L, C2),
	element(I3, L, C3),
	element(I4, L, C4),
	sum([C1, C2, C3, C4], #<, 2),
	search2x2(L, X1, Y, S).
	
%BOB ROSS RESTRICTION
convertCoordsToCells(_, [], Lt, Lf):-
	Lf = Lt.
	
convertCoordsToCells(L, [X-Y|Res], Lt, Lf):-
	coordsToIndice(X, Y, Index),
	element(Index, L, C),
	convertCoordsToCells(L, Res, [C|Lt], Lf).

findFriendsRestriction(L, X1, Y1, X2, Y2):-
	X11 is X1 + 1,
	X12 is X1 - 1,
	Y11 is Y1 + 1,
	Y12 is Y1 - 1,
	X21 is X2 + 1,
	X22 is X2 - 1,
	Y21 is Y2 + 1,
	Y22 is Y2 - 1,
	nTentsTreeAux(X11, Y1, [], Res1),
	nTentsTreeAux(X12, Y1, Res1, Res2),
	nTentsTreeAux(X1, Y11, Res2, Res3),
	nTentsTreeAux(X1, Y12, Res3, Res4),
	nTentsTreeAux(X21, Y2, Res4, Res5),
	nTentsTreeAux(X22, Y2, Res5, Res6),
	nTentsTreeAux(X2, Y21, Res6, Res7),
	nTentsTreeAux(X2, Y22, Res7, Res8),
	list_to_set(Res8, Res),
	convertCoordsToCells(L, Res, [], Lf),
	sum(Lf, #>, 1).
	
	
findFriends(_, _, _, []).

findFriends(L, X, Y, [Xf-Yf|Trees]):-
	DeltaX is Xf - X,
	DeltaY is Yf - Y,
	AbsDeltaX is abs(DeltaX),
	AbsDeltaY is abs(DeltaY),
	DeltaSum is AbsDeltaX + AbsDeltaY,
	DeltaSum == 2,
	findFriendsRestriction(L, X, Y, Xf, Yf),
	findFriends(L, X, Y, Trees).
	
findFriends(L, X, Y, [_|Trees]):-
	findFriends(L, X, Y, Trees).
	
	
findAllFriends(_, [], _).
	
findAllFriends(L, [X-Y|Tr], Trees):-
	findFriends(L, X, Y, Trees),
	findAllFriends(L, Tr, Trees).
	
	

%SPECIAL RESTRICTION
noTentsInTrees(_, []).
noTentsInTrees(L, [X-Y|Trees]):-
	coordsToIndice(X, Y, Index),
	element(Index, L, C),
	C #= 0,
	noTentsInTrees(L, Trees).

	
%____________________________________________________
tents:-
	displayEmptyBoard,
	statistics(walltime, [_ | [_]]),
	size(S),
	SSS is S * S,
	length(L, SSS),
	domain(L, 0, 1),
	tentsSolver(L, S),
	labeling([], L),
	statistics(walltime, [_ | [ExecutionTime]]),
	displaySolvedBoard(L),
	writeExecutionTime(ExecutionTime),
	fd_statistics.

tentsSolver(L, S):-
	equals(L),
	findall(X-Y, tree(X, Y), Trees),
	allTrees(L, Trees),
	gridValuesRestriction(L),
	noTentsInTrees(L, Trees),
	search2x2(L, 1, 1, S),
	findAllFriends(L, Trees, Trees).
	
writeExecutionTime(ExecutionTime):-
	write('Execution took '), 
	write(ExecutionTime), 
	write(' ms.'), 
	nl, nl.
	