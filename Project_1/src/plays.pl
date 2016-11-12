:- use_module(library(lists)).

index(M, Row, Col, Val):-
	nth0(Row, M, MR),
	nth0(Col, MR, Val).

%movePiece(Board, Xcoord, Ycoord, DeltaX, DeltaY, Resulting board)
%moves a unit or node DeltaX horizontaly and DeltaY vertically
%	
replace( L , X , Y , Z , R ):-
  append(RowPfx,[Row|RowSfx],L),
  length(RowPfx,X) ,
  append(ColPfx,[_|ColSfx],Row) ,
  length(ColPfx,Y) ,
  append(ColPfx,[Z|ColSfx],RowNew) ,
  append(RowPfx,[RowNew|RowSfx],R).

movePiece(B, Yi, Xi, Dx, Dy):-
	index(B, Yi, Xi, Val),
	replace(B, Yi, Xi, 'o', B),
	Xf is Xi + Dx,
	Yf is Yi + Dy,
	replace(B, Yf, Xf, Val, B),
	draw(B).
	
	


%checkWin(Board, Char that represents the Node)
%yes if node is surrounded
%no if node isn't
checkEnemy('W', r).
checkEnemy('R', w).

checkWin(B, Node):-
	checkEnemy(Node, Enemy),
	index(B, Row, Col, Node),
	write(1),
	Row1 is Row +1,
	((Row1 < 9 -> index(B, Row1, Col, Enemy);true);(Row1 < 9 -> index(B, Row1, Col, '-');true)),
	write(2),
	Col1 is Col +1,
	((Col1 < 9 -> index(B, Row, Col1, Enemy);true);(Col1 < 9 -> index(B, Row, Col1, '-');true)),
	write(3),
	Row2 is Row -1,
	((Row2 >= 0 -> index(B, Row2, Col, Enemy);true);(Row2 >= 0 -> index(B, Row2, Col, '-');true)),
	write(4),
	Col2 is Col -1,
	((Col2 >= 0 -> index(B, Row, Col2, Enemy);true);(Col2 >= 0 -> index(B, Row, Col2, '-');true)),
	write(Node),
	write(' wins'), nl .

	
%possible_moves(Board, Xcoord, Ycoord, List of Pairs).
%List of Pairs has the coordinates to where the unit can go
%
list(X, R):- append([X], [], R).

group_list_into_pairs([], []).
group_list_into_pairs([A, B | Tail], [[A, B] | NewTail]) :-
    group_list_into_pairs(Tail, NewTail).
	
group_lists(L1, L2, L3, L4, Res):-
	append(L1, L2, R),
	append(L3, L4, R2),
	append(R, R2, Temp),
	group_list_into_pairs(Temp, Res).
	

checkEmpty(B, X, Y, Res):-
	index(B, Y, X, 'o'),
	list(X, R1),
	list(Y, R2),
	append(R1, R2, Res).
	
checkEmpty(_, _, _, Res):-
	append([], [], Res).

	
possible_moves(B, X, Y, Res):-
	X1 is X -1,
	checkEmpty(B, X1, Y, Res1),
	Y1 is Y -1,
	checkEmpty(B, X, Y1, Res2),
	X2 is X + 1,
	checkEmpty(B, X2, Y, Res3),
	Y2 is Y -1,
	checkEmpty(B, X, Y2, Res4),
	group_lists(Res1, Res2, Res3, Res4, Res).


	
	
bounds(-1, 0).
bounds(9, 8).
bounds(X,X).
%%%%%%%%
%testes%
%%%%%%%%

i(R, C, Val):- board(B), index(B, R, C, Val).
test:- board(B), replace(B, 1, 1, 'R', B2), display_board(B2, 1).
draw:- board(B), vertical_coords, nl, nl, display_board(B, 1).
draw(B):- vertical_coords, nl, nl, display_board(B, 1).
win:- board(B), checkWin(B, 'W').
moves(X, Y):- board(B), possible_moves(B, X, Y, Res), write(Res). 

