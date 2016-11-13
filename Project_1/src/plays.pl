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


movePiece(B, Yi, Xi, Dx, Dy, NB, Val):-
	index(B, Yi, Xi, Val),
	replace(B, Yi, Xi, 'o', B1),
	Xf is Xi + Dx,
	Yf is Yi + Dy,
	replace(B1, Yf, Xf, Val, NB).




%checkWin(Board, Char that represents the Node)
%yes if node is surrounded
%no if node isn't
checkEnemy('W', r).
checkEnemy('R', w).
checkEnemy(r, 'W').
checkEnemy(w, 'R').


checkLose(B, Node):-
	checkEnemy(Node, Enemy),
	index(B, Row, Col, Node),
	Row1 is Row +1,
	(Row1 < 9 -> (index(B, Row1, Col, Enemy); index(B, Row1, Col, '-'));true),
	Col1 is Col +1,
	(Col1 < 9 -> (index(B, Row, Col1, Enemy); index(B, Row, Col1, '-'));true),
	Row2 is Row -1,
	(Row2 >= 0 -> (index(B, Row2, Col, Enemy); index(B, Row2, Col, '-'));true),
	Col2 is Col -1,
	(Col2 >= 0 -> (index(B, Row, Col2, Enemy); index(B, Row, Col2, '-'));true),
	write(Node), write(' Loses'), nl .

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
	
group_lists(L1, L2, L3, L4,L5,L6,L7,L8, Res):-
	append(L1, L2, R),
	append(L3, L4, R2),
	append(L5, L6, R3),
	append(L7, L8, R4),
	append(R , R2, Temp1),
	append(R3, R4, Temp2),
	append(Temp1, Temp2, Temp),
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
	Y2 is Y +1,
	checkEmpty(B, X, Y2, Res4),
	group_lists(Res1, Res2, Res3, Res4, Res).


find_units(Ls, Y, X, Clf,Cl,IncX,IncY,D,F):- 			
	Y > -1, 
	Y < 9,
	X > -1, 
	X < 9,
	Y2 is Y+IncY,
	X2 is X+IncX,
	index(Ls,Y2,X2, D),
	append([X2,Y2],Cl,Cl2),
	find_units(Ls, Y2, X2,Clf,Cl2,IncX,IncY, D, F).
	
find_units(Ls, Y, X, Clf,Cl,IncX,IncY,_,F):- 			
	Y > -1, 
	Y < 9,
	X > -1, 
	X < 9,
	Y2 is Y+IncY,
	X2 is X+IncX,
	index(Ls,Y2,X2, F),
	append(Cl,[],Clf).
	
find_units(Ls, Y, X, Clf,Cl,IncX,IncY,D,F):- 			
	Y > -1, 
	Y < 9,
	X > -1, 
	X < 9,
	Y2 is Y+IncY,
	X2 is X+IncX,
	E \== D,
	E \== F,
	index(Ls,Y2,X2, E),
	find_units(Ls, Y2, X2,Clf,Cl,IncX,IncY, D, F).

find_units(_, _, _, Clf, Cl, _, _ , _ , _):- 
	append(Cl,[],Clf).
	
find_my_units(Ls, Cl,X,Y,D,F):- 
	find_units(Ls, Y, X,Cl1,[],+1,0,D,F),
	find_units(Ls, Y, X,Cl2,[],-1,0,D,F),
	find_units(Ls, Y, X,Cl3,[],0,+1,D,F),
	find_units(Ls, Y, X,Cl4,[],0,-1,D,F),
	find_units(Ls, Y, X,Cl5,[],+1,+1,D,F),
	find_units(Ls, Y, X,Cl6,[],-1,+1,D,F),
	find_units(Ls, Y, X,Cl7,[],+1,-1,D,F),
	find_units(Ls, Y, X,Cl8,[],-1,-1,D,F),
	group_lists(Cl1, Cl2, Cl3, Cl4,Cl5,Cl6,Cl7,Cl8, Cl)
	.
	
get_piece_coords(X, Y):-
	write('insert x coord of piece to move(0-8) ex:5. '),
	read(X),
	write('insert y coord of piece to move(0-8) ex:5. '),
	read(Y).
	
get_move_coords(X, Y):-
	write('insert x coord of place to go (0-8) ex:5. '),
	read(X),
	write('insert y coord of place to go (0-8) ex:5. '),
	read(Y).


write_turn(P):-
	write_space, 
	write_space,
	write_space, 
	write(P), 
	write(' turn'), 
	nl, nl .
	
player_cicle(B, _,'R', A):- A = B.
player_cicle(B, _,'W', A):- A = B.

player_cicle(B, P, _, A):- 
	clear_console(30),
	write_turn(P),
	draw(B),
	get_unit(P,D),
	get_enemy_unit(P,F),
	find_active_units(B, D, F, Res2),
	write('active units: '),
	write_list(Res2,4), nl,
	unit_checker(_,_,Res2,X,Y),
	nl, possible_moves(B, X, Y, Res), 
	write('possible moves for that piece: '),
	write(Res), nl,
	move_checker(_,_,Res, Xf, Yf, X, Y),
	Dx is Xf - X,
	Dy is Yf - Y,
	movePiece(B, Y, X, Dx, Dy, NB, Val),
	player_cicle(NB, P, Val, A).

get_unit('R',r).
get_unit('W',w).
get_enemy_unit('R',w).
get_enemy_unit('W',r).
	
move_checker(_, _,[], Xf, Yf, X, Y):-
	Xf = X,
	Yf = Y.
	
move_checker(Xt,Yt,Res, Xf, Yf, _, _):-
	get_move_coords(Xt, Yt),
	member([Xt,Yt], Res),
	Xf = Xt,
	Yf = Yt.
	
move_checker(_, _,Res, Xf, Yf, _, _):- 
	nl,
	write('Chose a pair of coordinates from the list above'),
	nl,
	move_checker(_,_, Res, Xf, Yf, _, _).


	
unit_checker(Xt,Yt,Res, Xf, Yf):-
	get_piece_coords(Xt,Yt), 
	member([Xt,Yt], Res),
	Xf = Xt,
	Yf = Yt.	
	
unit_checker(_,_,Res, Xf, Yf):-
	nl,
	write('Chose a pair of coordinates from the list above'),
	nl,
	unit_checker(_,_,Res, Xf, Yf).
	
	
find_active_units(B, D, F, Res):-
	get_unit(T1,D),
	get_unit(T2,F),
	index(B, Xw, Yw, T1),
	index(B, Xr, Yr, T2),
	find_my_units(B, Clw, Yw, Xw, D, F),
	find_my_units(B, Clr, Yr, Xr, D, F),
	append(Clw, Clr, Cl),
	append([[Yw, Xw]], Cl, Clf),
	list_to_set(Clf, Res).
	
write_list([], _):- nl .

write_list(L, 0):-
	nl, write('              '), 
	write_list(L, 4).

write_list([L|Ls], N):-
	write(L), write('  '),
	Nn is N -1,
	write_list(Ls, Nn).
	
%the game cicle
%game_cicle(Board).

game_cicle(_, _, _, 1).

game_cicle(B, N, E, 0):-
	clear_console(30),
	player_cicle(B, N, ' ', NB),
	(checkLose(NB, E) -> game_cicle(NB, N, E, 1) ; game_cicle(NB, E, N, 0)).



	
	
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
win:- board(B), checkLose(B, 'W').
moves(X, Y):- board(B), possible_moves(B, X, Y, Res), write(Res). 
start:- board(B), game_cicle(B, 'W', 'R', 0).
