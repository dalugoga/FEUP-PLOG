:- use_module(library(lists)).

board([	[-,-,r,r,'R',r,r,-,-],
		[-,o,o,r,r,r,o,o,-],
		[o,o,o,o,r,o,o,o,o],
		[o,o,o,o,w,o,o,o,o],
		[o,o,o,o,o,o,o,o,o],
		[o,o,o,o,w,o,o,o,o],
		[o,o,o,o,w,o,o,o,o],
		[-,o,o,w,w,w,o,o,-],
		[-,-,w,w,'W',w,w,-,-]
		]).

vertical_lines(1):- write('    / |  |  |  |  | ' \ '    ').
vertical_lines(2):- write(' / |  |  |  |  |  |  | ' \ ' ').
vertical_lines(3):- write('|  |  |  |  |  |  |  |  |').
vertical_lines(4):- write('|  |  |  |  |  |  |  |  |').
vertical_lines(5):- write('|  |  |  |  |  |  |  |  |').
vertical_lines(6):- write('|  |  |  |  |  |  |  |  |').
vertical_lines(7):- write(' ' \ ' |  |  |  |  |  |  | /  ').
vertical_lines(8):- write('    ' \ ' |  |  |  |  | /     ').
vertical_lines(9):- write('').


horizontal_lines(1, ['  ','  ','--','--','--','--','  ','  ',' ']).
horizontal_lines(2, ['  ','--','--','--','--','--','--','  ',' ']).
horizontal_lines(3, ['--','--','--','--','--','--','--','--',' ']).
horizontal_lines(4, ['--','--','--','--','--','--','--','--',' ']).
horizontal_lines(5, ['--','--','--','--','--','--','--','--',' ']).
horizontal_lines(6, ['--','--','--','--','--','--','--','--',' ']).
horizontal_lines(7, ['--','--','--','--','--','--','--','--',' ']).
horizontal_lines(8, ['  ','--','--','--','--','--','--','  ',' ']).
horizontal_lines(9, ['  ','  ','--','--','--','--','  ','  ',' ']).


draw:- board(B), display_board(B, 1).

display_board([L1|Ls], X):-
	horizontal_lines(X,U),
	display_line(L1,U),
	nl,
	vertical_lines(X),
	Xi is X + 1,
	nl,
	display_board(Ls, Xi).
	
display_board([], 10):-nl.

display_line([E|Es], [U|Us]):-
	translate(E,V),
	write(V),
	translate(U,B),
	write(B),
	display_line(Es, Us).
	
display_line([],[]).

index(Matrix, Row, Col, Value):-
  nth0(Row, Matrix, MatrixRow),
  nth0(Col, MatrixRow, Value).

  
apply(P,LArgs) :- G =.. [P|LArgs], G. 

find_units(Ls, Y, X, Yi, Xi, Cl,IncX,IncY,D,F,Cn):- Cn < 10,Y > -1, Y < 9,X > 0, X < 10,Y2 is Y+IncY,X2 is X+IncX,index(Ls,Y2,X2, D), find_units(Ls, Y2, X2,Yi,Xi, [[X2|Y2]|Cl],IncX,IncY, D, F, Cn).
                                                     
find_units(Ls, Y, X, Yi, Xi,Cl,IncX,IncY,D,F,Cn):- Cn < 10,Y > -1, Y < 9,X > 0, X < 10,Y2 is Y+IncY,X2 is X+IncX, index(Ls,Y2,X2, F), write(Cl),nl,
										(IncX ==  0 -> 
											(IncY ==  1 -> X3 is 1, Y3 is IncY; X3 is -1, Y3 is IncY);
											(IncX ==  1 ->
												(IncY == -1 -> X3 is 0, Y3 is IncY; Y3 is IncY -1, X3 is IncX);
												(IncX == -1 -> 
													(IncY ==  1 -> X3 is 0, Y3 is IncY; Y3 is IncY +1, X3 is IncX)
												)
											)
										),Cn2 is Cn+1, 
										find_units(Ls, Yi,Xi,Yi,Xi, Cl,X3,Y3,D,F,Cn2).
                                                     
find_units(Ls, Y, X, Yi, Xi,Cl,IncX,IncY,D,F,Cn):- Cn < 10,Y > -1, Y < 9,X > 0, X < 10,Y2 is Y+IncY,X2 is X+IncX,E \== D,E \== F,index(Ls,Y2,X2, E),find_units(Ls, Y2, X2, Yi, Xi, Cl,IncX,IncY, D, F, Cn).

find_units(Ls, Y, X, Yi, Xi,Cl,IncX,IncY,D,F,Cn):-  Cn < 10,nl,
										(IncX ==  0 -> 
											(IncY ==  1 -> X3 is 1, Y3 is IncY; X3 is -1, Y3 is IncY);
											(IncX ==  1 ->
												(IncY == -1 -> X3 is 0, Y3 is IncY; Y3 is IncY -1, X3 is IncX);
												(IncX == -1 -> 
													(IncY ==  1 -> X3 is 0, Y3 is IncY; Y3 is IncY +1, X3 is IncX)
												)
											)
										),
										Cn2 is Cn+1, 
										find_units(Ls, Yi, Xi,Yi,Xi, Cl,X3,Y3,D,F,Cn2).
										

find_units(Ls, Y, X, Yi, Xi,Cl,IncX,IncY,D,F,Cn):- write(Cl),nl.

find_my_units(Ls, Cl,X,Y,D,F):- find_units(Ls, Y, X,[Cl1|Cl],+1,0,D,F),
								find_units(Ls, Y, X,[Cl2|Cl],-1,0,D,F),
								find_units(Ls, Y, X,[Cl3|Cl],0,+1,D,F),
								find_units(Ls, Y, X,[Cl4|Cl],0,-1,D,F),
								find_units(Ls, Y, X,[Cl5|Cl],+1,+1,D,F),
								find_units(Ls, Y, X,[Cl6|Cl],-1,+1,D,F),
								find_units(Ls, Y, X,[Cl7|Cl],+1,-1,D,F),
								find_units(Ls, Y, X,[Cl8|Cl],-1,-1,D,F), 
								append(Cl1,Cl,Cl),
								append(Cl2,Cl,Cl),
								append(Cl3,Cl,Cl),
								append(Cl4,Cl,Cl),
								append(Cl5,Cl,Cl),
								append(Cl6,Cl,Cl),
								append(Cl7,Cl,Cl),
								append(Cl8,Cl,Cl),
								write(Cl1),nl.
								
								

emptyList([]).
								
translate(x,'|  ').
translate(-, ' ').

translate(X,X). 
