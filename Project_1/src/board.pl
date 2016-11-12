:- use_module(library(lists)).
:- consult('plays.pl').
:-consult('menus.pl').

board([	[-,-,r,r,'R',r,r,-,-],
		[-,o,o,r,r,r,o,o,-],
		[o,o,o,o,r,o,o,o,o],
		[o,o,o,o,o,o,o,o,o],
		[o,o,o,o,o,o,o,o,o],
		[o,o,o,o,w,o,o,o,o],
		[o,o,o,o,w,o,o,o,o],
		[-,o,o,w,w,w,o,o,-],
		[-,-,w,w,'W',w,w,-,-]
		]).	
		
vertical_coords:- write('   1  2  3  4  5  6  7  8  9').
vertical_lines(1):- write('       / |  |  |  |  | ' \ '    ').
vertical_lines(2):- write('    / |  |  |  |  |  |  | ' \ ' ').
vertical_lines(3):- write('   |  |  |  |  |  |  |  |  |').
vertical_lines(4):- write('   |  |  |  |  |  |  |  |  |').
vertical_lines(5):- write('   |  |  |  |  |  |  |  |  |').
vertical_lines(6):- write('   |  |  |  |  |  |  |  |  |').
vertical_lines(7):- write('    ' \ ' |  |  |  |  |  |  | /  ').
vertical_lines(8):- write('       ' \ ' |  |  |  |  | /     ').
vertical_lines(9):- write('').



horizontal_lines(1, ['1  ','  ','  ','--','--','--','--','  ','  ']).
horizontal_lines(2, ['2  ','  ','--','--','--','--','--','--','  ']).
horizontal_lines(3, ['3  ','--','--','--','--','--','--','--','--']).
horizontal_lines(4, ['4  ','--','--','--','--','--','--','--','--']).
horizontal_lines(5, ['5  ','--','--','--','--','--','--','--','--']).
horizontal_lines(6, ['6  ','--','--','--','--','--','--','--','--']).
horizontal_lines(7, ['7  ','--','--','--','--','--','--','--','--']).
horizontal_lines(8, ['8  ','  ','--','--','--','--','--','--','  ']).
horizontal_lines(9, ['9  ','  ','  ','--','--','--','--','  ','  ']).


	
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
	translate(U,B),
	write(B),
	translate(E,V),
	write(V),
	display_line(Es, Us).
	
display_line([],[]).

translate(x,'|  ').
translate(-, ' ').

translate(X,X). 
