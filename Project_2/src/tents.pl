:-use_module(library(clpfd)).
:-use_module(library(lists)).

example(1, 	[
				[_,_,_,_,_,_],
				[_,_,2,2,_,_],
				[_,_,2,_,_,_],
				[_,_,2,_,_,_],
				[_,_,_,_,_,_],
				[_,_,_,_,_,_]
			]).
			
example(2, [_,_,_,_,_,2,_,_,2]).
			
display_board([]):- nl.

display_board([L1|Ls]):-
	display_line(L1),
	display_board(Ls).
	
display_line([]):- nl, nl.
display_line([E|Es]):-
	translate(E, V),
	write(V), write('  '),
	display_line(Es).
	

dis(L, 0):-
	nl,
	dis(L, 6).
dis([C|R], N):-
	write(C),
	write('  '),
	N2 is N - 1,
	dis(R, N2).
dis([], _).

/*
a([A|R]):-
	A #= 0,
	a(R).
a([2,A|R]):-
	A #= 1,
	a(R).
	
a([2|R]):-
	a(R).
	
a([1|R]):-
	a(R).
a([]).
*/
exactly(_, [], 0).
exactly(Color, [L|Ls], N):-
	Color #= L #<=> A,
	N #= M+A,
	exactly(Color, Ls, M).
	
sum_list([], 0).
sum_list([H|T], Sum) :-
   sum_list(T, Rest),
   Sum is H + Rest.
	
d(X, Z):- X#<=>1, write('A'), Z is 0.
d(X, Z):- nonvar(X), write('B'), Z is X.
d(_, Z):-  write('C'), Z is 0.
	
f(L, N, M):-
	element(N, L, X),
	X == 2,
	N1 is N - M,
	
	element(N1, L, C1),
	d(C1, Z1),
	N4 is N + M,
	
	element(N4, L, C2),
	d(C2, Z2),
	N7 is N + 1,
	
	element(N7, L, C3),
	d(C3, Z3),
	N8 is N - 1,
	
	element(N8, L, C4),
	d(C4, Z4),
	sum_list([Z1, Z2, Z3, Z4], Res),
	ResT #= Res + 1,
	sum([C1, C2, C3, C4], #=, ResT),
	nl.
f(_,_,_).
	
do_all(_, 0, _).
do_all(L, N, M):-
	f(L, N, M),
	N2 is N - 1,
	do_all(L, N2, M).
	
tents(L):-
	domain(L, 0, 2),
	%a(L),
	%f(L, 22, 6),
	do_all(L, 36, 6),
	%exactly(1, L, 9),
	/*element(1, L, C1),
	element(2, L, C2),
	element(3, L, C3),
	element(4, L, C4),
	element(5, L, C5),
	element(6, L, C6),
	sum([C1, C2, C3, C4, C5, C6], #=, 1),*/
	labeling([], L).
	

test:-example(1,L), append(L, L1), tents(L1), write(L1), nl, dis(L1, 6).