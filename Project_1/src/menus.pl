
write_space:- write('    ').

menu_ascii_art:-
	write_space, write('  _   _  ____  _____  ______  _____ '), nl,
	write_space, write(' | '\' | |/ __ '\'|  __ '\'|  ____|/ ____|'), nl,
	write_space, write(' |  '\'| | |  | | |  | | |__  | (___  '), nl,
	write_space, write(' | . ` | |  | | |  | |  __|  '\'___ '\' '), nl,
	write_space, write(' | |'\'  | |__| | |__| | |____ ____) |'), nl,
	write_space, write(' |_| '\'_|'\'____/|_____/|______|_____/ '), nl,
	write_space, write('                                    '), nl .
	
intro_menu:-
	menu_ascii_art,
	write_space, write('   DANIEL GARRIDO     NUNO FREITAS'),nl,
	nl,
	write_space, write('              PRESS ENTER              '), nl,
	get_char(_),
	text_menu.
	
clear_console(0):- nl.
clear_console(X):- nl, X1 is X - 1, clear_console(X1).

read_char(Message, X):-
  write_space, write(Message), nl,
  write_space, write('> '),
  get_char(X).
  
text_menu:-
	clear_console(30), nl,
	menu_ascii_art,
	write_space, write('1 - jogar'),nl,
	write_space, write('2 - sair'),nl,
	read_char('Enter a valid option', C),
	menu_handler(C).	
	
menu_handler('1'):- write('menu1').
menu_handler('2'):- write('EXITING..').
menu_handler(_):- text_menu.
	
	
nodes:- intro_menu.


  
