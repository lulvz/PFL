:- use_module(library(lists)).

% Initialize the game board as a 4x8 matrix.
initial_board([
    [nonexistent, nonexistent, empty, empty, empty, empty, empty, nonexistent],
    [empty, empty, empty, white_square, brown_square, empty, empty, empty],
    [empty, empty, empty, white_square, brown_square, empty, empty, empty],
    [nonexistent, empty, empty, empty, empty, empty, nonexistent, nonexistent]
]).

% Define player colors.
player(white).
player(brown).

% Get the next player.
next_player(white, brown).
next_player(brown, white).

% Define game pieces.
piece(nonexistent).
piece(empty).
piece(white_round).
piece(white_square).
piece(brown_round).
piece(brown_square).
piece(red_anchor).

display_cell(nonexistent) :- write('  '). % Nonexistent cells are not displayed.
display_cell(empty) :- write('. ').
display_cell(white_round) :- write('w ').
display_cell(white_square) :- write('W ').
display_cell(brown_round) :- write('b ').
display_cell(brown_square) :- write('B ').
display_cell(red_anchor) :- write('A ').

display_board([]).
display_board([Row|Rest]) :-
    display_row(Row),
    nl,
    display_board(Rest).

display_row([]).
display_row([Cell|Rest]) :-
    display_cell(Cell),
    display_row(Rest).

% Get the piece at the given coordinates.
get_piece(Board, I, J, Piece) :-
    nth1(I, Board, Row),
    nth1(J, Row, Piece).

% Predicate to remove a piece at a specific row and column on the board.
remove_piece(Board, I, J, NewBoard) :-
    nth1(I, Board, Row),
    replace(Row, J, empty, NewRow),
    replace(Board, I, NewRow, NewBoard).

% Predicate to set a piece at a specific row and column on the board.
set_piece(Board, I, J, Piece, NewBoard) :-
    nth1(I, Board, Row),
    replace(Row, J, Piece, NewRow),
    replace(Board, I, NewRow, NewBoard).

% Helper predicate to replace an element in a list at a given position.
replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 1,
    I1 is I - 1,
    replace(T, I1, X, R).

%play
play_game :-
    initial_board(Board),
    display_board(Board),
    player(white),
    play(Board, white).

play(Board, Player) :-
    write(Player), write('\'s turn.'), nl,
    write('Enter the coordinates of the piece you want to move(eg. i-j.):'), nl,
    read(I-J),
    get_piece(Board, I, J, Piece),
    write('Enter the coordinates of the destination(eg. i-j.):'), nl,
    read(I2-J2),
    % just move for now
    % TODO: make sure the move is valid
    set_piece(Board, I2, J2, Piece, NewBoard),
    remove_piece(NewBoard, I, J, NewBoard1),
    display_board(NewBoard1),
    next_player(Player, NextPlayer),
    play(NewBoard1, NextPlayer).
