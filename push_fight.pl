:- use_module(library(lists)).

% Initialize the game board as a 4x8 matrix.
initial_board([
    [nonexistent, nonexistent, nonexistent, wall,  wall,         wall,         wall,  wall,        nonexistent, nonexistent],
    [nonexistent, nonexistent, nonexistent, empty, empty,        empty,        empty, empty,       nonexistent, nonexistent],
    [nonexistent, empty,       empty,       empty, white_square, brown_square, empty, empty,       empty,       nonexistent],
    [nonexistent, empty,       empty,       empty, white_square, brown_square, empty, empty,       empty,       nonexistent],
    [nonexistent, nonexistent, empty,       empty, empty,        empty,        empty, nonexistent, nonexistent, nonexistent],
    [nonexistent, nonexistent, wall,        wall,  wall,         wall,         wall,  nonexistent, nonexistent, nonexistent]
]).

% Define player colors.
player(white).
player(brown).

% Get the next player.
next_player(white, brown).
next_player(brown, white).

% Define game pieces.
piece(nonexistent).
piece(wall).
piece(empty).
piece(white_round).
piece(white_square).
piece(brown_round).
piece(brown_square).
piece(red_anchor).

% Define pieces that belong to players.
player_pieces(white, [white_round, white_square]).
player_pieces(brown, [brown_round, brown_square]).

% Predicate to check if a cell is empty or an anchor.
is_empty(empty).

% Predicate to check if the move is valid.
is_valid_move(Board, I, J, I2, J2, Player) :-
    get_piece(Board, I, J, Piece),
    get_piece(Board, I2, J2, Destination),
    player_piece(Player, Piece),
    is_empty(Destination),
    is_valid_move_direction(Piece, I, J, I2, J2).

% Predicate to check if a move is in a valid direction.
is_valid_move_direction(white_round, I, J, I2, J2) :-
    (I2 is I + 1, (J2 is J + 1 ; J2 is J - 1)).
is_valid_move_direction(brown_round, I, J, I2, J2) :-
    (I2 is I - 1, (J2 is J + 1 ; J2 is J - 1)).
is_valid_move_direction(white_square, I, J, I2, J2) :-
    (abs(I2 - I) =:= 1, abs(J2 - J) =:= 1).
is_valid_move_direction(brown_square, I, J, I2, J2) :-
    (abs(I2 - I) =:= 1, abs(J2 - J) =:= 1).

display_cell(nonexistent) :- write('  '). % Nonexistent cells are not displayed.
display_cell(wall) :- write('# ').
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

% Get the piece at the given coordinates.
get_piece(Board, I, J, Piece) :-
    nth1(I, Board, Row),
    nth1(J, Row, Piece).

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
    write('You selected: '), write(Piece), nl,
    % Check if the piece belongs to the player.
    player_pieces(Player, PlayerPieces),
    write('Player pieces: '), write(PlayerPieces), nl,
    (member(Piece, PlayerPieces) ->
        write('Enter the coordinates of the destination(eg. i-j.):'), nl,
        read(I2-J2),
        (is_valid_move(Board, I, J, I2, J2, Player) ->
            % Valid move, update the board
            set_piece(Board, I2, J2, Piece, NewBoard),
            remove_piece(NewBoard, I, J, NewBoard1),
            display_board(NewBoard1),
            next_player(Player, NextPlayer),
            play(NewBoard1, NextPlayer);
            write('Invalid move. Try again.'), nl,
            play(Board, Player)
        );
        write('You cannot move this piece. Try again.'), nl,
        play(Board, Player)
    ).
