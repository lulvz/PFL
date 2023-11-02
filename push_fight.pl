:- use_module(library(lists)).

% Initialize the game board as a 4x8 matrix.
initial_board([
    [nonexistent, nonexistent, nonexistent, wall,  wall,         wall,         wall,  wall,        nonexistent, nonexistent],
    [nonexistent, nonexistent, nonexistent, empty, empty,        empty,        empty, empty,       nonexistent, nonexistent],
    [nonexistent, empty,       empty,       empty, white_square, brown_round, empty, empty,       empty,       nonexistent],
    [nonexistent, empty,       empty,       empty, white_round,  brown_square, empty, empty,       empty,       nonexistent],
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
is_empty(nonexistent). % Adding nonexistent as valid since you might need to check boundaries.

% Predicate to check if the move is valid.
is_valid_move(Board, I, J, I2, J2, Player) :-
    %  Check if the piece belongs to the player
    player_pieces(Player, PlayerPieces),
    get_piece(Board, I, J, Piece),
    member(Piece, PlayerPieces),
    get_piece(Board, I2, J2, Destination),
    is_empty(Destination),
    % is_valid_move_direction(Piece, I, J, I2, J2).
    write('Validating move: '), write(Piece), write(' from '), write(I), write('-'), write(J), write(' to '), write(I2), write('-'), write(J2), nl,
    is_clear_path(Board, I, J, I2, J2),
    write('Clear path: '), write(I), write('-'), write(J), write(' to '), write(I2), write('-'), write(J2), nl.

is_valid_move_direction(_, _, _, _, _). % Any piece can move in any direction.

% Predicate to check if there is a clear path between two positions on the board.
is_clear_path(Board, I1, J1, I2, J2) :-
    bfs(Board, [(I1, J1)], [], (I2, J2)).

% BFS using a queue and a set of visited nodes.
bfs(_, [], _, _) :-
    !, fail. % Queue is empty; target not found.
bfs(Board, [(I, J) | Rest], Visited, (I2, J2)) :-
    (I = I2, J = J2) % Found the target.
    ;
    % Explore neighbors
    findall((NI, NJ), 
        (valid_neighbor(Board, I, J, NI, NJ),
         \+ member((NI, NJ), Visited)), 
    Neighbors),
    append(Rest, Neighbors, NewQueue), % Add neighbors to the queue
    bfs(Board, NewQueue, [(I, J) | Visited], (I2, J2)).

% Valid neighboring cells for movement.
valid_neighbor(Board, I, J, NI, NJ) :-
    (NI is I + 1, NJ is J;
     NI is I - 1, NJ is J;
     NI is I, NJ is J + 1;
     NI is I, NJ is J - 1),
    get_piece(Board, NI, NJ, Piece),
    is_empty(Piece).

% Predicate to check if a move is in a valid direction.
% is_valid_move_direction(white_round, I, J, I2, J2) :-
%     (I2 is I + 1, (J2 is J + 1 ; J2 is J - 1)).
% is_valid_move_direction(brown_round, I, J, I2, J2) :-
%     (I2 is I - 1, (J2 is J + 1 ; J2 is J - 1)).
% is_valid_move_direction(white_square, I, J, I2, J2) :-
%     (abs(I2 - I) =:= 1, abs(J2 - J) =:= 1).
% is_valid_move_direction(brown_square, I, J, I2, J2) :-
%     (abs(I2 - I) =:= 1, abs(J2 - J) =:= 1).

display_cell(nonexistent) :- write('0 '). % Nonexistent cells are not displayed.
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
    play(Board, white, 2). % Player starts with 2 moves available.

play(Board, Player, 0) :- % When no more moves are left, transition to the push phase.
    write('Push phase for '), write(Player), nl,
    play_push(Board, Player).

play(Board, Player, MovesLeft) :-
    write(Player), write('\'s turn.'), nl,
    write('You have '), write(MovesLeft), write(' moves left.'), nl,
    
    write('Enter the coordinates of the piece you want to move(eg. i-j.):'), nl,
    read(I-J),
    get_piece(Board, I, J, Piece),
    write('You selected: '), write(Piece), nl,
    % Check if the piece belongs to the player. 
    % (redundant but just in case and makes errors faster to spot)
    player_pieces(Player, PlayerPieces),
    write('Player pieces: '), write(PlayerPieces), nl,
    (member(Piece, PlayerPieces) ->
        write('Enter the coordinates of the destination(eg. i-j.):'), nl,
        read(I2-J2),
        % Print out piece at the destination
        get_piece(Board, I2, J2, Destination),
        write('Destination: '), write(Destination), nl,
        (is_valid_move(Board, I, J, I2, J2, Player) ->
            % Valid move, update the board
            set_piece(Board, I2, J2, Piece, NewBoard),
            remove_piece(NewBoard, I, J, NewBoard1),
            display_board(NewBoard1),
            % next_player(Player, NextPlayer),
            NewMovesLeft is MovesLeft - 1,
            play(NewBoard1, Player, NewMovesLeft);
            write('Invalid move. Try again.'), nl,
            play(Board, Player, MovesLeft)
        );
        write('You cannot move this piece. Try again.'), nl,
        play(Board, Player, MovesLeft)
    ),

    NewMovesLeft is MovesLeft - 1,
    play(Board, Player, NewMovesLeft). % Continue the play phase.

play_push(Board, Player) :-
    write(Player), write('\'s push phase.'), nl,
    
    % todo push phase
   
    next_player(Player, NextPlayer),
    play(Board, NextPlayer, 2). % Assuming players start with 2 moves in the next play phase.
