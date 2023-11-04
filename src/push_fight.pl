:- use_module(library(lists)).

% Initialize the game board as a 6x10 matrix.
initial_board([
    [nonexistent, nonexistent, nonexistent, wall,  wall,         wall,         wall,  wall,        nonexistent, nonexistent],
    [nonexistent, nonexistent, nonexistent, empty, empty,        empty,        empty, empty,       nonexistent, nonexistent],
    [nonexistent, empty,       empty,       empty, white_square, brown_round, empty, empty,       empty,       nonexistent],
    [nonexistent, empty,       empty,       empty, white_round,  brown_square, empty, empty,       empty,       nonexistent],
    [nonexistent, nonexistent, empty,       empty, empty,        empty,        empty, nonexistent, nonexistent, nonexistent],
    [nonexistent, nonexistent, wall,        wall,  wall,         wall,         wall,  nonexistent, nonexistent, nonexistent]
]).

% Define players
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

% Gamestate([Board, Player, MovesLeft, [WhitePiecesLeft, BrownPiecesLeft], Anchor, AnchorPosition]) PiecesLeft is a list of the number of pieces left for each player.
% Anchor is the cell that the anchor is currently on top of (can only be squares) and AnchorPosition is the position of the anchor on the board.
% Game state that contains the board, curren player, number of moves left, if user is in push phase, and the number of pieces left for each player.
% initial_state(+Size, -GameState) Size is not used because the board of this game is kinda weird.
initial_state(Size, GameState) :-
    initial_board(Board),
    player(white),
    GameState = [Board, white, 2, [2, 2], nonexistent, [0, 0]].

% display_game(+GameState)
display_game(GameState) :-
    GameState = [Board, Player, MovesLeft, PiecesLeft, Anchor, AnchorPosition],

    % Padding
    write('#############################################'), nl,

    write('Player: '), write(Player), nl,
    % if moves left is 0, then the player is in the push phase.
    (MovesLeft = 0 -> write('Push phase.'), nl; write('Play phase, moves left: '), write(MovesLeft), nl),
    % white pieces is the first element in PiecesLeft, brown pieces is the second element.
    % write the number of pieces left for each player.
    write('Pieces left [white,brown]: '), write(PiecesLeft), nl,
    write('Anchor currently on top of: '), display_cell(Anchor), nl,
    write('Anchor position: '), write(AnchorPosition), nl,
    
    display_board(Board),

    write('#############################################'), nl.

% Predicate to check if a cell is empty or an anchor.
is_empty(empty).

% Predicate to check if the move is valid.
is_valid_move(Board, I, J, I2, J2, Player) :-
    %  Check if the piece belongs to the player
    player_pieces(Player, PlayerPieces),
    get_piece(Board, I, J, Piece),
    member(Piece, PlayerPieces),
    get_piece(Board, I2, J2, Destination),
    is_empty(Destination),
    write('Validating move: '), write(Piece), write(' from '), write(I), write('-'), write(J), write(' to '), write(I2), write('-'), write(J2), nl,
    is_clear_path(Board, I, J, I2, J2),
    write('Clear path: '), write(I), write('-'), write(J), write(' to '), write(I2), write('-'), write(J2), nl.

is_valid_move_direction(_, _, _, _, _). % Any piece can move in any direction.

% Predicate to check if there is a clear path between two positions on the board.
is_clear_path(Board, I1, J1, I2, J2) :-
    bfs(Board, [(I1, J1)], [], (I2, J2)).

is_clear_path(_, _, _, _, _) :- false. % No path found.

% BFS using a queue and a set of visited nodes.
bfs(_, [], _, _) :- false. % Queue is empty, no path found.

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

valid_neighbor_push(Board, I, J, NI, NJ) :-
    (NI is I + 1, NJ is J;
     NI is I - 1, NJ is J;
     NI is I, NJ is J + 1;
     NI is I, NJ is J - 1),
    get_piece(Board, NI, NJ, Piece),
    % Can only push pieces that belong to players
    (Piece = white_round; Piece = white_square; Piece = brown_round; Piece = brown_square).

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

% Predicate to push a piece in the direction of another piece.
push_piece(Board, FromRow, FromCol, ToRow, ToCol, NewBoard) :-
    get_piece(Board, FromRow, FromCol, Piece),
    valid_direction(FromRow, FromCol, ToRow, ToCol, Direction),
    % write the direction of the push
    write('Pushing '), write(Piece), write(' from '), write(FromRow), write('-'), write(FromCol), write(' to '), write(ToRow), write('-'), write(ToCol), write(' in direction '), write(Direction), nl,
    push_pieces_in_direction(Board, FromRow, FromCol, Direction, NewBoard).

% Define valid directions for pushing (up, down, left, right).
valid_direction(FromRow, FromCol, ToRow, ToCol, up) :-
    FromRow > ToRow,
    FromCol = ToCol.
valid_direction(FromRow, FromCol, ToRow, ToCol, down) :-
    FromRow < ToRow,
    FromCol = ToCol.
valid_direction(FromRow, FromCol, ToRow, ToCol, left) :-
    FromRow = ToRow,
    FromCol > ToCol.
valid_direction(FromRow, FromCol, ToRow, ToCol, right) :-
    FromRow = ToRow,
    FromCol < ToCol.

% Predicate to push a piece in the direction of another piece.
push_pieces_in_direction(Board, FromRow, FromCol, Direction, NewBoard) :-
    get_piece(Board, FromRow, FromCol, Piece),
    push_recursive(Board, FromRow, FromCol, Direction, Piece, NewBoard).

% Base case: No more pieces to push
% push_recursive(Board, _, _, _, _, Board).

% Recursive case: Push pieces in the specified direction
push_recursive(Board, Row, Col, Direction, Piece, NewBoard) :-
    write('Direction: '), write(Direction), nl,
    valid_direction_push(Direction),
    write('Valid direction: '), write(Direction), nl,
    adjacent_cell(Row, Col, Direction, NewRow, NewCol),
    write('Adjacent cell: '), write(NewRow), write('-'), write(NewCol), nl,
    get_piece(Board, NewRow, NewCol, NewPiece),
    write('New piece: '), write(NewPiece), nl,
    % If the adjacent cell is empty, move the current piece to that cell
    % and recursively continue pushing the remaining pieces
    ((NewPiece == white_round; NewPiece == white_square; NewPiece == brown_round; NewPiece == brown_square) ->
        write('Got in here'), nl,
        set_piece(Board, NewRow, NewCol, Piece, TempBoard),
        remove_piece(TempBoard, Row, Col, TempBoard2),
        push_recursive(TempBoard2, NewRow, NewCol, Direction, NewPiece, NewBoard)
    ;
        (is_empty(NewPiece) ->
            set_piece(Board, NewRow, NewCol, Piece, NewBoard)
        ;
        NewBoard = Board
        )
    ).

% Define valid directions for pushing (up, down, left, right).
valid_direction_push(up).
valid_direction_push(down).
valid_direction_push(left).
valid_direction_push(right).

% Calculate the coordinates of the adjacent cell in a given direction
adjacent_cell(Row, Col, up, NewRow, Col) :-
    NewRow is Row - 1.
adjacent_cell(Row, Col, down, NewRow, Col) :-
    NewRow is Row + 1.
adjacent_cell(Row, Col, left, Row, NewCol) :-
    NewCol is Col - 1.
adjacent_cell(Row, Col, right, Row, NewCol) :-
    NewCol is Col + 1.

% Move Validation and Execution
% move(+GameState, +Move, -NewGameState)
% Move format: move([FromRow, FromCol, ToRow, ToCol])
move(GameState, Move, NewGameState) :-
    GameState = [Board, Player, MovesLeft, PiecesLeft, Anchor, AnchorPosition],

    Move = [FromRow, FromCol, ToRow, ToCol],

    get_piece(Board, FromRow, FromCol, Piece),
    player_pieces(Player, PlayerPieces),

    (member(Piece, PlayerPieces) ->

        (MovesLeft > 0 -> % There are moves left for the current player.
            (is_clear_path(Board, FromRow, FromCol, ToRow, ToCol) ->
                set_piece(Board, ToRow, ToCol, Piece, NewBoard),
                remove_piece(NewBoard, FromRow, FromCol, NewBoard1),
                NewMovesLeft is MovesLeft - 1,
                NewGameState = [NewBoard1, Player, NewMovesLeft, PiecesLeft, Anchor, AnchorPosition],

                % Check if the player has won
                (PiecesLeft = [0, 0] ->
                    write('Player '), write(Player), write(' has won!'), nl,
                    abort

                % Continue the play_game phase
                ;
                    true
                )

            ;
                write('Invalid move. Try again.'), nl,
                NewGameState = GameState % Return the original game state
            )
        ;
            write('aadsfasdf'), nl,
            % Push phase
            % validate piece is a square
            ((Piece = white_square; Piece = brown_square) ->
                write('Validating push: '), write(Piece), write(' from '), write(FromRow), write('-'), write(FromCol), write(' to '), write(ToRow), write('-'), write(ToCol), nl,
                % validate the piece is adjacent to another piece
                (valid_neighbor_push(Board, FromRow, FromCol, ToRow, ToCol) ->
                    % Push the piece in the direction of the second piece
                    push_piece(Board, FromRow, FromCol, ToRow, ToCol, NewBoard),

                    % advance to next player
                    next_player(Player, NextPlayer),
                    NewGameState = [NewBoard, NextPlayer, 2, PiecesLeft, Anchor, AnchorPosition]
                ;
                    write('Invalid second piece.'), nl,
                    NewGameState = GameState % Return the original game state
                )
            ;
                write('You can only push with squares.'), nl,
                NewGameState = GameState % Return the original game state
            )
        )
    ;
        write('You cannot move this piece. Try again.'), nl,
        NewGameState = GameState % Return the original game state
    ).

% Update the 'play' predicate to use GameState
play :-
    initial_state(_, GameState), % Initialize the game state
    % display_game(GameState), % Display the initial game state
    play_game(GameState).

% Update 'play_game' to use 'move' predicate
play_game(GameState) :-
    GameState = [Board, Player, MovesLeft, PiecesLeft, Anchor, AnchorPosition],

    (MovesLeft > 0 -> % There are moves left for the current player.
        % Display the game state
        display_game(GameState),

        write('Enter the coordinates of the piece you want to move(eg. i-j.):'), nl,
        read(I-J),
        write('Enter the coordinates of the destination(eg. i-j.):'), nl,
        read(I2-J2),

        % Construct the Move
        Move = [I, J, I2, J2],

        % Validate and execute the move
        move(GameState, Move, NewGameState),

        % Continue the play_game phase
        play_game(NewGameState)
    ;
        % Display the game state
        display_game(GameState),

        write('Enter the coordinates of the piece you want to push(eg. i-j.):'), nl,
        read(I-J),
        write('Enter the coordinates of an adjacent piece to push(eg. i-j.):'), nl,
        read(I2-J2),

        % Construct the Move
        Move = [I, J, I2, J2],

        % Validate and execute the move
        move(GameState, Move, NewGameState),

        % Continue the play_game phase
        play_game(NewGameState)   
    ).
