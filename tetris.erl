-module(tetris).
-compile([export_all]).

-record(state, {board :: [] :: [list()] | tuple()}).

start() ->
    initial_state().

initial_state() ->
    #state{board = initial_board()}.

initial_board() ->
    lists:tabulate(10, fun (_) -> lists:seq(1, 20) end).

can_place_piece(State, Piece, X, Y) ->
    Board = State#state.board,
    PieceSize = length(Piece),
    Y + PieceSize - 1 =< 20, 
    can_place_piece(Board, Piece, X, Y).

can_place_piece(_, [], _, _) ->
    true;
can_place_piece(Board, [Row | Rest], X, Y) ->
    case lists:nth(Y, Board) of
        {ok, CurrentRow} ->
            case lists:nth(X, Row) of
                {ok, Cell} when Cell == 0 ->
                    can_place_piece(Board, Rest, X, Y + 1);
                _ ->
                    false
            end;
        _ ->
            false
    end.

place_piece(State, Piece, X, Y) ->
    Board = State#state.board,
    NewBoard = place_piece(Board, Piece, X, Y),
    #state{board = NewBoard}.

place_piece(Board, [], _, _) ->
    Board;
place_piece(Board, [Row | Rest], X, Y) ->
    NewBoard = update_board_row(Board, Row, X, Y),
    place_piece(NewBoard, Rest, X, Y + 1).

update_board_row(Board, _, _, Y) when Y > 20 ->
    Board;
update_board_row(Board, [], _, Y) ->
    Board;
update_board_row(Board, [Cell | Rest], X, Y) ->
    NewBoard = update_board_cell(Board, Cell, X, Y),
    update_board_row(NewBoard, Rest, X, Y + 1).

update_board_cell(Board, 0, X, Y) ->
    lists:sublist(Board, Y - 1) ++ [lists:sublist(lists:nth(Y, Board), X - 1) ++ [1] ++ lists:sublist(lists:nth(Y, Board), X)]
    ++ lists:sublist(Board, 21 - Y);
update_board_cell(Board, Cell, X, Y) ->
    Board.

move_left(State) ->
    {ok, Piece} = get_current_piece(State),
    X = State#state.current_x,
    Y = State#state.current_y,
    if can_place_piece(State, Piece, X - 1, Y) ->
           NewState = place_piece(State, Piece, X - 1, Y),
           NewState;
       true ->
           State
    end.

rotate(State) ->
    {ok, Piece} = get_current_piece(State),
    RotatedPiece = rotate_piece(Piece),
    X = State#state.current_x,
    Y = State#state.current_y,
    if can_place_piece(State, RotatedPiece, X, Y) ->
           NewState = place_piece(State, RotatedPiece, X, Y),
           NewState;
       true ->
           State
    end.

get_current_piece(State) ->
    {ok, CurrentPiece} = get_piece(State, State#state.current_piece),
    {ok, CurrentPiece}.


get_piece(State, PieceNum) ->
    {ok, Pieces} = get_all_pieces(State),
    {ok, lists:nth(PieceNum, Pieces)}.

get_all_pieces(_State) ->
    {ok, [
        [[1,1],[1,1]], 
        [[1,1,1,1]],  
    ]}.

rotate_piece(Piece) ->
    lists:zipwith(fun lists:nth/2, lists:foldl(fun(E, Acc) -> [E | Acc] end, [], Piece)).
