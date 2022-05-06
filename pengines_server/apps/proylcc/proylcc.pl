:- module(proylcc, 
	[  
		flick/3
	]).

:- use_module(proylcc:convert).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% flick(+Grid, +Color, -FGrid)
%
% FGrid es el resultado de hacer 'flick' de la grilla Grid con el color Color.
% Retorna false si Color coincide con el color de la celda superior izquierda de la grilla. 

flick(Grid, Color, FGrid):-
	Grid = [F|Fs],
	F = [X|Xs],
	Color \= X,
	FGrid = [[Color|Xs]|Fs].



flick(Grid, Origin, Color, Captured, FGrid, Finished) :-
	Origin = cell(X, Y, C),
	C \= Color,
	gridToCoords(Grid, CoordsList),
	adjacentsC(CoordsList, [Origin], [], AdjacentsC),
	paint(CoordsList, AdjacentsC, Color, FCoordsList),
	adjacentsC(FCoordsList, [cell(X, Y, Color)], [], AdjacentsC2),
    size(AdjacentsC2, Captured),
	finished(Captured, Finished),
	coordsToGrid(FCoordsList, FGrid).

size([], 0).
size([_ | Xs], Size) :-
    size(Xs, Size1), Size is Size1 + 1.


paint([], _, _, []).
paint(CoordsList, ToPaint, Color, PCoordsList) :-
	CoordsList = [cell(X, Y, C) | CoordsList1],
    member(cell(X, Y, C), ToPaint),
    remove(ToPaint, cell(X, Y, C), ToPaint1),
	paint(CoordsList1, ToPaint1, Color, PCoordsList1),
	PCoordsList = [cell(X, Y, Color) | PCoordsList1], !.
paint(CoordsList, ToPaint, Color, PCoordsList) :-
	CoordsList = [cell(X, Y, C) | CoordsList1],
	paint(CoordsList1, ToPaint, Color, PCoordsList1),
	PCoordsList = [cell(X, Y, C) | PCoordsList1].

remove(List, E, Res) :-
    append(L1, [E | L2], List),
    append(L1, L2, Res).

adjacentsC(_, [], Visited, Visited) :- !.
adjacentsC(CoordsList, ToVisit, Visited, Res) :-
	ToVisit = [Cell | ToVisitRem],
	sameColorAdjacents(Cell, CoordsList, Adjacents),
	findall(cell(X, Y, C), (member(cell(X, Y, C), Adjacents),
                          	\+ member(cell(X, Y, C), Visited),
                            \+ member(cell(X, Y, C), ToVisit)), NonVisitedAdjacents),
	append(NonVisitedAdjacents, ToVisitRem, ToVisitNext),
    Visited1 = [Cell | Visited],
	adjacentsC(CoordsList, ToVisitNext, Visited1, Res).

sameColorAdjacents(Cell, CoordsList, AdjacentsList) :-
	Cell = cell(X, Y, C),
	findall(cell(X1, Y1, C), (member(cell(X1, Y1, C), CoordsList),
								X1 >= 0, X1 < 14,
								Y1 >= 0, Y1 < 14,
								(X1 is X + 1, Y1 is Y;
								X1 is X - 1, Y1 is Y;
								Y1 is Y + 1, X1 is X;
								Y1 is Y - 1, X1 is X)), AdjacentsList).


finished(Captured, 1) :-
	Captured is 196, !.
finished(_, 0).