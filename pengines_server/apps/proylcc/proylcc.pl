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



flick(Grid, Origin, Color, PaintedNumber, FGrid, Finished) :-
	Origin = cell(_, _, _),
	gridToCoords(Grid, CoordsList),
	adjacentsC(CoordsList, [Origin], [], AdjacentsC),
	paint(CoordsList, AdjacentsC, Color, PaintedNumber, FCoordsList),
	finished(FCoordsList, Finished),
	coordsToGrid(FCoordsList, FGrid).

paint([], _, _, 0, []).

paint(CoordsList, ToPaint, Color, PaintedNumber, PCoordsList) :-
	CoordsList = [cell(X, Y, C) | CoordsList1],
    member(cell(X, Y, C), ToPaint),
    remove(ToPaint, cell(X, Y, C), ToPaint1),
	paint(CoordsList1, ToPaint1, Color, PaintedNumber1, PCoordsList1),
	PaintedNumber is PaintedNumber1 + 1,
	PCoordsList = [cell(X, Y, Color) | PCoordsList1], !.

paint(CoordsList, ToPaint, Color, PaintedNumber, PCoordsList) :-
	CoordsList = [cell(X, Y, C) | CoordsList1],
	paint(CoordsList1, ToPaint, Color, PaintedNumber, PCoordsList1),
	PCoordsList = [cell(X, Y, C) | PCoordsList1].

remove(List, E, Res) :-
    append(L1, [E | L2], List),
    append(L1, L2, Res).

adjacentsC(_, [], Visited, Visited) :- !.
adjacentsC(CoordsList, ToVisit, Visited, Res) :-
	ToVisit = [Cell | ToVisitRem],
	sameColorAdjacents(Cell, CoordsList, Adjacents),
	findall(cell(X, Y, C), (member(cell(X, Y, C), Adjacents),
                          	\+ member(cell(X, Y, C), Visited)), NonVisitedAdjacents),
	append(NonVisitedAdjacents, ToVisitRem, ToVisitNext),
    Visited1 = [Cell | Visited],
	adjacentsC(CoordsList, ToVisitNext, Visited1, Res).

sameColorAdjacents(Cell, CoordsList, AdjacentsList) :-
	Cell = cell(X, Y, C),
	findall(cell(X1, Y1, C), (member(cell(X1, Y1, C), CoordsList),
								X1 >= 0, Y1 >= 0,
								(X1 is X + 1, Y1 is Y;
								X1 is X - 1, Y1 is Y;
								Y1 is Y + 1, X1 is X;
								Y1 is Y - 1, X1 is X)), AdjacentsList).


finished(CoordsList, 1) :-
	CoordsList = [cell(_, _, Color) | _],
	forall(member(cell(_, _, C), CoordsList), C = Color), !.
finished(_, 0).