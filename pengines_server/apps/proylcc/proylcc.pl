:- module(proylcc, 
	[  
		captured/3,
		help/5,
		nonOptHelp/5,
		flick/6
	]).

:- use_module(proylcc:convert).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% captured(+Grid, +Origin, -Captured)
%
% Calcula la cantidad de celdas capturadas en la grilla Grid, con celda origen
% Origin y lo retorna en Captured. Predicado a ser usado solo al comenzar el juego,
% para calcular las celdas capturadas previo a cualquier flick.

captured(Grid, Origin, Captured) :-
	gridToCoords(Grid, CoordsList),
	adjacentsC(CoordsList, [Origin], [], AdjacentsC),
	length(AdjacentsC, Captured).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% flick(+Grid, +Origin, +Color, -Captured, -FGrid, -Finished)
%
% FGrid es el resultado de hacer flick de la grilla Grid con el color Color.
% Retorna false si Color coincide con el color de la celda Origen.
% Finished es un valor booleano que indica si la grilla esta completamente capturada.
% Captured es la cantidad de celdas capturdas luego del flick.

flick(Grid, Origin, Color, Captured, FGrid, Finished) :-
	Origin = cell(_, _, C),
	C \= Color,
	gridToCoords(Grid, CoordsList),
	adjacentsC(CoordsList, [Origin], [], AdjacentsC),
	paint(CoordsList, AdjacentsC, Color, FCoordsList),
	paintAll(AdjacentsC, Color, FAdjacentsC),
	adjacentsCExpansion(FCoordsList, FAdjacentsC, AdjacentsC2),
    length(AdjacentsC2, Captured),
	finished(Captured, Finished),
	coordsToGrid(FCoordsList, FGrid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% paint(+CoordsList, +ToPaint, +Color, -PCoordsList)
%
% PCoordsList es la grilla obtenida tras pintar las celdas ToPaint del color Color.
% CoordsList es la grilla a pintar. Tanto CoordsList como ToPaint y PCoordsList estan
% expresadas como lista de celdas.

paint([], _, _, []) :- !.
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% paintAll(+CoordsList, +Color, -PCoordsList)
%
% PCoordsList es la lista de celdas obtenida tras pintar todas las celdas del color Color.
% CoordsList es la lista de celdas a pintar.

paintAll([], _, []).
paintAll(CoordsList, Color, PCoordsList) :-
	CoordsList = [cell(X, Y, _) | CoordsList1],
	paintAll(CoordsList1, Color, PCoordsList1),
	PCoordsList = [cell(X, Y, Color) | PCoordsList1].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% remove(+List, +E, -Res)
%
% Remueve el elemento E de la lista List y retorna el resultado en Res

remove(List, E, Res) :-
    append(L1, [E | L2], List),
    append(L1, L2, Res).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% adjacentsC(+CoordsList, +ToVisit, +Visited, -Res)
%
% Calcula la clausura transitiva de la relacion de adyacencia entre celdas del mismo color.
% CoordsList es la grilla expresada como lista de celdas.
% Este predicado debe consultarse con la celda de origen como unico elemento de la lista ToVisit.
% La lista Visited se utiliza como contador interno para controlar las celdas visitadas recursivamente.

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% adjacentsCExpansion(+CoordsList, +AdjacentsC, -Res)
%
% Calcula la clausura transitiva de la relacion de adyacencia entre celdas del mismo color a partir de un conjunto de celdas.
% CoordsList es la grilla expresada como lista de celdas.
% Este predicado debe consultarse con un conjunto de celdas que sea resultado de un calculo previo de adjacentsC.

adjacentsCExpansion(CoordsList, AdjacentsC, Res) :-
	AdjacentsC = [cell(_, _, Color) | _],
	paint(CoordsList, AdjacentsC, dummy, CoordsList2),
	paintAll(AdjacentsC, dummy, AdjacentsC2),
	length(AdjacentsC2, Size),
	adjacentsCExpansionAux(CoordsList2, Color, Size, AdjacentsC2, AdjacentsC2, Res).
adjacentsCExpansionAux(CoordsList, Color, 0, ToVisit, Visited, Res) :-
	paint(CoordsList, Visited, Color, CoordsList2),
	paintAll(Visited, Color, Visited2),
	adjacentsC(CoordsList2, ToVisit, Visited2, Res), !.
adjacentsCExpansionAux(CoordsList, Color, PrevAdyacentsSize, ToVisit, Visited, Res) :-
	ToVisit = [cell(X1, Y1, _) | ToVisitRem],
	sameColorAdjacents(cell(X1, Y1, Color), CoordsList, Adjacents),
	findall(cell(X2, Y2, Color), (member(cell(X2, Y2, Color), Adjacents),
                          	\+ member(cell(X2, Y2, Color), Visited),
                            \+ member(cell(X2, Y2, Color), ToVisit)), NonVisitedAdjacents),
	append(ToVisitRem, NonVisitedAdjacents, ToVisitNext),
	PrevAdyacentsSize2 is PrevAdyacentsSize - 1,
	adjacentsCExpansionAux(CoordsList, Color, PrevAdyacentsSize2, ToVisitNext, Visited, Res).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% sameColorAdjacents(+Cell, +CoordsList, -AdjacentsList)
%
% Calcula las celdas adyacentes a una celda Cell tales que tienen el mismo color que Cell
% y las devuelve en AdjacentsList.
% CoordsList es la grilla expresada como lista de celdas.

sameColorAdjacents(Cell, CoordsList, AdjacentsList) :-
	Cell = cell(X, Y, C),
	findall(cell(X1, Y1, C), (member(cell(X1, Y1, C), CoordsList),
								X1 >= 0, X1 < 14,
								Y1 >= 0, Y1 < 14,
								(X1 is X + 1, Y1 is Y;
								X1 is X - 1, Y1 is Y;
								Y1 is Y + 1, X1 is X;
								Y1 is Y - 1, X1 is X)), AdjacentsList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% finished(+Captured, -Bool)
%
% Calcula si la grilla esta totalmente capturada a partir de la cantidad
% de celdas capturadas indicada en Captured, y retorna el resultado como
% true o false en Bool.

finished(Captured, true) :-
	Captured is 196, !.
finished(_, false).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% help(+Grid, +Origin, +Depth, -Path, -Captured)
%
% Calcula el mejor camino con profundidad Depth, y la cantidad de celdas que dicho camino captura, retornando los resultados
% en Path y Captured respectivamente.
% El calculo se realiza sobre la grilla Grid, desde la celda Origin.

help(Grid, Origin, Depth, Path, Captured) :- 
	gridToCoords(Grid, CoordsList),
	helpAux(CoordsList, Origin, Depth, Path, Captured).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% helpAux(+CoordsList, +Origin, +Depth, -Path, -Captured)
%
% Predicado auxiliar para el calculo de la ayuda, recive una lista de coordenadas en lugar de una matriz de colores.

helpAux(CoordsList, Origin, Depth, Path, Captured) :-
	adjacentsC(CoordsList, [Origin], [], AdjacentsC),
	findall(path(P, C), findPath(CoordsList, AdjacentsC, Depth, P, C), PathsList),
	findBestPath(PathsList, path(Path, Captured)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% findPath(+CoordsList, +PrevAdjacentsC, +Depth, -Path, -Captured)
%
% Calcula una posible secuancia de jugadas de longitud Depth, y la cantidad de celdas que dicha jugada captura,
% retorna los resultados en Path y Captured respectivamente.
% El calculo se realiza sobre la grilla expresada como lista de celdas en CoordsList, utilizando los adjacentC previamente
% calculados y dados en PrevAdjacentsC.

findPath(CoordsList, PrevAdjacentsC, 0, [], Captured) :-
	adjacentsCExpansion(CoordsList, PrevAdjacentsC, AdjacentsC),
	length(AdjacentsC, Captured), !.
findPath(CoordsList, PrevAdjacentsC, Depth, Path, Captured) :-
	adjacentsCExpansion(CoordsList, PrevAdjacentsC, AdjacentsC),
	length(AdjacentsC, Size),
	\+ finished(Size, true), !,
	color(Color),
	PrevAdjacentsC = [cell(_, _, C) | _],
	Color \= C,
	paint(CoordsList, AdjacentsC, Color, FCoordsList),
	paintAll(AdjacentsC, Color, FAdjacentsC),
	Path = [Color | Path2],
	Depth2 is Depth - 1,
	findPath(FCoordsList, FAdjacentsC, Depth2, Path2, Captured).
findPath(_, _, _, [], Captured) :- 
	finished(Captured, true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% color(?Color)
%
% Hechos que determinan los posibles colores.

color(r).
color(v).
color(p).
color(g).
color(b).
color(y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% findBestPath(+Paths, -BestPath)
%
% Busca el mejor camino BestPath entre una lista de caminos Paths.

findBestPath([Path], Path) :- !.
findBestPath([Path1, Path2 | Paths], BestPath) :-
	\+ better(Path2, Path1),
	findBestPath([Path1 | Paths], BestPath), !.
findBestPath([_ | Paths], BestPath) :-
	findBestPath(Paths, BestPath).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% better(+Path1, +Path2)
%
% Compara dos caminos Path1 y Path2. El predicado tiene exito si Path1 es mejor que Path2,
% y falla en caso contrario.

better(path(Path1, Captured1), path(Path2, Captured2)) :-
	length(Path1, Size1),
	length(Path2, Size2),
	(Captured1 > Captured2; Size1 < Size2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% nonOptHelp(+Grid, +Origin, +Depth, -Path, -Captured)
%
% Calcula un buen camino con profundidad Depth, y la cantidad de celdas que dicho camino captura, retornando los resultados
% en Path y Captured respectivamente.
% El camino calculado no necesariamente es el mejor.
% El calculo se realiza sobre la grilla Grid, desde la celda Origin.

nonOptHelp(Grid, Origin, Depth, Path, Captured) :-
	gridToCoords(Grid, CoordsList),
	nonOptHelpAux(CoordsList, Origin, Depth, Path, Captured).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% nonOptHelpAux(+CoordsList, +Origin, +Depth, -Path, -Captured)
%
% Predicado auxiliar para el calculo de la ayuda no optima, recive una lista de coordenadas en lugar de una matriz de colores.

nonOptHelpAux(CoordsList, Origin, Depth, Path, Captured) :-
	Depth =< 3,
	helpAux(CoordsList, Origin, Depth, Path, Captured), !.
nonOptHelpAux(CoordsList, Origin, Depth, Path, Captured) :-
	adjacentsC(CoordsList, [Origin], [], AdjacentsC),
	findall(path(P, C), findPath(CoordsList, AdjacentsC, 3, P, C), PathsList),
	findBestPath(PathsList, path([FirstMove | _], _)),
	paint(CoordsList, AdjacentsC, FirstMove, FCoordsList),
	Origin = cell(X, Y, _),
	Depth1 is Depth - 1,
	nonOptHelpAux(FCoordsList, cell(X, Y, FirstMove), Depth1, Path1, Captured),
	Path = [FirstMove | Path1], !.
nonOptHelpAux(_, _, _, [], Captured) :-
	finished(Captured, true).