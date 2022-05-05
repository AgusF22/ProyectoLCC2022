:- module(convert, 
	[  
		gridToCoords/2,
        coordsToGrid/2
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% gridToCoords(+Grid, -CoordsList)
%
% CoordsList es la grilla Grid expresada como lista de celdas cell(X, Y, Color).

gridToCoords(Grid, CoordsList) :-
	gridToCoordsAux(Grid, 0, CoordsList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% gridToCoordsAux(+Grid, +I, -CoordsList)
%
% CoordsList es la grilla Grid expresada como lista de celdas cell(X, Y, Color).
% I es un contador de filas.

gridToCoordsAux([], _, []).
gridToCoordsAux(Grid, I, CoordsList) :-
	Grid = [F | Fs],
    rowToCoords(F, I, 0, CoordsList1),
    I1 is I + 1,
    gridToCoordsAux(Fs, I1, CoordsList2),
    append(CoordsList1, CoordsList2, CoordsList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% rowToCoords(+Row, +I, +J, -CoordsList)
%
% CoordsList es la fila Row expresada como lista de celdas cell(X, Y, Color).
% I es el numero de fila actual, y J es el contador de elementos de la fila.

rowToCoords([], _, _, []) :- !.
rowToCoords(Row, I, J, CoordsList) :-
	Row = [X | Xs],
	J1 is J + 1,
	rowToCoords(Xs, I, J1, CoordsList1),
	CoordsList = [cell(I, J, X) | CoordsList1].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% coordsToGrid(+CoordsList, -Grid)
%
% CoordsList es la grilla Grid expresada como lista de celdas cell(X, Y, Color).
% Grid es la grilla expresada como matriz de caracteres.

coordsToGrid(CoordsList, Grid) :-
	coordsToGridAux(CoordsList, 0, Grid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% coordsToGridAux(+CoordsList, +I, -Grid)
%
% CoordsList es la grilla Grid expresada como lista de celdas cell(X, Y, Color).
% I es el numero de fila actual.
% Grid es la grilla expresada como matriz de caracteres.

coordsToGridAux([], _, []) :- !.
coordsToGridAux(CoordsList, I, Grid) :-
	findall(cell(X, Y, C), (member(cell(X, Y, C), CoordsList), X is I), CellsRow),
	append(CellsRow, CoordsList1, CoordsList),
	I1 is I + 1,
	coordsToGridAux(CoordsList1, I1, Grid1),
    rowToColorList(CellsRow, Row),
	Grid = [Row | Grid1].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% rowToColorList(+Row, -List)
%
% Row es una fila expresada como lista de celdas.
% List es la misma fila expresada como lista de caracteres.

rowToColorList([], []).
rowToColorList(Row, List) :-
    Row = [cell(_, _, Color) | Row1],
    rowToColorList(Row1, List1),
    List = [Color | List1].