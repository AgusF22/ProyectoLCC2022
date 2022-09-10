:- module(init, [ init/1 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% init(-Grid)
%
% Inicializa la grilla.

init(Grid) :-
	randomInit(Grid, 14).		% Descomentar para inicializacion aleatoria
%	defaultInit(Grid).			% Descomentar para inicializacion por defecto
%	testInit(Grid).				% Descomentar para casos de prueba
		% Solo descomentar una de las tres lineas anteriores a la vez

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% defaultInit(-Grid)
%
% Inicializa la grilla en configuracion predeterminada.

defaultInit([
		 [y,g,b,g,v,y,p,v,b,p,v,p,v,r],
		 [r,r,p,p,g,v,v,r,r,b,g,v,p,r],
		 [b,v,g,y,b,g,r,g,p,g,p,r,y,y],
		 [r,p,y,y,y,p,y,g,r,g,y,v,y,p],
		 [y,p,y,v,y,g,g,v,r,b,v,y,r,g],
		 [r,b,v,g,b,r,y,p,b,p,y,r,y,y],
		 [p,g,v,y,y,r,b,r,v,r,v,y,p,y],
		 [b,y,v,g,r,v,r,g,b,y,b,y,p,g],
		 [r,b,b,v,g,v,p,y,r,v,r,y,p,g],
		 [v,b,g,v,v,r,g,y,b,b,b,b,r,y],
		 [v,v,b,r,p,b,g,g,p,p,b,y,v,p],
		 [r,p,g,y,v,y,r,b,v,r,b,y,r,v],
		 [r,b,b,v,p,y,p,r,b,g,p,y,b,r],
		 [v,g,p,b,v,v,g,g,g,b,v,g,g,g]
		 ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% testInit(-Grid)
%
% Inicializa la grilla en una configuracion de prueba.

testInit(

	% Pegar caso de prueba aqui

).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% randomInit(-Grid, +RowNumber)
%
% Inicializa la grilla con RowNumber filas de forma aleatoria.

randomInit([], 0) :- !.
randomInit(Grid, RowNumber) :-
	RowNumber >= 0,
	Grid = [Row | Rows],
	randomRow(Row, 14),
	RowNumber1 is RowNumber - 1,
	randomInit(Rows, RowNumber1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% randomRow(-Row, +Size)
%
% Crea una fila de tamaño Size de forma aleatoria.

randomRow([], 0) :- !.
randomRow(Row, Size) :-
	Size >= 0,
	Row = [Color | Colors],
	randomColor(Color),
	Size1 is Size - 1,
	randomRow(Colors, Size1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% randomColor(-Color)
%
% Devuelve un color aleatorio.

randomColor(Color) :-
	random_between(0,5,Int),
	color(Int, Color).

color(0,r).
color(1,v).
color(2,p).
color(3,g).
color(4,b).
color(5,y).