:- module(proylcc, 
	[  
		flick/6,
        ayuda/5
	]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% flick(+Grid, +CN, -NGrid,+PosX,+PosY,-CantTotal)
% Grid grilla de colores pasada por parametro
% CN color nuevo a ser comparado
% PosX es la posicion "X" de la celda pasada por parametro
% PosY es la posicion "Y" de la celda pasada por parametro
% Retorna NGrid es el resultado de hacer 'flick' de la grilla Grid con el color CN.
% Retorna CantTotal con la cantidad de celdas capturadas. 

flick(Grid, CN, NGrid, PosX, PosY, CantTotal):-
	obtenerElemGrilla(PosX, PosY, Grid, E),
    CN\=E,
	cambiarC(Grid, E, CN, PosX, PosY, NGrid),
    adyCStar([PosX, PosY], NGrid, _Res, CantTotal).

%Implementacion como lo dijo mauro
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
colors([r,p,g,b,y]).

allColorsButC(Color, Colors):-
	colors(AllColors), 
	delete(AllColors, Color, Colors).

%Elimina de colors el elemento de la Pos pasada, y devuelve un arreglo de 5 elementos
ayuda(Grid, PosX, PosY, PE, Best):-
	obtenerElemGrilla(PosX, PosY, Grid, E),
    allColorsButC(E, Colors), 
    helpAll(Grid, Colors, PosX, PosY, PE, Best).
    
helpOne(Grid, Color, PosX, PosY, PE, Best):-
    PE > 1,
    flick(Grid, Color, NGrid, PosX, PosY, _CantTotal),
    allColorsButC(Color, Colors), 
	PEAux is PE - 1,
	helpAll(NGrid, Colors, PosX, PosY, PEAux, Best).

helpOne(Grid, Color, PosX, PosY, PE, CantTotal):-
    flick(Grid, Color, _NGrid, PosX, PosY, CantTotal),
    PE = 1.

helpAll(_Grid, [], _PosX, _PosY, _PE, 0).

helpAll(Grid, Colors, PosX, PosY, PE, Best):-
	[C|Cs] = Colors,
 	helpOne(Grid, C, PosX, PosY, PE, BestPathOne),
 	helpAll(Grid, Cs, PosX, PosY, PE, BestPathAll), 
 	mayorC(BestPathOne, BestPathAll, Best).

mayorC(BestPathOne, BestPathAll, R):-
    BestPathOne > BestPathAll,
    Aux = BestPathOne,
    R is Aux.

mayorC(BestPathOne, BestPathAll, R):-
    BestPathOne < BestPathAll,
    Aux = BestPathAll,
    R is Aux.

mayorC(BestPathOne, BestPathAll, 0):-
    BestPathOne = BestPathAll.
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% obtenerElemGrilla(+PosX, +PosY, +Grid, -E)
% PosX es la posicion "X" de la celda pasada por parametro
% PosY es la posicion "Y" de la celda pasada por parametro
% Grid grilla de colores pasada por parametro
% E elemento a retornar
% Retorna el elemento E de la grila Grid en la posicion (PosX,PosY).
obtenerElemGrilla(PosX, PosY, Grid, E):- 
    nth0(PosX, Grid, List), nth0(PosY, List, E).



%codigo de mauro%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
adyCStar(Origin, Grid, Res, Long) :-
    adyCStarSpread([Origin], [], Grid, Res),
    length(Res, Long).

adyCStarSpread([], Vis, _Grid, Vis).

adyCStarSpread(Pend, Vis, Grid, Res):-
    Pend = [P|Ps],
    findall(A, 
	        (
    	        adyC(P, Grid, A),
        	    not(member(A, Pend)),
            	not(member(A, Vis))
	        ), 
            AdyCP),
    append(AdyCP, Ps, NPend),
    adyCStarSpread(NPend, [P|Vis], Grid, Res).

/* 
 * adyC(+P, +Grid, -A)
 */

adyC(P, Grid, A):-
    ady(P, Grid, A),
    color(P, Grid, C),
    color(A, Grid, C).

/* 
 * ady(+P, +Grid, -A)
 */

ady([X, Y], Grid, [X1, Y]):-
    length(Grid, L),
    X < L - 1,
    X1 is X + 1.

ady([X, Y], _Grid, [X1, Y]):-
    X > 0,
    X1 is X - 1.

ady([X, Y], Grid, [X, Y1]):-
    Grid = [F|_],
    length(F, L),
    Y < L - 1,
    Y1 is Y + 1.

ady([X, Y], _Grid, [X, Y1]):-
    Y > 0,
    Y1 is Y - 1.

/* 
 * color(P, Grid, C)
 */

color([X,Y], Grid, C):-
    nth0(X, Grid, F),
    nth0(Y, F, C). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%cambiarC(+Grid, +CA, +CN, +PosX, +PosY, -NGrid, -CantTotales)
% Grid grilla de colores pasada por parametro
% CA color actual del elemento de la posicion(PosX,PosY).
% CN color nuevo a comparar
% PosX es la posicion "X" de la celda pasada por parametro
% PosY es la posicion "Y" de la celda pasada por parametro
% NGrid grilla reemplazada con el color nuevo CN
% CantTotales cantidad total de celdas capturadas
% El predicado cambiarC/7, modifica el color en la grilla si mi color actual CA 
% es igual al color del elemento E en la posicion (PosX,PosY). 
cambiarC(Grid, CA, CN, PosX, PosY, NGrid):-
   obtenerElemGrilla(PosX, PosY, Grid, E), CA=E,
   cambiarElemG(PosX, PosY, Grid, CN, AuxGrid), 
   adyacentes(AuxGrid, CA, CN, PosX, PosY, NGrid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%cambiarC(+Grid, +CA, +_CN, +PosX, +PosY, -Grid, -0)
% Grid grilla de colores pasada por parametro
% CA color actual del elemento de la posicion(PosX,PosY).
% CN color nuevo a comparar
% PosX es la posicion "X" de la celda pasada por parametro
% PosY es la posicion "Y" de la celda pasada por parametro
% Grid grilla sin cambios
% CantTotales cantidad total de celdas capturadas
% El predicado cambiarC/7, retorna la grilla sin modificar si mi color actual CA 
% es distinto al color del elemento E en la posicion (PosX,PosY). 
cambiarC(Grid, CA, _CN, PosX, PosY, Grid):-
     obtenerElemGrilla(PosX, PosY, Grid, E), CA\=E.  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%cambiarC(+Grid, +CA, +_CN, +PosX, +PosY, -Grid, -0)
% Grid grilla de colores pasada por parametro
% CA color actual del elemento de la posicion(PosX,PosY).
% CN color nuevo a comparar
% PosX es la posicion "X" de la celda pasada por parametro
% PosY es la posicion "Y" de la celda pasada por parametro
% Grid grilla sin cambios
% CantTotales cantidad total de celdas capturadas
% El predicado cambiarC/7, verifica que la posicion (PosX,PosY) sea valida. 
cambiarC(Grid, _CA, _CN, PosX, PosY, Grid):-
(PosX<0); (PosY<0); (PosX>13); (PosY>13).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%replace(+Pos, +L, +EN, -ListN)
% Pos posicion a buscar en la lista
% L lista ingresada por parametro
% EN elemento a reemplazar
% ListN lista nueva con el elemento EN reemplazado.
% Dada una posicion en una lista, reemplaza el valor de esa posicion por EN 
%y lo retorna en una nueva lista
replace(Pos, L, EN, ListN) :-
  nth0(Pos, L, _, R),
  nth0(Pos, ListN, EN, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%cambiarElemG(+PosX, +PosY, +Grid, +CN, -NueGrid)
% PosX es la posicion "X" de la celda pasada por parametro
% PosY es la posicion "Y" de la celda pasada por parametro
% Grid grilla de colores pasada por parametro
% CN color nuevo a comparar
% NuevaGrid grilla con el elemento reemplazado en la posicion (PosX,PosY). 
% Retorna una nueva grilla actualizada con el elemento reemplazado
cambiarElemG(PosX, PosY, Grid, CN, NuevaGrid):-
    nth0(PosX, Grid, L),
    replace(PosY, L, CN, ListN),
    replace(PosX, Grid, ListN, NuevaGrid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%adyacentes(+Grid, +CA, +CN, +PosX, +PosY, -NGrid, -CantAdy)
% Grid grilla de colores pasada por parametro
% CA color actual del elemento de la posicion(PosX,PosY).
% CN color nuevo a comparar
% PosX es la posicion "X" de la celda pasada por parametro
% PosY es la posicion "Y" de la celda pasada por parametro
% NGrid grilla grilla con los adyacentes 
% CantAdy cantidad total de celdas capturadas
% Retorna una grilla con los adyacentes y la cantidad de adaycentes capturados.
adyacentes(Grid, CA, CN, PosX, PosY, NGrid):-
    AdyArriba is PosX-1, 
    AdyAbajo is PosX+1,
    AdyIzq is PosY-1,
    AdyDer is PosY+1,
    cambiarC(Grid, CA, CN, AdyArriba, PosY, Grid1),
	cambiarC(Grid1, CA, CN, PosX, AdyIzq, Grid2),
	cambiarC(Grid2, CA, CN, AdyAbajo, PosY, Grid3),
	cambiarC(Grid3, CA, CN, PosX, AdyDer, NGrid).