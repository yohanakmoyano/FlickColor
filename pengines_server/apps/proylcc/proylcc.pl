:- module(proylcc, 
	[  
		flick/5
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% flick(+Grid, +Color, -FGrid)
%
% FGrid es el resultado de hacer 'flick' de la grilla Grid con el color Color.
% Retorna false si Color coincide con el color de la celda superior izquierda de la grilla. 

flick(Grid, CN, FGrid, PosX, PosY):-
	obtenerElemGrilla(PosX, PosY, Grid, E),
	cambiarC(Grid, E, CN, PosX, PosY, FGrid).

%No tengo que verificar si la pos se cae en flick, 
%on click nunca entra con negativos o fuera de rango

%Obtengo el Elemento E de una Grilla Grid
obtenerElemGrilla(PosX, PosY, Grid, E):- 
    nth0(PosX, Grid, List), nth0(PosY, List, E).

%pintar Un elemento de mi grilla 
%busco mi elemento E, si es igual lo cambio de color y busco sus adyacentes
cambiarC(Grid, CA, CN, PosX, PosY, NGrid):-
   obtenerElemGrilla(PosX, PosY, Grid, E), CA=E,
   cambiarElemG(PosX, PosY, Grid, CN, AuxGrid),
   adyacentes(AuxGrid, CA, CN, PosX, PosY, NGrid).
    
cambiarC(Grid, CA, _CN, PosX, PosY, Grid):-
     obtenerElemGrilla(PosX, PosY, Grid, E), CA\=E.  

cambiarC(Grid, _CA, _CN, PosX, PosY, Grid):-
(PosX<0); (PosY<0); (PosX>13); (PosY>13).

replace(Pos, L, EN, ListN) :-
  nth0(Pos, L, _, R),
  nth0(Pos, ListN, EN, R).

cambiarElemG(PosX, PosY, Grid, CN, NueGrid):-
    nth0(PosX, Grid, L),
    replace(PosY, L, CN, ListN),
    replace(PosX, Grid, ListN, NueGrid).

adyacentes(Grid, CA, CN, PosX, PosY, NGrid):-
    AdyArriba is PosX-1, 
    AdyAbajo is PosX+1,
    AdyIzq is PosY-1,
    AdyDer is PosY+1,
    cambiarC(Grid, CA, CN, AdyArriba, PosY, Grid1),
	cambiarC(Grid1, CA, CN, PosX, AdyIzq, Grid2),
	cambiarC(Grid2, CA, CN, AdyAbajo, PosY, Grid3),
	cambiarC(Grid3, CA, CN, PosX, AdyDer, NGrid).
    
obtenerElemF(PosX, Grid, E):- nth0(PosX, Grid, E).
