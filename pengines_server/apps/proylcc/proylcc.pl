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

% No tengo que verificar si la pos se cae en flick, 
% on click nunca entra con negativos o fuera de rango

% Obtengo el Elemento E de una Grilla Grid
obtenerElemGrilla(PosX, PosY, Grid, E):- 
    nth0(PosX, Grid, List), nth0(PosY, List, E).

% pintar Un elemento de mi grilla 
% busco mi elemento E, si es igual lo cambio de color y busco sus adyacentes
cambiarC(Grid, CA, CN, PosX, PosY, NGrid):-
   cambiarColorG(Grid, CA, CN, PosX, PosY, NGrid),
   adyacentes(Grid, CA, CN, PosX, PosY, NGrid).
    
cambiarC(Grid, CA, CN, PosX, PosY, NGrid):-
     not(cambiarColorG(Grid, CA, CN, PosX, PosY, NGrid)). 
    
adyacentes(Grid, CA, CN, PosX, PosY, NGrid):-
    AdyArriba is PosX-1, 
    AdyAbajo is PosX+1,
    AdyIzq is PosY-1,
    AdyDer is PosY+1,
    cambiarC(Grid, CA, CN, AdyArriba, PosY, NGrid),
	cambiarC(Grid,PosX,AdyIzq,X,Y,NGrid),
	cambiarC(Grid,AdyAbajo,PosY,X,Y,NGrid),
	cambiarC(Grid,PosX,AdyDer,X,Y,NGrid).
    
    
obtenerElemF(PosX, Grid, E):- nth0(PosX, Grid, E).

replace(Pos, L, EN, ListN) :-
  nth0(Pos, L, _, R),
  nth0(Pos, ListN, EN, R).

cambiarColorF(CA, CN, PosY, Grid, ListN):-
   obtenerElemF(PosY, Grid, CA), replace(PosY, Grid, CN, ListN).

cambiarColorG(Grid, CA, CN, PosX, PosY, GridN):-
   obtenerElemF(PosX, Grid, F), cambiarColorF(CA, CN, PosY, F, L),
   replace(PosX, Grid, L, GridN).   