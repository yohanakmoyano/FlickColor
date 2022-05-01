:- module(proylcc, 
	[  
		flick/3
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% flick(+Grid, +Color, -FGrid)
%
% FGrid es el resultado de hacer 'flick' de la grilla Grid con el color Color.
% Retorna false si Color coincide con el color de la celda superior izquierda de la grilla. 

flick(Grid,Color,Grid,_PosX,_PosY):-
	Grid = [F|_],
	F = [Color|_].

flick(Grid,Color,FGrid,PosX,PosY):-
	Grid = [F|_],
	F = [X|_],
	pintar(X,Color,Grid,PosX,PosY,FGrid,_).

pintar(Ant,Color,Grid,X,Y,Rta,CantPintado):-
	getColorEnPos(Grid,X,Y,PosCol),
	Ant=PosCol,
	cambiarColorEnPosicion(Color,Grid,X,Y,RtaA),
	pintarContorno(Ant,Color,RtaA,X,Y,Rta,CPTotal),
	CantPintado is CPTotal+1.

pintar(Ant,_,Grid,X,Y,Grid,0):-
	getColorEnPos(Grid,X,Y,PosCol),
	Ant\=PosCol.

pintar(_,_,[G|Grid],X,Y,[G|Grid],0):-
	X<0;
	Y<0;
	largo([G|Grid],LF),	X>=LF;
	largo(G,LC),	Y>=LC.

pintarContorno(Ant,Color,Grid,X,Y,Rta,CantPintado):-
	Xmen is X-1,Ymen is Y-1,
	Xmas is X+1,Ymas is Y+1,
	pintar(Ant,Color,Grid,Xmen,Y,RtaA,CPA),
	pintar(Ant,Color,RtaA,Xmas,Y,RtaB,CPB),
	pintar(Ant,Color,RtaB,X,Ymas,RtaC,CPC),
	pintar(Ant,Color,RtaC,X,Ymen,Rta,CPD),
	CantPintado is CPA+CPB+CPC+CPD.

getColorEnPos([G|_],X,0,Rta):-
	getColorEnLista(G,X,Rta).
getColorEnPos([_|Grid],X,Y,Rta):-
	YY is Y-1,
	getColorEnPos(Grid,X,YY,Rta).

getColorEnLista([L|_],0,L).
getColorEnLista([_|Ls],X,Rta):-
	XX is X-1,
	getColorEnLista(Ls,XX,Rta).

reemplazarEnLista(Color,[_|Ls],0,[Color|Ls]).
reemplazarEnLista(Color,[L|Ls],X,[L|Rta]):-
	XX is X-1,
	reemplazarEnLista(Color,Ls,XX,Rta).

cambiarColorEnPosicion(Color,[G|Grid],X,0,[Rta|Grid]):-
	reemplazarEnLista(Color,G,X,Rta).
cambiarColorEnPosicion(Color,[G|Grid],X,Y,[G|Rta]):-
	YY is Y-1,
	cambiarColorEnPosicion(Color,Grid,X,YY,Rta).

largo([],0).
largo([_|Xs],Rta):- largo(Xs,Rtaa),
	Rta is Rtaa+1.