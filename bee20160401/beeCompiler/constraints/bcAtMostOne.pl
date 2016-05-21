% Author: Amit Metodi
% Last Updated: 29/01/2016

:- module(bcAtMostOne, [ ]).
:- use_module('../auxs/auxLiterals').
%:- use_module('../auxs/atMostOne').

amoType([
         bcAtMostOne:amoSimplify,
         skipSimplify,
         0,
         bcAtMostOne:amo2cnf,
         atMostOne
        ]):-!.

amoSimplify(Constr,NewConstr,Changed):-!,
    Constr=bc(Type,Vec),
    atMostOne:atMostOneSimplify(Vec,NVec,FoundOne,Changed),
    ((ground(FoundOne) ; NVec=[] ; NVec=[_]) -> % FoundOne==(1 or 2)
        Changed=1,
        NewConstr=none ;
    (Changed==1 ->
        NewConstr=bc(Type,NVec)
    ;
        NewConstr=Constr
    )).
    
    
:- if(bSettings:atMostOneEncoding(product)).

%%% ----- Product Encoding for At Most One ----- %%%

amo2cnf(bc(_Type,Vec),CnfH-CnfT):-!,
    plits2lits(Vec,Xs),!,
    length(Xs,N),!,
    atMostOne2clauses(Xs,N,CnfH-CnfT).

atMostOne2clauses(Xs,XsLen,CnfH-CnfT):-!,
    (XsLen < 25 ->
        atMostOneDirectCnf(Xs,CnfH-CnfT)
    ;
        calculateDs(XsLen,Ulen,Vlen),
        length(U,Ulen),
        atMostOne2clauses(U,Ulen,CnfH-CnfM1),
        length(V,Vlen),!,
        atMostOne2clauses(V,Vlen,CnfM1-CnfM2),!,
        atMostOne2clauses_d1(U,Xs,U,CnfM2-CnfM3),!,
        atMostOne2clauses_d2(V,Xs,Ulen,CnfM3-CnfT)
    ).

calculateDs(N,D1,D2):-
    D1 is ceil(sqrt(N)),
    D2 is ceil(N / D1).

atMostOne2clauses_d1([Ui|Us],[Xij|Xs],OrgUs,[[-Xij,Ui]|CnfH]-CnfT):-!,
    atMostOne2clauses_d1(Us,Xs,OrgUs,CnfH-CnfT).
atMostOne2clauses_d1(_,[],_,Cnf-Cnf):-!.
atMostOne2clauses_d1([],Xs,Us,CnfH-CnfT):-!,
    atMostOne2clauses_d1(Us,Xs,Us,CnfH-CnfT).

atMostOne2clauses_d2([Vi],Xs,_,CnfH-CnfT):-!,
    xiDragY(Xs,Vi,CnfH-CnfT).
atMostOne2clauses_d2([Vi|Vs],Xs,ULen,CnfH-CnfT):-!,
    auxLists:listSplit(ULen,Xs,XsVi,RXs),
    xiDragY(XsVi,Vi,CnfH-CnfM),!,
    atMostOne2clauses_d2(Vs,RXs,ULen,CnfM-CnfT).

:- elif(bSettings:atMostOneEncoding(commander(_))).

amo2cnf(bc(_Type,Vec),CnfH-CnfT):-!,
    plits2lits(Vec,Xs),!,
    length(Xs,N),
    atMostOneDirectCnf(Xs,N,CnfH-CnfT).

atMostOne2clauses(Xs,N,CnfH-CnfT):-!,
    bSettings:atMostOneEncoding(commander(K)),!,
    amoCommanderCnf(Xs,N,K,CnfH-CnfT).

amoCommanderCnf(Xs,N,K,CnfH-CnfT):-!,
   (N =< K+1 ->
       atMostOneDirectCnf(Xs,CnfH-CnfT)
   ;
       auxLists:listCalcPartitions(N,K,Parts,0,PartsCnt),
       bcExactlyOne:exoCommanderCnf_(Parts,Xs,Ys,CnfH-CnfM),!,
       amoCommanderCnf(Ys,PartsCnt,K,CnfM-CnfT)
   ).

:- elif(bSettings:atMostOneEncoding(direct)).

%%% ----- Standard Encoding for At Most One ----- %%%

amo2cnf(bc(_Type,Vec),CnfH-CnfT):-!,
    plits2lits(Vec,Xs),!,
    atMostOneDirectCnf(Xs,CnfH-CnfT).

atMostOne2clauses(Xs,_XsLen,CnfH-CnfT):-!,
    atMostOneDirectCnf(Xs,CnfH-CnfT).

:- else.
:- bSettings:atMostOneEncoding(X), writef('ERROR: "%w" is wrong value for bSettings:atMostOneEncoding !\n',[X]),flush_output,halt.
:- endif.


atMostOneDirectCnf([_],Cnf-Cnf):-!.
atMostOneDirectCnf([X|Xs],CnfH-CnfT):-!,
        xiDragY(Xs,-X,CnfH-CnfM),
        atMostOneDirectCnf(Xs,CnfM-CnfT).

xiDragY([Xi|Xs],Y,[[-Xi,Y]|CnfH]-CnfT):-!,
    xiDragY(Xs,Y,CnfH-CnfT).
xiDragY([],_,Cnf-Cnf):-!.