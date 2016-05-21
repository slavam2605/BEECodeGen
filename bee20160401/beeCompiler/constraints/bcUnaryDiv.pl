% Author: Amit Metodi
% Last Updated: 01/07/2012

:- module(bcUnaryDiv, [ ]).
:- use_module('../auxs/auxLiterals').

%%% ------------------------- %%%
%%% add constraints to parser %%%
%%% ------------------------- %%%

:- Head=int_div(A,B,C,ConstrsH-ConstrsT),
   Body=(
       !,
       bcInteger:getUnaryNumber(A,Au),
       bcInteger:getUnaryNumber(B,Bu),
       auxUnarynum:unarynumDiffK(Bu,0),
       bcInteger:getUnaryNumber(C,Cu),
       bcUnaryDiv:udivType(Type),
       bcUnaryDiv:udivSimplify(bc(Type,[Au, Bu, Cu]), Constr, 1),
       (Constr==none ->
           ConstrsH=ConstrsT
       ;
           ConstrsH=[Constr|ConstrsT]
       )
   ),
   bParser:addConstraint(Head,Body).


%%% ------------------------- %%%
%%% constraints types         %%%
%%% ------------------------- %%%
udivType([
            bcUnaryDiv:udivSimplify,
            skipSimplify,
            bcUnaryDiv:udivDecompose,
            0,
            udiv
           ]).
uposdivType([
            bcUnaryDiv:uposdivSimplify,
            skipSimplify,
            0,
            bcUnaryDiv:uposdiv2cnf,
            udivpos
           ]).


% ---------------------------------
% | Simplify Unary Times          |
% ---------------------------------
udivSimplify(Constr, NewConstr, Changed):-!,
    Constr=bc(Type,[A, B, C]),
    auxUnarynum:unarynumIsRangeChanged(A,NA,Changed),
    auxUnarynum:unarynumIsChangedOrConst(B,NB,Changed),
    auxUnarynum:unarynumIsRangeChanged(C,NC,Changed),!,
    (Changed==1 ->
        (NB=(Bval,Bval,_) ->
            NewConstr=none,
            auxUnarynum:unarynumDivByConst(NA,Bval,FA),
            auxUnarynum:unarynumEquals(FA,NC) ;
         /*
         (NC=(Cval,Cval,_) -> ???
         (NA=(Aval,Aval,_) -> ???
         */
         udivBoundPropg(NA,NB,NC,FA,FB,FC,Changed2,AllPos),
         (AllPos==1 ->
             uposdivType(PosType),
             uposdivSimplify(bc(PosType,[FA,FB,FC]), NewConstr, 1) ;
         (Changed2==1 ->
             udivSimplify(bc(Type,[FA,FB,FC]), NewConstr, 1)
         ;
             NewConstr=bc(Type,[FA,FB,FC])
         )))
     ;
       NewConstr=Constr
     ).

udivBoundPropg(A,B,C,NA,NB,NC,Changed,AllPos):-!,
    A=(Amin,Amax,_),
    B=(Bmin,Bmax,_),
    ((Amin >= 0 , Bmin >= 0) -> % (A=pos,B=pos)
        Changed=1,
        AllPos=1,
        auxUnarynum:unarynumSetMin(C,0,FC),
        uposdivBoundPropg(A,B,FC,NA,NB,NC,_) ;
    ((Amax =<0 , Bmax =<0) -> % (A=neg,B=neg)
        Changed=1,
        AllPos=1,
        auxUnarynum:unarynumNeg(A,NgA),
        auxUnarynum:unarynumNeg(B,NgB),
        auxUnarynum:unarynumSetMin(C,0,FC),
        uposdivBoundPropg(NgA,NgB,FC,NA,NB,NC,_) ;
    ((Amax =< 0, Amin < 0) -> % (A=neg)
        Changed=1,
        auxUnarynum:unarynumNeg(A,NgA),
        auxUnarynum:unarynumNeg(C,NgC),
        udivBoundPropg(NgA,B,NgC,NA,NB,NC,_,AllPos) ;
    ((Bmax =< 0, Bmin < 0) -> % (B=neg)
        Changed=1,
        auxUnarynum:unarynumNeg(B,NgB),
        auxUnarynum:unarynumNeg(C,NgC),
        udivBoundPropg(NgB,A,NgC,NA,NB,NC,_,AllPos) ;
    C=(Cmin,Cmax,_),
    ((Cmin >=0, Amin >= 0) -> % (A=pos, C=pos)
        Changed=1,
        AllPos=1,
        auxUnarynum:unarynumSetMin(B,0,FB),
        uposdivBoundPropg(A,FB,C,NA,NB,NC,_) ;
    ((Cmax =<0, Amin >= 0) -> % (A=pos, C=neg)
        Changed=1,
        AllPos=1,
        auxUnarynum:unarynumNeg(B,NgB),
        auxUnarynum:unarynumSetMin(NgB,0,FB),
        auxUnarynum:unarynumNeg(C,NgC),
        uposdivBoundPropg(A,FB,NgC,NA,NB,NC,_) ;
    ((Cmin > 0, Bmin > 0) -> % (B=pos, C=pos) - when C=0 - A can still be negative.
        Changed=1,
        AllPos=1,
        auxUnarynum:unarynumSetMin(A,0,FA),
        uposdivBoundPropg(FA,B,C,NA,NB,NC,_) ;
    ((Cmax < 0, Bmin > 0) -> % (B=pos, C=neg)
        Changed=1,
        AllPos=1,
        auxUnarynum:unarynumNeg(A,NgA),
        auxUnarynum:unarynumSetMin(NgA,0,FA),
        auxUnarynum:unarynumNeg(C,NgC),
        uposdivBoundPropg(FA,B,NgC,NA,NB,NC,_) ;

    %% TODO (B=pos & c=pos) and (B=pos & c=neg) - when C=0 - A can still be negative.

    ((Cmax =<0, Cmin < 0) ->  %% (C=neg) and (-inf<A<inf) and (-inf<B<inf)
        % (Cmin < 0) is for avoid endless loop when Cmax==Cmin==0
        Changed=1,
        auxUnarynum:unarynumNeg(B,NgB),
        auxUnarynum:unarynumNeg(C,NgC),
        udivBoundPropg(A,NgB,NgC,NA,NB,NC,_,AllPos) ;

    %%% left with the possible ranges
    % (both,both,both)
    % (pos,both,both)
    % (both,pos,both)
    % (both,both,pos)
    NA=A,
    NB=B,
    NC=C
/*
    TODO:
       when A or C are pos (and B!=0) can fix range
*/
    ))))))))).




% ---------------------------------
% | Simplify Positive Unary Times |
% ---------------------------------
uposdivSimplify(Constr, NewConstr, Changed):-!,
    Constr=bc(Type,[A, B, C]),
%     auxUnarynum:unarynumIsChangedOrConst(A,NA,Changed),
    auxUnarynum:unarynumIsRangeChanged(A,NA,Changed),
    auxUnarynum:unarynumIsChangedOrConst(B,NB,Changed),
%     auxUnarynum:unarynumIsChangedOrConst(C,NC,Changed),!,
    auxUnarynum:unarynumIsRangeChanged(C,NC,Changed),!,
    (Changed==1 ->
        (NB=(Bval,Bval,_) ->
            NewConstr=none,
            auxUnarynum:unarynumDivByConst(NA,Bval,FA),
            auxUnarynum:unarynumEquals(FA,NC) ;
        (NC=(0,0,_) ->
             auxUnarynum:unarynumAddConst(NA,1,Au),
             bcUnaryLEq:unaryLEqType(LEqType),
             bcUnaryLEq:unaryLEqSimplify(bc(LEqType,[Au,NB]), NewConstr, 1) ;
         uposdivBoundPropg(NA,NB,NC,NNA,NNB,NNC,DomChanged),
         (DomChanged==1 ->
               uposdivSimplify(bc(Type,[NNA,NNB,NNC]), NewConstr, 1)
         ;
               NewConstr=bc(Type,[NNA,NNB,NNC])
         )))
     ;
         NewConstr=Constr
     ).


% -----------------------------
% |   Unary Div Simplify -    |
% |   Boundary propagation    |
% -----------------------------
uposdivBoundPropg(A,B,C,NA,NB,NC,Changed):-!,
    A=(Amin,Amax,_),
    B=(Bmin,Bmax,_),
    C=(Cmin,Cmax,_),

    NBmin is max(Bmin,ceil(Amin/Cmax)),
    (Cmin > 0 ->
        NBmax is min(Bmax,Amax // Cmin)
    ;
        NBmax = Bmax
    ),
    
    NAmin is max(Amin,Cmin*NBmin),
    NAmax is min(Amax,(Cmax + 1)*NBmax - 1),

    NCmax is min(Cmax, NAmax // NBmin),
    NCmin is max(Cmin, NAmin // NBmax),
    
    auxUnarynum:unarynumBoundsChanged(A,NAmin,NAmax,NA,Changed),
    auxUnarynum:unarynumBoundsChanged(C,NCmin,NCmax,NC,Changed),
    ((NBmax =< NAmax + 1 ; NAmax < NBmin) ->
        auxUnarynum:unarynumBoundsChanged(B,NBmin,NBmax,NB,Changed)
    ;
        auxUnarynum:unarynumBoundsChanged(B,NBmin,NBmax,TB,Changed),
        Changed=1,
        FBmax is NAmax + 1,
        auxUnarynum:unarynumFocusOn(TB,NBmin,FBmax,NB)
    ).


% -----------------------
% | decompose unary div |
% -----------------------
udivDecompose(bc(_,[A,B,C]),Constrs):-!,
    % (both,both,both)
    % (pos,both,both)
    % (both,both,pos)

    % Get A pos and neg
    A=(Amin,_),
    (Amin >=0 -> % (pos,both,both)
        Alt0= -1,
        (Amin>0 ->
            Agt0=1
        ;
            A=(_,_,[Agt0|_],_)
        ),
        AMAXu=A,
        MaxAconstr=none
    ;
        bcUnaryAbs:splitUnaryNumToPosNeg(A,Apos,Aneg),
        Apos=(_,Amax1,[Agt0|_],_),
        Aneg=(_,Amax2,[Alt0|_],_),
        Amax3 is max(Amax1,Amax2),
        auxUnarynum:unarynumNewInRange(0,Amax3,AMAXu),
        MaxAconstr=bc(MaxType,[AMAXu,Apos,Aneg])
    ),!,

    % Get B pos and neg
    B=(Bmin,_),
    (Bmin >=0 -> % (pos,both,both)
        Blt0= -1,
        (Bmin>0 ->
            Bgt0=1
        ;
            B=(_,_,[Bgt0|_],_)
        ),
        BMAXu=B,
        MaxBconstr=none
    ;
        bcUnaryAbs:splitUnaryNumToPosNeg(B,Bpos,Bneg),
        Bpos=(_,Bmax1,[Bgt0|_],_),
        Bneg=(_,Bmax2,[Blt0|_],_),
        Bmax3 is max(Bmax1,Bmax2),
        auxUnarynum:unarynumNewInRange(1,Bmax3,BMAXu),
        MaxBconstr=bc(MaxType,[BMAXu,Bpos,Bneg])
    ),!,
    
    % Get C pos and neg
    C=(Cmin,_),
    (Cmin >= 0 ->
        Clt0= -1,
        (Cmin>0 ->
            Cgt0=1
        ;
            C=(_,_,[Cgt0|_],_)
        ),
        CMAXu=C,
        MaxCconstr=none
    ;
        bcUnaryAbs:splitUnaryNumToPosNeg(C,Cpos,Cneg),
        Cpos=(_,Cmax1,[Cgt0|_],_),
        Cneg=(_,Cmax2,[Clt0|_],_),
        Cmax3 is max(Cmax1,Cmax2),
        auxUnarynum:unarynumNewInRange(0,Cmax3,CMAXu),
        MaxCconstr=bc(MaxType,[CMAXu,Cpos,Cneg])
    ),!,
    
    bcAtLeastOne:aloType(ALOType),
    auxLiterals:lits2plits([-Agt0,-Bgt0,-Clt0],OrVec1),
    auxLiterals:lits2plits([-Alt0,-Blt0,-Clt0],OrVec2),
    auxLiterals:lits2plits([-Agt0,-Blt0,-Cgt0],OrVec3),
    auxLiterals:lits2plits([-Alt0,-Bgt0,-Cgt0],OrVec4),

    bcUnaryMax:unaryMaxType(MaxType),

    uposdivType(DivType),!,
    uposdivSimplify(bc(DivType,[AMAXu,BMAXu,CMAXu]), DivConstr, 1),!,

    RConstrs=[
              MaxAconstr,
              MaxBconstr,
              MaxCconstr,
              DivConstr,
              bc(ALOType,OrVec1),
              bc(ALOType,OrVec2),
              bc(ALOType,OrVec3),
              bc(ALOType,OrVec4)
             ],
    removeNoneConstrs(RConstrs,Constrs).

removeNoneConstrs([none|Cs],NNCs):-!,
    removeNoneConstrs(Cs,NNCs).
removeNoneConstrs([C|Cs],[C|NNCs]):-!,
    removeNoneConstrs(Cs,NNCs).
removeNoneConstrs([],[]):-!.


% --------------------------
% | encode pos unary times |
% --------------------------
uposdiv2cnf(bc(_,[A,B,C]),CnfH-CnfT):-!,
    A=(Amin,_,Abits,_),
    B=(Bmin,Bmax,Bbits,_),
    C=(Cmin,_,Cbits,_),

    I is Bmin + 1,
    J is Cmin + 1,
    K is Amin + 1,
    %  C>=j -> A>= Bmin*j
    encodeDiv_Bmin(Cbits,J,Bmin,Abits,K,CnfH-CnfM1),
    encodeDiv_Bmax(Cbits,J,Bmax,Abits,K,CnfM1-CnfM2),
    (Cmin > 0 ->
        encodeDiv_Cmin(Bbits,I,Cmin,Abits,K,CnfM2-CnfM3)
    ;
        CnfM2=CnfM3
    ),
    encodeDiv(Bbits,I,Cbits,J,Abits,K,CnfM3-CnfT).


encodeDiv([Bi|Bs],I,Cs,J,As,K,CnfH-CnfT):-!,
    %  Bi & Cj -> Ai*j    (B>=i & C>=j -> A>= i*j)
    encodeDiv_pos(Cs,J,Bi,I,As,K,CnfH-CnfM1),
    %  -Bi & -Cj -> -A[(i-1)*j]     (B<i & C<j -> A < (i-1)*j)
    encodeDiv_neg(Cs,J,Bi,I,As,K,CnfM1-CnfM2),
    I1 is I + 1,
    encodeDiv(Bs,I1,Cs,J,As,K,CnfM2-CnfT).
encodeDiv([],_,_,_,_,_,Cnf-Cnf):-!.


encodeDiv_pos([Cj|Cs],J,Bi,I,As,K,CnfH-CnfT):-!,
    Adrop is I*J - K,
    (Adrop >= 0 ->
        (auxLists:listDropFrom(Adrop,As,[Aij|RAs]) ->
            CnfH =[[-Bi, -Cj, Aij]|CnfM],
            J1 is J + 1,
            NK is K + Adrop + 1,
            encodeDiv_pos(Cs,J1,Bi,I,RAs,NK,CnfM-CnfT)
        ;
            CnfH =[[-Bi, -Cj]|CnfT]
        )
    ;
        J1 is J + 1,
        encodeDiv_pos(Cs,J1,Bi,I,As,K,CnfH-CnfT)
    ).
encodeDiv_pos([],_,_,_,_,_,Cnf-Cnf):-!.

encodeDiv_neg([Cj|Cs],J,Bi,I,As,K,CnfH-CnfT):-!,
    Adrop is (I - 1)*J - K,
    (Adrop >= 0 ->
        (auxLists:listDropFrom(Adrop,As,[Aij|RAs]) ->
            CnfH =[[Bi, Cj, -Aij]|CnfM],
            J1 is J + 1,
            NK is K + Adrop + 1,
            encodeDiv_neg(Cs,J1,Bi,I,RAs,NK,CnfM-CnfT)
        ;
            CnfH=CnfT
        )
    ;
        CnfH =[[Bi, Cj]|CnfM],
        J1 is J + 1,
        encodeDiv_neg(Cs,J1,Bi,I,As,K,CnfM-CnfT)
    ).
encodeDiv_neg([],J,Bi,I,As,K,CnfH-CnfT):-!,
    Adrop is (I - 1)*J - K,
    (Adrop >= 0 ->
        (auxLists:listDropFrom(Adrop,As,[Aij|_]) ->
            CnfH =[[Bi, -Aij]|CnfT]
        ;
            CnfH =CnfT
        )
    ;
        writeln(div_simplify_isnt_complete(neg_b)),
        CnfH =[[Bi]|CnfT]
    ).





encodeDiv_Bmin([Cj|Cs],J,Bmin,As,K,CnfH-CnfT):-!,
    Adrop is Bmin*J - K,
    (Adrop >= 0 ->
        (auxLists:listDropFrom(Adrop,As,[Aij|RAs]) ->
            CnfH =[[-Cj, Aij]|CnfM],
            J1 is J + 1,
            NK is K + Adrop + 1,
            encodeDiv_Bmin(Cs,J1,Bmin,RAs,NK,CnfM-CnfT)
        ;
            writeln(div_simplify_isnt_complete(bmin_c)),
            CnfH =[[-Cj]|CnfT]
        )
    ;
        J1 is J + 1,
        encodeDiv_Bmin(Cs,J1,Bmin,As,K,CnfH-CnfT)
    ).
encodeDiv_Bmin([],_,_,_,_,Cnf-Cnf):-!.

encodeDiv_Bmax([Cj|Cs],J,Bmax,As,K,CnfH-CnfT):-!,
    Adrop is Bmax*J - K,
    (Adrop >= 0 ->
        (auxLists:listDropFrom(Adrop,As,[Aij|RAs]) ->
            CnfH =[[Cj, -Aij]|CnfM],
            J1 is J + 1,
            NK is K + Adrop + 1,
            encodeDiv_Bmax(Cs,J1,Bmax,RAs,NK,CnfM-CnfT)
        ;
            CnfH=CnfT
        )
    ;
        writeln(div_simplify_isnt_complete(bmax_c)),
        CnfH =[[Cj]|CnfT]
    ).
encodeDiv_Bmax([],J,Bmax,As,K,CnfH-CnfT):-!,
    Adrop is Bmax*J - K,
    (Adrop >= 0 ->
        (auxLists:listDropFrom(Adrop,As,[Aij|_]) ->
            writeln(div_simplify_isnt_complete(bmax_a)),
            CnfH =[[-Aij]|CnfT]
        ;
            CnfH =CnfT
        )
    ;
        writeln(div_simplify_isnt_complete(bmax_unsat)),
        throw(unsat)
    ).
    
    
encodeDiv_Cmin([Bi|Bs],I,Cmin,As,K,CnfH-CnfT):-!,
    Adrop is I*Cmin - K,
    (Adrop >= 0 ->
        (auxLists:listDropFrom(Adrop,As,[Aij|RAs]) ->
            CnfH =[[-Bi, Aij]|CnfM],
            I1 is I + 1,
            NK is K + Adrop + 1,
            encodeDiv_Cmin(Bs,I1,Cmin,RAs,NK,CnfM-CnfT)
        ;
            writeln(div_simplify_isnt_complete(cmax_b)),
            CnfH =[[-Bi]|CnfT]
        )
    ;
        I1 is I + 1,
        encodeDiv_Cmin(Bs,I1,Cmin,As,K,CnfH-CnfT)
    ).
encodeDiv_Cmin([],_,_,_,_,Cnf-Cnf):-!.
