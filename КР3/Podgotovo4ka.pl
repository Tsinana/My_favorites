
%15without6

%1
%digit_sum(+D,?Sum)
digit_sum(D,Sum):-digit_sum(D,0,Sum).
digit_sum(0,Sum,Sum):-!.
digit_sum(D,S,Sum):- D1 is (D div 10), S1 is S +(D mod 10),digit_sum(D1,S1,Sum).

%2
%list_min(+List,?Min)
list_min([H|T],Min):- list_min(T,H,Min).
list_min([],M,M):-!.
list_min([H|T],M,Min):- H<M, !, list_min(T,H,Min).
list_min([_|T],M,Min):- list_min(T,M,Min).

%3
%string_write(+List)
string_write([]):- !.
string_write([H|T]):- write(H), string_write(T).

%comb(+List,K,-Sochet)
comb(List,K,Sochet):- sochetWR(List,Sochet,K),string_write(Sochet),nl,fail.

%sochet(Set,NewSet,K)
sochetWR(_,[],0):-!.
sochetWR([H|Set],[H|NewSet],K):-K1 is K - 1,sochetWR([H|Set],NewSet,K1).
sochetWR([_|Set],NewSet,K):-sochetWR(Set,NewSet,K).

%4
%inList1(+List,?H)
inList1([[H,Q]|_],[H,Q]).
inList1([[_|_]|T],[H,Q]):-inList1(T,[H,Q]).

%listQuantityDigit(+List,+D,?Qu)
listQuantityDigit(List,D,Qu):- lqd(List,D,0,Qu).
lqd([],_,Q,Q):-!.
lqd([H|T],D,Q,Qu):- digit_sum(H,Sum),Sum is D,!,Q1 is Q + 1, lqd(T,D,Q1,Qu).
lqd([_|T],D,Q,Qu):- lqd(T,D,Q,Qu).


%listFunc(+List1,-List2)
listFunc(List1,List2):- listFunc(List1,List3,[]),listFunc(List1,List1,List3,List2).

listFunc([],List2,List2):-!.
listFunc([H|T],List3,List2):-digit_sum(H,Sum),inList1(List2,[Sum,_]),!,
    listFunc(T,List3,List2).
listFunc([H|T],List3,List2):-digit_sum(H,Sum),
    append(List2,[[Sum,_]],ListNew2),listFunc(T,List3,ListNew2).

listFunc([],_,L,L):-!.
listFunc([H|T],List1,List3,List2):-digit_sum(H,Sum),listQuantityDigit(List1,Sum,Qu),
    inList1(List3,[Sum,Qu]),listFunc(T,List1,List3,List2).

%5
%listMax(+List,-X)

listMax([[H,Id]|T],X):-lm(T,Id,Y),inList1([[H,Id]|T],[X,Y]).
lm([],X,X):-!.
lm([[_,H]|T],Mx,X):- H > Mx, !,
    lm(T,H,X).
lm([[_,_]|T],Mx,X):- lm(T,Mx,X).

%listMaxUp(+List,-Max)
%listMax(List,Max):-listMax

%XXX

%функция Эйлера
funcAiler(Digit,Qu):-fa(Digit,1,0,Qu).
fa(D,D,Q,Q):-!.
fa(D,I,Q,Qu):- nod(D,I,1),!,
    I1 is I + 1, Q1 is Q + 1, fa(D,I1,Q1,Qu).
fa(D,I,Q,Qu):- I1 is I + 1, fa(D,I1,Q,Qu).

nod(D,I,X):- 0 is D mod I,!, X is I.
nod(D,I,X):- D1 is I, I1 is D mod I, nod(D1,I1,X).

%listPr(+List,?Pr)
listPr(List,Pr):- lp(List,1,Pr).
lp([],G,G):-!.
lp([H|T],P,X):- P1 is P * H, lp(T,P1,X).

%Вариант 4

inList([H|_],H).
inList([_|T],H):-inList(T,H).
%1
%isSimple(+Digit)
isSimple(D):-digitQuantityDiv(D,C),C is 2;D is 1.

%digitQuantityDiv(+Digit,?Qu)
digitQuantityDiv(D,Qu):- aqd(D,1,0,Qu).
aqd(D,D1,I,I):-D is D1 - 1,!.
aqd(D,I,Q,Qu):- 0 is D mod I,!,
    I1 is I + 1,Q1 is Q + 1,aqd(D,I1,Q1,Qu).
aqd(D,I,Q,Qu):-I1 is I + 1,aqd(D,I1,Q,Qu).

%2

%3
%listSimple(List)
listSimple([]):-true.
listSimple([H|T]):-isSimple(H),!,listSimple(T).
listSimple(_):-false.

%4
listfunc2(List1,List2):-lf2(List1,[],List2).
lf2([],L,L):-!.
lf2([H|T],List2,List3):-isSimple(H),not(inList(List2,H)),!,
    append(List2,[H],NL),lf2(T,NL,List3).
lf2([_|T],List2,List3):-lf2(T,List2,List3).

listfunc3(List1,List2):-lf3(List1,[],List2).

lf3([],L,L):-!.
lf3([H|T],List2,List3):-lf33(H,List2,NL),lf3(T,NL,List3).

lf33(H,List2,NL):-lf33(H,1,List2,NL).
lf33(H,H1,L,L):-H is H1 - 1,!.
lf33(H,I,List2,NL):- 0 is H mod I,not(inList(List2,I)),!,
    append(List2,[I],NL2),I1 is I + 1,lf33(H,I1,NL2,NL).
lf33(H,I,List2,NL):-I1 is I + 1, lf33(H,I1,List2,NL).

lf4(List,K):-listfunc2(List,NList),comb(NList,K,Sochet),listSimple(Sochet),string_write(Sochet).

%Вариант 5.
%1
digitSov(D):- ds(D,Sum),D is Sum.
ds(D,Sum):-ds(D,1,0,Sum).
ds(D,D1,S,S):- D1 is D - 1,!.
ds(D,I,S,Sum):-0 is D mod I,!,S1 is S + I,ds(D,I,S1,Sum).

%2
listSov(List1,List2):-listSov(List1,[],List2).
listSov([],L,L):-!.
listSov([H|T],List,List2):-digitSov(H),!,append(List,[H],LN),listSov(T,LN,List2).
listSov([_|T],List,List2):-listSov(T,List,List2).

%3
pr(List,Per):-per(List,[],Per).

inListEx([H|T],H,T).
inListEx([H|T],El,[H|TT]):-inListEx(T,El,TT).

per([],PP,PP):-!.
per(List,Per,PP):-inListEx(List,El,A),per(A,[El|Per],PP).

prK(List,Per,K):-perK(List,[],K,Per).

perK(_,PP,0,PP):-!.
perK(List,Per,K,PP):-inListEx(List,El,_),K1 is K - 1,perK(List,[El|Per],K1,PP).

sochet(_,[],0).
sochet([H|Set],[H|SubSet],K):- K1 is K - 1, sochet(Set,SubSet,K1).
sochet([_|Set],SubSet,K):-sochet(Set,SubSet,K).

inListId(List,Id,El):-ili(List,1,Id,El).
ili([H|_],I,I,H):-!.
ili([_|T],I,Id,H):-I1 is I + 1, ili(T,I1,Id,H).

%4
pr1():-
    %длина 6 _._._._._._   над abcde
    List=[_,_,_,_,_,_],
    sochet([1,2,3,4,5,6],[P1,P2,P3],3),
    inListId(List,P1,a),
    inListId(List,P2,a),
    inListId(List,P3,a),
    inListEx([1,2,3,4,5,6],P1,NL),
    inListEx(NL,P2,NL1),
    inListEx(NL1,P3,[Pos1,Pos2,Pos3]),
    prK([b,c,d,e],[PP1,PP2,PP3],3),
    inListId(List,Pos1,PP1),
    inListId(List,Pos2,PP2),
    inListId(List,Pos3,PP3),
    write(List),nl, fail.

%вариант 3 говна вариант

%1
max_cifr_list(N,Max):-mc(N,Max).
mc([H|[]],H):-!.
mc([H|T],M):-mc(T,M1),
    (H<M1,M is M1);
    M is H.

max_cifr(N,Max):-mc1(N,Max).
mc1(N,M):-0 is N div 10,!,M is N.
mc1(N,M):-
    H is N div 10,
    T is N mod 10,
    mc1(H,M1),
    (T < M1, M is M1);
    T is N mod 10,
    M is T.

%2
comb1(List,SubSet):-sochetNR(List,SubSet),write(SubSet),nl,fail.

sochetNR([],[]).
sochetNR([H|Set],[H|SubSet]):-sochetNR(Set,SubSet).
sochetNR([_|Set],SubSet):-sochetNR(Set,SubSet).


powerset([], []).
powerset([H|Sub_set], [H|SetTail]) :- powerset(Sub_set, SetTail).
powerset(Sub_set, [_|SetTail]) :- powerset(Sub_set, SetTail).
powerset(Set) :- powerset(A, Set), write("\t"), write(A), nl, fail.

%3
equal_max([H|T]):-max_cifr(H,M),em(T,M).

em([],_):-true,!.
em([H|T],M):-max_cifr(H,M),em(T,M).

%4
listFunc3(List):-
    max_cifr_list(List,M),sochetNR(List,Sochet),
    max_cifr_list(Sochet,M),write(Sochet),nl,fail.

%5
listFunc4(List):- write(List).

%uber0 Вариант 1
%1
vz_prost(X,Y):-vp(X,Y).
vp(X,Y):- 0 is X mod Y,!,Y is 1.
vp(X,Y):-
    X1 is Y,
    Y1 is X mod Y,
    vp(X1,Y1).

%2

digitsum(D,S):-ds1(D,0,S).
ds1(0,S,S):-!.
ds1(D,S,Sn):-
    T is D div 10,
    H is D mod 10,
    S1 is S + H,
    ds1(T,S1,Sn).
%
%
max_list(List1,Idx):-ml(List1,[],List2),write(List2),fnc(List2,I),listFind(List2,El,I),
    listFind(List1,El,Idx).

ml([],L,L):-!.
ml([H|T],NL,List2):- digitsum(H,S),S<H,vz_prost(H,S), !,
    append(NL,[H],LL),ml(T,LL,List2).
ml([_|T],NL,List2):- ml(T,NL,List2).

fnc([H|T],Idx):-digitsum(H,M),fnc(T,M,1,0,Idx).

fnc([],_,_,M,M):-!.
fnc([H|T],M,I,_,Idx):-digitsum(H,M1),M1 > M, !,
    I1 is I + 1,
    fnc(T,M1,I1,I,Idx).
fnc([_|T],M,I,IM,Idx):-I1 is I + 1, fnc(T,M,I1,IM,Idx).

listFind(List,El,Idx):-lf(List,0,El,Idx).
lf([H|_],I,H,I):-!.
lf([_|T],I,H,In):-
    I1 is I + 1,
    lf(T,I1,H,In).

%Вариант 2*

%1
fib(1,1):-!.
fib(2,1):-!.
fib(N,A):- N1 is N - 1, N2 is N - 2, fib(N1, X1), fib(N2, X2), A is X1 + X2.

fib_d(_,B,N,N,X):-!, X is B.
fib_d(A1,A2,N1,N,X):- B is A1 + A2, N2 is N1 + 1, fib_d(A2,B,N2,N,X).
fib_d(N,X):- fib_d(1,1,2,N,X).

%2
listFib([],0):-!.
listFib([[N,X]|T],C):-fib(N,X), !,
    listFib(T,C1),C is C1 + 1.
listFib([_|T],C):-listFib(T,C).

%3

inListEx1([H|T],H,T).
inListEx1([H|T],El,[H|TT]):-inListEx1(T,El,TT).

razm(List,K,Razm):-b_a_r_NR(List,[],K,Razm).

b_a_r_NR(_,Per,0,Per):-!.
b_a_r_NR(List,Per,K,Razm):- inListEx1(List,El,A),K1 is K - 1, b_a_r_NR(A,[El|Per],K1,Razm).

razmWR(List,K,Razm):-barWR(List,[],K,Razm).

barWR(_,Per,0,Per):-!.
barWR(List,Per,K,Razm):- inListEx1(List,El,_), K1 is K - 1, barWR(List,[El|Per],K1,Razm).

%4 в падлу описание задачи так себе

%5
inList2(List,Id,El):-ili1(List,1,Id,El).

ili1([H|_],I,I,H).
ili1([_|T],I,Id,El):-
    I1 is I + 1,
    ili(T,I1,Id,El).

fff:-
    List=[_,_,_,_,_,_],
    razm([a,b,c,d,e],3,[R1,R2,R3]),
    inList2(List,1,R1),inList2(List,2,R2),inList2(List,3,R3),
    razmWR([v,w,x,z,y],3,[R11,R22,R33]),
    inList2(List,4,R11),inList2(List,5,R22),inList2(List,6,R33),
    write(List),nl,fail.

