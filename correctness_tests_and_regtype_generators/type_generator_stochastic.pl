:- module(type_generator_stochastic, [tnum/1,gentypes/5,pretty_print_type_definitions/1], [hiord, fsyntax, assertions, doccomments]).

%! \title Stochastic regtype generator.
%! \module
% 
%  This module contains the operations required to stochastically
%  generate all possible regular type combinations from a list of
%  allowed functors & basic types, subject to a bound on the number of
%  types and clauses per type.


:- use_module(library(streams)).
:- use_module(library(stream_utils)).
:- use_module(typeslib(typeslib)).
:- use_module(typeslib(regtype_basic_lattice)).
:- use_module(library(lists)).
:- use_module(library(aggregates)).
:- use_module(engine(io_basic)).
:- use_module(library(random)).
:- use_module(library(write)).
:- use_module(library(sort)).
:- use_module(library(iso_misc), [once/1]).


% ------------------------------------------------------------------
:- pred tnum(BasicType)
   # "`BasicType` is a basic type of the regtype basic lattice.".

tnum(bot).
tnum(term).
tnum(vr).
tnum(gnd).
tnum(atm).
tnum(num).
tnum(int).
tnum(nnegint).
tnum(flt).
tnum(struct).
tnum(gndstr).


:- pred gentypes(NumTypes,MaxClauses,BasicTypes,Functors,Definitions)
    # "`Definitions` is a list of randomly generated type
    definitions, all types are reachable by one of the first two
    generated types.  It includes up to `NumTypes` type
    definitions of up to `MaxClauses` clauses each, using the
    basic types in `BasicTypes` and the functors (with their
    corresponding arities) in `Functors`.

    Example: @tt{gentypes(2,3,[int,atm,flt],[a/1,f/3],D).}".

% gentypes(2,3,[int,atm,flt,rtwe],[e,a/3,df/e,r/0,atm/5,a/1,f/3],D).

gentypes(NumTypes,MaxClauses,BasicTypes,Functors,Definitions) :-
    get_atom_names('t',0,NumTypes,Types),
    get_free_types(Types,BasicTypes,AllTypes),
    get_all_clauses(AllTypes,Functors,AllClauses),
    repeat,
    once(get_type_definitions(Types,MaxClauses,AllTypes,AllClauses,'nil','nil',Definitions)).
    %pretty_print_type_definitions(Definitions).

    
get_atom_names(_,N,N,[]):-!.
get_atom_names(A,X,N,[TN|Types]) :-
    X1 is X + 1,
    atom_number(AN,X1),
    atom_concat(A,AN,TN),
    get_atom_names(A,X1,N,Types).

get_free_types(DefTypes,BaseTypes,AllTypes) :-
    clean_non_basic(BaseTypes,BaseTypes1),
    append(BaseTypes1,DefTypes,AllTypes).

clean_non_basic([],[]).
clean_non_basic([B|Bs],[B|Bs1]) :- tnum(B),!,clean_non_basic(Bs,Bs1).
clean_non_basic([B|Bs],Bs1):- display('The type: '), display(B), display(' is not in the base type lattice.'),nl,clean_non_basic(Bs,Bs1).

get_all_clauses(AllTypes,Functors,AllClauses) :-
    clean_functors(AllTypes,Functors,RealFunctors,[]),
    append(AllTypes,RealFunctors,AllClauses).

clean_functors(_,[],[],_).
clean_functors(T,[F|Fs],Fs1,Fn):-
    F = Name/_,
    member(Name,T),!,
    display('The functor: '), display(F), display(' is named after a type and has been excluded.'),nl,clean_functors(T,Fs,Fs1,Fn).
clean_functors(T,[F|Fs],Fs1,Fn):-
    F = Name/_,
    member(Name,Fn),!,
    display('The functor: '), display(F), display(' is named after another functor and has been excluded.'),nl,clean_functors(T,Fs,Fs1,Fn).
clean_functors(T,[F|Fs],[F|Fs1],Fn):-
    F = Name/N,
    integer(N), N >= 0, !,
    clean_functors(T,Fs,Fs1,[Name|Fn]).
clean_functors(T,[F|Fs],Fs1,Fn) :- display('The functor: '), display(F), display(' does not properly describe its arity.'),nl,clean_functors(T,Fs,Fs1,Fn).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_type_definitions([],_,_,_,_,_,[]):-!.
get_type_definitions([T|Types],MaxClauses,AllTypes,AllClauses,L1,L2,[D|Definitions]) :-
    f_check(L1,L2,T),!,
    random(1,2,X),
    get_type_definition(X,T,MaxClauses,AllTypes,AllClauses,D,Ts),
    sort(Ts,TT),
    f_update(L1,L2,(T,TT),L1_1,L2_1),
    get_type_definitions(Types,MaxClauses,AllTypes,AllClauses,L1_1,L2_1,Definitions).
get_type_definitions([T|Types],MaxClauses,AllTypes,AllClauses,L1,L2,Definitions) :-!,
    select(T,AllTypes,AllTypes1),
    select(T,AllClauses,AllClauses1),
    get_type_definitions(Types,MaxClauses,AllTypes1,AllClauses1,L1,L2,Definitions).
get_type_definitions(_,_,_,_,_,_,[]).

f_check('nil','nil',_):-!.
f_check((_,_),'nil',_):-!.
f_check('nil',(_,F),T):-member(T,F).
f_check((_,F1),(_,F2),T):-append(F1,F2,F),member(T,F).

f_update('nil','nil',T,T,'nil'):-!.
f_update('nil',(ID,F),(T,TT),'nil',(ID,U)):-member(T,F),!, union(F,TT,U).
f_update((ID,F),'nil',(T,TT),'nil',(ID,U)):-member(T,F),!,union(F,TT,U).
f_update((ID,F),'nil',(T,TT),'nil',(T,U)):-member(ID,TT),!,union(F,TT,U).
f_update((ID,F),'nil',(T,TT),(ID,F),(T,TT)):-!.

f_update((_,[]),(ID,F),(T,TT),'nil',(ID,U)):- member(T,F),!, union(F,TT,U).
f_update((ID,F),(_,[]),(T,TT),'nil',(ID,U)):- member(T,F),!, union(F,TT,U).

f_update((ID1,F1),(ID2,F2),(T,TT),'nil',(ID1,U)):-
    member(T,F1),member(ID2,TT),!,union(F1,TT,U1),union(U1,F2,U).
f_update((ID1,F1),(ID2,F2),(T,TT),(ID1,U),(ID2,F2)):-
    member(T,F1),!,union(F1,TT,U).
f_update((ID1,F1),(ID2,F2),(T,TT),'nil',(ID2,U)):-
    member(T,F2),member(ID1,TT),!,union(F2,TT,U2),union(U2,F1,U).
f_update((ID1,F1),(ID2,F2),(T,TT),(ID1,F1),(ID2,U)):-
    member(T,F2),!,union(F2,TT,U).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_type_definition(_,_,_,_,[],_,_):-!.
get_type_definition(_,Type,1,AllTypes,AllClauses,[Definition],Ts) :-!,
    random_select(Clause,AllClauses,_),
    def_this_thing(Type,Clause,AllTypes,Definition,Ts).
get_type_definition(1,Type,Clauses,AllTypes,AllClauses,Definition,Ts) :-
    Clauses > 1,
    Clauses1 is Clauses -1,
    random(1,2,X),
    get_type_definition(X,Type,Clauses1,AllTypes,AllClauses,Definition,Ts).
get_type_definition(2,Type,Clauses,AllTypes,AllClauses,[D|Definition],Ts1) :-
    random_select(Clause,AllClauses,AllClauses1),
    def_this_thing(Type,Clause,AllTypes,D,T),
    append(T,Ts,Ts1),
    Clauses > 1,
    Clauses1 is Clauses -1,
    random(1,2,X),
    get_type_definition(X,Type,Clauses1,AllTypes,AllClauses1,Definition,Ts).

def_this_thing(Type,BaseType,_AllTypes,Definition,[]):-
    tnum(BaseType),!,
    def_from_type(Type,BaseType,Definition).

def_this_thing(Type,Functor,AllTypes,Definition,Ts):-
    Functor = Name/Arity, !,
    def_from_functor(Type,Name,Arity,AllTypes,Definition,Ts).

def_this_thing(Type,BaseDefType,AllTypes,Definition,[BaseDefType]):-
    member(BaseDefType,AllTypes),!,
    def_from_type(Type,BaseDefType,Definition).

def_from_type(Type,Clause,(NX :- AX)) :-
    NX =.. [Type,TMP],
    AX =.. [Clause,TMP].


def_from_functor(Type,Functor,0,_,(NX :- true),[]) :-
    NX =.. [Type,Functor].


def_from_functor(Type,Functor,Arity,AllTypes,(FX :- AX),Ts1) :-
    length([V|Vars],Arity),
    FX =.. [Type,NX],
    NX =.. [Functor,V|Vars],
    random_select(T,AllTypes,_),
    EV =.. [T,V],
    add_referenced_type(T,Ts,Ts1),
    get_elem_types((EV),Vars,AllTypes,(AX),Ts).

get_elem_types((EV),[],_,(EV),[]).
get_elem_types((EV),[V|Vars],AllTypes,(AX1),Ts1) :-
    random_select(T,AllTypes,_),
    EV1 =.. [T,V],
    AX1 =.. [',',EV,AX],
    add_referenced_type(T,Ts,Ts1),
    get_elem_types((EV1),Vars,AllTypes,(AX),Ts).

add_referenced_type(T,Ts,Ts):- tnum(T),!.
add_referenced_type(T,Ts,Ts):- T = _/_,!.
add_referenced_type(T,Ts,[T|Ts]).

random_select(Elem,List,Lis):-
    List = [_|_],
    random(1,~length(List),X),
    selectnth(List,1,X,Elem,Lis).

selectnth([],_,_,_,[]).
selectnth([Elem|Lis],K,K,Elem,Lis):-!.
selectnth([L|Lis],N,K,Elem,[L|Lis2]):-
    N1 is N+1,
    selectnth(Lis,N1,K,Elem,Lis2).

member_tail(X,[X|Xs],Xs).
member_tail(Y,[_|Xs],Xs1) :-
    member_tail(Y,Xs,Xs1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred pretty_print_type_definitions(TypeDefinitions)
   #"Prints the list `TypeDefinitions` of type definitions in a readble format.".
   
pretty_print_type_definitions(TypeDefinitions) :-
    print('Definitions to print:'), nl, nl,
    \+ \+ (
        numbervars(TypeDefinitions,0,_),
        pprint(TypeDefinitions)
    ).

pprint([]).
pprint([D|Defs]):-
    ppprint(D),
    pprint(Defs).

ppprint([]):- nl.
ppprint([Clause|Cs]):-
    print('  '), print(Clause),nl,
    ppprint(Cs).