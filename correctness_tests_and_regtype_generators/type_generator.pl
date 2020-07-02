:- module(type_generator, [tnum/1,gentypes/5,pretty_print_type_definitions/1], [hiord, fsyntax, assertions, doccomments]).

%! \title Automatic regtype generator.
%! \module
% 
%  This module contains the operations required to generate all
%  possible regular types from a list of allowed functors & basic
%  types, subject to a bound on the number of clauses per type.


:- use_module(library(streams)).
:- use_module(library(stream_utils)).
:- use_module(typeslib(typeslib)).
:- use_module(typeslib(regtype_basic_lattice)).
:- use_module(library(lists)).
:- use_module(library(aggregates)).
:- use_module(engine(io_basic)).
:- use_module(library(random)).
:- use_module(library(write)).


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

:- pred gentypes(NumTypes,MaxClauses,BaseTypes,Functors,Definitions) #
    "@Var{Definitions} is a list of secuentally generated type
    definitions.  It includes up to @Var{NumTypes} type definitions of
    up to @Var{MaxClauses} clauses each, using the basic types in
    @Var{BaseTypes} and the functors (with their corresponding
    arities) in @Var{Functors}.

    Example: @tt{gentypes(2,3,[int,atm,flt],[a/1,f/3],D).}".


% gentypes(2,3,[int,atm,flt,rtwe],[e,a/3,df/e,r/0,atm/5,a/1,f/3],D).

gentypes(NumTypes,MaxClauses,BasicTypes,Functors,Definitions) :-
    get_atom_names('t',NumTypes,Types),
    get_free_types(Types,BasicTypes,AllTypes),
    get_all_clauses(AllTypes,Functors,AllClauses),
    get_type_definitions(Types,MaxClauses,AllTypes,AllClauses,Definitions),
    pretty_print_type_definitions(Definitions).

get_atom_names(_,0,[]):-!.
get_atom_names(A,N,[TN|Types]) :-
    N1 is N - 1,
    atom_number(AN,N),
    atom_concat(A,AN,TN),
    get_atom_names(A,N1,Types).

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

get_type_definitions([],_,_,_,[]).
get_type_definitions([T|Types],MaxClauses,AllTypes,AllClauses,[D|Definitions]) :-
    get_type_definition(T,MaxClauses,AllTypes,AllClauses,D),
    get_type_definitions(Types,MaxClauses,AllTypes,AllClauses,Definitions).

get_type_definition(Type,1,AllTypes,AllClauses,[Definition]) :-
    member_tail(Clause,AllClauses,_),
    def_this_thing(Type,Clause,AllTypes,Definition).

get_type_definition(Type,Clauses,AllTypes,AllClauses,Definition) :-
    Clauses > 1,
    Clauses1 is Clauses -1,
    get_type_definition(Type,Clauses1,AllTypes,AllClauses,Definition).
get_type_definition(Type,Clauses,AllTypes,AllClauses,[D|Definition]) :-
    member_tail(Clause,AllClauses,AllClauses1),
    def_this_thing(Type,Clause,AllTypes,D),
    Clauses > 1,
    Clauses1 is Clauses -1,
    get_type_definition(Type,Clauses1,AllTypes,AllClauses1,Definition).

def_this_thing(Type,BaseType,_AllTypes,Definition):-
    tnum(BaseType),!,
    def_from_type(Type,BaseType,Definition).

def_this_thing(Type,Functor,AllTypes,Definition):-
    Functor = Name/Arity, !,
    def_from_functor(Type,Name,Arity,AllTypes,Definition).

def_this_thing(Type,BaseDefType,AllTypes,Definition):-
    member(BaseDefType,AllTypes),!,
    def_from_type(Type,BaseDefType,Definition).

def_from_type(Type,Clause,(NX :- AX)) :-
    NX =.. [Type,TMP],
    AX =.. [Clause,TMP].


def_from_functor(Type,Functor,0,_,(NX :- true)) :-
    NX =.. [Type,Functor].


def_from_functor(Type,Functor,Arity,AllTypes,(FX :- AX)) :-
    length([V|Vars],Arity),
    FX =.. [Type,NX],
    NX =.. [Functor,V|Vars],
    select(T,AllTypes,_),
    EV =.. [T,V],
    get_elem_types((EV),Vars,AllTypes,(AX)).

get_elem_types((EV),[],_,(EV)).
get_elem_types((EV),[V|Vars],AllTypes,(AX1)) :-
    select(T,AllTypes,_),
    EV1 =.. [T,V],
    AX1 =.. [',',EV,AX],
    get_elem_types((EV1),Vars,AllTypes,(AX)). 

member_tail(X,[X|Xs],Xs).
member_tail(Y,[_|Xs],Xs1) :-
    member_tail(Y,Xs,Xs1).

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