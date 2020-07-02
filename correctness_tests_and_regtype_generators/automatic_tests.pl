:- module(automatic_tests, _, [hiord, fsyntax,assertions, doccomments]).

%! \title Automatic testing for regtypes.
%! \module
%
%  This module contains automatic testing tools for @ref{typeslib}'s
%  operations on regular types. It includes property based
%  verification for the main 1 and 2-ary operations.

:- use_module(type_generator_stochastic).
:- use_module(printcolor).
:- use_module(library(streams)).
:- use_module(library(stream_utils)).
:- use_module(typeslib(typeslib)).
:- use_module(typeslib(regtype_basic_lattice)).
:- use_module(library(lists)).
:- use_module(library(aggregates)).
:- use_module(engine(io_basic)).
:- use_module(library(random)).
:- use_module(library(write)).

%main_test(7,3,[int,flt,atm],[f/3,g/2]).
main_test(NumTypes,MaxClauses,BaseTypes,Functors) :-
    gentypes(NumTypes,MaxClauses,BaseTypes,Functors,Definitions),
    extra_cleanup_types,
    insert_types(Definitions),
    post_init_types,
    test_pair((t1,t2)),
    pretty_print_type_definitions(Definitions),
    show_types,
    get_line(X),
    end(X).

end("1").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_types([]).
insert_types([D|Defs]):-
    fetch_name(D,Name),
    NameDef =.. [Name,_],
    insert_user_type_pred_def(NameDef,D),
    insert_types(Defs).

fetch_name([(F:-_)|_],Name):-
     functor(F,Name,_).  
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_pair(Pair) :-
    test_empty_type(Pair), nl,
    test_is_ground_type(Pair), nl,
    test_is_infinite_type(Pair), nl,
    test_equivalent_to_top_type(Pair), nl,
    test_equivalent_to_numeric(Pair), nl,
    test_dz_type_included(Pair), nl,
    %test_no_reset_union_pairs(Pair), nl,
    test_union_pairs(Pair), nl,
    test_intersect_pairs_0(Pair), nl,
    test_intersect_pairs_2(Pair), nl.
    %test_edz_type_included(Pairs),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_intersect_pairs_2((P1,P2)) :-
    type_intersection_2(P1,P2,T),
    underline,
    display_color(blue,intersection_2(P1,P2,T)), nl,
    intersection_MT(P1,P2,T).

test_intersect_pairs_0((P1,P2)) :-
    type_intersection_0(P1,P2,T),
    underline,
    display_color(blue,intersection_0(P1,P2,T)), nl,
    intersection_MT(P1,P2,T).

%Property testing for the intersection operator, does not consider that the
%intersection has to be the biggest set contained by both of the operands.
intersection_MT(P1,P2,T):-
    intersection_p1(T,P1),
    intersection_p1(T,P2),
    intersection_p2(T,P1),
    intersection_p2(T,P2),
    intersection_p3(T,P1),
    intersection_p3(T,P2),!,
    display_color(green,'All good with the intersection.'), nl.

intersection_MT(_,_,_):- display_color(red,'Something went wrong in the intersection.'), nl.

%Intersection is le than both original types.
intersection_p1(Inter,T) :-
    dz_type_included(Inter,T),
    display_color(blue,Inter =< T), nl.

%Intersection is invariant to the intersecion with an original type
intersection_p2(Inter,T) :-
    type_intersection_2(Inter,T,X),
    dz_equivalent_types(Inter,X),
    display_color(blue,Inter = T /\ Inter),nl.

%The union of the intersection with its component is the given component
intersection_p3(Inter,T) :-
    resetunion,
    type_union(Inter,T,X),
    dz_equivalent_types(T,X),
    display_color(blue,T = T \/ Inter),nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_no_reset_union_pairs((P1,P2)) :-
    type_union(P1,P2,T),
    underline,
    display_color(blue,non_reset_union(P1,P2,T)), nl,
    union_MT(P1,P2,T).

test_union_pairs((P1,P2)) :-
    resetunion, %necessary even after resetting the database.
    type_union(P1,P2,U),
    underline,
    display_color(blue, union(P1,P2,U)), nl,
    union_MT(P1,P2,U).


%Property testing for the union operator, does not consider that the
%union has to be the smallest set that contains both of the operands.
union_MT(P1,P2,U):-
    union_p1(U,P1),
    union_p1(U,P2),
    union_p2(U,P1),
    union_p2(U,P2),
    union_p3(U,P1),
    union_p3(U,P2),!,
    display_color(green,'All good with the union.'), nl.

union_MT(_,_,_):- display_color(red,'Something went wrong in the union.'), nl.

%union is ge than both original types
union_p1(U,T) :-
    dz_type_included(T,U),
    display_color(blue,T =< U), nl.

%The intersection of a union with one of their components is that comoponent
union_p2(U,T) :-
    type_intersection_2(U,T,X),
    dz_equivalent_types(T,X),
    display_color(blue,T = T /\ U),nl.

%Union is invariant to the union with an original type
union_p3(U,T) :-
    resetunion,
    type_union(U,T,X),
    dz_equivalent_types(U,X),
    display_color(blue,U = T \/ U),nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
test_empty_type((P1,P2)) :-
    test_empty(P1), nl,
    test_empty(P2).

test_empty(Type) :-
    is_empty_type(Type),!,
    underline,
    display_color(blue,empty(Type)), nl,
    empty_MT(Type).
test_empty(Type) :-
    underline,
    display_color(blue,not_empty(Type)), nl,
    not_empty_MT(Type).

empty_MT(T) :-
    dz_equivalent_types(T,bot),!,
    display_color(blue,T = bot),nl, %IS THIS PROPERTY ACTUALLY OKAY?????
    display_color(green,'All good with the empty check.'), nl.

empty_MT(_):- display_color(red,'Something went wrong in the empty check.'),nl.

not_empty_MT(T) :-
    dz_type_included(T,bot),!,  %IS THIS PROPERTY ACTUALLY OKAY?????
    display_color(red,'Something went wrong in the empty check.'),nl.
    

not_empty_MT(T):-
    display_color(blue,T),display_color(blue,' =/= bot'),nl,
    display_color(green,'All good with the non-empty check.'), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

test_dz_type_included((P1,P2)):-
    test_dz_include(P1,P2),
    test_dz_include(P2,P1).

test_dz_include(T1,T2):-
    dz_type_included(T1,T2),!,
    underline,
    display_color(blue,dz_included(T1,T2)), nl,
    inclusion_MT(T1,T2).

test_dz_include(T1,T2):-
    underline,
    display_color(blue,not_dz_included(T1,T2)), nl,
    no_inclusion_MT(T1,T2).

inclusion_MT(T1,T2):-
    inclusion_p1(T1,T2),!,
    display_color(green,'All good with the inclusion.'), nl.
inclusion_MT(_,_):- display_color(red,'Something went wrong in the inclusion.'), nl.

%The intersection of the small and the big types is the small type
inclusion_p1(S,B):-
    type_intersection_2(S,B,X),
    dz_equivalent_types(S,X),
    display_color(blue,S = S /\ B),nl.

%The union of the small and the big types is the big type
inclusion_p2(S,B):-
    resetunion,
    type_union(s,B,X),
    dz_equivalent_types(B,X),
    display_color(blue,B = S \/ B),nl.

no_inclusion_MT(_T1,_T2):-
    %no_inclusion_p1(T1,T2),
    !,display_color(green,'All good with the inclusion.'), nl.
no_inclusion_MT(_,_):- display_color(red,'Something went wrong in the inclusion.'), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

test_equivalent_to_top_type((P1,P2)):-
    test_eq_top(P1),
    test_eq_top(P2).

test_eq_top(Type) :-
    equivalent_to_top_type(Type),!,
    underline,
    display_color(blue,eq_top(Type)), nl.
test_eq_top(Type) :-
    underline,
    display_color(blue,not_eq_top(Type)), nl.

test_equivalent_to_numeric((P1,P2)):-
    test_eq_num(P1),
    test_eq_num(P2).

test_eq_num(Type) :-
    equivalent_to_numeric(Type),!,
    underline,
    display_color(blue,eq_num(Type)), nl.
test_eq_num(Type) :-
    underline,
    display_color(blue,not_eq_num(Type)), nl.

test_is_ground_type((P1,P2)):-
    test_ground_type(P1),
    test_ground_type(P2).

test_ground_type(Type) :-
    is_ground_type(Type),!,
    underline,
    display_color(blue,ground(Type)), nl.
test_ground_type(Type) :-
    underline,
    display_color(blue,not_ground(Type)), nl.

test_is_infinite_type((P1,P2)):-
    test_infinite_type(P1),
    test_infinite_type(P2).

test_infinite_type(Type) :-
    is_infinite_type(Type),!,
    underline,
    display_color(blue,infinite(Type)), nl.
test_infinite_type(Type) :-
    underline,
    display_color(blue,not_infinite(Type)), nl.

