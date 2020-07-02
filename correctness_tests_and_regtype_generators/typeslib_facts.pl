:- module(_, _, [hiord, fsyntax,assertions, doccomments]).


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

:- import(typedef,[pgm_typedef/2,
lib_typedef/2,
pgm_paramtypedef/2,
lib_paramtypedef/2,
pgm_param_type_symbol_renaming/2,
param_matching_mode/1,
lib_param_type_symbol_renaming/2]).

:- import(typeslib,['$already_validated$'/1,
computed_type_intersec/3,
computed_infinite_type/1,
computed_empty_type/1,
no_simplified_type/1,
pgm_computed_type_inclusion/2,  
lib_computed_type_inclusion/2,  
pgm_dz_pair/2, 
lib_dz_pair/2, 
pgm_equiv_type/2,
lib_equiv_type/2,
types_used_to_colapse_others/1,     
lib_types_used_to_colapse_others/1, 
type_symbols_used_to_colapse_others/1,    
lib_type_symbols_used_to_colapse_others/1,
pgm_required_type/1,
lib_required_type/1, 
pgm_def_equiv_type/2,
lib_def_equiv_type/2,
pgm_def_subtype_basic/2,
lib_def_subtype_basic/2,
pgm_param_type_hook/3,
lib_param_type_hook/3,
typ_sym_counter/1,
lib_typ_sym_counter/1,
param_typ_sym_counter/1,
lib_param_typ_sym_counter/1,
typ_param_sym_counter/1,
lib_typ_param_sym_counter/1,
pgm_user_type/1, 
lib_user_type/1,
uniontriple/3,
uniontriple_VR/3,
functor_types/3, 
param_type_depth/2,
tmp_param_type_symbol_renaming/2,
tmp_tyren/1,
tmp_tydef/1,
tmp_parren/1,
pgm_type_name/3, 
typ_name_counter/1,
pgm_equiv_name/2,
lib_type_name/3, 
lib_typ_name_counter/1,
lib_equiv_name/2]).


gen_to_print(Ls, T) :-
    % [(F,A) | Ls]
    ( member((F,A), Ls),
      functor(Call, F, A),
      call(Call),
      T = Call,
      display(T), nl,
      fail
    ;
        true
    ).

list_type_asserts(L):-
    gen_to_print([
('$already_validated$',1),
(computed_type_intersec,3),
(computed_infinite_type,1),
(computed_empty_type,1),
(no_simplified_type,1),
(pgm_computed_type_inclusion,2),  
(lib_computed_type_inclusion,2),  
(pgm_dz_pair,2), 
(lib_dz_pair,2), 
(pgm_equiv_type,2),
(lib_equiv_type,2),
(types_used_to_colapse_others,1),     
(lib_types_used_to_colapse_others,1), 
(type_symbols_used_to_colapse_others,1),    
(lib_type_symbols_used_to_colapse_others,1),
(pgm_required_type,1),
(lib_required_type,1), 
(pgm_def_equiv_type,2),
(lib_def_equiv_type,2),
(pgm_def_subtype_basic,2),
(lib_def_subtype_basic,2),
(pgm_param_type_hook,3),
(lib_param_type_hook,3),
(typ_sym_counter,1),
(lib_typ_sym_counter,1),
(param_typ_sym_counter,1),
(lib_param_typ_sym_counter,1),
(typ_param_sym_counter,1),
(lib_typ_param_sym_counter,1),
(pgm_user_type,1), 
(lib_user_type,1),
(uniontriple,3),
(uniontriple_VR,3),
(functor_types,3), 
(param_type_depth,2),
(tmp_param_type_symbol_renaming,2),
(tmp_tyren,1),
(tmp_tydef,1),
(tmp_parren,1),
(pgm_typedef,2),
(lib_typedef,2),
(pgm_paramtypedef,2),
(lib_paramtypedef,2),
(pgm_param_type_symbol_renaming,2),
(lib_param_type_symbol_renaming,2),
(param_matching_mode,1),
(pgm_type_name,3), 
(typ_name_counter,1),
(pgm_equiv_name,2),
(lib_type_name,3), 
(lib_typ_name_counter,1),
(lib_equiv_name,2)],L).

