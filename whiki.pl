:- include('include/common.pl').
:- include('include/utils.pl').
:- include('config.pl').
:- include('engine/engine.pl').
:- initialization(main).
main :- loop, commit, halt.
