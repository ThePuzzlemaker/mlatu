:- module(mlatu, [rewrite/2, user_equiv/2]).
:- dynamic([user_equiv/2], [multifile, discontiguous]).

equiv(Xs, Other) :-
    user_equiv(Xs, Other) ;
    builtin_equiv(Xs, Other).

builtin_equiv(['dup',X|Xs], Other) :-
    equiv([X,X|Xs], Other).

builtin_equiv(['pop',_|Xs], Other) :-
    equiv(Xs, Other).

builtin_equiv(['i',[]|Xs], Other) :-
    equiv(Xs, Other).
builtin_equiv(['i',[H|T]|Xs], Other) :-
    append([H|T], Xs, NewXs),
    equiv(NewXs, Other).

builtin_equiv(['cons',[],X|Xs], Other) :-
    equiv([[X]|Xs], Other).
builtin_equiv(['cons',[H|T],X|Xs], Other) :-
    equiv([[X,H|T]|Xs], Other).

builtin_equiv(['dip',[],X|Xs], Other) :-
    equiv([X|Xs], Other).
builtin_equiv(['dip',[H|T],X|Xs], Other) :-
    append([H|T], Xs, NewXs),
    equiv([X|NewXs], Other).

builtin_equiv([], []).
builtin_equiv([X|Xs], Other) :-
    equiv(Xs, New),
    (Xs \= New ->
        equiv([X|New], Other) ;
        Other = [X|Xs]).

rewrite(List, Other) :-
    equiv(List, Other),
    List \= Other.
