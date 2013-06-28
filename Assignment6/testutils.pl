% Tests utilities

shorttimeout(10).
longtimeout(300).

:- abolish(timeout/1).
:- shorttimeout(Timeout),
   assert(timeout(Timeout)).

:- set_prolog_flag(verbose, silent).
:- use_module(library(plunit)).
:- use_module(library(pldoc)).

runCheck(Pred, Form, Answers) :- nl,
                                 timeout(Timeout),
                                 call_with_time_limit(Timeout,
                                     findall(Form, Pred, List)),
                                 inform(quick_permutation(List, Answers),
                                       list(Answers), list(List)),
                                 !.

sortedCheck(Pred, Form, Answers) :- nl,
                                    timeout(Timeout),
                                    call_with_time_limit(Timeout,
                                        findall(Form, Pred, List)),
                                    sort(List, Sorted),
                                    inform(quick_permutation(Sorted, Answers),
                                          list(Answers), list(Sorted)),
                                    !.

sorteachCheck(Pred, Form, Answers) :- nl,
                                      timeout(Timeout),
                                      call_with_time_limit(Timeout,
                                          findall(Form, Pred, List)),
                                      sortEach(List, SortedList),
                                      sortEach(Answers, SortedAnswers),
                                      inform(quick_permutation(SortedList, SortedAnswers),
                                            list(Answers), list(List)),
                                      !.

firstAnswer(Pred, Form, Answer) :- nl,
                                   timeout(Timeout),
                                   (call_with_time_limit(Timeout, Pred) ->
                                   !,
                                   inform(Form == Answer, Answer, Form);
                                   inform(Answer)).

firstAnswerUnify(Pred, Form, Answer) :- nl,
                                        timeout(Timeout),
                                        (call_with_time_limit(Timeout, Pred) ->
                                        !,
                                        inform(Form = Answer, Answer, Form);
                                        inform(Answer)).

getTrue(Pred) :- nl,
                 timeout(Timeout),
                 inform(call_with_time_limit(Timeout, Pred), true, false).

getFail(Pred) :- nl,
                 timeout(Timeout),
                 inform(call_with_time_limit(Timeout, not(Pred)), false, true).

checkVar(Name, Var) :- var(Var) ->
                       otherwise;
                       write('Expected value '(Name)),
                       write(' as variable, but calculated '(Var)),
                       write(', so '),
                       fail.

long(Goal) :- nl,
              longtimeout(Long),
              Minutes is ceiling(Long / 60),
              write('This can take until '),
              write(Minutes),
              write(' minutes... '),
              retract(timeout(Timeout)),
              assert(timeout(Long)),
              call_cleanup(Goal,
                           (retract(timeout(_)),
                            assert(timeout(Timeout)))).

inform(Query, Expected, Calculated) :- Query ->
                                       otherwise;
                                       write('Expected '(Expected)),
                                       write(' but calculated '(Calculated)),
                                       write(', so '),
                                       false.

inform(Expected) :- write('Expected '(Expected)),
                    write(' but failed, so '),
                    false.

quick_permutation(X, Y) :- msort(X, XX),
                           msort(Y, YY),
                           equal(XX, YY).

sortEach([], []).
sortEach([List|Rest], [SortedList|SortedRest]) :- msort(List, SortedList),
                                                  sortEach(Rest, SortedRest).

equal(XX, YY) :- XX == YY,
                 !.
equal(XX, YY) :- var(XX),
                 var(YY),
                 !.
equal((X, XX), (Y, YY)) :- !,
                           equal(X, Y),
                           equal(XX, YY).
equal([X|XX], [Y|YY]) :- !,
                         equal(X, Y),
                         equal(XX, YY).
equal(XX, YY) :- compound(XX),
                 compound(YY),
                 !,
                 XX =.. X,
                 YY =.. Y,
                 equal(X, Y).
