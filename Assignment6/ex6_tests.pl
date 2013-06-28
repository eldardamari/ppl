% Test cases for assignment 6.
% To use this file: open new Prolog session, then load this file.

% File syntax: test(<name>) :- <actions>.

:- abolish(product/5).
:- [testutils, 'ex6-aux', ex6].
:- begin_tests(ex6_tests).

% Question 1.1 part a
% The tests with additional products at the file end.

test(check_barcodes_product_01) :- getFail(check_barcodes_product(_)).

% Question 1.2

test(phone_product_01) :- runCheck(phone_product(pita, P),
                                   (P),
                                   [(8877665)]).

test(phone_product_02) :- runCheck(phone_product(chips, P),
                                   (P),
                                   [(5456464), (2002001)]).

test(phone_product_03) :- runCheck(phone_product(N, 4564564),
                                   (N),
                                   [(preserved_milk), (fresh_milk)]).

test(phone_product_04) :- getFail(phone_product(lego, _P)).

test(phone_product_05) :- getFail(phone_product(_N, 1234567)).

test(phone_product_11) :- runCheck(phone_product(N, P),
                                   (N, P),
                                   [ (fresh_milk, 4564564),
                                     (low_fat_milk, 4564567),
                                     (preserved_milk, 4564564),
                                     (white_bread, 4574746),
                                     (rye_bread, 4574746),
                                     (pita, 8877665),
                                     (chips, 5456464),
                                     (chips, 2002001),
                                     (crisps, 2002001),
                                     (cola, 6785785),
                                     (lighter, 7855775) ]).

test(phone_product_12) :- getFail(phone_product(A, A)).

test(phone_product_13) :- sortedCheck((phone_product(N1, P),
                                       phone_product(N2, P),
                                        N1 \= N2),
                                      (P),
                                      [(2002001), (4564564), (4574746)]).

% Question 1.3

test(unrefrigerated_ordering_01) :- sortedCheck(unrefrigerated_ordering(dairy, MCD),
                                                (MCD),
                                                [(5)]).

test(unrefrigerated_ordering_02) :- sortedCheck(unrefrigerated_ordering(snack, MCD),
                                                (MCD),
                                                [(28)]).

test(unrefrigerated_ordering_03) :- sortedCheck(unrefrigerated_ordering(C, 28),
                                                (C),
                                                [(bread), (snack)]).

test(unrefrigerated_ordering_04) :- getFail(unrefrigerated_ordering(toys, _MCD)).

test(unrefrigerated_ordering_05) :- getFail(unrefrigerated_ordering(_C, 1)).

test(unrefrigerated_ordering_11) :- sortedCheck(unrefrigerated_ordering(C, MCD),
                                                (C, MCD),
                                                [(bread, 10), (bread, 28), (dairy, 5), (misc, 10), (snack, 28)]).

 % Question 1.4

test(unrefrigerated_ordering_list_01) :- sorteachCheck(unrefrigerated_ordering_list(List),
                                                       (List),
                                                       [[(bread, 10), (bread, 28), (misc, 10), (snack, 28), (dairy, 5)]]).

% Question 2.1

test(deep_reverse_01) :- getTrue(deep_reverse([a, b, c], [c, b, a])).

test(deep_reverse_02) :- runCheck(deep_reverse([a, b, c], X),
                                  (X),
                                  [([c, b, a])]).
              
test(deep_reverse_03) :- runCheck(deep_reverse([a, b, [c, [c, 4], [], 8], [11], 3], X),
                                  (X),
                                  [([3, [11], [8, [], [4, c], c], b, a])]).

test(deep_reverse_04) :- getFail(deep_reverse([a, b, c], [b, c, a])).

test(deep_reverse_11) :- runCheck(deep_reverse([[], [[[]]]], X),
                                  (X),
                                  [([[[[]]], []])]).

test(deep_reverse_12) :- runCheck(deep_reverse([a, b, q, [c, d, e, f, [c, c, c, c, c, 4], [], 8], [11], 3], X),
                                  (X),
                                  [([3, [11], [8, [], [4, c, c, c, c, c], f, e, d, c], q, b, a])]).

% Question 2.2

test(sublist_perm_01) :- getTrue(sublist_perm([a, b, c], [a])).

test(sublist_perm_02) :- getTrue(sublist_perm([a, b, c], [c, a])).

test(sublist_perm_03) :- getFail(sublist_perm([a, a, b, c], [a, a, a])).

test(sublist_perm_04) :- getTrue(sublist_perm([r, a, s, d, c, b], [a, b, c])).

test(sublist_perm_05) :- getFail(sublist_perm([r, a, s, d, c, b], [a, b, f])).

test(sublist_perm_06) :- sortedCheck(sublist_perm([a, b, c], X),
                                     (X),
                                     [([a, b, c]), ([a, c, b]), ([b, a, c]),
                                      ([b, c, a]), ([c, a, b]), ([c, b, a]),
                                      ([a, b]), ([a, c]), ([b, a]),
                                      ([b, c]), ([c, a]), ([c, b]),
                                      ([a]), ([b]), ([c]), ([])]).

test(sublist_perm_11) :- getTrue(sublist_perm([a, a, a, a], [a, a, a, a])).

test(sublist_perm_12) :- getTrue(sublist_perm([a, a, a, a], [a, a, a])).

test(sublist_perm_13) :- getFail(sublist_perm([a, a, a, a], [a, a, a, a, a])).

% Question 2.3.a

test(cfg_01) :- getTrue(s([the, cat, saw, a, dog])).

test(cfg_02) :- getTrue(s([the, cat, ate, the, mouse, in, the, house])).

test(cfg_03) :- getTrue(s([the, big, funny, mouse, ate, a, cat])).

test(cfg_04) :- getTrue(s([the, big, funny, mouse, ate, a, tall, cat])).

test(cfg_05) :- getTrue(s([the, funny, cat, saw, a, dog, in, the, tall, house])).

test(cfg_06) :- getTrue(s([a, funny, beautiful, cat, saw, a, dog, in, the, tall, house])).

test(cfg_07) :- getTrue(s([a, funny, beautiful, cat, saw, a, dog, in, the, tall, beautiful, house])).

test(cfg_08) :- getFail(s([a, funny, beautiful, cat, in, the, tall, beautiful, house, saw, a, dog])).

test(cfg_09) :- getFail(s([a, cat, ate, it])).

test(cfg_10) :- getTrue(s([a, tall, cat, ate, the, mouse, gladly])).

test(cfg_11) :- getTrue(s([a, tall, cat, ate, the, mouse, in, the, house, hungrily])).

test(cfg_12) :- getTrue(s([the, cat, ate, the, mouse, in, the, funny, house, hungrily])).

test(cfg_13) :- getTrue(s([the, big, funny, mouse, ate, a, tall, cat, on, a, dog, gladly])).

test(cfg_14) :- getFail(s([a, tall, cat, ate, gladly, the, mouse])).

test(cfg_15) :- getFail(s([a, tall, cat, gladly, ate, the, mouse])).

test(cfg_21) :- getFail(s([])).

test(cfg_22) :- getFail(s([a, cat, saw, a, cat, ate, a, cat])).

% Question 2.3.b

test(subcfg_01) :- long(getTrue(subCFG([the, cat, saw, a, dog, in, a, funny, beautiful, house],
                                       [the, cat, saw, a, house]))).

test(subcfg_02) :- long(getTrue(subCFG([the, cat, saw, a, dog, in, a, funny, beautiful, house],
                                       [the, house, saw, a, beautiful, funny, dog]))).

test(subcfg_03) :- long(getTrue(subCFG([the, big, funny, mouse, ate, a, tall, cat, on, a, dog],
                                       [a, tall, cat, ate, a, big, mouse, on, the, funny, dog]))).

test(subcfg_04) :- long(getFail(subCFG([the, big, funny, mouse, ate, a, tall, cat, on, a, dog],
                                       [a, tall, cat, ate, a, big, mouse, on, a, funny, dog]))).

test(subcfg_11) :- long(getTrue(subCFG([the, cat, saw, a, dog, in, a, funny, beautiful, house],
                                       [the, cat, saw, a, dog, in, a, funny, beautiful, house]))).

:- end_tests(ex6_tests).
:- (run_tests(ex6_tests),
   !;
   assert(testfail)).

% Additional tests

:- findall(product(A, B, C, D, E), product(A, B, C, D, E), List),
   assert(allproducts(List)).

:- abolish(product/5),
   allproducts(List),
   forall(member(El, [product(972000456745, matches, 05, false, fire_ltd)|List]), assert(El)).

:- begin_tests(q1_1_b_tests).

% Question 1.1 part b

test(check_barcodes_product_02) :- sortedCheck(check_barcodes_product(Wrong),
                                              (Wrong),
                                              [(972000456745)]).

:- end_tests(q1_1_b_tests).
:- (run_tests(q1_1_b_tests),
   !;
   assert(testfail)).

:- abolish(product/5),
   allproducts(List),
   forall(member(El, [product(972000456745, lighter, 05, false, fire_inc)|List]), assert(El)).

:- begin_tests(q1_1_c_tests).

% Question 1.1 part c

test(check_barcodes_product_03) :- sortedCheck(check_barcodes_product(Wrong),
                                              (Wrong),
                                              [(972000456745)]).

:- end_tests(q1_1_c_tests).
:- (run_tests(q1_1_c_tests),
   !;
   assert(testfail)).

:- abolish(product/5),
   allproducts(List),
   forall(member(El, [product(972000456745, matches, 05, false, fire_inc)|List]), assert(El)).

:- begin_tests(q1_1_d_tests).

% Question 1.1 part d

test(check_barcodes_product_04) :- sortedCheck(check_barcodes_product(Wrong),
                                              (Wrong),
                                              [(972000456745)]).

test(check_barcodes_product_11) :- getTrue(check_barcodes_product(972000456745)).

test(check_barcodes_product_12) :- getFail(check_barcodes_product(972000232543)).

:- end_tests(q1_1_d_tests).

:- (run_tests(q1_1_d_tests),
   not(retract(testfail)),
   writeln('The test suite succeeded'));
   retractall(testfail),
   writeln('The test suite failed').

:- abolish(product/5).
