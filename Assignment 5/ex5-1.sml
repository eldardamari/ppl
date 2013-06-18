use("./ex5-aux.sml");
(******************* 1.1 *************************)
(*
* Signature: get_all_vars(prop)
* Purpose: takes a propositional formula as argument and returns a list of all variables (without duplications) in it
* Type: fn : prop -> string list
* Example: 
- get_all_vars(Disj(Conj (Atom ("x1"), Atom ("x2")), Disj (Atom ("x1"), Neg (Atom ("x3")))));
val it = ["x2","x1","x3"]: string list
*)
(*	Write your code here... *)
val rec get_all_vars =
	fn(p:prop) =>
		let 
			val rec filter_vars =
				fn(l1,[]) => l1
				| (l1,head::tail) => if List.exists (fn x => x = head) l1
										then filter_vars(l1,tail)
										else filter_vars(l1@[head],tail);
			val rec helper = 
				fn(Atom(p),l) => filter_vars(l,[p])
				| (Neg(p),l) => filter_vars(l,get_all_vars(p))
				| (Conj(p1,p2),l) => filter_vars(l,get_all_vars(p1)@get_all_vars(p2))
				| (Disj(p1,p2),l) => filter_vars(l,get_all_vars(p1)@get_all_vars(p2))
		in		
			helper(p,[])
		end;

get_all_vars(Disj(Conj (Atom ("x1"), Atom ("x2")), Disj (Atom ("x1"), Neg (Atom ("x3")))));		

(******************* 1.2 *************************)
(*
* Signature: satisfies(formula, assignment)
* Purpose: returns true if and only if the assignment satisfies the formula
* Type: fn : prop * prop list -> bool
* Pre-Condition: assignment is a proper assignment, and get_all_vars(formula)=get_all_vars(assignment).
* Examples: 
 - satisfies(Atom("x1"), [Atom("x1"), Neg(Atom("x3"))]);
 val it = true : bool
 - satisfies(Atom("x3"), [Atom("x1"), Neg(Atom("x3"))]);
 val it = false : bool
 - satisfies(Neg(Atom("x1")), [Atom("x1"), Neg(Atom("x3"))]);
 val it = false : bool
 - satisfies(Neg(Atom("x3")), [Atom("x1"), Neg(Atom("x3"))]);
 val it = true : bool
 - satisfies(Conj(Atom("x1"),Neg(Atom("x3"))), [Atom("x1"), Neg(Atom("x3"))]);
 val it = true : bool
 - satisfies(Conj(Atom("x1"),Atom("x3")), [Atom("x1"), Neg(Atom("x3"))]);
 val it = false : bool
 - satisfies(Disj(Atom("x1"),Atom("x3")), [Atom("x1"), Neg(Atom("x3"))]);
 val it = true : bool
 - satisfies(Disj(Conj(Atom("x1"),Atom("x3")),Conj(Atom("x1"),Neg(Atom("x3")))), [Atom("x1"), Neg(Atom("x3"))]);
 val it = true : bool
 - satisfies(Disj(Conj(Disj(Atom("x1"),Neg(Atom("x3"))),Atom("x3")),Conj(Atom("x1"),Neg(Atom("x3")))), 
             [Atom("x1"), Neg(Atom("x3"))]);
 val it = true : bool *)

val rec satisfies =
	fn (Atom(p),ls) => List.exists (fn x => x = Atom(p)) ls
	| (Neg(p),ls) => not(satisfies(p,ls))
	| (Conj(p1,p2),ls) => satisfies(p1,ls) andalso satisfies(p2,ls)
	| (Disj(p1,p2),ls) => satisfies(p1,ls) orelse satisfies(p2,ls);

satisfies(Disj(Conj(Disj(Atom("x1"),Neg(Atom("x3"))),Atom("x3")),Conj(Atom("x1"),Neg(Atom("x3")))),[Atom("x1"), Neg(Atom("x3"))]);
satisfies(Disj(Conj(Atom("x1"),Atom("x3")),Conj(Atom("x1"),Neg(Atom("x3")))), [Atom("x1"), Neg(Atom("x3"))]);
satisfies(Conj(Atom("x1"),Atom("x3")), [Atom("x1"), Neg(Atom("x3"))]);
