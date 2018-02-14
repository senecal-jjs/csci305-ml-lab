(***************************************************************
*
* CSCI 305 - ML Programming Lab
*
* Jacob Senecal
* jacobsenecal@yahoo.com
*
***************************************************************)

(* Define your data type and functions here *)
fun f [] = [] (* a *)
  | f (x::xs) = (x+1) :: (f xs); (* b *)

(* Define a "set" datatype that has two different types Set and Empty *)
datatype 'element set = Set of 'element * 'element set | Empty;

(* The function isMember checks if an element is a member of a set, returning
   a boolean value which depends on the result of the check *)

fun isMember e Empty      = false
|   isMember e (Set(x,q)) = e=x orelse isMember e q;

fun member x nil = false
|   member x (h::t) = x=h orelse member x t;


fun list2Set [t] = Set(t, Empty)
|   list2Set (h::t) =
      if member h t then list2Set t
      else Set(h, list2Set t)

(* fun remove(x,L) =
  if (L=[]) then []
  else (if (x=hd(L))
    then remove(x, tl(L))
    else hd(L)::remove(x,tl(L)));

fun eliminateDuplicates(L) =
  if (L=[]) then []
  else hd(L)::remove(hd(L), eliminateDuplicates(tl(L))); *)



val l = [2,2,3,3];
member 2 l;
list2Set l;

(* val s = Set(2,Empty);
isMember 2 s; *)
(* val t = Set("a", Empty);
isMember "a" t; *)



(* Simple function to stringify the contents of a Set of characters *)
(* fun stringifyCharSet Empty = ""
  | stringifyCharSet (Set(y, ys)) = Char.toString(y) ^ " " ^ stringifyCharSet(ys);

(* Simple function to stringify the contents of a Set of ints *)
fun stringifyIntSet Empty = ""
  | stringifyIntSet (Set(w, ws)) = Int.toString(w) ^ " " ^ stringifyIntSet(ws);

(* Simple function to stringify the contents of a Set of strings *)
fun stringifyStringSet Empty = ""
  | stringifyStringSet (Set(z, zs)) = z ^ " " ^ stringifyStringSet(zs);

(* Simple function that prints a set of integers *)
fun print_int x = print ("{ " ^ stringifyIntSet(x) ^ "}\n");

(* Simple function that prints a set of strings *)
fun print_str x = print ("{ " ^ stringifyStringSet(x) ^ "}\n");

(* Simple function that prints a set of characters *)
fun print_chr x = print ("{ " ^ stringifyCharSet(x) ^ "}\n");

list2Set [1, 3, 2];
list2Set [#"a", #"b", #"c"];
list2Set [];
list2Set [6, 2, 2];
list2Set ["x", "y", "z", "x"]; *)

(* Question 1 *)
(* f [3, 1, 4, 1, 5, 9] *)

(* Question 5 *)
(* val quest5 = isMember "one" (list2Set ["1", "2", "3", "4"]);
print ("\nQuestion 5: " ^ Bool.toString(quest5) ^ "\n");

(* Question 7 *)
val quest7 = list2Set ["it", "was", "the", "best", "of", "times,", "it", "was", "the", "worst", "of", "times"];
print "\nQuestion 7: ";
print_str quest7;
print "\n";

(* Question 9 *)
print "\nQuestion 9: ";
print_str (union (list2Set ["green", "eggs", "and"]) (list2Set ["ham"]));

(* Question 10 *)
print "\nQuestion 10: ";
print_str (intersect (list2Set ["stewed", "tomatoes", "and", "macaroni"]) (list2Set ["macaroni", "and", "cheese"])); *)
