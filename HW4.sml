(*
Zac Kasper
3/30/2017
HW4 - SML
*)

(*1*)
fun exists (v, []) = false
	| exists (v, (x::rest)) = if (v = x) then true
							else exists (v, rest); 

(*2*)							
fun listIntersect (x::rest) (y::ext) = if exists (x, ext) then x
									   else listIntersect rest ext
	| listIntersect L1 [] = false
	| listIntersect [] L2 = false;

(*3*)	
fun union L1 [] = L1 
| union L1 (x::rest) = if (exists (x, L1)) then union L1 rest
					   else union (x::L1) rest;

(*4*)
fun pairNleft n L =
let 
	fun nSegment 0 L seg = seg 
		(*counter finished, return segment*)
	| nSegment n [] seg = seg 
		(*input list ended before counter, return segment*)
	| nSegment n (x::rest) seg = nSegment (n-1) rest (x::seg)
		(*add item to segment, decrement counter*) 
	fun nCut 0 L = L 
	| nCut n [] = [] 
	| nCut n (x::rest)  = nCut (n-1) rest 
		(*returns remainder of list with n elements cut off*)
	fun length [] = 0 | length (x::rest) = 1 + length rest 
	fun leftCut n L = nSegment ((length L) mod n) L []
		(*returns leftmost remainder of list*)
	fun rightCut n L = nCut ((length L) mod n) L 
		(*returns rightmost remainder of list*)
	fun splitter n L = (nSegment n L)::(splitter n (nCut n L))
		(*works like pairNright, used after leftmost remainder separated*)
	fun pLaux n L = (leftCut n L)::(splitter n (rightCut n L))
in 
	pLaux n L 
end;
fun pairNright n L =
let 
	fun nSegment 0 L seg = seg 
		(*counter finished, return segment*)
	| nSegment n [] seg = seg 
		(*input list ended before counter, return segment*)
	| nSegment n (x::rest) seg = nSegment (n-1) rest (x::seg)
		(*add item to segment, decrement counter*) 
	fun nCut 0 L = L 
	| nCut n [] = [] 
	| nCut n (x::rest)  = nCut (n-1) rest 
		(*returns remainder of list with n elements cut off*)
	fun pRaux N L = (nSegment N L)::(pRaux N (nCut N L))
in 
	pRaux n L
end;

(*5*)
fun filter pred L =
let 
	fun revAppend ([], L) = L
			| revAppend(x::rest, L) = revAppend(rest, x::L)
	fun reverse L = revAppend(L, [])
	fun auxFilter pred [] acc = acc 
	| auxFilter pred (x::rest) acc = if pred x then (auxFilter pred rest (x::acc))
									 else (auxFilter pred rest acc)
in
	reverse (auxFilter pred L [])
end;

(*6*)
fun mergeSort L = 
let 
	fun unitList [] = [] 
	| unitList (x::rest) = [x]::(unitList rest)
	fun merge [] L2 = L2 
	| merge L1 [] = L1 
	| merge (x::xrest) (y::yrest) =
		if x < y then x::(merge xrest (y::yrest))
		else y::(merge (x::xrest) yrest)
	fun foldl f base [] = base 
	| foldl f base (x::rest) = foldl f (f base x) rest 
in 
	foldl merge [] (unitList L)
	(*I couldn't find a way to traditionally 
	implement the merge function over groups of equally 
	sized lists || other examples online use a "split"
	function that allows for recursive calls to 
	eventually merge from inside out, creating even sized 
	lists to be merged together*)
end;
fun mergeSort2 L = 
let 
	fun unitList [] = [] 
	| unitList (x::rest) = [x]::(unitList rest)
	fun exclMerge [] L2 = L2 
	| exclMerge L1 [] = L1 
	| exclMerge (x::xrest) (y::yrest) =
		if x = y then x::(exclMerge xrest yrest) 
			(*duplicate remover case*)
		else if x < y then x::(exclMerge xrest (y::yrest))
		else y::(exclMerge (x::xrest) yrest)
	fun foldl f base [] = base 
	| foldl f base (x::rest) = foldl f (f base x) rest 
in 
	foldl exclMerge [] (unitList L)
end;

(*7 -- UNFINISHED*)
datatype either = ImAString of string | ImAnInt of int;
datatype eitherTree = LEAF of either | INTERIOR of (either * eitherTree * eitherTree);
fun eitherSearch tree N = 
let 
	fun eitherIsInt (ImAnInt _) = true | eitherIsInt (ImAString _) = false
	fun either2Int (ImAnInt x) = x
		(*function not called unless x is proved to be ImAnInt*)
	fun eitherAux (LEAF x) n = 
		if ((eitherIsInt x) andalso ((either2Int x) = n)) then true 
		else false
			(*current path to leaf doesn't contain a value equal to n*)
	| eitherAux (INTERIOR (x, t1, t2)) n = 
		if ((eitherIsInt x) andalso ((either2Int x) = n)) then true
		else ((eitherAux t1 n) orelse (eitherAux t2 n))
			(*evaluates each subtree, if one call is true, fun returns true*)
in 
	eitherAux tree N 
end;
fun eitherTest = 
	val I1 = ImAnInt 1
	val I2 = ImAnInt 2
	val I3 = ImAnInt 3
	val I4 = ImAnInt 4
	val I5 = ImAnInt 5
	val S1 = ImAString "a"
	val S2 = ImAString "b"
	val S3 = ImAString "c"
	val S4 = ImAString "d"
	val S5 = ImAString "e"
	(*leaf nodes*)
	val N9 = INTERIOR (S1, S2)
	val N8 = INTERIOR (I4, I5)
	val N7 = INTERIOR (S4, S5)
	val N6 = INTERIOR ((ImAnInt 6), N9, S3)
	val N5 = INTERIOR ((ImAnInt 6), I3, N8)
	val N4 = INTERIOR (I1, I2)
	val N3 = INTERIOR ((ImAnInt 6), N6, N7)
	val N2 = INTERIOR ((ImAnInt 6), N4, N5)
	val N1 = INTERIOR ((ImAnInt 6), N2, N3)
	(*tree starts at Node1*)
	(*interior nodes all have value 6*)
	val test1 = eitherSearch N1 5
	val test2 = eitherSearch N1 7
	
	case (test1 and (not test2)) of 
		true => true
	  | false => false;

(*8*)
datatype 'a Tree = LEAF of 'a | NODE of ('a Tree) list;
fun treeToString f (LEAF x) = f x
| treeToString f (NODE L) = String.concat (map (treeToString f) L);