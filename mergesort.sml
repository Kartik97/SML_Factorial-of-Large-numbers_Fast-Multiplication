
fun merge ([],[]) = []
	| merge([],rl) = rl
	| merge (ll,[]) =ll
	| merge (x::ll,y::rl) =	if x<y then x::merge(ll,y::rl) else y::merge(x::ll,rl);


fun split [] = ([],[])
	| split (x::[]) = ([x],[])
	| split (x::y::l) = let val (M,N)= split l in (x::M,y::N) end;


fun mergesort [] = []
	| mergesort (x::[]) = [x]
	| mergesort l =
			let val (M,N) = split l in merge(mergesort M,mergesort N) end;