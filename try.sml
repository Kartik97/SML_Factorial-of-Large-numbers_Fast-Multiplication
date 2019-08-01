use "supp.sml";

fun add([],[],cy) = []
	| add(x::A,y::B,cy) = 
		let val l = add(A,B,(x+y+cy) div 10000) in (x+y+cy) mod 10000 ::l end