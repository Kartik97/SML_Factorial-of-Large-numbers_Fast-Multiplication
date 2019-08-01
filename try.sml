fun reverse [] = []
	| reverse (x::l) = 
		let val l = reverse  l in l @ [x] end

fun add([],[],cy) = if (cy=0) then [] else [cy] 
	| add(x::A,y::B,cy) = 
		let val l = add(A,B,(x+y+cy) div 10000) in l @ [(x+y+cy) mod 10000 ] end

