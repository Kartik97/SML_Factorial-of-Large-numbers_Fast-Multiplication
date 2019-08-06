fun add([],[],extra) = (0,[])
	| add([x],[y],extra) = ((x+y) div 10000 , [((x+y) mod 10000)]) 
	| add(x::A,y::B,extra) = 
		let
			val (cy,res) = if (extra <> 0) then add(A,y::B,extra-1) else add(A,B,0);
		in
			if (extra <> 0) then ((x+cy) div 10000 , ((x+cy) mod 10000)::res) else ((x+y+cy) div 10000 , ((x+y+cy) mod 10000)::res)
		end

fun checkadd(A,B) = 
	let val lenA = List.length(A)
		val lenB = List.length(B)
	in 
		let val (cy,res) = if(lenA > lenB) then add(A,B,lenA-lenB) else add(B,A,lenB-lenA)
		in
			if (cy = 1) then 1::res else res
		end
	end