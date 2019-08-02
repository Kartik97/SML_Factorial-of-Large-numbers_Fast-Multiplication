fun reverse [] = []
	| reverse (x::l) = 
		let val l = reverse  l in l @ [x] end

fun bigger([],[]) = true
	| bigger(x::A,y::B) = 
		if (x = y) then bigger(A,B) 
		else if (x>y) then true 
			 else false

fun act([],[],diff) = []
	| act (x::A,y::B,diff) =
		let val l = if diff=1 then 
							  if (x-1)<y then act(A,B,1) else act(A,B,0) 
					else if x<y then act(A,B,1) else act(A,B,0) 
		in if diff=1 then 
					  if (x-1)<y then l @ [(10000+(x-1-y))] else l @ [(x-1-y)]
			else if x<y then l @ [(10000+(x-y))] else l @ [(x-y)]
		end   

fun pad n = if n=0 then [] 
			else 0 :: pad(n-1)

fun add([],[],cy) = if (cy=0) then [] else [cy]                             	        (*ADD pass two reversed lists*)
	| add(x::A,y::B,cy) = 
		let val l = add(A,B,(x+y+cy) div 10000) in l @ [(x+y+cy) mod 10000 ] end

fun sub(A,B) = if (bigger(A,B)) then (0,act(reverse(A),reverse(B),0)) else (1,act(reverse(B),reverse(A),0))


fun karatsuba A B = 
let val l1=length(A) and l2=length(B) 
in if(l1>l2) then multiply (A,makeEqual(B,l1-l2),l1)
		else multiply (makeEqual(A,l2-l1),B,l2)
end