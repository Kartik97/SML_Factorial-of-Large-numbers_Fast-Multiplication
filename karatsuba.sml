(* Helper Functions *)
(*---------------------------------------------------------------------------------------------*)

fun length [] = 0
	| length (x::rest) = 1+length(rest)

fun makeEqual (l,x) = if x>0 then makeEqual(0::l,x-1) else l 

fun make(A,B) = if (length A > length B) then (A,makeEqual (B,length(A)-length(B))) else (makeEqual(A,length(B)-length(A)),B)

fun splitMul n = 
	if n=0 then []
	else let val l = splitMul(n div 10000) in l @ [n mod 10000] end

fun singleMultiply(x::A,y::B) = if (x*y = 0) then [0] else splitMul(x*y)

fun sublist ([],len,cur) = ([],[])
	| sublist(x::A,len,cur) = 
		let val (m,n) = sublist(A,len,cur+1)
		in if (cur< (len div 2)) then (x::m,n) else (m,x::n)
		end

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

fun equated(A,B,C) = 
	let
		val l1 = length A; 
		val l2 = length B;
		val l3 = length C;	
	in
		if l1>=l2 andalso l1>=l3 then (A,makeEqual(B,l1-l2),makeEqual(C,l1-l3))
		else if l2>=l1 andalso l2>=l1 then (makeEqual(A,l2-l1),B,makeEqual(C,l2-l3))
		else (makeEqual(A,l3-l1),makeEqual(B,l3-l2),C)
	end

fun final (A,B,C,l) = 
	let
		val z = (l div 2) + (l mod 2)
		val (C,AB) = make(C,add(reverse(A),reverse(B),0))
		val (s,v) = sub(C,AB);
		val p_p1= A @ pad (2 * z);
		val p_p3 = v @ pad z
		val (P1,P2) = make(p_p1,B)
		val added = add(reverse(P1),reverse(P2),0)
		val (F,S) = make(added,p_p3)
	in
		 add(reverse(F),reverse(S),0)			
	end

fun multiply (A,B,l) = 
	if(l=0) then []
	else if (l=1) then singleMultiply(A,B)
		 else let val (xl,xr) = sublist(A,l,0); val (yl,yr) = sublist(B,l,0)
		 		  val (a,c) = make(xl,yl); val (b,d) = make(xr,yr)
	 		  in let
		 	  		val p1 = multiply(a,c,l div 2); val p2 = multiply(b,d, l div 2 + l mod 2); 
		 	  		val (h1,h2) = make(xl,xr); val (j1,j2) = make(yl,yr);
		 	  		val res1 = add(reverse(h1),reverse(h2),0); val res2 = add(reverse(j1),reverse(j2),0) ; 
		 	  		val (n1,n2) = make(res1,res2);
		 			val p3=multiply(n1,n2, length(n1));
		 			val (e1,e2,e3) = equated(p1,p2,p3)
			 	  in
			 	  	final(e1,e2,e3,l)
			 	  end
		 	  end

fun trim [] = []
	| trim [0] = [0]
	| trim (x::A) = let
						val trimmed = if ( x <> 0 ) then A else trim A
					in
						if (x <> 0) then x::trimmed else trimmed
					end

(*---------------------------------------------------------------------------------------------*)

fun karatsuba A B = 
let val l1=length(A) and l2=length(B) 
in if(l1>l2) then trim (multiply (A,makeEqual(B,l1-l2),l1))
		else trim (multiply (makeEqual(A,l2-l1),B,l2))
end