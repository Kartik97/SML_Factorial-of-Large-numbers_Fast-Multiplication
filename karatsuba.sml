use "supp.sml";
use "extra.sml";

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


