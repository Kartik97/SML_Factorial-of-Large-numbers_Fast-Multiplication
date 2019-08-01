use "supp.sml";
(*)
fun multiply(A,B) = 
	let val l=length(A) 
	in if(l=0) then [0] 
	   else if(l=1) then singleMultiply(A,B)
	   	    else let val (a,b) = sublist(A,len,0) and val (c,d) = sublist(B,len,0)
	   	    	 in 
	   	    	 end
	end 
*)
fun karatsuba (A,B) = 
	let val l1=length(A) and l2=length(B) 
	in if(l1>l2) then multiply(A,makeEqual(B,l1-l2))
			else multiply(makeEqual(A,l2-l1),B)
	end