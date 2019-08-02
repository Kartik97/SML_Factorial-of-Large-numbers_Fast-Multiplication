use "supp.sml";
use "extra.sml";

(*fun multiply(A,B) = 
	let val l=length(A) 
	in if(l=0) then ([0],[0],[0]) 
	   else if(l=1) then ([0],[0],singleMultiply(A,B))
	   	    else let val (a,b) = sublist(A,l,0) ; val (c,d) = sublist(B,l,0)
	   	    	 in let val p1=multiply(a,c); val p2=multiply(b,d); val p3=multiply(add(a,b,0),add(c,d,0))
	   	    	 	in (p1,p2,p3)
	   	    	 	end 
	   	    	 end
	end 
*)

fun multiply (A,B,l) = 
	if(l=0) then []
	else if (l=1) then singleMultiply(A,B) 
		 else let val (a,b) = sublist(A,l,0); val (c,d) = sublist(b,l,0)
		 	  in let
		 	  		val p1 = multiply(a,c,length(a)); val p2 = multiply(b,d,length(b)); val res1 = add(a,b,0); val res2 = add(c,d,0)
		 	  	 in
		 	  			res1
		 	  	 end
		 	  end


