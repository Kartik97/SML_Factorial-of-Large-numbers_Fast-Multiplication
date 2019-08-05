exception Invalid_Input_exception of string
fun fromString s = 
	let val l = explode(s)
	in let fun convert []=[]
				| convert (x::rest) = let val intlist = convert(rest) 
									  in if ord(x)>=48 andalso ord(x)<=57 then (ord(x)-48)::intlist else raise (Invalid_Input_exception (s))
									  end

			fun base4 [] = []
				| base4(x::[]) = [x]
				| base4(x::y::[]) = [x+y*10]
				| base4(x::y::z::[]) = [x+y*10+z*100]
				| base4(x::y::z::k::l) = let val equalbase = base4(l) in (x+y*10+z*100+k*1000)::equalbase end
		in List.rev(base4(List.rev(convert(l))))
		end
	end

fun rem [] = ""
	| rem (x::[]) = Int.toString(x)
	| rem (x::l) =  if (x = 0) then rem (l) ^ "0000" else
					if ( x div 10 = 0 ) then rem (l) ^ "000" ^ Int.toString(x) else 
					if ( x div 100 = 0 ) then rem (l) ^ "00" ^ Int.toString(x) else 
					if ( x div 1000 = 0 ) then rem (l) ^ "0" ^ Int.toString(x) else 
						rem (l) ^ Int.toString(x)

fun toString [] = ""
	| toString (l) = rem( List.rev (l));

(* Takes reversed lists as input and gives the sum of those lists in reversed order *)
fun add([],[],cy) = if (cy=0) then [] else [cy]
	| add(x::A,[],cy) = let val l = add(A,[],(x+cy) div 10000) in ((x+cy) mod 10000) ::l end
	| add([],y::B,cy) = let val l = add([],B,(y+cy) div 10000) in ((y+cy) mod 10000) ::l end
	| add(x::A,y::B,cy) = 
		let val l = add(A,B,(x+y+cy) div 10000) in ((x+y+cy) mod 10000) :: l end

fun sub([],[],diff) = []
	| sub(x::A,[],diff) = 
		let val l = sub(A,[],0)
		in if diff=1 then (x-1)::l else  x::l
		end
	| sub([],y::B,diff) = [0]
	| sub(x::A,y::B,diff) =
		let val l = if diff=1 then 
							  if (x-1)<y then sub(A,B,1) else sub(A,B,0) 
					else if x<y then sub(A,B,1) else sub(A,B,0) 
		in if diff=1 then 
					  if (x-1)<y then (10000+(x-1-y))::l else (x-1-y)::l
			else if x<y then (10000+(x-y))::l else (x-y)::l
		end

fun singleMultiply([x],[y]) = if (x*y = 0) then [0] 
								else 
									let
										fun splitMul n = 
											if n=0 then []
											else let val l = splitMul(n div 10000) in (n mod 10000) :: l end
									in
										splitMul(x*y)
									end

fun makeEqual (l,x) = if x>0 then makeEqual(0::l,x-1) else l 

fun make(A,B) = if (length A > length B) then (A,List.rev(makeEqual (List.rev(B),length(A)-length(B)))) 
				else (List.rev(makeEqual (List.rev(A),length(B)-length(A))),B)

fun final (A,B,C,l) = 
	let
		val z = (l div 2) + (l mod 2)
		val AB = (add(A,B,0))
		val v = sub(C,AB,0);
		val p_p1=  makeEqual(A,(2 * z))
		val p_p3 = makeEqual(v , z)
		val added = (add(p_p1,B,0))
	in
		 add(added,p_p3,0)
	end

fun multiply (A,B) = 
	if (length(A)=1 andalso length(B)=1) then singleMultiply(A,B)
	else let val (x,y) = make(A,B);
		 		  val l = List.length(x);  
		 		  val (b,a) = List.splitAt(x,(l div 2) + (l mod 2)); val (d,c) = List.splitAt(y,(l div 2) + (l mod 2))
	 		  in let
		 	  		val p1 = multiply(a,c); val p2 = multiply(b,d); 
		 	  		val res1 = add(a,b,0); val res2 = add(c,d,0) ; 
		 			val p3=multiply(res1,res2);
			 	  in
			 	  	final(p1,p2,p3,l)
			 	  end
		 	  end

fun trim [] = []
	| trim [0] = [0]
	| trim (x::A) = let
						val trimmed = if ( x <> 0 ) then A else trim A
					in
						if (x <> 0) then x::trimmed else trimmed
					end

fun karatsuba A B = trim(List.rev(multiply(List.rev(A),List.rev(B))))

fun create ([],cur)=[]
	| create (no,cur) = 
		let
			val cmp = sub (no, cur,0)
		in
			if (trim(cmp) = [0] ) then [karatsuba no] else (karatsuba cur) :: create(no,add(cur,[1],0))
		end

fun apply ([],start) = start
	| apply (f::rest,start) = 
		let
		 	val temp = f start
		 in
		 	apply (rest,temp)
		 end 

fun factorial str =
	let
		val strConverted = fromString(str)	 
		val functions = if ( strConverted = [0] ) then [] else create (strConverted,[1])
	in
		if (strConverted = [0]) then "1"
		else
			let
				val applied = apply(functions,[1])
			in
				toString (applied)
			end	
	end