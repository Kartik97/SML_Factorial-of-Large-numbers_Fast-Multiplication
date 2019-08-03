(* Exception for invalid Inputs *)
exception Invalid_Input_exception

(* Helper Functions *)
(*---------------------------------------------------------------------------------------------*)

(*  length()
	Finding out the length of a list
	Input = A list
	Output = Integer length of the list
*)
fun length [] = 0
	| length (x::rest) = 1+length(rest)

(*	makeEqual()
	Increasing the length of a list by appending required number of zeros at the front
	Input = A list and the length to be increased
	Output = List with required length
*)
fun makeEqual (l,x) = if x>0 then makeEqual(0::l,x-1) else l 

(*	make()
	For making equal length lists - uses makeEqual to increase the length of the shorter list
	Input = Two lists 
	Output = Two lists of equal length
*)
fun make(A,B) = if (length A > length B) then (A,makeEqual (B,length(A)-length(B))) else (makeEqual(A,length(B)-length(A)),B)

(*	splitMul()
	Returns a number into list of 10^4 base numbers
	Input = Integer
	Output = List of base 10^4 numbers
*)
fun splitMul n = 
	if n=0 then []
	else let val l = splitMul(n div 10000) in l @ [n mod 10000] end

(*	singleMultiply()
	Multiplies two 10^4 base numbers
	Input = Two lists with a single number in each
	Output = A list with the result of multiplication of the two numbers	
*)
fun singleMultiply(x::A,y::B) = if (x*y = 0) then [0] else splitMul(x*y)

(*	sublist()
	Splits a list in half and returns the two resultant halves
	Input = A list
	Output = Resultant halves
*)
fun sublist ([],len,cur) = ([],[])
	| sublist(x::A,len,cur) = 
		let val (m,n) = sublist(A,len,cur+1)
		in if (cur< (len div 2)) then (x::m,n) else (m,x::n)
		end

(*	reverse()
	For reversing a list
	Input = A list
	Output = Reverse of the list
*)
fun reverse [] = []
	| reverse (x::l) = 
		let val l = reverse  l in l @ [x] end

(*	bigger()
	Returns the larger of the two numbers represented as lists of 10^4 base numbers
	Input = Two lists
	Output = true if first equivalent 10^4 base number is larger than the second otherwise false
*)
fun bigger([],[]) = true
	| bigger(x::A,y::B) = 
		if (x = y) then bigger(A,B) 
		else if (x>y) then true 
			 else false

(*	act()
	Helper function for performing subtraction. (For directly calling give larger number as first list)
	Input = two lists and a borrow 
	Output = A list which is the result of difference between the two lists
*)
fun act([],[],diff) = []
	| act (x::A,y::B,diff) =
		let val l = if diff=1 then 
							  if (x-1)<y then act(A,B,1) else act(A,B,0) 
					else if x<y then act(A,B,1) else act(A,B,0) 
		in if diff=1 then 
					  if (x-1)<y then l @ [(10000+(x-1-y))] else l @ [(x-1-y)]
			else if x<y then l @ [(10000+(x-y))] else l @ [(x-y)]
		end   

(*	pad()
	Creating a list of zeros which can be later appended to any list to pad zeros at the end
	Input = integer
	Output = list of zeros of given input
*)
fun pad n = if n=0 then [] 
			else 0 :: pad(n-1)

(*	add()
	For adding two numbers represented as lists of 10^4 base numbers
	Input = Two lists and a carry initially 0. (Lists to be passed in reverse order)
	Output = List containing result of addition
*)
fun add([],[],cy) = if (cy=0) then [] else [cy]
	| add(x::A,y::B,cy) = 
		let val l = add(A,B,(x+y+cy) div 10000) in l @ [(x+y+cy) mod 10000 ] end

(*	sub()
	For subtracting two lists
	Input = Two lists
	Output = A tuple with the first value as sign and second value is the result of subtraction
*)
fun sub(A,B) = if (bigger(A,B)) then (0,act(reverse(A),reverse(B),0)) else (1,act(reverse(B),reverse(A),0))

(*	equated()
	For equating the length of three strings
	Input = three lists
	Output = A tuple of three lists of equal sizes
*)
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

(*  final()
	funtion implementing the last part of karatsuba algorithm 
	Input = three lists(intermediate values of karatsuba algorithm) and the length of the original list 
	Output = final product of the original two lists
*)
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

(*	multiply()
	To multiply two lists
	Input = two lists of equal length and their length
	Output = Final product of the two lists
*)
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

(*	trim()
	Removing the leading zeros from a list
	Input = A list
	Output = List without leading zeros
*)
fun trim [] = []
	| trim [0] = [0]
	| trim (x::A) = let
						val trimmed = if ( x <> 0 ) then A else trim A
					in
						if (x <> 0) then x::trimmed else trimmed
					end

(*---------------------------------------------------------------------------------------------*)

(*	karatsuba
	Higher order function implementing the karatsuba algorithm with the help of multiply function
	Input = two lists
	Output = The product of the lists
*)
fun karatsuba A B = 
let val l1=length(A) and l2=length(B) 
in if(l1>l2) then trim (multiply (A,makeEqual(B,l1-l2),l1))
		else trim (multiply (makeEqual(A,l2-l1),B,l2))
end

(*  fromString
	Takes a string as input and returns an equivalent list of base 10^4 numbers as output
*)
fun fromString s = 
	let val l = explode(s)
	in let fun convert []=[]
				| convert (x::rest) = let val intlist = convert(rest) 
									  in if ord(x)>=48 andalso ord(x)<=57 then intlist @ [ord(x)-48] else raise Invalid_Input_exception 
									  end

			fun base4 [] = []
				| base4(x::[]) = [x]
				| base4(x::y::[]) = [x+y*10]
				| base4(x::y::z::[]) = [x+y*10+z*100]
				| base4(x::y::z::k::l) = let val equalbase = base4(l) in equalbase @ [x+y*10+z*100+k*1000] end
		in base4(convert(l))
		end
	end

(*  rem
	For removing the leading zeros from the resultant string. Used by toString
	Input = A list of integers. Reverse of the list is to be passed
	Output = String quivalent of the list without leading zeros
*)
fun rem [] = ""
	| rem (x::[]) = Int.toString(x)
	| rem (x::l) =  if (x = 0) then rem (l) ^ "0000" else
					if ( x div 10 = 0 ) then rem (l) ^ "000" ^ Int.toString(x) else 
					if ( x div 100 = 0 ) then rem (l) ^ "00" ^ Int.toString(x) else 
					if ( x div 1000 = 0 ) then rem (l) ^ "0" ^ Int.toString(x) else 
						rem (l) ^ Int.toString(x)

(*  toString()
	Converts list of integers to string using rem function
	Input = A list of integers
	Output = String equivalent of the list
*)
fun toString [] = ""
	| toString (l) = rem( reverse (l));
 
(*	create()
	Creates a list of karatsuba functions from n to 1
	Input = Input list, current value, increment to be made
	Output = A list of karatsuba funcitons
*)
fun create ([],cur,inc)=[]
	| create (l,cur,inc) = 
		let
			val (s,cmp) = sub (l, cur)
		in
			if (trim(cmp) = [0] ) then [karatsuba l] else (karatsuba cur) :: create(l,add(reverse(cur),reverse(inc),0),inc)
		end

(*  apply()
	The list of karatsuba functions is applied.
	Input = the list of karatsuba functions and a starting value of [1] for applying to the function
	Output = The final factorial after applying all the karatsuba functions
*)
fun apply ([],start) = start
	| apply (x::l,start) = 
		let
		 	val temp = x start
		 in
		 	apply (l,temp)
		 end 


(*  factorial()
	For calculating the factorial using karatsuba algorithm
	Input = input string of number whose factorial is to be calculated
	Output = string containing the factorial of the number in the input string
*)
fun factorial str =
	let
		val (v1,v2) = (make(fromString(str),[1]))	 
		val n = if ( v1 = [0] ) then [] else create (v1,v2,v2)
	in
		if (v1 = [0]) then "1"
		else
			let
				val applied = apply(n,v2)
			in
				toString (applied)
			end	
	end