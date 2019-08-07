(*SML Program for calculating factorial of numbers using karatsuba algorithm.

-Some Functions are dependent on other functions and are called accordingly so when called alone they might not give the expected output.
-Each individul number in a list is in base 10^4.
-Mostly computations are done on lists with reversed order. 
	Example : Number "12345678" is represented as [5678,1234] for computational purposes.
*)

(* Exception *)
exception Invalid_Input_exception of string

(*	fromString function : Takes a string as input and converts the string into list of base 10^4 numbers
	Input = A string
	Output = String converted into list of base 10^4 numbers in normal order
*)
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

(*	trim function : Removes leading zeros from a list of numbers
	Input = A list of numbers
	Output = List of numbers without leading zeros
*)
fun trim [] = []
	| trim [0] = [0]
	| trim (x::A) = let
						val trimmed = if ( x <> 0 ) then A else trim A
					in
						if (x <> 0) then x::trimmed else trimmed
					end

(*	toString funtion : Takes a list of base 10^4 numbers and converts them into a string
	Input = A list of base 10^4 numbers in normal order
	Output = Converted string
*)
fun toString [] = ""
	| toString (l) = 
		let
			fun rem [] = ""
				| rem (x::[]) = Int.toString(x)
				| rem (x::l) =  if (x = 0) then rem (l) ^ "0000" else
								if ( x div 10 = 0 ) then rem (l) ^ "000" ^ Int.toString(x) else 
								if ( x div 100 = 0 ) then rem (l) ^ "00" ^ Int.toString(x) else 
								if ( x div 1000 = 0 ) then rem (l) ^ "0" ^ Int.toString(x) else 
									rem (l) ^ Int.toString(x)
		in
			rem( List.rev (trim(l)))
		end

(*  add function : Adds two lists of base 10^4 numbers. The length of the lists may differ.
	Input = First list in reversed order, second list in reversed order, carry (initially 0)
	Output = Sum of the first two lists in reversed order
*)
fun add([],[],cy) = if (cy=0) then [] else [cy]
	| add(x::A,[],cy) = let val l = add(A,[],(x+cy) div 10000) in ((x+cy) mod 10000) ::l end
	| add([],y::B,cy) = let val l = add([],B,(y+cy) div 10000) in ((y+cy) mod 10000) ::l end
	| add(x::A,y::B,cy) = 
		let val l = add(A,B,(x+y+cy) div 10000) in ((x+y+cy) mod 10000) :: l end

(*	sub function : Finds the difference between two lists of numbers. 
	The length of the lists may differ but the equivalent number of the first list must be larger than the number of the second list. 
	The function in the rest of the code has been called according to this convention.
	Input = A list in reversed order, List in reversed order, borrow (initially 0)
	Output = Difference of the two lists in reversed order.
*)
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

(*	singleMultiply function : Used to calculate the product of two single digits in base 10^4
	Input = List containing a single 10^4 base number, List containing a single 10^4 base number
	Output = List of number in reversed order containing the result of the product of the numbers
*)
fun singleMultiply([x],[y]) = if (x*y = 0) then [0] 
								else 
									let
										fun splitMul n = 
											if n=0 then []
											else let val l = splitMul(n div 10000) in (n mod 10000) :: l end
									in
										splitMul(x*y)
									end

(*	makeEqual function : Used to pad 0's at the start of the list
	Input = A list,Number of zeros to be padded
	Output = Original list with 0's padded at the beginning
*)
fun makeEqual (l,x) = if x>0 then makeEqual(0::l,x-1) else l 

(*	make function : Used to make the length of two lists equal by adding zeros at the front of the shorter list
	Input = A tuple of two lists in reversed order.
	Ouput = A tuple with equal length lists. Output lists are also in reversed order.
*)
fun make(A,B) = if (length A > length B) then (A,List.rev(makeEqual (List.rev(B),length(A)-length(B)))) 
				else (List.rev(makeEqual (List.rev(A),length(B)-length(A))),B)

(*	final function : Used for the combining the result of the divided lists by the multiply function
	Input = Three lists and the length of the original list in the multiply function
	Output = Final product of the three lists in accordance with the karatsuba algorithm
*)
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

(*	multiply function: Function used to provide divide the lists into two parts and then multiply the individual parts.
	Input = List in reversed order, list in reversed order
	Output = List in reversed order representing the product of the above two lists
*)
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

(*	karatsuba : Function implementing karatsuba algorithm
	Input = Two lists of numbers in normal order.
	Output = Single list in normal order representing the product of the above numbers
*)
fun karatsuba A B = trim(List.rev(multiply(List.rev(A),List.rev(B))))

(*	create function : Used to create a list of function of karatsuba from 1 to n for computing the factorial
	Input = A list, current value of the number whose karatsuba function is to be created
	Output = A list of karatsuba functions requiring only 1 parameter.
*)
fun create ([],cur)=[]
	| create (no,cur) = 
		let
			val cmp = sub (no, cur,0)
		in
			if (trim(cmp) = [0] ) then [karatsuba no] else (karatsuba cur) :: create(no,add(cur,[1],0))
		end

(*	apply function : Applies values to the list of karatsuba functions
	Input = The list of Karatsuba functions created by the create function, Starting value which is to be applied
	Output = The final result of applying values to all the functions in the list
*)
fun apply ([],start) = start
	| apply (f::rest,start) = 
		let
		 	val temp = f start
		 in
		 	apply (rest,temp)
		 end

(*	factorial function : Fucntion for computing the factorial of a number
	Input = Number in string form whose factorial is to be calculated
	Ouput = Resultant factorial in string form
*)
fun factorial str =
	let
		val strConverted = fromString(str)	 
		val functions = if ( strConverted = [0] ) then [] else create (List.rev(strConverted),[1])
	in
		if (strConverted = [0]) then "1"
		else
			let
				val applied = apply(functions,[1])
			in
				toString (applied)
			end