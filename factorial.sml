use "karatsuba.sml";

exception Invalid_Input_exception

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

fun rem [] = ""
	| rem (x::[]) = Int.toString(x)
	| rem (x::l) =  if (x = 0) then rem (l) ^ "0000" else
					if ( x div 10 = 0 ) then rem (l) ^ "000" ^ Int.toString(x) else 
					if ( x div 100 = 0 ) then rem (l) ^ "00" ^ Int.toString(x) else 
					if ( x div 1000 = 0 ) then rem (l) ^ "0" ^ Int.toString(x) else 
						rem (l) ^ Int.toString(x)

fun toString [] = ""
	| toString (l) = rem( reverse (l));
 
fun create ([],cur,inc)=[]
	| create (l,cur,inc) = 
		let
			val (s,cmp) = sub (l, cur)
		in
			if (trim(cmp) = [0] ) then [karatsuba l] else (karatsuba cur) :: create(l,add(reverse(cur),reverse(inc),0),inc)
		end

fun apply ([],start) = start
	| apply (x::l,start) = 
		let
		 	val temp = x start
		 in
		 	apply (l,temp)
		 end 

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