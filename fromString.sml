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