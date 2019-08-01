fun toString []= ""
	| toString (l) = let fun convert []=[]
								   | convert (x::rest) = 
								   let val charlist = convert(rest) in chr(x+48)::charlist end
					 in implode(convert(l))
					 end