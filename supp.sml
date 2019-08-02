fun length [] = 0
	| length (x::rest) = 1+length(rest)

fun make(A,B) = if (length A > length B) then (A,makeEqual (B,length(A)-length(B))) else (makeEqual(A,length(B)-length(A)),B)

fun makeEqual (l,x) = if x>0 then makeEqual(0::l,x-1) else l 

fun splitMul n = 
	if n=0 then []
	else let val l = splitMul(n div 10000) in l @ [n mod 10000] end

fun singleMultiply(x::A,y::B) = if (x*y = 0) then [0] else splitMul(x*y)

fun sublist ([],len,cur) = ([],[])
	| sublist(x::A,len,cur) = 
		let val (m,n) = sublist(A,len,cur+1)
		in if (cur< (len div 2)) then (x::m,n) else (m,x::n)
		end