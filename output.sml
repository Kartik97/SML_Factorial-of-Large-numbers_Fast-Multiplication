fun printToOutStream outstream str = let val os = outstream
                                     in
                                       TextIO.output(os,str);
                                       TextIO.closeOut os
                                     end;

val os = TextIO.openOut "res.txt";

printToOutStream os res;


fun writeReal (real, filename) = 
    let val fd = TextIO.openOut filename
        val _ = map ( fn i =>  TextIO.output (fd, Int.toString i ^ "\r\n")) real
        val _ = TextIO.closeOut fd
    in () end

writeReal (res, "res.txt")

fun count [] = 0
	| count (x::l) = if(x = 0) then (1 + count(l)) else 0