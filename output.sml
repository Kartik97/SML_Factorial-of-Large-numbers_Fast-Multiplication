fun printToOutStream outstream str = let val os = outstream
                                     in
                                       TextIO.output(os,str);
                                       TextIO.closeOut os
                                     end;

val os = TextIO.openOut "res.txt";

printToOutStream os it;