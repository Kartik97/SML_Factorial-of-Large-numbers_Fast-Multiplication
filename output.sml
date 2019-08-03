fun printToOutStream outstream str = let val os = outstream
                                     in
                                       TextIO.output(os,str);
                                       TextIO.closeOut os
                                     end;

val os = TextIO.openOut "C:/programs/testfile.txt";

printToOutStream os "Hello SML IO";