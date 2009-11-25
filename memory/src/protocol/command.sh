/usr/lib/jvm/java-6-openjdk/bin/javac WriterReader.java 
/usr/lib/jvm/java-6-openjdk/jre/bin/java -XX:+UnlockDiagnosticVMOptions -XX:PrintAssemblyOptions=hsdis-print-bytes -XX:CompileCommand=print,WriterReader.write WriterReader > write.txt
/usr/lib/jvm/java-6-openjdk/jre/bin/java -XX:+UnlockDiagnosticVMOptions -XX:PrintAssemblyOptions=hsdis-print-bytes -XX:CompileCommand=print,WriterReader.read WriterReader > read.txt

