/usr/lib/jvm/java-6-openjdk/bin/javac WriterReader.java 
/usr/lib/jvm/java-6-openjdk/jre/bin/java -XX:+UnlockDiagnosticVMOptions -XX:PrintAssemblyOptions=hsdis-print-bytes -XX:CompileCommand=print,WriterReader.write WriterReader > write.txt
/usr/lib/jvm/java-6-openjdk/jre/bin/java -XX:+UnlockDiagnosticVMOptions -XX:PrintAssemblyOptions=hsdis-print-bytes -XX:CompileCommand=print,WriterReader.read WriterReader > read.txt

/usr/java/jdk1.6.0_17/bin/javac com/drw/membar/*.java
/usr/java/jdk1.6.0_17/bin/java -cp . -server -XX:+UnlockDiagnosticVMOptions -XX:PrintAssemblyOptions=hsdis-print-bytes -XX:CompileCommand=print,com/drw/membar/FirstThread.protocol com.drw.membar.Dekker > first.txt
/usr/java/jdk1.6.0_17/bin/java -cp . -server -XX:+UnlockDiagnosticVMOptions -XX:PrintAssemblyOptions=hsdis-print-bytes -XX:CompileCommand=print,com/drw/membar/SecondThread.protocol com.drw.membar.Dekker > second.txt
