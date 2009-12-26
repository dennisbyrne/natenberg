# set JAVAC=/usr/java/jdk1.6.0_17/bin/javac
# set JAVA=/usr/java/jdk1.6.0_17/bin/java
set JAVAC=/usr/lib/jvm/java-6-openjdk/bin/javac
set JAVA=/usr/lib/jvm/java-6-openjdk/jre/bin/java

/usr/lib/jvm/java-6-openjdk/bin/javac com/drw/membar/*.java
/usr/lib/jvm/java-6-openjdk/jre/bin/java -cp . -server -XX:+UnlockDiagnosticVMOptions -XX:PrintAssemblyOptions=hsdis-print-bytes -XX:CompileCommand=print,com/drw/membar/FirstThread.protocol com.drw.membar.Dekker > first.txt
/usr/lib/jvm/java-6-openjdk/jre/bin/java -cp . -server -XX:+UnlockDiagnosticVMOptions -XX:PrintAssemblyOptions=hsdis-print-bytes -XX:CompileCommand=print,com/drw/membar/SecondThread.protocol com.drw.membar.Dekker > second.txt
