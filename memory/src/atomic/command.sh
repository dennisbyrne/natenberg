/usr/lib/jvm/java-6-openjdk/bin/javac Counter.java 
/usr/lib/jvm/java-6-openjdk/jre/bin/java -XX:+UnlockDiagnosticVMOptions -XX:PrintAssemblyOptions=hsdis-print-bytes -XX:CompileCommand=print,*AtomicInteger.incrementAndGet Counter > atomic.txt
