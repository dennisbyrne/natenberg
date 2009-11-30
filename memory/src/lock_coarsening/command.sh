/usr/lib/jvm/java-6-openjdk/bin/javac LockCoarsening.java
/usr/lib/jvm/java-6-openjdk/jre/bin/java -server -XX:+UnlockDiagnosticVMOptions -XX:PrintAssemblyOptions=hsdis-print-bytes -XX:-UseBiasedLocking -XX:CompileCommand=print,LockCoarsening.twoLocks LockCoarsening > twoLocks.txt
