package com.drw.membar;

    public class Dekker {

      static volatile boolean intentFirst = false;
      static volatile boolean intentSecond = false;
      static volatile int turn = 0;

      public static void main(String[] _){
        new FirstThread().start();
    	new SecondThread().start();
    	System.out.println("done");
      }

      static void criticalSection(){
        
      }
}
