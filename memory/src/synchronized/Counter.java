class Counter{

  static int counter = 0;

  public static void main(String[] _){
       for(int i = 0; i < 100000; i++)
          inc();
  }

  static synchronized void inc(){ counter += 1; }

}
