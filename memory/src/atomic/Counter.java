import java.util.concurrent.atomic.AtomicInteger;

class Counter{

  static AtomicInteger counter = new AtomicInteger(0);

  public static void main(String[] args){
    for(int i = 0; i < 1000000; i++)
      counter.incrementAndGet(); 
  }

}
