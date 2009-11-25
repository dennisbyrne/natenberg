	class WriterReader{
	
	  static volatile int a = 0;
	  static volatile int b = 0;
	
	  public static void main(String[] _){
	    Thread reader = new Thread(){
	      public void run(){
	        for(int i = 0; i < 10000; i++)
	          read();
	      }
	    };	  
	    Thread writer = new Thread(){
	      public void run(){
	        for(int i = 0; i < 10000; i++)
	          write(i);
	      }
	    };
	    reader.start(); 
	    writer.start();
	  }

	  static void read(){
	    int aLocal = a;
	    int bLocal = b;
	  }
	
	  static void write(int newValue){ 
	    a = newValue;
	    b = newValue;
	  }
	
	}
