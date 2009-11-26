	class WriterReader{
	
	  static volatile int shared = 0;
	  static volatile boolean initialized = false;
	
	  public static void main(String[] _){
		Thread reader = new Thread(){
	      public void run(){
	        for(int i = 0; i < 10000; i++)
	          read();
	      }
	    };
	    reader.start();
	    
	    Thread writer = new Thread(){
	      public void run(){
	        for(int i = 0; i < 10000; i++)
	          write();
	      }
	    };
	    writer.start();
	  }

	  static int read(){
		return initialized ? shared : -1;
	  }
	
	  static void write(){
		shared = 42;
	    initialized = true;
	  }
	
	}
