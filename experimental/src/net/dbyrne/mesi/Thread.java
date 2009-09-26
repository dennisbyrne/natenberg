package net.dbyrne.mesi;

public class Thread {
	
	private final Processor processor;
	
	Thread(Processor p){
		this.processor = p;
	}
	
	Integer read(Integer address){
		return processor.read(address);
	}
	
}
