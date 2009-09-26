package net.dbyrne.mesi;

import java.util.HashMap;
import java.util.Map;

public class Processor {
	
	private final Memory memory;
	private Map<Integer, Line> cache = new HashMap<Integer, Line>();
	
	Processor(Memory m){
		this.memory = m;
	}
	
	public Integer read(Integer address) {
		
	}
	
}
