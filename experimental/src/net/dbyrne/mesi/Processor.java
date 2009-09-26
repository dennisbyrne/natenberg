package net.dbyrne.mesi;

import java.util.HashMap;
import java.util.Map;

import net.dbyrne.mesi.line.ExclusiveLine;
import net.dbyrne.mesi.line.Line;
import net.dbyrne.mesi.line.SharedLine;

public class Processor {
	
	private final Memory memory;
	private final Cache cache;
	private final Bus bus;
	private Map<Integer, Runnable> writeBacks = new HashMap<Integer, Runnable>();
	
	Processor(Memory m, Map<Integer, Line> cache, Bus bus){
		this.memory = m;
		this.cache = new Cache(cache);
		this.bus = bus;
		this.bus.add(this);
	}
	
	public Integer read(Integer address) {
		if(this.cache.miss(address)){
			Line line = this.bus.readRequest(address, this);
			if(line == null){
				// and now our neighbors don't have it
				line = this.memory.read(address);
			}
			this.cache.put(address, line);
		}
		return this.cache.get(address).getValue();
	}

	public void write(final Integer address, final Integer value) {
		this.cache.put(address, new ExclusiveLine(value));
		writeBacks.put(address, new Runnable(){
			@Override public void run() {
				memory.write(address, value);
			}
		});
	}

	public Line readRequest(Integer address) {
		Line line = this.cache.get(address);
		if(line != null){
			line = line.shared();
			this.cache.put(address, line);
		}
		return line;
	}

	public void flush() {
		// not entirely accurate, as the processor can re-order these
		for(Runnable runnable : this.writeBacks.values())
			runnable.run();
		this.writeBacks.clear();
	}

}
