package net.dbyrne.mesi;

import java.util.HashMap;
import java.util.Map;

import net.dbyrne.mesi.line.ExclusiveLine;
import net.dbyrne.mesi.line.Line;

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
	
	/**
	 * An Invalid line must be fetched (to the Shared or Exclusive states) to satisfy a read.
	 */
	public Integer read(Integer address) {
		if(this.cache.miss(address)){
			Line line = this.bus.readRequest(address, this);
			if(line == null){
				line = this.memory.read(address);
			}
			this.cache.put(address, line);
		}
		return this.cache.get(address).getValue();
	}

	/**
	 * A write may only be performed if the cache line is in the Modified or Exclusive state.
	 * If it is in the Shared state, all other cached copies must be invalidated first. 
	 * This is typically done by a broadcast operation known as Read For Ownership (RFO).
	 */
	public void write(final Integer address, final Integer value) {
		Line line = this.cache.get(address);
		if((line == null) || (line != null && line.isWriteable() && !line.getValue().equals(value))){
			doWrite(address, value);
		}else{
			this.bus.readForOwnership(address);
			doWrite(address, value);
		}
	}

	private void doWrite(final Integer address, final Integer value) {
		this.cache.put(address, new ExclusiveLine(value));
		// intentionally losing any previous write back for this address
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
		// the Set is appropriate, as the processor can re-order these
		for(Map.Entry<Integer, Runnable> writeback : this.writeBacks.entrySet())
			writeback.getValue().run();
		this.writeBacks.clear();
	}

	public void invalidate(Integer address) {
		this.cache.invalidate(address);
	}

}
