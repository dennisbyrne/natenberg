package net.dbyrne.mesi;

import java.util.Map;

import net.dbyrne.mesi.line.Line;

public class Cache{

	private final Map<Integer, Line> map;
	private Integer lastAccessed;

	Cache(Map<Integer, Line> cache){
		this.map = cache;
		this.lastAccessed = null;
	}

	public Line get(Integer key) {
		this.lastAccessed = key;
		return map.get(key);
	}

	public Line put(Integer key, Line value) {
		if(map.size() > 2)
			throw new IllegalStateException(map.size() + " for this exercise, the cache is 2 and mm is 4 ");
		if(map.size() == 2)
			map.remove(lastAccessed);
		return map.put(key, value);
	}

	public boolean containsKey(Integer address) {
		return map.containsKey(address);
	}
	
	public boolean miss(Integer address){
		return !this.map.containsKey(address) || !this.map.get(address).isReadable();
	}

	public void invalidate(Integer address) {
		if(containsKey(address)){
			Line line = get(address);
			put(address, line.invalid());
		}
	}

}
