package net.dbyrne.mesi;

import net.dbyrne.mesi.line.ExclusiveLine;

public class MemoryImpl implements Memory{

	private Integer[] data;
	
	public MemoryImpl(int one, int two, int three, int four) {
		this.data = new Integer[]{one, two, three, four};
	}

	@Override public ExclusiveLine read(Integer address) {
		return new ExclusiveLine(data[address]);
	}
	
	@Override public void write(Integer address, Integer value){
		data[address] = value;
	}

}
