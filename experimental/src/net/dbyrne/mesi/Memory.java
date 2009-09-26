package net.dbyrne.mesi;

import net.dbyrne.mesi.line.ExclusiveLine;

public interface Memory {

	ExclusiveLine read(Integer address);
	
	void /* write behind */ write(Integer address, Integer value);

}
