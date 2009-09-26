package net.dbyrne.mesi;

import java.util.HashSet;
import java.util.Set;

import net.dbyrne.mesi.line.Line;

public class Bus {
	
	private Set<Processor> peers = new HashSet<Processor>();
	
	void add(Processor p){
		this.peers.add(p);
	}
	
	public Line readRequest(Integer address, Processor to){
		Line line = null;
		for(Processor from : peers){
			if(to == from)
				continue;
			line = from.readRequest(address);
			if(line != null)
				break;
		}
		return line;
	}
	
}
