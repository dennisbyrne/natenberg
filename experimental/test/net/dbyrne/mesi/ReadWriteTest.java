package net.dbyrne.mesi;

import static org.junit.Assert.*;

import java.util.HashMap;
import java.util.Map;

import net.dbyrne.mesi.Memory;
import net.dbyrne.mesi.Processor;
import net.dbyrne.mesi.Thread;

import org.junit.Test;


public class ReadWriteTest {

	@Test
	public void noRaceCondition(){
		Map<Integer, Integer> data = new HashMap<Integer, Integer>(){{
			put(1, 1);
		}};
		Memory memory = new Memory(data);
		Processor a = new Processor(memory);
		Processor b = new Processor(memory);
		Thread aThread = new Thread(a);
		Thread bThread = new Thread(b);

		Integer x = aThread.read(1);
		assertEquals(x, 1);
	}
	
}
