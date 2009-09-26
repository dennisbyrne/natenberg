package net.dbyrne.mesi;

import static org.junit.Assert.assertEquals;

import java.util.HashMap;

import net.dbyrne.mesi.line.ExclusiveLine;
import net.dbyrne.mesi.line.Line;

import org.jmock.Expectations;
import org.jmock.Mockery;
import org.junit.Test;

public class ReadWriteTest {

	private Mockery mockery = new Mockery();
	private Bus bus = new Bus();
	
	@Test
	public void exclusiveCacheHit(){
		final Memory memory = mockery.mock(Memory.class);
		mockery.checking(new Expectations() {{
			one(memory).write(1, 99);
		}});
		Processor aProcessor = new Processor(memory, new HashMap<Integer, Line>(), bus);
		Thread a = new Thread(aProcessor);
		a.write(1, 99);
		assertEquals(99, a.read(1));
		assertEquals(99, a.read(1));
		aProcessor.flush();
		mockery.assertIsSatisfied();
	}
	
	@Test
	public void sharedCacheHit(){
		final Memory memory = mockery.mock(Memory.class);
		mockery.checking(new Expectations() {{
			one(memory).read(1);
			will(returnValue(new ExclusiveLine(0)));
			one(memory).write(1, 1);
		}});

		Processor aProcessor = new Processor(memory, new HashMap<Integer, Line>(), bus);
		Thread a = new Thread(aProcessor);
		Thread b = new Thread(aProcessor);

		assertEquals(0, a.read(1));
		a.write(1, 1);
		aProcessor.flush();
		assertEquals(1, b.read(1));
		mockery.assertIsSatisfied();
	}

}
