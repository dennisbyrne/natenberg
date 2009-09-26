package net.dbyrne.mesi;

import static org.junit.Assert.assertEquals;

import java.util.HashMap;
import java.util.Map;

import net.dbyrne.mesi.line.ExclusiveLine;
import net.dbyrne.mesi.line.InvalidLine;
import net.dbyrne.mesi.line.Line;

import org.jmock.Expectations;
import org.jmock.Mockery;
import org.junit.Test;

public class ProcessorTest {

	private Mockery mockery = new Mockery();
	private Bus bus = new Bus();

	@Test
	public void invalidLinesMustBeFetchedFromPeers(){
		final Memory memory = mockery.mock(Memory.class);
		Map<Integer, Line> acache = new HashMap<Integer, Line>() {{
			put(1, new InvalidLine(99));
		}};
		Processor a = new Processor(memory, acache, bus);
		Map<Integer, Line> bcache = new HashMap<Integer, Line>() {{
			put(1, new ExclusiveLine(100));
		}};
		Processor b = new Processor(memory, bcache, bus);
		assertEquals("reading through proc a comes from cache of b", 100, a.read(1));
		this.mockery.assertIsSatisfied();
	}
	
	@Test
	public void invalidLinesMustBeFetchedFromMemory(){
		final Memory memory = mockery.mock(Memory.class);
		mockery.checking(new Expectations() {{
			one(memory).read(1);
			will(returnValue(new ExclusiveLine(100)));
		}});
		Map<Integer, Line> cache = new HashMap<Integer, Line>() {{
			put(1, new InvalidLine(99));
		}};
		Processor processor = new Processor(memory, cache, bus);
		assertEquals(100, processor.read(1));
		this.mockery.assertIsSatisfied();
	}
	
	@Test
	public void processorShouldOnlyWriteToTheSameAddressInMemoryOnce(){
		final Memory memory = mockery.mock(Memory.class);
		mockery.checking(new Expectations() {{
			one(memory).write(1, 2);
		}});
		Processor processor = new Processor(memory, new HashMap<Integer, Line>(), bus);
		processor.write(1, 1);
		assertEquals(1, processor.readRequest(1).getValue());
		processor.write(1, 2);
		processor.flush();
		this.mockery.assertIsSatisfied();
	}

	@SuppressWarnings("serial")
	@Test
	public void processorShouldAvoidMainMemoryWhenCacheHitExclusive() {
		final Memory memory = mockery.mock(Memory.class);
		Map<Integer, Line> cache = new HashMap<Integer, Line>() {{
			put(1, new ExclusiveLine(99));
		}};
		Processor processor = new Processor(memory, cache, bus);
		assertEquals(99, processor.read(1));
		this.mockery.assertIsSatisfied();
	}

}
