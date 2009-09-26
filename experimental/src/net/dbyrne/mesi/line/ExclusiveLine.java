package net.dbyrne.mesi.line;

public class ExclusiveLine extends Line{

	public ExclusiveLine(Integer value) {
		super(value);
	}

	@Override
	public boolean isWriteable() {
		return true;
	}
	
}
