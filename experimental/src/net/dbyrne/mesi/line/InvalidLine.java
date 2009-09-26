package net.dbyrne.mesi.line;

public class InvalidLine extends Line{
	public InvalidLine(Integer value) {
		super(value);
	}
	
	@Override
	public boolean isReadable() {
		return false;
	}
}
