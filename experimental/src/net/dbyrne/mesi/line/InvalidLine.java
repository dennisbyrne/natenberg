package net.dbyrne.mesi.line;

public class InvalidLine extends Line{
	public InvalidLine(Integer value) {
		super(value);
	}
	
	/**
	 * A cache may satisfy a read from any state except Invalid.
	 */
	@Override
	public boolean isReadable() {
		return false;
	}
}
