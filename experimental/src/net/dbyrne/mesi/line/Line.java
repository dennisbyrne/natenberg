package net.dbyrne.mesi.line;

public abstract class Line {

	private final Integer value;

	Line(Integer value){
		this.value = value;
	}
	
	public Integer getValue(){
		return value;
	}

	public Line shared() {
		return new SharedLine(this.value);
	}

	public boolean isReadable() {
		return true;
	}
	
	public boolean isWriteable(){
		return false;
	}

	public Line invalid() {
		return new InvalidLine(this.value);
	}
	
}
