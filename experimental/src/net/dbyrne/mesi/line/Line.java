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

	@Override
	public boolean equals(Object obj) {
		return value.equals(obj);
	}

	@Override
	public int hashCode() {
		return value.hashCode();
	}
	
}
