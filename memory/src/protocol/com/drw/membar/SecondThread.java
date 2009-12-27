package com.drw.membar;

import static com.drw.membar.Dekker.criticalSection;
import static com.drw.membar.Dekker.intentFirst;
import static com.drw.membar.Dekker.intentSecond;
import static com.drw.membar.Dekker.turn;

public class SecondThread extends Thread {

	@Override public void run() {
		for(int i = 0; i < 100000; i++)
			protocol();
	}	
	
	private void protocol(){
		intentSecond = true;

		while (intentFirst)
		  if (turn != 1) {
		    intentSecond = false;
		    while (turn != 1) {}
		    intentSecond = true;
		  }

		criticalSection();

		turn = 0;
		intentSecond = false;
	}

}
