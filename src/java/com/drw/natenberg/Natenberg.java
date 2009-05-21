package com.drw.natenberg;

import java.io.IOException;

import net.sf.json.JSONException;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

public class Natenberg {

	private OtpNode node = null;
	private OtpMbox mailbox = null;
	
	private final State start = new StateTransitionLogger(new StartState());
	private final State receiving = new StateTransitionLogger(new ReceivingState());
	private final State success = new StateTransitionLogger(new EndState(0));
	private final State fail = new StateTransitionLogger(new EndState(1));
	
	private final View view = new View();
	
	private interface State{ // FSM
		void transition();
	}
	
	public static void main(String[] args){
		new Natenberg().go();
	}

	public void go(){
		start.transition();
	}
	
	private class StartState implements State{
		public void transition() {
			try {
				node = new OtpNode("view@127.0.0.1");
				mailbox = node.createMbox("graph");
				receiving.transition();
			} catch (IOException e) {
				fail.transition();
			}
		}
	}
	
	private class ReceivingState implements State{
		private Long sequence = 1l;
		public void transition() {
			while(true){
				try {
					view.show(mailbox.receive(), sequence++);
					System.out.println("Message Processed");
				} catch (OtpErlangExit e) {
					success.transition();
				} catch (OtpErlangDecodeException e) {
					fail.transition();
				} catch (ClassCastException cce){
					cce.printStackTrace();
					view.show(cce.getClass().getName() + " " + cce.getCause(), sequence);
				} catch(JSONException je){
					je.printStackTrace();
					view.show(je.getClass().getName(), sequence);
				}
			}
		}
	}
	
	private class EndState implements State{
		private int returnValue;
		EndState(int returnValue){
			this.returnValue = returnValue;
		}
		public void transition() {
			if(mailbox != null)
				mailbox.close();
			if(node != null)
				node.close();
			System.exit(returnValue);
		}
	}
	
	private class StateTransitionLogger implements State{
		private final State wrapped;
		public StateTransitionLogger(State wrapped) {
			this.wrapped = wrapped;
		}
		public void transition() {
			String name = wrapped.getClass().getName();
			System.out.println("Entered " + name);
			wrapped.transition();
			System.out.println("Exit " + name);
		}
	}
	
}
