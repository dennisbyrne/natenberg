package net.dbyrne.natenberg;

import static java.lang.System.out;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import static javax.servlet.http.HttpServletResponse.SC_INTERNAL_SERVER_ERROR;
import net.sf.json.JSONObject;

import org.mortbay.jetty.servlet.Context;
import org.mortbay.jetty.servlet.DefaultServlet;
import org.mortbay.jetty.servlet.ServletHolder;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

public class Server {

	private OtpNode node = null;
	private String PEER_NAME = "server@127.0.0.1";
	
	public static void main(String[] args) throws Exception{
		new Server().go();
	}

	private void go() throws Exception {
		node = new OtpNode("web@127.0.0.1");
		out.println(OtpNode.class.getSimpleName() + " started w/ " + node.cookie());
		if (!node.ping(PEER_NAME, 3000l))
			throw new RuntimeException("Could not ping " + PEER_NAME);
		org.mortbay.jetty.Server server = new org.mortbay.jetty.Server(8080);
		Context root = new Context(server,"/",Context.SESSIONS);
		root.addServlet(new ServletHolder(new JsonServelet()), "*.json");
		root.addServlet(HomeServlet.class, "/");
		server.start();
		server.join();
	}

	@SuppressWarnings("serial")
	public static class HomeServlet extends DefaultServlet {
		@Override protected void doGet(HttpServletRequest request,
				HttpServletResponse response) throws ServletException, IOException {
			String html = "<html>" +
						 	"<head></head>" + 
							"<body></body>" +
						  "</html>";
			response.setContentType("text/html");
			response.getWriter().print(html);
		}
	}
	
	@SuppressWarnings("serial")
	public class JsonServelet extends DefaultServlet {
		@Override protected void doGet(HttpServletRequest request,
				HttpServletResponse response) throws ServletException, IOException {
			response.setContentType("application/json");
			OtpMbox mailbox = node.createMbox();
			mailbox.send("rex", PEER_NAME, createMsg(mailbox.self()));
			String res = null;
			try {
				OtpErlangTuple tuple = (OtpErlangTuple) mailbox.receive(3000l);
				res = new JSONObject()
					.accumulate("node", tuple.elementAt(1).toString())
					.toString();
			} catch (Exception e) {
				response.setStatus(SC_INTERNAL_SERVER_ERROR);
				res = createErrorMsg(e);
			}
			node.close();
			response.getWriter().print(res);
		}
		
		String createErrorMsg(Exception e){
			return new JSONObject()
				.accumulate("error", e.getClass() + ":" + e.getMessage())
				.toString();
		}
		
		OtpErlangTuple createMsg(OtpErlangPid returnAddress) {
			OtpErlangObject[] call = new OtpErlangObject[]{
				new OtpErlangAtom("call"),
				new OtpErlangAtom("erlang"),
				new OtpErlangAtom("node"),
				new OtpErlangList(),
				new OtpErlangAtom("user")
			};
			return new OtpErlangTuple(
				new OtpErlangObject[]{
					returnAddress,
					new OtpErlangTuple(call)
				}
			);
		}
	}

}
