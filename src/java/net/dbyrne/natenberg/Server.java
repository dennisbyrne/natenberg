/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package net.dbyrne.natenberg;

import static java.lang.System.out;
import static javax.servlet.http.HttpServletResponse.SC_INTERNAL_SERVER_ERROR;
import static net.sf.json.JSONObject.fromObject;
import static org.mortbay.jetty.servlet.Context.SESSIONS;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import net.sf.json.JSONObject;

import org.mortbay.jetty.handler.ResourceHandler;
import org.mortbay.jetty.servlet.Context;
import org.mortbay.jetty.servlet.DefaultServlet;
import org.mortbay.jetty.servlet.ServletHolder;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

public class Server {
	private OtpNode node = null;
	private final String PEER_NAME = "server@127.0.0.1";

	public static void main(String[] args) throws Exception{
		new Server().go();
	}

	private void go() throws Exception {
		node = new OtpNode("web@127.0.0.1");
		out.println(node.node() + " started w/ " + node.cookie());
		if (!node.ping(PEER_NAME, 3000l)){
			node.close();
			throw new RuntimeException("Could not ping " + PEER_NAME);
		}
		org.mortbay.jetty.Server server = new org.mortbay.jetty.Server(8080);
		Context statik = new Context(server, "/", SESSIONS);
		statik.setHandler(new ResourceHandler());
		statik.setResourceBase("./src/web");
		Context root = new Context(server, "/", SESSIONS);
		root.addServlet(new ServletHolder(new JsonServelet()), "*.json");
		server.start();
		server.join();
		// TODO node.close();
	}

	@SuppressWarnings("serial")
	public class JsonServelet extends DefaultServlet {
		@Override protected void doGet(HttpServletRequest request,
				HttpServletResponse response) throws ServletException, IOException {
			OtpMbox mailbox = node.createMbox();
			String[] split = request.getRequestURL().toString().replaceAll(".json", "").split("/");
			String function = split[split.length - 1];
			mailbox.send("rex", PEER_NAME, newMsg(mailbox.self(), function));
			JSONObject res = null;
			try {
				OtpErlangTuple tuple = (OtpErlangTuple) mailbox.receive(3000l);
				String json = tuple.elementAt(1).toString();
				res = fromObject(json.substring(1, json.length() - 1));
			} catch (Exception e) {
				e.printStackTrace();
				response.setStatus(SC_INTERNAL_SERVER_ERROR);
				res = newErrorMsg(e);
			}
			mailbox.close();
			response.setContentType("application/json");
			response.getWriter().print(res);
		}
	}

	private OtpErlangTuple newMsg(OtpErlangPid returnAddress, String function) {
		return new OtpErlangTuple(new OtpErlangObject[]{
			returnAddress,
			new OtpErlangTuple(new OtpErlangObject[]{
				new OtpErlangAtom("call"),
				new OtpErlangAtom("gen_server"),
				new OtpErlangAtom("call"),
				new OtpErlangList(new OtpErlangObject[]{ /* args */
						new OtpErlangAtom("natenberg"),
						new OtpErlangAtom(function)}),
				new OtpErlangAtom("user")})});
	}

	private JSONObject newErrorMsg(Exception e){
		return new JSONObject().accumulate("error", e.getClass() + ":" + e.getMessage());
	}
}
