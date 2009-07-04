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

import static java.awt.Color.decode;
import static java.awt.Color.black;
import static java.lang.Long.toHexString;
import static net.sf.json.JSONObject.fromObject;

import java.awt.Color;
import java.awt.Graphics;

import javax.swing.JFrame;
import javax.swing.JPanel;

import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

@SuppressWarnings("serial")
public class Client {

	private JFrame frame;
	private static final Integer MARGIN = 5;

	public void show(OtpErlangObject received, Long sequence) {
		OtpErlangTuple msg = (OtpErlangTuple) received;
		String data = msg.elementAt(1).toString();
		String json = data.substring(1, data.length() - 1);
		JSONObject root = fromObject(json);
		JSONArray lines = (JSONArray) root.get("lines");
		JSONArray labels = (JSONArray) root.get("labels");
		JPanel panel = new GraphPanel(lines, labels);
		Integer width = root.getInt("width");
		Integer height = root.getInt("height");
		String description = root.getString("description");
		show(panel, description + " (Message # " + sequence.toString() + ")", width, height);
	}

	private class GraphPanel extends JPanel{
		private final JSONArray lines;
		private final JSONArray labels;
		GraphPanel(JSONArray lines, JSONArray labels) {
			this.lines = lines;
			this.labels = labels;
		}
		public void paintComponent(Graphics g){
			paintLabels(g, 0);
			paintLines(g, 0);
		}
		private void paintLines(Graphics g, Integer i){
			if(i == lines.size())
				return;
			JSONObject line = lines.getJSONObject(i);
			JSONObject from = line.getJSONObject("from");
			JSONObject to = line.getJSONObject("to");
			Color color = line.has("color") ? 
					decode("0x" + toHexString(line.getLong("color"))) : black;
			g.setColor(color);
			g.drawLine(from.getInt("x") + MARGIN,
					   from.getInt("y") + MARGIN,
					   to.getInt("x") + MARGIN,
					   to.getInt("y") + MARGIN);
			paintLines(g, ++i);
		}
		private void paintLabels(Graphics g, Integer i){
			if(i == labels.size())
				return;
			JSONObject label = labels.getJSONObject(i);
			JSONObject pt = label.getJSONObject("pt");
			String text = label.getString("text");
			g.drawString(text,
						 pt.getInt("x") + MARGIN + 5,
						 pt.getInt("y") + MARGIN + 15);
			paintLabels(g, ++i);
		}
	}

	private void show(JPanel panel, String description, Integer width, Integer height) {
		if(frame != null)
			frame.dispose();
		frame = new JFrame(description);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		// add 30 to make up for the window bar
		frame.setSize(width + MARGIN + 5, height + MARGIN + 30);
		frame.add(panel);
		frame.setVisible(true);
		frame.setResizable(false);
		frame.setBackground(Color.WHITE);
	}

	public void show(final String error, final String message, Long sequence) {
		JPanel panel = new JPanel(){
			public void paintComponent(java.awt.Graphics g){
				g.drawString(message, getWidth() / 3, getHeight() / 2);
			}
		};
		show(panel, error + " : Message # " + sequence.toString(), 400, 100);
	}
}
