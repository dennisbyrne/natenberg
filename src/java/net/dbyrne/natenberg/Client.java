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

import static java.awt.Color.black;
import static java.awt.Color.decode;
import static java.lang.Long.toHexString;
import static org.json.simple.JSONValue.parse;

import java.applet.Applet;
import java.awt.Color;
import java.awt.Graphics;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

/**
 * Graphs a JSON message via Swing.
 * @author Dennis Byrne
 */
@SuppressWarnings("serial")
public class Client extends Applet {
	private static final Integer MARGIN = 5;
	private JSONArray lines = new JSONArray();
	private JSONArray labels = new JSONArray();
	
    public void init(){}

    public void stop(){}

    public void paint(Graphics g){
    	paintLabels(g);
    	paintLines(g);
    }

    public void draw(String state){ /* called by JavaScript */
    	JSONObject root = (JSONObject) parse(state);
		lines = (JSONArray) root.get("lines");
		labels = (JSONArray) root.get("labels");
		repaint(); repaint(); repaint();
    }

	private void paintLines(Graphics g){
		for(int i = 0; i < this.lines.size(); i++){
			JSONObject line = (JSONObject) lines.get(i);
			JSONObject from = (JSONObject) line.get("from");
			JSONObject to = (JSONObject) line.get("to");
			Color color = line.containsKey("color") ?
				decode("0x" + toHexString(((Long)line.get("color")))) : black;
			g.setColor(color);
			g.drawLine(((Long)from.get("x")).intValue() + MARGIN,
					((Long)from.get("y")).intValue() + MARGIN,
					((Long)to.get("x")).intValue() + MARGIN, 
					((Long)to.get("y")).intValue() + MARGIN);
		}
	}

	private void paintLabels(Graphics g){
		for(int i = 0; i < labels.size(); i++){
			JSONObject label = (JSONObject) labels.get(i);
			JSONObject pt = (JSONObject) label.get("pt");
			String text = label.get("text").toString();
			g.drawString(text, ((Long)pt.get("x")).intValue() + MARGIN + 5, 
					((Long)pt.get("y")).intValue() + MARGIN + 15);
		}
	}

}
