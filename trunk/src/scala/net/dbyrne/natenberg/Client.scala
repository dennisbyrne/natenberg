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
package net.dbyrne.natenberg

import java.awt.Color._
import java.lang.Long._
import org.json.simple.JSONValue._
import org.json.simple.JSONObject._

import java.applet.Applet
import java.awt.Color
import java.awt.Graphics

import org.json.simple.JSONArray
import org.json.simple.JSONObject

/**
 * Graphs a JSON message via Swing.
 * @author Dennis Byrne
 */
class Client extends Applet {
  	private final val MARGIN = 5
	private final var lines = new JSONArray
	private final var labels = new JSONArray

	override def init = {}

    override def stop = {}
    
    override def paint(g:Graphics) = {
		paintLabels(g)
    	paintLines(g)
    }
    
    def draw(state:String){ /* called by JavaScript */
    	val root = parse(state).asInstanceOf[JSONObject]
		lines = root.get("lines").asInstanceOf[JSONArray]
		labels = root.get("labels").asInstanceOf[JSONArray]
		repaint; repaint; repaint;
    }

    private def paintLines(g:Graphics){
    	0.until(lines.size - 1).foreach(i => {
    		val line:JSONObject = lines.get(i).asInstanceOf[JSONObject]
    		val from = line.get("from").asInstanceOf[JSONObject]
    		val to = line.get("to").asInstanceOf[JSONObject]
    		val color:Color = if(line.containsKey("color"))
    				Color.decode("0x" + toHexString((line.get("color").asInstanceOf[Long]))) else black
    		g.setColor(color)
    		g.drawLine((from.get("x").asInstanceOf[Long]).intValue + MARGIN,
				(from.get("y").asInstanceOf[Long]).intValue + MARGIN,
				(to.get("x").asInstanceOf[Long]).intValue + MARGIN, 
				(to.get("y").asInstanceOf[Long]).intValue + MARGIN)      
    	})
	}

    private def paintLabels(g:Graphics){
    	0.until(labels.size - 1).foreach(i => {
			val label = labels.get(i).asInstanceOf[JSONObject]
			val pt = label.get("pt").asInstanceOf[JSONObject]
			val text = label.get("text").toString
			g.drawString(text, (pt.get("x").asInstanceOf[Long]).intValue + MARGIN + 5, 
					(pt.get("y").asInstanceOf[Long]).intValue + MARGIN + 15)
    	})
	}
}
