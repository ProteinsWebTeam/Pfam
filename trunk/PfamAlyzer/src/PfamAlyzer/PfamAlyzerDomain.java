/*
 * Created on 2003-dec-04
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package PfamAlyzer;

import java.awt.*;

/**
 * @author volker
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class PfamAlyzerDomain  {
	static final int DISTANCE = 1;
	static final int PFAMA = 2;
	static final int PFAMB = 3;
	
	public int type;
	public String id;
	public String acc;
	public int seq_start;
	public int seq_end;
	public int colour;
	
	public int startX;
	public int stopX;
	public int leftOverlap = 0;
	
	public void paintSmall(Graphics2D g, int posY, int scaleY) {
		if (type==PFAMA) {
			int height = scaleY;
			int startY = posY-scaleY/2;
			Color c = PfamAColours.getColour(colour).darker();
			GradientPaint gradient1 = new GradientPaint(startX,startY,c,startX, startY+height/2,PfamAColours.getColour(colour));
			GradientPaint gradient2 = new GradientPaint(startX,startY+height/2,PfamAColours.getColour(colour),startX, startY+height,c);
			g.setPaint(gradient1);
			int xcor[] = {startX+5, stopX-5, stopX, stopX, startX, startX};
			int ycor[] = {startY, startY, startY+5, startY+height/2, startY+height/2, startY+5};
			Polygon p = new Polygon(xcor, ycor, 6);
			g.fill(p);
			g.setPaint(gradient2);
			int xcor1[] = {startX, stopX, stopX, stopX-5, startX+5, startX};
			int ycor1 [] = {startY+height/2, startY+height/2, startY+height-5, startY+height, startY+height, startY+height-5};
			p = new Polygon(xcor1, ycor1, 6);
			g.fill(p);	
			g.setColor(Color.BLACK);
			int xcor2[] = {startX+5, stopX-5, stopX, stopX, stopX-5, startX+5, startX, startX};
			int ycor2 [] = {startY, startY, startY+5, startY+height-5, startY+height, startY+height, startY+height-5, startY+5};
			g.drawPolygon(xcor2, ycor2, 8);
		}
		else if (type==DISTANCE) {
			int height = scaleY/3;
			int startY = posY-scaleY/6;
			g.setColor(Color.GRAY);
			g.fillRect(startX, posY-2, stopX-startX+1, 3);
			g.setColor(Color.DARK_GRAY);
			g.fillRect(startX, posY+1, stopX-startX+1, 3);
		}
		else {
			int height = scaleY;
			int startY = posY-height/2;
			g.setPaint(PfamBColours.getColour(colour));
			g.fill(new Rectangle(startX, startY, stopX-startX+1, height));
			g.setColor(Color.black);
			g.drawRect(startX, startY,stopX-startX+1, height);
		}	
		if (leftOverlap != 0) {
			g.setColor(Color.RED);
			g.fillRect(startX, posY+scaleY, leftOverlap-startX, 2);
			g.setColor(Color.BLACK);
		}
	}
		
	public void paintComponent(Graphics2D g, int posY, int scaleY, boolean shadow) {
		if (type==PFAMA) {
			int height = scaleY;
			int startY = posY-scaleY/2;
			//g.clearRect(startX, startY, stopX-startX+1, height);
			if (shadow) {
				g.setColor(Color.LIGHT_GRAY);
				int xcor0[] = {startX+11, stopX+1, stopX+6, stopX+6, stopX+1, startX+11, startX+6, startX+6};
				int ycor0 [] = {startY+4, startY+4, startY+9, startY+height-1, startY+height+4, startY+height+4, startY+height-1, startY+9};
				g.fillPolygon(xcor0, ycor0, 8);
			}
			Color c = PfamAColours.getColour(colour).darker();
			GradientPaint gradient1 = new GradientPaint(startX,startY,c,startX, startY+height/2,PfamAColours.getColour(colour));
			GradientPaint gradient2 = new GradientPaint(startX,startY+height/2,PfamAColours.getColour(colour),startX, startY+height,c);
			g.setPaint(gradient1);
			int xcor[] = {startX+5, stopX-5, stopX, stopX, startX, startX};
			int ycor[] = {startY, startY, startY+5, startY+height/2, startY+height/2, startY+5};
			Polygon p = new Polygon(xcor, ycor, 6);
			g.fill(p);
			g.setPaint(gradient2);
			int xcor1[] = {startX, stopX, stopX, stopX-5, startX+5, startX};
			int ycor1 [] = {startY+height/2, startY+height/2, startY+height-5, startY+height, startY+height, startY+height-5};
			p = new Polygon(xcor1, ycor1, 6);
			g.fill(p);	
			g.setColor(Color.BLACK);
			int xcor2[] = {startX+5, stopX-5, stopX, stopX, stopX-5, startX+5, startX, startX};
			int ycor2 [] = {startY, startY, startY+5, startY+height-5, startY+height, startY+height, startY+height-5, startY+5};
			g.drawPolygon(xcor2, ycor2, 8);
			FontMetrics fm = g.getFontMetrics();
			int widths[] = fm.getWidths();
			int strlen = 0;
			String str = "";
			/*int i = 0;
			while (strlen<stopX-startX-5 && i<id.length()) {
				str += id.charAt(i);
				strlen += widths[(int) id.charAt(i)];
				i++;
			}*/
			str = id;
			while (fm.stringWidth(str)>stopX-startX-5 && str.length()>0)
				str = str.substring(0, str.length()-1);
			if (id.length() != str.length()) {
				if (str.length() == 1)
					str = "»";
				else if (str.length()>1)
					str = str.substring(0, str.length()-1) + "»";
			}
			g.drawString(str, startX+5, posY+6);
		}
		else if (type==DISTANCE) {
			int height = scaleY/3;
			int startY = posY-scaleY/6;
			if (shadow) {
				g.setColor(Color.LIGHT_GRAY);
				g.fillRect(startX+6, posY+2, stopX-startX+1
						, 6);
			}
			g.setColor(Color.GRAY);
			g.fillRect(startX, posY-2, stopX-startX+1, 3);
			g.setColor(Color.DARK_GRAY);
			g.fillRect(startX, posY+1, stopX-startX+1, 3);
		}
		else {
			int height = scaleY;
			int startY = posY-height/2;
			if (shadow) {
				g.setColor(Color.LIGHT_GRAY);
				g.fillRect(startX+6, startY+4, stopX-startX+1, height);
			}
			g.setPaint(PfamBColours.getColour(colour));
			g.fill(new Rectangle(startX, startY, stopX-startX+1, height));
			g.setColor(Color.black);
			g.drawRect(startX, startY,stopX-startX+1, height);
			FontMetrics fm = g.getFontMetrics();
			int widths[] = fm.getWidths();
			int strlen = 0;
			String str = "";
			/*int i = 0;
			while (strlen<stopX-startX-5 && i<id.length()) {
				str += id.charAt(i);
				strlen += widths[(int) id.charAt(i)];
				i++;
			}*/
			str = id;
			while (fm.stringWidth(str)>stopX-startX-5 && str.length()>0)
				str = str.substring(0, str.length()-1);
			if (id.length() != str.length()) {
				if (str.length() == 1)
					str = "»";
				else if (str.length()>1)
					str = str.substring(0, str.length()-1) + "»";
			}
			g.drawString(str, startX+5, posY+6);
		}	
		if (leftOverlap != 0) {
			g.setColor(Color.RED);
			g.fillRect(startX, posY+scaleY, leftOverlap-startX, 2);
			g.setColor(Color.BLACK);
		}
	}
}
