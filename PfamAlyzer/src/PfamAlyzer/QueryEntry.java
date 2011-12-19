/*
 * Created on 1-apr-2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package PfamAlyzer;

import javax.swing.*;
import java.awt.*;


/**
 * @author volker
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class QueryEntry extends JComponent {
	static final int GIVENDOMAIN = 1;
	static final int DISTANCE = 2;
	static final int CLAN = 3;
	static final int DOMAINS = 1;
	static final int AA = 2;
		
	int type;
	String id;
	boolean notAcc;
	int min;
	int max;
	int distanceType;
	int wNo;
	int colour;
	
	int startX;
	int stopX;
	
	public void paintComponent(Graphics2D g, int posY, boolean shadow) {
		if (type==GIVENDOMAIN || type==CLAN) {		
			int height = 25;
			int startY = posY-12;
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
			int i = 0;
			while (strlen<stopX-startX-6 && i<id.length()) {
				str += id.charAt(i);
				strlen += widths[(int) id.charAt(i)];
				i++;
			}
			if (type==CLAN)
				str = "Clan "+id;
			else
				str = id;
			while (fm.stringWidth(str)>stopX-startX-6)
				str = str.substring(0, str.length()-2);
			g.drawString(str, startX+5, posY+6);
			
			if (notAcc) {
				g.setColor(Color.red);
				g.drawLine(startX, posY-12, stopX, posY+12);
				g.drawLine(stopX, posY-12, startX, posY+12);
			}
		}
		else if (type==DISTANCE) {
			if (shadow) {
				g.setColor(Color.LIGHT_GRAY);
				g.fillRect(startX+5, posY+2,stopX-startX+1, 6);
			}
			g.setColor(Color.GRAY);
			g.fillRect(startX, posY-2, stopX-startX+1, 3);
			g.setColor(Color.DARK_GRAY);
			g.fillRect(startX, posY+1, stopX-startX+1, 3);
			
			g.setColor(Color.BLACK);
			FontMetrics fm = g.getFontMetrics();
			if (min==-1)
				g.drawString("*", startX+3, posY-5);
			else
				g.drawString((new Long(min)).toString(), startX+3, posY-5);
			if (max==-1)
				g.drawString("*", stopX-10, posY-5);
			else {
				String str = (new Long(max)).toString();				
				g.drawString(str, stopX-3-fm.stringWidth(str), posY-5);
			}
		}
		else {
			colour = 2;
			
			int height = 25;
			int startY = posY-12;
			if (shadow) {
				g.setColor(Color.LIGHT_GRAY);
				int xcor0[] = {startX+6, stopX+6, stopX+6, startX+6};
				int ycor0 [] = {startY+4, startY+4, startY+height+4, startY+height+4};
				g.fillPolygon(xcor0, ycor0, 4);
			}				
			g.setColor(Color.yellow);
			int xcor1[] = {startX, stopX, stopX, startX};
			int ycor1 [] = {startY, startY, startY+height, startY+height};
			g.fillPolygon(xcor1, ycor1, 4);
			g.fillRect(startX, posY-12, stopX-startX, 25);
			g.setColor(Color.black);
			g.drawPolygon(xcor1, ycor1, 4);
			
			FontMetrics fm = g.getFontMetrics();
			int widths[] = fm.getWidths();
			int strlen = 0;
			String str = "";
			int i = 0;
			while (strlen<stopX-startX-6 && i<id.length()) {
				str += id.charAt(i);
				strlen += widths[(int) id.charAt(i)];
				i++;
			}
			str = id;
			while (fm.stringWidth(str)>stopX-startX-6)
				str = str.substring(0, str.length()-2);
			g.drawString(str, startX+5, posY+6);
			
			if (notAcc) {
				g.setColor(Color.red);
				g.drawLine(startX, posY-12, stopX, posY+12);
				g.drawLine(stopX, posY-12, startX, posY+12);
			}
		}	
	}
}
