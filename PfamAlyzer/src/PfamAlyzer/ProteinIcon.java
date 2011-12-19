/*
 * Created on 9-jan-2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package PfamAlyzer;

import java.util.*;
import javax.swing.*;
import java.awt.*;

public class ProteinIcon implements Icon {	
	private final int height = 60;
	private int width;
	private PfamAlyzerProtein protein;
	private Vector domains;
	
	ProteinIcon(PfamAlyzerProtein p) {
		protein = p;
		domains = p.domains;
		width = 1000;
	}
	
	public void paintIcon(Component c, Graphics g, int x, int y) {
		Graphics2D g2 = (Graphics2D) g;
		String str = protein.id+ " ["+ protein.species + "]";
		g.setColor(Color.BLACK);
		g.drawString(str, x, y+20);
		for (int i=0; i<domains.size(); i++) 
			((PfamAlyzerDomain) domains.get(i)).paintSmall(g2, y+35, 15);	
	}
	
	public int getIconWidth() {
		return width;
	}
	
	public int getIconHeight() {
		return height;
	}
}
