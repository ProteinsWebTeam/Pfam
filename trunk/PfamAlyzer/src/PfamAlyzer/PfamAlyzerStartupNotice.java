package PfamAlyzer;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

/**
 * Insert the type's description here.
 * Creation date: (2001-06-27 19.39.20)
 * @author: 
 */
public class PfamAlyzerStartupNotice extends JDialog  {
	
	public class PfamAlyzerStartupNoticeEventHandler extends WindowAdapter {
		
		public PfamAlyzerStartupNoticeEventHandler() {
			super();
		}
		
		public void windowClosing(WindowEvent e) {
			
			PfamAlyzerStartupNotice od = (PfamAlyzerStartupNotice)e.getSource();
			od.setVisible(false);
			od.dispose();
		}		
	}
	
	public String startupText;
	
	public PfamAlyzerStartupNotice(JFrame ow, String text) {
		super(ow, "PfamAlyzer", true);
		
		Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
		setSize(300, 250);
		setLocation(dim.width/2, dim.height/2-200);
		addWindowListener(new PfamAlyzerStartupNoticeEventHandler());
		//setLayout(null);
		startupText = text;
		
		show();
	}
	
	public int paintText(Graphics g, String s, int x1, int x2, int y, int height) {
		String str = s;
		if (s.length()==0)
			return y + height;
		int i;
		FontMetrics fm = g.getFontMetrics();
		if (str.charAt(0)==' ')
			str = str.substring(1);
		while (str.length()>0) {
			i = str.length()-1;
			while (fm.stringWidth(str.substring(0,i))>x2-x1 && i>0)
				i--;
			if (i==str.length()-1) {
				g.drawString(str, x1, y);
				str = "";
				y += height;
			}
			else {
				g.drawString(str.substring(0,i), x1, y);
				str = str.substring(i);
				if (str.charAt(0)==' ')
					str = str.substring(1);
				y += height;
			}
		}
		return  y;
	}
	
	public void paint (Graphics g) {
		super.paint(g);
		
		Dimension dim = getSize();
		Font f;
		FontMetrics fm;
		
		int h = 20;
		int y = 60;
		f = new Font("Times Roman", Font.BOLD, 14);
		g.setFont(f);
		g.drawString("PfamAlyzer", dim.width/2-50, y);
		f = new Font("Times Roman", Font.PLAIN, 12);
		y = 80;
		y = paintText(g, startupText, 10, dim.width-10, y, h);
		
	}
	
}
