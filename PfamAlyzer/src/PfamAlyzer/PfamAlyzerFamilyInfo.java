package PfamAlyzer;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.io.*;

public class PfamAlyzerFamilyInfo extends JFrame {
	
	public class PfamAlyzerInfoDialogEventHandler extends WindowAdapter {
		
		public PfamAlyzerInfoDialogEventHandler() {
			super();
		}
		
		public void windowClosing(WindowEvent e) {
			
			PfamAlyzerFamilyInfo od = (PfamAlyzerFamilyInfo)e.getSource();
			od.setVisible(false);
			od.dispose();
		}		
	}
	
	public class PfamAlyzerInfoPanel extends JPanel {
		
		PfamAlyzerFamilyInfo info;
		
		PfamAlyzerInfoPanel(PfamAlyzerFamilyInfo i) {
			setBackground(Color.WHITE);
			info = i;
		}
		
		public void paint(Graphics g) {
			super.paint(g);
			
			Dimension dim = getSize();
			Font f;
			FontMetrics fm;

			int y = startY+h;
			f = new Font("Times Roman", Font.ITALIC, 12);
			g.setFont(f);
			fm = g.getFontMetrics();
			x2 = x1 + 20 + fm.stringWidth("Accession number:");
			g.drawString("Accession number:", x1, y);
			f = new Font("Times Roman", Font.PLAIN, 12);
			g.setFont(f);
			g.drawString(info.acc, x2, y);
			y += h;
			f = new Font("Times Roman", Font.ITALIC, 12);
			g.setFont(f);
			g.drawString("Identifier:", x1, y);
			f = new Font("Times Roman", Font.PLAIN, 12);
			g.setFont(f);
			g.drawString(info.id, x2, y);
			y += h;
			f = new Font("Times Roman", Font.ITALIC, 12);
			g.setFont(f);
			g.drawString("Description:", x1, y);
			f = new Font("Times Roman", Font.PLAIN, 12);
			g.setFont(f);
			y = paintText(g, info.description, x2, dim.width-20, y, h);
			if (comment!=null) {
				f = new Font("Times Roman", Font.ITALIC, 12);
				g.setFont(f);
				g.drawString("Comment:", x1, y);
				f = new Font("Times Roman", Font.PLAIN, 12);
				g.setFont(f);
				y = paintText(g, info.comment, x2, dim.width-20, y, h);
			}
			if (firstPaint) {
				setSize(300,y+20);
				info.setSize(300,y+20);
				firstPaint = false;
			}
		}
	}
	
	public ServerCommunication server;
	public JFrame frame;
	
	public String id;
	public String acc;
	public String author;
	public String type;
	public String num_seed;
	public String num_full;
	public String description;
	public String comment;
	public int x1 = 5;
	public int x2;
	public int startY = 5;
	public int h = 20;
	public boolean firstPaint = true;
	
	public PfamAlyzerFamilyInfo(ServerCommunication s, String str, Point loc) {
		super();
		setTitle("Family info");
		
		server = s;
		id = str;
		Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
		setSize(300, 500);
		setLocation(loc);
		loadDescription(id);
		setBackground(Color.WHITE);
		PfamAlyzerInfoPanel ip = new PfamAlyzerInfoPanel(this);
		getContentPane().add(ip);
		
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
				while  (i>0 && str.charAt(i)!=' ')
					i--;
				if (i==0) {
					g.drawString("...", x1, y);
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
		}
		return  y;
	}
	
	/*public void update(Graphics g) {
		super.update(g);
		
		Dimension dim = getSize();
		Font f;
		FontMetrics fm;

		int y = startY+2*h;
		f = new Font("Times Roman", Font.ITALIC, 12);
		g.setFont(f);
		fm = g.getFontMetrics();
		x2 = x1 + 20 + fm.stringWidth("Accession number:");
		g.drawString("Accession number:", x1, y);
		f = new Font("Times Roman", Font.PLAIN, 12);
		g.setFont(f);
		g.drawString(acc, x2, y);
		y += h;
		f = new Font("Times Roman", Font.ITALIC, 12);
		g.setFont(f);
		g.drawString("Identifier:", x1, y);
		f = new Font("Times Roman", Font.PLAIN, 12);
		g.setFont(f);
		g.drawString(id, x2, y);
		y += h;
		f = new Font("Times Roman", Font.ITALIC, 12);
		g.setFont(f);
		g.drawString("Description:", x1, y);
		f = new Font("Times Roman", Font.PLAIN, 12);
		g.setFont(f);
		y = paintText(g, description, x2, dim.width-20, y, h);
		if (comment!=null) {
			f = new Font("Times Roman", Font.ITALIC, 12);
			g.setFont(f);
			g.drawString("Comment:", x1, y);
			f = new Font("Times Roman", Font.PLAIN, 12);
			g.setFont(f);
			y = paintText(g, comment, x2, dim.width-20, y, h);
		}
	}
	*/
	/* public void paint(Graphics g) {
		super.paint(g);
		
		Dimension dim = getSize();
		Font f;
		FontMetrics fm;

		int y = startY+2*h;
		f = new Font("Times Roman", Font.ITALIC, 12);
		g.setFont(f);
		fm = g.getFontMetrics();
		x2 = x1 + 20 + fm.stringWidth("Accession number:");
		g.drawString("Accession number:", x1, y);
		f = new Font("Times Roman", Font.PLAIN, 12);
		g.setFont(f);
		g.drawString(acc, x2, y);
		y += h;
		f = new Font("Times Roman", Font.ITALIC, 12);
		g.setFont(f);
		g.drawString("Identifier:", x1, y);
		f = new Font("Times Roman", Font.PLAIN, 12);
		g.setFont(f);
		g.drawString(id, x2, y);
		y += h;
		f = new Font("Times Roman", Font.ITALIC, 12);
		g.setFont(f);
		g.drawString("Description:", x1, y);
		f = new Font("Times Roman", Font.PLAIN, 12);
		g.setFont(f);
		y = paintText(g, description, x2, dim.width-20, y, h);
		if (comment!=null) {
			f = new Font("Times Roman", Font.ITALIC, 12);
			g.setFont(f);
			g.drawString("Comment:", x1, y);
			f = new Font("Times Roman", Font.PLAIN, 12);
			g.setFont(f);
			y = paintText(g, comment, x2, dim.width-20, y, h);
		}
		if (firstPaint) {
			setSize(300,y+20);
			firstPaint = false;
		}
	} */
	
	private void loadDescription(String id) {
		try {
			BufferedReader buf = server.doRequest("family/"+id+"/desc", "output=pfamalyzer");	
			StringTokenizer st = new StringTokenizer(buf.readLine(), "\t");
			acc = st.nextToken();
			author = st.nextToken();
			type = st.nextToken();
			num_seed = st.nextToken();
			num_full = st.nextToken();
			if (st.hasMoreTokens()) 
				description = st.nextToken();
			if (st.hasMoreTokens()) 
				comment = st.nextToken();
		} catch (Exception e) {
      PfamAlyzerExceptionDialog ed = new PfamAlyzerExceptionDialog(frame, e.toString());
      e.printStackTrace();
    }
	}

}
