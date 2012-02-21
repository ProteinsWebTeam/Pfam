/*
 * Created on 2003-dec-04
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package PfamAlyzer;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import java.io.*;

/**
 * @author volker
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class DomainQueryResultPanel extends JPanel implements MouseListener, MouseMotionListener {
	
	public class DqResultDomainPopupMenu extends JPopupMenu implements ActionListener{
		
		public String id;
		Point loc = null;
		
		DqResultDomainPopupMenu(String s) {
			super();
			id = s;
			JMenuItem it;
			it = new JMenuItem("Short info");
			it.addActionListener(this);
			add(it);
			it = new JMenuItem("Domain viewer");
			it.addActionListener(this);
			add(it);
			if (frame.domainBrowserPrefix != null) {
				it = new JMenuItem("Browser");
				it.addActionListener(this);
				add(it);
			}
		}
		
		public void paint(Graphics g) {
			super.paint(g);
			if (loc == null)
				loc = getLocationOnScreen();
		}
		
		public void actionPerformed(ActionEvent e) {
			String command = e.getActionCommand();
			if (command=="Short info") {
				setCursor(new Cursor(Cursor.WAIT_CURSOR));
				PfamAlyzerFamilyInfo fi = new PfamAlyzerFamilyInfo(frame.applet.pfamServer, id, loc);
				setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			}
			else if (command=="Domain viewer") {
				frame.showBrowsePfam(id);
			}
			else if (command=="Browser") {
				TreeMap t = frame.domainsById;
				PfamADomain domain = (PfamADomain) t.get(id);
				if (domain != null)
					frame.applet.showURL(frame.domainBrowserPrefix+domain.pfamA_acc);
			}
		}
	}
	
	public class DqResultProteinPopupMenu extends JPopupMenu implements ActionListener{
		
		public PfamAlyzerProtein protein;
		Point loc = null;
		
		DqResultProteinPopupMenu(PfamAlyzerProtein p) {
			super();
			protein = p;
			JMenuItem it;
			it = new JMenuItem("Protein viewer");
			it.addActionListener(this);
			add(it);
			it = new JMenuItem("Find architecture");
			it.addActionListener(this);
			add(it);
			if (frame.proteinBrowserPrefix != null) {
				it = new JMenuItem("Browser");
				it.addActionListener(this);
				add(it);
			}
		}
		
		public void paint(Graphics g) {
			super.paint(g);
			if (loc == null)
				loc = getLocationOnScreen();
		}
		
		public void actionPerformed(ActionEvent e) {
			String command = e.getActionCommand();
			if (command=="Protein viewer") 
				frame.showBrowseSwissPfam(protein.acc);
			else if (command=="Find architecture") {
				dqp.proteinControl.newQuery(protein);
				dqp.repaint();
			}
			else if (command=="Browser") 
				frame.applet.showURL(frame.proteinBrowserPrefix+protein.acc);		
		}
	}
	
	private int height = 20;
	private Vector proteinList;
	private ServerCommunication server; 
	private DomainQueryPanel dqp;
	private PfamAlyzerFrame frame;
	private PfamAColours coloursA;
	private PfamBColours coloursB;
	private int prefWidth = 700;
	private int width;
	
	DomainQueryResultPanel(DomainQueryPanel p) {
		super();
		dqp = p;
		frame = dqp.frame;
		coloursA = dqp.coloursA;
		proteinList = new Vector();
		server = dqp.frame.applet.pfamServer;
		addMouseListener(this);
		addMouseMotionListener(this);
		width = prefWidth;
		setBackground(Color.WHITE);
	}
	
	public Dimension getPreferredSize() {
		return new Dimension(width, proteinList.size()*3*height+20);
	}
	
	public Vector getProteinList() {
		return proteinList;
	}
	
	public void mouseClicked(MouseEvent e) {
		setCursor(new Cursor(Cursor.WAIT_CURSOR));
		Point loc = e.getPoint();
		PfamAlyzerProtein p;
		for (int i=0; i<proteinList.size(); i++) {
			p = (PfamAlyzerProtein) proteinList.get(i);
			if (loc.y > p.posY-p.height && loc.y < p.posY+p.height) {
				for (int k=0; k<p.domains.size(); k++) {
					PfamAlyzerDomain d = (PfamAlyzerDomain) p.domains.get(k);
					if (loc.x > d.startX && loc.x < d.stopX) {
						if (d.type==PfamAlyzerDomain.PFAMA)
							(new DqResultDomainPopupMenu(d.id)).show(this, loc.x, loc.y);
					}
				}
			}
			else if (loc.y > p.posYname-p.height && loc.y < p.posYname+p.height) 
				(new DqResultProteinPopupMenu(p)).show(this, loc.x, loc.y);
		}
		setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
	}

	public void mouseEntered(MouseEvent e) {}
	
	public void mouseExited(MouseEvent e) {}	
	
	public void mousePressed(MouseEvent e) {}
	
	public void mouseReleased(MouseEvent e) {}	
	
	public void mouseDragged(MouseEvent e) {}
	
	public void mouseMoved(MouseEvent e) {
		Point loc = e.getPoint();
		PfamAlyzerProtein p;
		for (int i=0; i<proteinList.size(); i++) {
			p = (PfamAlyzerProtein) proteinList.get(i);
			if (loc.y > p.posY-p.height && loc.y < p.posY+p.height) {
				for (int k=0; k<p.domains.size(); k++) {
					PfamAlyzerDomain d = (PfamAlyzerDomain) p.domains.get(k);
					if (loc.x > d.startX && loc.x < d.stopX) {
						if (d.type==PfamAlyzerDomain.PFAMA)  {
							setCursor(new Cursor(Cursor.HAND_CURSOR));
							return;
						}								
					}
				}
			}
			else if (loc.y > p.posYname-p.height && loc.y < p.posYname+p.height) {
				setCursor(new Cursor(Cursor.HAND_CURSOR));
				return;
			}								
		}
		setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
	}	
	
	public void rescale(int width) {
		int y = 50;
		int max = 0;
		PfamAlyzerProtein p;
		for (int i=0; i<proteinList.size(); i++) {
			p = (PfamAlyzerProtein) proteinList.get(i);
			max = max<p.length?p.length:max;
		}
		double scale = (new Double(max)).doubleValue() / (new Double(width)).doubleValue();
		for (int i=0; i<proteinList.size(); i++) {
			p = (PfamAlyzerProtein) proteinList.get(i);
			p.recalculate(10, y, scale, height);
			y += 3*height;
		}
	}
	
	public void paintComponent(Graphics g) {
		super.paintComponent(g);
		width = prefWidth;
		Graphics2D g2 = (Graphics2D) g;
		g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		PfamAlyzerProtein p;
		for (int i=0; i<proteinList.size(); i++) {
			p = (PfamAlyzerProtein) proteinList.get(i);
			int w = p.paintComponent(g2, frame.viewProteinDescription, frame.viewShadow);
			width = width<w?w:width;
		}
	}
	
	public void newList(Vector list) {
		coloursB = new PfamBColours();
		proteinList = new Vector();
		try {
			String query = "output=pfamalyzer&acc=";
			for (int i=0; i<list.size(); i++) {
				query += (String) list.get(i) + ","; 
			}
			query = query.substring(0,query.length()-1);
			BufferedReader buf = server.doRequest("protein", query);
			String line = null;
			
			PfamAlyzerProtein p = null;
			while ((line = buf.readLine()) != null) {
			  
				StringTokenizer st = new StringTokenizer(line, "	");
				if (st.hasMoreTokens()) {
					String type = st.nextToken();
					if (type.equals("P")) {
            /* System.err.println( "type = P" ); */
						p = new PfamAlyzerProtein();
						proteinList.add(p);
						p.domains = new Vector();
						p.acc = st.nextToken();
						p.id = st.nextToken();
						p.description = st.nextToken();
						p.length = Integer.parseInt(st.nextToken());
						p.species = st.nextToken();
						p.taxonomy = st.nextToken();
						p.condenseTaxonomy();
						if (st.nextToken().equals("0"))
							p.is_fragment = false;
						else 
							p.is_fragment = true;
					}
					else if (type.equals("A")) {
            /* System.err.println( "type = A" ); */
						PfamAlyzerDomain domain = new PfamAlyzerDomain();
						domain.id = st.nextToken();
						domain.acc = st.nextToken();
						domain.seq_start = Integer.parseInt(st.nextToken());
						domain.seq_end = Integer.parseInt(st.nextToken());
						domain.type = PfamAlyzerDomain.PFAMA;
						domain.colour = coloursA.getColour(domain.id, 1);
						int k=0;
						while (k<p.domains.size() && ((PfamAlyzerDomain) p.domains.get(k)).seq_start<domain.seq_start)
							k++;
						p.domains.add(k, domain);
					}
					else if (type.equals("B")) {
            /* System.err.println( "type = B" ); */
						PfamAlyzerDomain domain = new PfamAlyzerDomain();
						domain.id = st.nextToken();
						domain.acc = st.nextToken();
						domain.seq_start = Integer.parseInt(st.nextToken());
						domain.seq_end = Integer.parseInt(st.nextToken());
						domain.type = PfamAlyzerDomain.PFAMB;
						domain.colour = coloursB.getColour(domain.id, 1);
						int k=0;
						while (k<p.domains.size() && ((PfamAlyzerDomain) p.domains.get(k)).seq_start<domain.seq_start)
							k++;
						p.domains.add(k, domain);
					}
				}
			}
		} catch (Exception ex) {
      PfamAlyzerExceptionDialog ed = new PfamAlyzerExceptionDialog("DomainQueryResultPanel.newList "+ex.toString());
      ex.printStackTrace();
    }	
		for (int i=0; i<proteinList.size(); i++) {
			PfamAlyzerProtein pr = (PfamAlyzerProtein) proteinList.get(i);
			pr.insertDistances();
		}
		rescale(prefWidth);
		coloursA.freeTemporary();
	}

}
