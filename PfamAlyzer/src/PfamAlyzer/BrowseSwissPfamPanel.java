/*
 * Created on 2003-dec-10
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package PfamAlyzer;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.io.*;
/**
 * @author volker
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class BrowseSwissPfamPanel extends JPanel implements ActionListener {

	public class ProteinPanel extends JPanel implements MouseListener, MouseMotionListener {
		
		public boolean firstpaint = true;
		public Dimension dimension;
		private int height = 20;
		private PfamAlyzerProtein protein;
		private ServerCommunication server; 
		private PfamAlyzerFrame frame;
		private PfamAColours coloursA;
		private PfamBColours coloursB;
		private int prefWidth = 700;
		private int width;
		private int startY = 50;
		private int x0 = 20;
		private int x1 = 70;
		private int x2;
		
		ProteinPanel(PfamAlyzerFrame f, PfamAColours a, PfamBColours b) {
			super();
			dimension = new Dimension (prefWidth, 600);
			frame = f;
			coloursA = a;
			coloursB = b;
			protein = null;
			server = frame.applet.pfamServer;
			addMouseListener(this);
			addMouseMotionListener(this);
			width = prefWidth;
			setBackground(Color.WHITE);
		}
		
		public Dimension getMiminimSize() {
			return dimension;
		}	
		
		public Dimension getPreferredSize() {
			return dimension;
		}
		
		public void mouseDragged(MouseEvent e) {}
		
		public void mouseMoved(MouseEvent e) {
			if (protein == null)
				return;
			Point loc = e.getPoint();
			if (loc.y > protein.posY-protein.height && loc.y < protein.posY+protein.height) {
				for (int k=0; k<protein.domains.size(); k++) {
					PfamAlyzerDomain d = (PfamAlyzerDomain) protein.domains.get(k);
					if (loc.x > d.startX && loc.x < d.stopX) {
						if (d.type==PfamAlyzerDomain.PFAMA)  {
							setCursor(new Cursor(Cursor.HAND_CURSOR));
							return;
						}
					}
				}
			}
			else if (loc.y > protein.posYname-protein.height && loc.y < protein.posYname+protein.height) {
				frame.setCursor(new Cursor(Cursor.HAND_CURSOR));
				return;
			}	
			setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		}	
	
		public void mouseClicked(MouseEvent e) {
			if (protein == null)
				return;
			frame.setCursor(new Cursor(Cursor.WAIT_CURSOR));
			Point loc = e.getPoint();
			if (loc.y > protein.posY-protein.height && loc.y < protein.posY+protein.height) {
				for (int k=0; k<protein.domains.size(); k++) {
					PfamAlyzerDomain d = (PfamAlyzerDomain) protein.domains.get(k);
					if (loc.x > d.startX && loc.x < d.stopX) {
						if (d.type==PfamAlyzerDomain.PFAMA) {
							Point loc1 = getLocationOnScreen();
							loc1.x += e.getX();
							loc1.y += e.getY();
							new PfamAlyzerFamilyInfo(server, d.id, loc1);	
						}
					}
				}
			}
			else if (loc.y > protein.posYname-protein.height && loc.y < protein.posYname+protein.height) 
				frame.applet.showURL(frame.proteinBrowserPrefix+protein.acc);
			frame.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		}
		
		public void mouseEntered(MouseEvent e) {}
		
		public void mouseExited(MouseEvent e) {}	
		
		public void mousePressed(MouseEvent e) {}
		
		public void mouseReleased(MouseEvent e) {}	
		
		public void rescale(int width) {
			int y = 110;
			double scale = (new Double(protein.length+x0)).doubleValue() / (new Double(width)).doubleValue();
			protein.recalculate(x0,y,scale,height);
		}
		
		public void paintComponent(Graphics g) {
			super.paintComponent(g);		
			Graphics2D g2 = (Graphics2D) g;
			g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
			
			if (protein!=null) {
				Dimension dim = getSize();
				Font f = new Font("Times Roman", Font.BOLD, 20);
				Font f1 = new Font("Times Roman", Font.ITALIC, 13);
				Font f2 = new Font("Times Roman", Font.PLAIN, 13);
				Font f3 = new Font("Times Roman", Font.BOLD, 13);
				Font f4 = new Font("Times Roman", Font.PLAIN, 12);
				g2.setFont(f);
				FontMetrics fm = g2.getFontMetrics();
				String str = "SWISSPROT entry: "+protein.acc;
				int titlelength = fm.stringWidth(str);
				g2.drawString(str, Math.max(0,dim.width/2-titlelength/2), startY);
				
				g2.setFont(f4);
				width = prefWidth;
				int w = protein.paintComponent(g2, false, frame.viewShadow);
				width = width<w?w:width;

				int y = 170;
				g2.setFont(f3);
				g2.drawString("General information:", x0, y);
				y += height;
				g2.setFont(f1);
				fm = g2.getFontMetrics();
				x2 = x1 + 20 + fm.stringWidth("Accession number:");
				int h = fm.getHeight();
				g2.drawString("Accession number:", x1, y);
				g2.setFont(f2);
				g2.drawString(protein.acc, x2, y);
				y += height;
				g2.setFont(f1);
				g2.drawString("Identifier:", x1, y);
				g2.setFont(f2);
				g2.drawString(protein.id, x2, y);
				y += height;
				g2.setFont(f1);
				g2.drawString("Species:", x1, y);
				g2.setFont(f2);
				g2.drawString(protein.species, x2, y);
				y += height;
				g2.setFont(f1);
				g2.drawString("Taxonomy:", x1, y);
				g2.setFont(f2);
				if (frame.viewCondTaxonomy)
					y = paintText(g2, protein.condensed_taxonomy, x2, dim.width-20, y, h);
				else
					y = paintText(g2, protein.taxonomy, x2, dim.width-20, y, h);
				g2.setFont(f1);
				g2.drawString("Length:", x1, y);
				g2.setFont(f2);
				g2.drawString((new Integer(protein.length)).toString(), x2, y);
				y += height;
				g2.setFont(f1);
				g2.drawString("Description:", x1, y);
				g2.setFont(f2);
				y = paintText(g2, protein.description, x2, dim.width-20, y, h);			
				
				g2.setFont(f3);
				g2.drawString("Architecture:", x0, y);
				y += height;
				g2.setFont(f1);
				PfamAlyzerDomain d;
				String s;
				for (int i=0; i<protein.domains.size(); i++) {
					d = (PfamAlyzerDomain) protein.domains.get(i);
					if (d.type==PfamAlyzerDomain.PFAMA) {
						g2.setFont(f1);
						g2.drawString("Pfam-A domain:", x1, y);
						g2.setFont(f2);
						if (d.leftOverlap != 0) {
							g2.setColor(Color.RED);
							s = d.id + ",  " + d.seq_start + " - " + d.seq_end + " !!! Overlap !!!";
							y = paintText(g2, s, x2, dim.width-20, y, h);
							g2.setColor(Color.BLACK);
						}
						else {
							s = d.id + ",  " + d.seq_start + " - " + d.seq_end;
							y = paintText(g2, s, x2, dim.width-20, y, h);	
						}
					}
					if (d.type==PfamAlyzerDomain.PFAMB) {
						g2.setFont(f1);
						g2.drawString("Pfam-B domain:", x1, y);
						g2.setFont(f2);
						if (d.leftOverlap != 0) {
							g2.setColor(Color.RED);
							s = d.id + ",  " + d.seq_start + " - " + d.seq_end + " !!! Overlap !!!";
							y = paintText(g2, s, x2, dim.width-20, y, h);
							g2.setColor(Color.BLACK);
						}
						else {
							s = d.id + ",  " + d.seq_start + " - " + d.seq_end;
							y = paintText(g2, s, x2, dim.width-20, y, h);	
						}	
					}
				}
			} 
		}
		
		public void loadProtein(String s) {
			try {
				frame.setCursor(new Cursor(Cursor.WAIT_CURSOR));
				BufferedReader buf = server.doRequest("protein/"+s, "output=pfamalyzer");
				String line = null;
				while ((line = buf.readLine()) != null) {
					StringTokenizer st = new StringTokenizer(line, "\t");
					if (st.hasMoreTokens()) {
						String type = st.nextToken();
						if (type.equals("P")) {
							protein = new PfamAlyzerProtein();
							protein.domains = new Vector();
							protein.acc = st.nextToken();
							protein.id = st.nextToken();
							protein.description = st.nextToken();
							protein.length = Integer.parseInt(st.nextToken());
							protein.species = st.nextToken();
							protein.taxonomy = st.nextToken();
							protein.condenseTaxonomy();
							if (st.nextToken().equals("0"))
								protein.is_fragment = false;
							else 
								protein.is_fragment = true;
						}
						else if (type.equals("A")) {
							PfamAlyzerDomain domain = new PfamAlyzerDomain();
							domain.id = st.nextToken();
							domain.acc = st.nextToken();
							domain.seq_start = Integer.parseInt(st.nextToken());
							domain.seq_end = Integer.parseInt(st.nextToken());
							domain.type = PfamAlyzerDomain.PFAMA;
							domain.colour = coloursA.getColour(domain.id, 1);
							int k=0;
							while (k<protein.domains.size() && ((PfamAlyzerDomain) protein.domains.get(k)).seq_start<domain.seq_start)
								k++;
							protein.domains.add(k, domain);
						}
						else if (type.equals("B")) {
							PfamAlyzerDomain domain = new PfamAlyzerDomain();
							domain.id = st.nextToken();
							domain.acc = st.nextToken();
							domain.seq_start = Integer.parseInt(st.nextToken());
							domain.seq_end = Integer.parseInt(st.nextToken());
							domain.type = PfamAlyzerDomain.PFAMB;
							domain.colour = coloursB.getColour(domain.id, 1);
							int k=0;
							while (k<protein.domains.size() && ((PfamAlyzerDomain) protein.domains.get(k)).seq_start<domain.seq_start)
								k++;
							protein.domains.add(k, domain);
						}
					}
				}
			} catch (Exception ex) {
				frame.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
				PfamAlyzerExceptionDialog ed = new PfamAlyzerExceptionDialog(ex.toString());
        ex.printStackTrace();
			}
			
			frame.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			if (protein!=null) {
				protein.insertDistances();
				rescale(prefWidth);
			}
			coloursA.freeTemporary();
		}
		
		public int paintText(Graphics2D g, String s, int x1, int x2, int y, int height) {
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
	} 	
	
	public PfamAlyzerFrame frame;
	public ProteinPanel proteinPanel;
	public JPanel selectPanel;
	public JSplitPane split1;
	public JScrollPane sp1;
	public JTextField protein;
	
	private JButton dqBtn;
	private JButton brBtn;
	
	BrowseSwissPfamPanel(PfamAlyzerFrame f) {
		setLayout(new BorderLayout());
		frame = f;
		
		selectPanel = new JPanel();
		SpringLayout layout = new SpringLayout();
		selectPanel.setLayout(layout);
		selectPanel.setBackground(Color.WHITE);
		
		JLabel label = new JLabel("Identifier/Acc. no:");
		selectPanel.add(label);	
		SpringLayout.Constraints constr = layout.getConstraints(label);
		constr.setX(Spring.constant(15));
		constr.setHeight(Spring.constant(20));
		constr.setY(Spring.constant(20));
		protein = new JTextField();
		selectPanel.add(protein);	
		constr = layout.getConstraints(protein);
		constr.setX(Spring.constant(15));
		constr.setHeight(Spring.constant(20));
		constr.setY(Spring.constant(40));
		layout.putConstraint(SpringLayout.EAST, selectPanel, 15,SpringLayout.EAST, protein);
		
		JButton shBtn = new JButton("Show");
		shBtn.addActionListener(this);
		selectPanel.add(shBtn);
		constr = layout.getConstraints(shBtn);
		constr.setHeight(Spring.constant(20));
		constr.setWidth(Spring.constant(175));
		constr.setX(Spring.constant(22));
		constr.setY(Spring.constant(75));
		
		dqBtn = new JButton("Domain query");
		dqBtn.addActionListener(this);
		dqBtn.setEnabled(false);
		selectPanel.add(dqBtn);
		constr = layout.getConstraints(dqBtn);
		constr.setHeight(Spring.constant(20));
		constr.setWidth(Spring.constant(175));
		constr.setX(Spring.constant(22));
		constr.setY(Spring.constant(100));
		
		brBtn = new JButton("Browser");
		brBtn.addActionListener(this);
		brBtn.setEnabled(false);
		selectPanel.add(brBtn);
		constr = layout.getConstraints(brBtn);
		constr.setHeight(Spring.constant(20));
		constr.setWidth(Spring.constant(175));
		constr.setX(Spring.constant(22));
		constr.setY(Spring.constant(125));
		
		proteinPanel = new ProteinPanel(frame, new PfamAColours(), new PfamBColours());
		sp1 = new JScrollPane(proteinPanel); 
		
		split1 = new JSplitPane (JSplitPane.HORIZONTAL_SPLIT, selectPanel, sp1);
		split1.setDividerLocation(200);
		add(split1, BorderLayout.CENTER);
	}
	
	public void actionPerformed(ActionEvent e) {
		if (e.getActionCommand().equals("Browser")) {
			if (!protein.getText().equals(""))
				frame.applet.showURL(frame.proteinBrowserPrefix+protein.getText());
			if (proteinPanel.protein != null)
				frame.applet.showURL(frame.proteinBrowserPrefix+proteinPanel.protein.acc);
		}
		else if  (e.getActionCommand().equals("Show")) {
			proteinPanel.protein = null;
			proteinPanel.loadProtein(protein.getText());
			repaint();
			if (proteinPanel.protein == null) {
				brBtn.setEnabled(false);
				dqBtn.setEnabled(false);
				new PfamAlyzerExceptionDialog(frame, "Query returned no match.");
			}
			else {
				brBtn.setEnabled(true);
				dqBtn.setEnabled(true);	
			}
		}
		else if  (e.getActionCommand().equals("Domain query")) {
			if (proteinPanel.protein == null && protein.getText().length()>0)
				proteinPanel.loadProtein(protein.getText());
			if (proteinPanel.protein != null)
				frame.showDomainQuery(proteinPanel.protein);
		}
	}
}
