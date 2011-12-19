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
import java.net.*;
import java.util.*;
import java.io.*;

/**
 * @author volker
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class HMMSearchPanel extends JPanel {
	private static final int PFAMA = 1;
	private static final int PFAMB = 2;
	
	public class HMMSearchPopupMenu extends JPopupMenu implements ActionListener{
		
		public PfamAlyzerDomain domain;
		public PfamAlyzerProtein protein;
		public ProteinPanel qp;
		Point loc = null;
		
		HMMSearchPopupMenu(PfamAlyzerProtein p, PfamAlyzerDomain d, ProteinPanel pl) {
			super();
			protein = p;
			domain = d;
			qp = pl;
			JMenuItem it;
			if (d.type == PfamAlyzerDomain.PFAMA) {
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
			it = new JMenuItem("Find architecture");
			it.addActionListener(this);
			add(it);
		}
		
		public void paint(Graphics g) {
			super.paint(g);
			if (loc == null)
				loc = getLocationOnScreen();
		}
		
		public void actionPerformed(ActionEvent e) {
			String command = e.getActionCommand();
			if (command=="Short info") {
				frame.setCursor(new Cursor(Cursor.WAIT_CURSOR));
				PfamAlyzerFamilyInfo fi = new PfamAlyzerFamilyInfo(frame.applet.pfamServer, domain.id, loc);
				frame.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			}
			else if (command=="Domain viewer") {
				frame.showBrowsePfam(domain.id);
			}
			else if (command=="Find architecture") {
				frame.showDomainQuery(protein);
			}
			else if (command=="Browser") {
				frame.applet.showURL(frame.domainBrowserPrefix+domain.acc);
			}
		}
	}
	
	private class HMMSearchEntry {
		private int type;
		private String id;
		private String acc;
		private String description;
		private String seq_start;
		private String seq_end;
		private String model_start;
		private String model_end;
		private double score;
		private double evalue;
		private String db;
		private boolean ga_exceed = false;
		private boolean na_exceed = false; 
	}
	
	private class ProteinPanel extends JPanel implements MouseListener, MouseMotionListener {
		
		private PfamAlyzerProtein protein;
		private Vector domains; 
		private Dimension dimension;
		private ServerCommunication server; 
		private boolean firstpaint = true;
		
		private ProteinPanel() {
			super();
			setBackground(Color.WHITE);
			dimension = new Dimension (width, 600);
			server = frame.applet.pfamServer;
			addMouseListener(this);
			addMouseMotionListener(this);
		}
		
		//public Dimension getMiminumSize() {
		//	return dimension;
		//}	
		
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
						if (e.getModifiers()==MouseEvent.BUTTON1_MASK) {
							if (d.type==PfamAlyzerDomain.PFAMA) {
								Point loc1 = getLocationOnScreen();
								loc1.x += e.getX();
								loc1.y += e.getY();
								new PfamAlyzerFamilyInfo(server, d.id, loc1);	
							}
						}
						else {
							(new HMMSearchPopupMenu(protein, d, this)).show(this, loc.x, loc.y);
						}
					}
				}
			}
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
		
		public void newResult(BufferedReader buf) throws IOException {
			domains = new Vector();
			protein = new PfamAlyzerProtein();
			String line = null;
			while ((line = buf.readLine()) != null) {
				StringTokenizer st2 = new StringTokenizer(line, "\t");
				if (st2.countTokens() != 1) {
					HMMSearchEntry e = new HMMSearchEntry();
					String s = st2.nextToken();
					if (s.equals("A"))
						e.type = PFAMA;
					else
						e.type = PFAMB;
					e.acc = st2.nextToken();
					e.id = st2.nextToken();
					e.seq_start = st2.nextToken(); 
					e.seq_end = st2.nextToken();
					e.model_start = st2.nextToken();
					e.model_end = st2.nextToken();
					e.score = Double.parseDouble(st2.nextToken());
					e.evalue = Double.parseDouble(st2.nextToken());
					e.db = st2.nextToken();
					e.description = st2.nextToken();
					while (st2.hasMoreTokens()) {
						String t = st2.nextToken();
						if (t.equals("GAEX"))
							e.ga_exceed = true;
						else if (t.equals("NAEX"))
							e.na_exceed = true;
					}
					domains.add(e);
					PfamAlyzerDomain d = new PfamAlyzerDomain();
					if (e.type == PFAMA) {
						d.type = PfamAlyzerDomain.PFAMA;
						d.colour = coloursA.getColour(e.id, 1);
					}
					else
						d.type = PfamAlyzerDomain.PFAMB;
					d.acc = e.acc;
					d.id = e.id;
					d.seq_start = Integer.parseInt(e.seq_start);
					d.seq_end = Integer.parseInt(e.seq_end);
					protein.domains.add(d);
				}
			}
			protein.length = sequence.getText().length();
			protein.insertDistances();
			int y = 50;
			double scale = (new Double(protein.length+x0)).doubleValue() / (new Double(width)).doubleValue();
			protein.recalculate(x0, y, scale, height);
			coloursA.freeTemporary();
			firstpaint = false;
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
				g2.setFont(f2);
				FontMetrics fm = g2.getFontMetrics();
				
				g2.setFont(f4);
				//width = prefWidth;
				int w = protein.paintComponent(g2, false, frame.viewShadow);
				g2.setColor(Color.BLACK);
				//width = width<w?w:width;
				
				int y = 110;		
				g2.setFont(f3);
				g2.drawString("Predicted architecture:", x0, y);
				x2 = x1 + 20;
				x3 = x2 + 20 + fm.stringWidth("Accession number:");
				y += height;
				g2.setFont(f1);
				int h = fm.getHeight();
				HMMSearchEntry d;
				String s;
				for (int i=0; i<domains.size(); i++) {
					d = (HMMSearchEntry) domains.get(i);
					if (d.type==PFAMA) {
						g2.setFont(f1);
						g2.drawString("Pfam-A domain:", x1, y);
						y +=height;
						g2.setFont(f2);
						g2.drawString("Accession number:", x2, y);
						y = paintText(g2, d.acc, x3, dim.width-20, y, h);
						g2.drawString("Identifier:", x2, y);
						y = paintText(g2, d.id, x3, dim.width-20, y, h);
						g2.drawString("Description:", x2, y);
						y = paintText(g2, d.description, x3, dim.width-20, y, h);
						g2.drawString("seq_start:", x2, y);
						y = paintText(g2, d.seq_start, x3, dim.width-20, y, h);
						g2.drawString("seq_end:", x2, y);
						y = paintText(g2, d.seq_end, x3, dim.width-20, y, h);
						g2.drawString("model_start:", x2, y);
						y = paintText(g2, d.model_start, x3, dim.width-20, y, h);										
						g2.drawString("model_end:", x2, y);
						y = paintText(g2, d.model_end, x3, dim.width-20, y, h);
						/*g2.drawString("score:", x2, y);
						y = paintText(g2, d.score, x3, dim.width-20, y, h);
						g2.drawString("evalue:", x2, y);
						y = paintText(g2, d.evalue, x3, dim.width-20, y, h);*/
						g2.drawString("Database:", x2, y);
						y = paintText(g2, d.db, x3, dim.width-20, y, h);
						if (d.ga_exceed)
							y = paintText(g2, "GA exceeded", x3, dim.width-20, y, h);
						if (d.na_exceed)
							y = paintText(g2, "NA exceeded", x3, dim.width-20, y, h);
					}
					else if (d.type==PFAMB) {
						g2.setFont(f1);
						g2.drawString("Pfam-B domain:", x1, y);
						g2.setFont(f2);
						s = d.id + ",  " + d.seq_start + " - " + d.seq_end;
						y = paintText(g2, s, x2, dim.width-20, y, h);		
					}
				}
				//if (firstpaint) {
					dimension.height = y;
					firstpaint = false;
				//}
			}
		}
	}
	
	private class InputPanel extends JPanel implements ActionListener {	
		
		private Dimension dim;
	
		private InputPanel() {

			setLayout (new BoxLayout (this, BoxLayout.Y_AXIS));
			setBackground(Color.WHITE);

			JPanel topPanel = new JPanel ();
			JPanel midPanel = new JPanel ();
			JPanel bottomPanel = new JPanel ();
			add (topPanel);
			add (midPanel);
			add (bottomPanel);
			topPanel.setBackground(Color.WHITE);
			midPanel.setBackground(Color.WHITE);
			bottomPanel.setBackground(Color.WHITE);
			
			submitBtn = new JButton("Submit query");
			submitBtn.addActionListener(this);
			bottomPanel.add(submitBtn);
			
			resultBtn = new JButton("Retrieve result");
			resultBtn.addActionListener(this);
			bottomPanel.add(resultBtn);
		
			JLabel label1 = new JLabel("Job no.:");
			bottomPanel.add(label1);	
		
			id = new JTextField(30);
			id.setBackground(Color.WHITE);
			bottomPanel.add(id);
		
			cLabel = new JLabel("Cutoff strategy:");
			midPanel.add(cLabel);	

			evalue = new JRadioButton("E-value");
			evalue.setBackground(Color.WHITE);
			midPanel.add(evalue);

			ev = new JTextField();
			evalue.setBackground(Color.WHITE);
			midPanel.add(ev);
			ev.setText("1.0");

			ga = new JRadioButton("Pfam gathering threshold (GA)");
			midPanel.add(ga);
			ga.setSelected(true);
			ga.setBackground(Color.WHITE);
			
			ButtonGroup bg = new ButtonGroup();
			bg.add(evalue);
			bg.add(ga);
			
			JLabel label2 = new JLabel("Protein sequence query:");
			topPanel.add(label2);	

			sequence = new JTextArea(4, 40);
			sp2 = new JScrollPane(sequence);
			topPanel.add(sp2);

			dim = new Dimension(720, 150);
			
			//sequence.setText("MAGAASPCANGCGPSAPSDAEVVHLCRSLEVGTVMTLFYSKKSQRPERKTFQVKLETRQITWSRGADKIEGAIDIREIKEIRPGKTSRDFDRYQEDPAFRPDQSHCFVILYGMEFRLKTLSLQATSEDEVNMWIRGLTWLMEDTLQAATPLQIERWLRKQFYSVDRNREDRISAKDLKNMLSQVNYRVPNMRFLRERLTDLEQRTSDITYGQFAQLYRSLMYSAQKTMDLPFLEASALRAGERPELCRVSLPEFQQFLLEYQGELWAVDRLQVQEFMLSFLRDPLREIEEPYFFLDEFVTFLFSKENSIWNSQLDEVCPDTMNNPLSHYWISSSHNTYLTGDQFSSESSLEAYARCLRMGCRCIELDCWDGPDGMPVIYHGHTLTTKIKFSDVLHTIKEHAFVASEYPVILSIEDHCSIAQQRNMAQYFKKVLGDTLLTKPVDIAADGLPSPNQLKRKILIKHKKLAEGSAYEEVPTSVMYSENDISNSIKNGILYLEDPVNHEWYPHYFVLTSSKIYYSEETSSDQGNEDEEEPKEASGSTELHSNEKWFHGKLGAGRDGRHIAERLLTEYCIETGAPDGSFLVRESETFVGDYTLSFWRNGKVQHCRIHSRQDAGTPKFFLTDNLVFDSLYDLITHYQQVPLRCNEFEMRLSEPVPQTNAHESKEWYHASLTRAQAEHMLMRVPRDGAFLVRKRNEPNSYAISFRAEGKIKHCRVQQEGQTVMLGNSEFDSLVDLISYYEKHPLYRKMKLRYPINEEALEKIGTAEPDYGALYEGRNPGFYVEANPMPTFKCAVKALFDYKAQREDELTFTKSAIIQNVEKQEGGWWRGDYGGKKQLWFPSNYVEEMVSPAALEPEREHLDENSPLGDLLRGVLDVPACQIAVRPEGKNNRLFVFSISMASVAHWSLDVAADSQEELQDWVKKIREVAQTADARLTEGKMMERRKKIALELSELVVYCRPVPFDEEKIGTERACYRDMSSFPETKAEKYVNKAKGKKFLQYNRLQLSRIYPKGQRLDSSNYDPLPMWICGSQLVALNFQTPDKPMQMNQALFLAGGHCGYVLQPSVMRDEAFDPFDKSSLRGLEPCAICIEVLGARHLPKNGRGIVCPFVEIEVAGAEYDSIKQKTEFVVDNGLNPVWPAKPFHFQISNPEFAFLRFVVYEEDMFSDQNFLAQATFPVKGLKTGYRAVPLKNNYSEGLELASLLVKIDVFPAKQENGDLSPFGGASLRERSCDASGPLFHGRAREGSFEARYQQPFEDFRISQEHLADHFDGRDRRTPRRTRVNGDNRL");
		}
		
		public void actionPerformed(ActionEvent e) {
			if (e.getSource()==submitBtn)
				doQueuedHMMSearch();
			else if (e.getSource()==resultBtn)
				getResult();
		}
		
		public Dimension getMinimumSize() {
			return dim;
		}
		
		public Dimension getPreferredSize() {
			return dim;
		}	
	}
	
	public PfamAlyzerFrame frame;
	public ProteinPanel proteinPanel;
	public InputPanel inputPanel;
	public JSplitPane split1;
	public JScrollPane sp1;
	public JScrollPane sp2;
	public JScrollPane sp3;
	public JTextArea sequence;
	public JRadioButton evalue;
	public JRadioButton ga;
	public JTextField ev;
	public JLabel cLabel;
	public JTextField id;
	public JLabel modeCombo;
	public JButton submitBtn;
	public JButton resultBtn;
	private ProgressIndicator progress;
	private PfamAColours coloursA;
	private ServerCommunication server; 
	private int height = 20;
	private int startY = 50;
	private int x0 = 20;
	private int x1 = 50;
	private int x2;
	private int x3;
	private int width = 700;
	
	HMMSearchPanel(PfamAlyzerFrame f) {
		setLayout(new BorderLayout());
		frame = f;
		server = frame.applet.pfamServer;
		inputPanel = new InputPanel();
	
		proteinPanel = new ProteinPanel();
		sp1 = new JScrollPane(proteinPanel);
		sp3 = new JScrollPane(inputPanel); 
		
		split1 = new JSplitPane (JSplitPane.VERTICAL_SPLIT, sp3, sp1);
		split1.setDividerLocation(170);
		add(split1, BorderLayout.CENTER);
		coloursA = new PfamAColours();
	}
	
	public void setEnabled(boolean b) {
		//super.setEnabled(b);
		proteinPanel.setEnabled(b);
		inputPanel.setEnabled(b);
		split1.setEnabled(b);
		sp1.setEnabled(b);
		sp2.setEnabled(b);
		sp3.setEnabled(b);
		sequence.setEnabled(b);
		evalue.setEnabled(b);
		ga.setEnabled(b);
		ev.setEnabled(b);
		submitBtn.setEnabled(b);
		id.setEnabled(b);
	}
	
	public void doQueuedHMMSearch() {
		proteinPanel.protein = null;
		proteinPanel.repaint();
		setEnabled(false);
		setCursor(new Cursor(Cursor.WAIT_CURSOR));
		String cs = "ga=1";
		if (evalue.isSelected())
			cs = "evalue="+ev.getText();
		String options = "&altoutput=1&"+cs;
		try {
			String parameter = "seq="+URLEncoder.encode(sequence.getText(), "UTF-8")+options;
			BufferedReader buf = server.doRequest("sequence/search/", parameter);

			String s = buf.readLine();
			if (s.equals("ERR:")) {
				PfamAlyzerExceptionDialog ed = new PfamAlyzerExceptionDialog(frame, buf.readLine());
			}
			else id.setText(s);
			setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			setEnabled(true);
		}  catch (Exception e) {
			PfamAlyzerExceptionDialog ed = new PfamAlyzerExceptionDialog(frame, e.toString());
      e.printStackTrace();
			setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			setEnabled(true);
		} 
	}
	
	public void getResult() {
		proteinPanel.protein = null;
		proteinPanel.repaint();
		setEnabled(false);
		setCursor(new Cursor(Cursor.WAIT_CURSOR));
		try {
			String parameter = "jobId="+URLEncoder.encode(id.getText(), "UTF-8");
			BufferedReader buf = server.doRequest("results/search/sequence", parameter + "&altoutput=2" );
			String s = buf.readLine();
			if (s.equals("ERR:")) {
				PfamAlyzerExceptionDialog ed = new PfamAlyzerExceptionDialog(frame, buf.readLine());
			}
			else {
				sequence.setText(s);
				s = buf.readLine();
				if (s.equals("ga")) {
					ga.setSelected(true);
				}
				else {
					evalue.setSelected(true);
					ev.setText(s);
				}
				proteinPanel.newResult(buf);
				sp2.revalidate();
				//progress.done();
				repaint();
			}
			setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			setEnabled(true);
		}  catch (Exception e) {
			PfamAlyzerExceptionDialog ed = new PfamAlyzerExceptionDialog(frame, e.toString());
      e.printStackTrace();
			setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			setEnabled(true);
		} 
	}
	
	/*
	public void doHMMSearch() {
		proteinPanel.protein = null;
		proteinPanel.repaint();
		setEnabled(false);
		setCursor(new Cursor(Cursor.WAIT_CURSOR));
		String cs = "ga";
		if (evalue.isSelected())
			cs = "evalue&evalue="+ev.getText();
		String sm = "merge";
		if (modeCombo.getSelectedIndex()==1)
			sm = "both";
		else if (modeCombo.getSelectedIndex()==2)
			sm = "fs";
		else if (modeCombo.getSelectedIndex()==3)
			sm = "ls";
		String options = "&cutoff_strategy="+cs+"&search_mode="+sm;
		try {
			String parameter = "protseq="+URLEncoder.encode(sequence.getText(), "UTF-8")+options;
			progress = new ProgressIndicator(frame);
			if (frame.hmmSearchTimeFactor != -1) {
				int extime = (16*sequence.getText().length())/frame.hmmSearchTimeFactor;
				if (modeCombo.getSelectedIndex()==0 || modeCombo.getSelectedIndex()==1)
					extime*=2; 
				progress.setText("Running (approx. "+extime+" sec) ...");
			}
			else
				progress.setText("Running HMM search  ...");
			ThreadedServerCommunication thread = new ThreadedServerCommunication("runHMMSearch.pl", parameter, this, this.frame.applet);
			//String r = "A	PF00169	PH	33	142	1	92	43.3	6.7e-10	glocal	PH domain	GAEX	\nA	PF00388	PI-PLC-X	321	465	1	153	328.7	8e-96	glocal	Phosphatidylinositol-specific phospholipase C, X domain	GAEX	\nA	PF00017	SH2	550	639	1	82	117.8	2.5e-32	glocal	SH2 domain	GAEX	\nA	PF00017	SH2	668	741	1	82	108.9	1.2e-29	glocal	SH2 domain	GAEX	\nA	PF00018	SH3	794	849	1	58	85.5	1.3e-22	glocal	SH3 domain	GAEX	\nA	PF00387	PI-PLC-Y	952	1070	1	128	177.2	3.2e-50	glocal	Phosphatidylinositol-specific phospholipase C, Y domain	GAEX	\nA	PF00168	C2	1090	1177	1	88	75.0	1.9e-19	glocal	C2 domain	GAEX	";
			//requestDone(r);
		}  catch (Exception e) {
			PfamAlyzerExceptionDialog ed = new PfamAlyzerExceptionDialog(frame, e.toString());
			setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			setEnabled(true);
		} 
	}*/
}
