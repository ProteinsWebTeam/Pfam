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
import javax.swing.text.*;
/**
 * @author volker
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class BrowsePfamPanel extends JPanel implements ItemListener,  ActionListener {

	public class AlphabeticList extends JComboBox {
		
		AlphabeticList () {
			setBackground(Color.WHITE);
			addItem("A");	addItem("B");	addItem("C");	addItem("D");	addItem("E");	addItem("F");	addItem("G");	addItem("H");	addItem("I");	addItem("J");
			addItem("K");	addItem("L");	addItem("M");	addItem("N");	addItem("O");	addItem("P");	addItem("Q");	addItem("R");	addItem("S"); addItem("T");
			addItem("U");	addItem("V");	addItem("W");	addItem("X");	addItem("Y");	addItem("Z");	addItem("Numbers");	addItem("DUF"); addItem("Clans");
		}	
	}
	
	public class DomainList extends JList implements MouseListener{
		 
		private BrowsePfamPanel panel;
		
		public DomainList(ListModel listModel, BrowsePfamPanel p){
			super(listModel);
			panel = p;
			addMouseListener(this);
		}
			
		public void mouseExited(MouseEvent ev) {}
		
		public void mousePressed(MouseEvent ev) {}
		
		public void mouseClicked(MouseEvent ev) {
			panel.updateDomain(getSelectedValue().toString());
		}
		
		public void mouseReleased(MouseEvent ev) {}
		
		public void mouseEntered(MouseEvent ev) {}
	}
	
	public class ClanMember {
		public String pfamA_acc;
		public String pfamA_id;
		public Rectangle rect;
		
		ClanMember() {
			rect = new Rectangle();
		}
	}
		
	public class LiteratureReference {
		public String medline;
		public String title;
		public String author;
		public String journal;
		public Rectangle rect;
				
		LiteratureReference() {
			rect = new Rectangle();
		}
	}
	
	public class Database {
			public String db_id;
			public String comment;
			public String db_link;
			public String other_params;
			public Rectangle rect;
			
			Database() {
				db_id = "";
				comment = "";
				db_link = "";
				other_params = "";
				rect = new Rectangle();
			}
		}
	
	public class InterproGo {
		public String interpro_id;
		public String interpro_abstract;
		public String go_function;
		public String go_component;
		public String go_process;
		public Rectangle rect;
		
		InterproGo() {
			interpro_id = "";
			interpro_abstract = "";
			go_function = "";
			go_component = "";
			go_process = "";
			rect = new Rectangle();
		}
	}
	
	public class ItemPanel extends JPanel implements MouseListener, MouseMotionListener {
		public String id = null;
		public String acc;
		public String author;
		public String type;
		public String num_seed;
		public String num_full;
		public String description;
		public String comment;
		public String model_length;
		public String seed_source;
		public String deposited_by;
		public String sequence_GA;
		public String domain_GA;
		public String sequence_NC;
		public String domain_NC;
		public String sequence_TC;
		public String domain_TC;
		public String buildMethod;
		public String searchMethod;
		public String created;
		public Vector literature;
		public Vector interprogo;
		public Vector database;
		public Vector members;
		public int x0 = 20;
		public int x1 = 70;
		public int x2;
		public int startY = 50;
		public int h = 20 ;
		public boolean firstpaint = true;
		public boolean domain;
		public Dimension dimension;
		
		ItemPanel(AlignmentPanel ap) {
			super();
			dimension = new Dimension (600,700);
			setBackground(Color.WHITE);
			addMouseListener(this);
			addMouseMotionListener(this);
		}
		
		public Dimension getPreferredSize() {
			return dimension;
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
		
		public int paintText2(Graphics2D g, String s, int x1, int x2, int x3, int y, int height) {
			String str = s;
			if (s.length()==0)
				return y + height;
			int i;
			int leftX = x1;
			FontMetrics fm = g.getFontMetrics();
			if (str.charAt(0)==' ')
				str = str.substring(1);
			while (str.length()>0) {
				i = str.length()-1;
				while (fm.stringWidth(str.substring(0,i))>x3-leftX && i>0)
					i--;
				if (i==str.length()-1) {
					g.drawString(str, leftX, y);
					str = "";
					y += height;
				}
				else {
					while  (i>0 && str.charAt(i)!=' ')
						i--;
					if (i==0) {
						g.drawString("...", leftX, y);
						str = "";
						y += height;
					}
					else {
						g.drawString(str.substring(0,i), leftX, y);
						str = str.substring(i);
						if (str.charAt(0)==' ')
							str = str.substring(1);
						y += height;
					}
				}
				leftX = x2;
			}
			return  y;
		}
		
		public void paintComponent(Graphics g1) {
			super.paintComponent(g1);
			Graphics2D g = (Graphics2D) g1;
			g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
					RenderingHints.VALUE_ANTIALIAS_ON);
			if (domain) {
				if (id != null) {
					Dimension dim = getSize();
					Font f = new Font("Times Roman", Font.BOLD, 20);
					Font f1 = new Font("Times Roman", Font.ITALIC, 13);
					Font f2 = new Font("Times Roman", Font.PLAIN, 13);
					Font f3 = new Font("Times Roman", Font.BOLD, 13);
					g.setFont(f);
					FontMetrics fm = g.getFontMetrics();
					String str = "Pfam-A domain: " + id;
					int titlelength = fm.stringWidth(str);
					g.drawString(str, Math.max(0, dim.width / 2 - titlelength
							/ 2), startY);
					int y = startY + 2 * h;
					g.setFont(f3);
					g.drawString("General information:", x0, y);
					y += h;
					g.setFont(f1);
					fm = g.getFontMetrics();
					x2 = x1 + 20 + fm.stringWidth("Accession number:");
					int height = fm.getHeight();
					g.drawString("Accession number:", x1, y);
					g.setFont(f2);
					g.drawString(acc, x2, y);
					y += h;
					g.setFont(f1);
					g.drawString("Description:", x1, y);
					g.setFont(f2);
					y = paintText(g, description, x2, dim.width - 20, y, height);
					if (comment != null) {
						g.setFont(f1);
						g.drawString("Comment:", x1, y);
						g.setFont(f2);
						y = paintText(g, comment, x2, dim.width - 20, y, height);
					}
					g.setFont(f1);
					g.drawString("Model length:", x1, y);
					g.setFont(f2);
					y = paintText(g, model_length, x2, dim.width - 20, y,
							height);
					g.setFont(f1);
					g.drawString("Seed source:", x1, y);
					g.setFont(f2);
					y = paintText(g, seed_source, x2, dim.width - 20, y, height);
					g.setFont(f1);
					g.drawString("Deposited by:", x1, y);
					g.setFont(f2);
					y = paintText(g, deposited_by, x2, dim.width - 20, y,
							height);
					g.setFont(f1);
					g.drawString("Number in seed:", x1, y);
					g.setFont(f2);
					y = paintText(g, num_seed, x2, dim.width - 20, y, height);
					g.setFont(f1);
					g.drawString("Number in full:", x1, y);
					g.setFont(f2);
					y = paintText(g, num_full, x2, dim.width - 20, y, height);
					
					if (interprogo.size() > 0) {
						y += h;
						g.setFont(f3);
						g.drawString("Interpro:", x0, y);
						y += h;
						for (int i = 0; i < interprogo.size(); i++) {
							InterproGo ig = (InterproGo) interprogo.get(i);
							ig.rect.y = y - height / 2;
							ig.rect.x = x1;
							ig.rect.width = dim.width - 20;
							if (ig.interpro_id != null) {
								g.setFont(f1);
								g.drawString("Interpro id:", x1, y);
								g.setFont(f2);
								y = paintText(g, ig.interpro_id, x2,
										dim.width - 20, y, height);
							}
							if (ig.interpro_abstract != null) {
								g.setFont(f1);
								g.drawString("Interpro abstract:", x1, y);
								g.setFont(f2);
								y = paintText(g, ig.interpro_abstract, x2,
										dim.width - 20, y, height);
							}
							if (ig.go_component != null) {
								g.setFont(f1);
								g.drawString("GO component:", x1, y);
								g.setFont(f2);
								y = paintText(g, ig.go_component, x2,
										dim.width - 20, y, height);
							}
							if (ig.go_process != null) {
								g.setFont(f1);
								g.drawString("GO process:", x1, y);
								g.setFont(f2);
								y = paintText(g, ig.go_process, x2,
										dim.width - 20, y, height);
							}
							if (ig.go_function != null) {
								g.setFont(f1);
								g.drawString("GO function:", x1, y);
								g.setFont(f2);
								y = paintText(g, ig.go_function, x2,
										dim.width - 20, y, height);
							}
							ig.rect.height = y - height / 2;
						}
					}

					if (literature.size() > 0) {
						y += h;
						g.setFont(f3);
						g.drawString("Literature references:", x0, y);
						y += h;
						for (int i = 0; i < literature.size(); i++) {
							LiteratureReference lit = (LiteratureReference) literature
									.get(i);
							String litstr = lit.author + ". " + lit.title
									+ lit.journal + " Medline: " + lit.medline;
							g.setFont(f2);
							lit.rect.y = y - height / 2;
							lit.rect.x = x1;
							lit.rect.width = dim.width - 20;
							y = paintText2(g, litstr, x1, x1 + 50,
									dim.width - 20, y, height);
							lit.rect.height = y - height / 2;
						}
					}

					if (database.size() > 0) {
						y += h;
						g.setFont(f3);
						g.drawString("Database links:", x0, y);
						y += h;
						for (int i = 0; i < database.size(); i++) {
							Database db = (Database) database.get(i);
							String dbstr = db.comment + db.db_link
									+ db.other_params;
							g.setFont(f1);
							g.drawString(db.db_id, x1, y);
							g.setFont(f2);
							db.rect.y = y - height / 2;
							db.rect.x = x1;
							db.rect.width = dim.width - 20;
							y = paintText(g, dbstr, x2, dim.width - 20, y,
									height);
							db.rect.height = y - height / 2;
						}
					}

					y += h;
					g.setFont(f3);
					g.drawString("HMMer build information:", x0, y);
					y += h;
					g.setFont(f1);
					g.drawString("Build method:", x1, y);
					g.setFont(f2);
					y = paintText(g, buildMethod, x2, dim.width - 20, y, height);
					g.setFont(f1);
					g.drawString("Search method:", x1, y);
					g.setFont(f2);
					y = paintText(g, searchMethod, x2, dim.width - 20, y,
							height);
					g.setFont(f1);
					g.drawString("Gathering cutoff:", x1, y);
					g.setFont(f2);
					y = paintText(g, domain_GA + " " + sequence_GA, x2,
							dim.width - 20, y, height);
					g.setFont(f1);
					g.drawString("Trusted cutoff:", x1, y);
					g.setFont(f2);
					y = paintText(g, domain_TC + " " + sequence_TC, x2,
							dim.width - 20, y, height);
					g.setFont(f1);
					g.drawString("Noise cutoff:", x1, y);
					g.setFont(f2);
					y = paintText(g, domain_NC + " " + sequence_NC, x2,
							dim.width - 20, y, height);

					g.setFont(f1);
					g.drawString("Created:", x1, y);
					g.setFont(f2);
					y = paintText(g, created, x2, dim.width - 20, y, height);

					if (firstpaint) {
						dimension.height = y;
						firstpaint = false;
					}
				}
			}
			else {
				if (acc != null) {
					Dimension dim = getSize();
					Font f = new Font("Times Roman", Font.BOLD, 20);
					Font f1 = new Font("Times Roman", Font.ITALIC, 13);
					Font f2 = new Font("Times Roman", Font.PLAIN, 13);
					Font f3 = new Font("Times Roman", Font.BOLD, 13);
					g.setFont(f);
					FontMetrics fm = g.getFontMetrics();
					String str = "Pfam clan: " + id;
					int titlelength = fm.stringWidth(str);
					g.drawString(str, Math.max(0, dim.width / 2 - titlelength
							/ 2), startY);
					int y = startY + 2 * h;
					g.setFont(f3);
					g.drawString("General information:", x0, y);
					y += h;
					g.setFont(f1);
					fm = g.getFontMetrics();
					x2 = x1 + 20 + fm.stringWidth("Accession number:");
					int height = fm.getHeight();
					g.drawString("Accession number:", x1, y);
					g.setFont(f2);
					g.drawString(acc, x2, y);
					y += h;
					g.setFont(f1);
					g.drawString("Description:", x1, y);
					g.setFont(f2);
					y = paintText(g, description, x2, dim.width - 20, y, height);
					if (comment != null) {
						g.setFont(f1);
						g.drawString("Comment:", x1, y);
						g.setFont(f2);
						y = paintText(g, comment, x2, dim.width - 20, y, height);
					}					
					g.setFont(f1);
					g.drawString("Created:", x1, y);
					g.setFont(f2);
					y = paintText(g, created, x2, dim.width - 20, y, height);

					if (literature.size() > 0) {
						y += h;
						g.setFont(f3);
						g.drawString("Literature references:", x0, y);
						y += h;
						for (int i = 0; i < literature.size(); i++) {
							LiteratureReference lit = (LiteratureReference) literature
									.get(i);
							String litstr = lit.author + ". " + lit.title
									+ lit.journal + " Medline: " + lit.medline;
							g.setFont(f2);
							lit.rect.y = y - height / 2;
							lit.rect.x = x1;
							lit.rect.width = dim.width - 20;
							y = paintText2(g, litstr, x1, x1 + 50,
									dim.width - 20, y, height);
							lit.rect.height = y - height / 2;
						}
					}

					if (database.size() > 0) {
						y += h;
						g.setFont(f3);
						g.drawString("Database links:", x0, y);
						y += h;
						for (int i = 0; i < database.size(); i++) {
							Database db = (Database) database.get(i);
							String dbstr = db.db_link
									+ db.other_params;
							g.setFont(f1);
							g.drawString(db.db_id, x1, y);
							g.setFont(f2);
							db.rect.y = y - height / 2;
							db.rect.x = x1;
							db.rect.width = dim.width - 20;
							y = paintText(g, dbstr, x2, dim.width - 20, y,
									height);
							db.rect.height = y - height / 2;
						}
					}
					
					if (members.size() > 0) {
						y += h;
						g.setFont(f3);
						g.drawString("Members:", x0, y);
						y += h;
						for (int i = 0; i < members.size(); i++) {
							ClanMember m = (ClanMember) members.get(i);
							g.setFont(f1);
							g.drawString(m.pfamA_acc, x1, y);
							g.setFont(f2);
							m.rect.y = y - height / 2;
							m.rect.x = x1;
							m.rect.width = dim.width - 20;
							y = paintText(g, m.pfamA_id, x2, dim.width - 20, y,
									height);
							m.rect.height = y - height / 2;
						}
					}
					
					if (firstpaint) {
						dimension.height = y;
						firstpaint = false;
					}
				}
			}
		}	
		
		public void mouseClicked(MouseEvent e) {
			Point loc = e.getPoint();
			for (int i=0; i<literature.size(); i++) {
				LiteratureReference lit = (LiteratureReference) literature.get(i);
				if (lit.rect.x<=loc.x && lit.rect.width>=loc.x && lit.rect.y<=loc.y && lit.rect.height>=loc.y) 
					if (frame.literaturePrefix != null)
						frame.applet.showURL(frame.literaturePrefix+lit.medline);
			}
			for (int i=0; i<members.size(); i++) {
				ClanMember m = (ClanMember) members.get(i);
				if (m.rect.x<=loc.x && m.rect.width>=loc.x && m.rect.y<=loc.y && m.rect.height>=loc.y) 
					frame.showBrowsePfam(m.pfamA_id);
			}
			for (int i=0; i<database.size(); i++) {
				Database db = (Database) database.get(i);
				if (db.rect.x<=loc.x && db.rect.width>=loc.x && db.rect.y<=loc.y && db.rect.height>=loc.y) {
					if (db.db_id.equals("URL")) 
						frame.applet.showURL(db.comment);
					else if (db.db_id.equals("SCOP") && frame.scopPrefix != null)
						frame.applet.showURL(frame.scopPrefix + db.comment);
					else if (db.db_id.equals("PROSITE") && frame.prositePrefix != null)
						frame.applet.showURL(frame.prositePrefix + db.comment);
					else if (db.db_id.equals("HOMSTRAD") && frame.homestradPrefix != null)
						frame.applet.showURL(frame.homestradPrefix + db.comment);		
					else if (db.db_id.equals("PRINTS") && frame.printsPrefix != null)
						frame.applet.showURL(frame.printsPrefix + db.comment);	
				}
			}
			for (int i=0; i<interprogo.size(); i++) {}
		}
		
		public void mouseEntered(MouseEvent e) {}
		
		public void mouseExited(MouseEvent e) {}	
		
		public void mousePressed(MouseEvent e) {}
		
		public void mouseReleased(MouseEvent e) {}
		
		public void mouseDragged(MouseEvent e) {}
		
		public void mouseMoved(MouseEvent e) {
			if (domain) {
				if (id == null)
					return;
				Point loc = e.getPoint();
				for (int i=0; i<literature.size(); i++) {
					LiteratureReference lit = (LiteratureReference) literature.get(i);
					if (lit.rect.x<=loc.x && lit.rect.width>=loc.x && lit.rect.y<=loc.y && lit.rect.height>=loc.y) 
						if (frame.literaturePrefix != null) {
							frame.setCursor(new Cursor(Cursor.HAND_CURSOR));
							return;
						}
				}
				for (int i=0; i<database.size(); i++) {
					Database db = (Database) database.get(i);
					if (db.rect.x<=loc.x && db.rect.width>=loc.x && db.rect.y<=loc.y && db.rect.height>=loc.y) {
						if (db.db_id.equals("URL")) {
							frame.setCursor(new Cursor(Cursor.HAND_CURSOR));
							return;
						}
						else if (db.db_id.equals("SCOP") && frame.scopPrefix != null) {
							frame.setCursor(new Cursor(Cursor.HAND_CURSOR));
							return;
						}
						else if (db.db_id.equals("PROSITE") && frame.prositePrefix != null) {
							frame.setCursor(new Cursor(Cursor.HAND_CURSOR));
							return;
						}
						else if (db.db_id.equals("HOMSTRAD") && frame.homestradPrefix != null) {
							frame.setCursor(new Cursor(Cursor.HAND_CURSOR));
							return;
						}
						else if (db.db_id.equals("PRINTS") && frame.printsPrefix != null) {
							frame.setCursor(new Cursor(Cursor.HAND_CURSOR));
							return;
						}
					}
				}
				for (int i=0; i<interprogo.size(); i++) {
					InterproGo in = (InterproGo) interprogo.get(i);
					if (in.rect.x<=loc.x && in.rect.width>=loc.x && in.rect.y<=loc.y && in.rect.height>=loc.y) 
						if (frame.goPrefix != null && frame.interproPrefix != null) {
							frame.setCursor(new Cursor(Cursor.HAND_CURSOR));
							return;
						}
				}
				frame.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			}
			else {
				if (acc==null)
					return;
				Point loc = e.getPoint();
				for (int i=0; i<literature.size(); i++) {
					LiteratureReference lit = (LiteratureReference) literature.get(i);
					if (lit.rect.x<=loc.x && lit.rect.width>=loc.x && lit.rect.y<=loc.y && lit.rect.height>=loc.y) 
						if (frame.literaturePrefix != null) {
							frame.setCursor(new Cursor(Cursor.HAND_CURSOR));
							return;
						}
				}
				for (int i=0; i<members.size(); i++) {
					ClanMember m = (ClanMember) members.get(i);
					if (m.rect.x<=loc.x && m.rect.width>=loc.x && m.rect.y<=loc.y && m.rect.height>=loc.y) 
						if (frame.literaturePrefix != null) {
							frame.setCursor(new Cursor(Cursor.HAND_CURSOR));
							return;
						}
				}
				for (int i=0; i<database.size(); i++) {
					Database db = (Database) database.get(i);
					if (db.rect.x<=loc.x && db.rect.width>=loc.x && db.rect.y<=loc.y && db.rect.height>=loc.y) {
						if (db.db_id.equals("URL")) 
							frame.applet.showURL(db.comment);
						else if (db.db_id.equals("SCOP") && frame.scopPrefix != null) {
							frame.setCursor(new Cursor(Cursor.HAND_CURSOR));
							return;
						}
						else if (db.db_id.equals("PROSITE") && frame.prositePrefix != null) {
							frame.setCursor(new Cursor(Cursor.HAND_CURSOR));
							return;
						}
						else if (db.db_id.equals("HOMSTRAD") && frame.homestradPrefix != null) {
							frame.setCursor(new Cursor(Cursor.HAND_CURSOR));
							return;
						}
						else if (db.db_id.equals("PRINTS") && frame.printsPrefix != null) {
							frame.setCursor(new Cursor(Cursor.HAND_CURSOR));
							return;
						}
					}
				}
				frame.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			}
		}	
		
		public void loadDescription(String s, boolean d) {
			literature = new Vector();
			interprogo = new Vector();
			database = new Vector();
			members = new Vector();
			firstpaint = true;
			domain = d;
			frame.setCursor(new Cursor(Cursor.WAIT_CURSOR));
			if (domain) {
				try {
					String line = null;
					BufferedReader buf = frame.applet.pfamServer.doRequest("family/"+s, "output=pfamalyzer");
					while ((line = buf.readLine() ) != null) {
						if (line.startsWith("ID:")) {
							id = s;
							StringTokenizer st = new StringTokenizer(line.substring(3), "\t");
							acc = st.nextToken();
							description = st.nextToken();
							model_length =  st.nextToken();
							author = st.nextToken();
							seed_source =  st.nextToken();
							deposited_by =  st.nextToken();
							type = st.nextToken();
							sequence_GA =  st.nextToken();
							domain_GA =  st.nextToken();
							sequence_TC =  st.nextToken();
							domain_TC =  st.nextToken();
							sequence_NC =  st.nextToken();
							domain_NC =  st.nextToken();
							buildMethod =  st.nextToken();
							searchMethod =  st.nextToken();
							num_seed = st.nextToken();
							num_full = st.nextToken();
							created = st.nextToken();
							if (st.hasMoreTokens()) 
								comment = st.nextToken();
							else 
								comment = null;
						}
						else if (line.startsWith("IG:")) {
							InterproGo ig = new InterproGo();
							StringTokenizer st = new StringTokenizer(line.substring(3), "\t");
							ig.interpro_id = st.nextToken();
							ig.interpro_abstract = st.nextToken();
							ig.go_function = st.nextToken();
							if (ig.go_function.equals("NIX"))
								ig.go_function = null;
							ig.go_component = st.nextToken();
							if (ig.go_component.equals("NIX"))
								ig.go_component = null;
							ig.go_process = st.nextToken();
							if (ig.go_process.equals("NIX"))
								ig.go_process = null;
							interprogo.add(ig);
						}
						else if (line.startsWith("DB:")) {
							Database db = new Database();
							StringTokenizer st = new StringTokenizer(line.substring(3), "\t");
							db.db_id = st.nextToken();
							if (st.hasMoreTokens()) 
								db.comment = st.nextToken();
							if (st.hasMoreTokens()) 
								db.db_link = st.nextToken();
							if (st.hasMoreTokens()) 
								db.other_params = st.nextToken();
							database.add(db);
						}
						else if (line.startsWith("LR:")) {
							LiteratureReference l = new LiteratureReference();
							StringTokenizer st = new StringTokenizer(line.substring(3) ,"\t");
							l.medline = st.nextToken();
							if (st.hasMoreTokens()) 
								l.title = st.nextToken();
							if (st.hasMoreTokens()) 
								l.author = st.nextToken();
							if (st.hasMoreTokens()) 
								l.journal = st.nextToken();
							literature.add(l);
						}
					}
				} catch (Exception e) {
					frame.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
					PfamAlyzerExceptionDialog ed = new PfamAlyzerExceptionDialog(frame, e.toString());
          e.printStackTrace();
				}
			}
			else {
				try {
					String line = null;
					BufferedReader buf = frame.applet.pfamServer.doRequest("clan/"+s,"output=pfamalyzer");
					while ((line = buf.readLine() ) != null) {
						if (line.startsWith("ID:")) {
							StringTokenizer st = new StringTokenizer(line.substring(3), "\t");
							id = s;
							acc = st.nextToken();
							description = st.nextToken();	
							author = st.nextToken();
							created = st.nextToken();
							if (st.hasMoreTokens()) 
								comment = st.nextToken();
							else 
								comment = null;
						}
						else if (line.startsWith("DB:")) {
							Database db = new Database();
							StringTokenizer st = new StringTokenizer(line.substring(3), "\t");
							db.db_id = st.nextToken();
							if (st.hasMoreTokens()) 
								db.comment = st.nextToken();
							if (st.hasMoreTokens()) 
								db.db_link = st.nextToken();
							if (st.hasMoreTokens()) 
								db.other_params = st.nextToken();
							database.add(db);
						}
						else if (line.startsWith("LR:")) {
							LiteratureReference l = new LiteratureReference();
							StringTokenizer st = new StringTokenizer(line.substring(3) ,"\t");
							l.medline = st.nextToken();
							if (st.hasMoreTokens()) 
								l.title = st.nextToken();
							if (st.hasMoreTokens()) 
								l.author = st.nextToken();
							if (st.hasMoreTokens()) 
								l.journal = st.nextToken();
							literature.add(l);
						}
						else if (line.startsWith("ME:")) {
							ClanMember m = new ClanMember();
							StringTokenizer st = new StringTokenizer(line.substring(3) ,"\t");
							m.pfamA_acc = st.nextToken();
							m.pfamA_id = st.nextToken();
							members.add(m);
						}
					}
				} catch (Exception e) {
					frame.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
					PfamAlyzerExceptionDialog ed = new PfamAlyzerExceptionDialog(frame, e.toString());
          e.printStackTrace();
				}
			}
			frame.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		}
	}
	
	public class AlignmentPanel extends JTextArea implements MouseListener {
		
		AlignmentPanel() {
			super();
			setEditable(false);
			setFont(new Font("Courier", Font.PLAIN, 14));
			addMouseListener(this);
		}
		
		public Dimension getPreferredSize() {
			Dimension dim = super.getPreferredSize();
			dim.width += 20;
			return dim;
		}
		
		public void loadAlignment() {
			if (domainPanel.domain == false)
				return;
			if (domainPanel.id == null)
				return;
			if (domainPanel.id == "")
				return;
			try {
				frame.setCursor(new Cursor(Cursor.WAIT_CURSOR));
				/* String request = "acc="+domainPanel.acc+"&format=mul"; */
				/* if (showFull.isSelected()) */
				/* 	request += "&type=full"; */
				/* else */
				/* 	request += "&type=seed"; */
				/* String line = null; */
				/* String aln = ""; */

				/* BufferedReader buf = frame.applet.pfamServer.doRequest("getAlignment", request); */

        String alnType = showFull.isSelected() ? "full" : "seed";
				BufferedReader buf = frame.applet.pfamServer.doRequest("family/"+domainPanel.acc+"/alignment/"+alnType,"");

				DefaultStyledDocument doc = new DefaultStyledDocument();
				DefaultEditorKit edit = new DefaultEditorKit();
				edit.read(buf, doc, 0);
				setDocument(doc);	
			} catch (Exception e) {
				frame.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
				PfamAlyzerExceptionDialog ed = new PfamAlyzerExceptionDialog(frame, e.toString());
        e.printStackTrace();
			}
			frame.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		}
		
		public void newDomain() {
			setText("Click here to see alignment.");
		}

		public void noDomain() {
			setText("Not applicable.");
		}
		
		public void mouseClicked(MouseEvent e) {
			loadAlignment();
		}
		
		public void mouseEntered(MouseEvent e) {}
		
		public void mouseExited(MouseEvent e) {}	
		
		public void mousePressed(MouseEvent e) {}
		
		public void mouseReleased(MouseEvent e) {}	
		
	}
	
	public Vector pfamADomains;
	public AlphabeticList alphabeticList;
	public DomainList domainList;
	public PfamAlyzerFrame frame;
	public DefaultListModel domainListModel;
	public ItemPanel domainPanel;
	public AlignmentPanel alignPanel;
	public JRadioButton showFull;
	public JRadioButton showSeed;
	public JPanel selectPanel;
	public JSplitPane split1;
	public JSplitPane split2;
	public JScrollPane sp2;
	public boolean domain = true;
	
	BrowsePfamPanel(PfamAlyzerFrame f) {
		setLayout(new BorderLayout());
		frame = f;
		
		selectPanel = new JPanel();
		SpringLayout layout = new SpringLayout();
		selectPanel.setLayout(layout);
		selectPanel.setBackground(Color.WHITE);
		
		alphabeticList = new AlphabeticList();
		alphabeticList.addItemListener(this);
		selectPanel.add(alphabeticList);
		
		SpringLayout.Constraints constr = layout.getConstraints(alphabeticList);
		constr.setX(Spring.constant(35));
		constr.setHeight(Spring.constant(25));
		constr.setY(Spring.constant(20));
		
		JComponent bottom = selectPanel;
		
		if (frame.domainBrowserPrefix !=null) {
			JButton okBtn = new JButton("Browser");
			okBtn.addActionListener(this);
			selectPanel.add(okBtn);
			constr = layout.getConstraints(okBtn);
			constr.setHeight(Spring.constant(25));
			constr.setWidth(Spring.constant(90));
			constr.setX(Spring.constant(35));
			layout.putConstraint(SpringLayout.SOUTH, selectPanel, 15,SpringLayout.SOUTH, okBtn);
			bottom = okBtn;
		}
		
		if (frame.nifasPrefix !=null) {
			JButton nifasBtn = new JButton("Nifas");
			nifasBtn.addActionListener(this);
			selectPanel.add(nifasBtn);
			constr = layout.getConstraints(nifasBtn);
			constr.setHeight(Spring.constant(25));
			constr.setWidth(Spring.constant(90));
			constr.setX(Spring.constant(35));
			layout.putConstraint(SpringLayout.SOUTH, bottom, 30,SpringLayout.SOUTH, nifasBtn);
			bottom = nifasBtn;
		}
				
		showFull = new JRadioButton("Full alignment");
		showFull.addActionListener(this);
		layout.putConstraint(SpringLayout.NORTH, bottom, 10,SpringLayout.SOUTH, showFull);
		constr = layout.getConstraints(showFull);
		constr.setX(Spring.constant(15));
		showFull.setBackground(Color.WHITE);
		selectPanel.add(showFull);
		showSeed = new JRadioButton("Seed alignment");
		showSeed.addActionListener(this);
		layout.putConstraint(SpringLayout.NORTH, showFull, 0,SpringLayout.SOUTH, showSeed);
		constr = layout.getConstraints(showSeed);
		constr.setX(Spring.constant(15));
		showSeed.setBackground(Color.WHITE);
		selectPanel.add(showSeed);
		showSeed.setSelected(true);
		
		ButtonGroup bg = new ButtonGroup();
		bg.add(showSeed);
		bg.add(showFull);
		
		domainListModel = new DefaultListModel();
		domainList = new DomainList(domainListModel, this);
		JScrollPane sp = new JScrollPane(domainList); 
		updateList("A");
		selectPanel.add(sp);
		constr = layout.getConstraints(sp);
		constr.setX(Spring.constant(15));
		constr.setY(Spring.constant(70));
		layout.putConstraint(SpringLayout.NORTH, showSeed,15,SpringLayout.SOUTH, sp);
		layout.putConstraint(SpringLayout.EAST, selectPanel, 15,SpringLayout.EAST, sp);
		
		alignPanel = new AlignmentPanel();
		sp2 = new JScrollPane(alignPanel);
		domainPanel = new ItemPanel(alignPanel);
		JScrollPane sp1 = new JScrollPane(domainPanel);
		//alignPanel = new AlignmentPanel();
		//sp2 = new JScrollPane(alignPanel); 

		split2 = new JSplitPane(JSplitPane.VERTICAL_SPLIT, sp1, sp2);
		split2.setDividerLocation(200);
		split1 = new JSplitPane (JSplitPane.HORIZONTAL_SPLIT, selectPanel, split2);
		split1.setDividerLocation(170);
		add(split1, BorderLayout.CENTER);
	}
	
	private void updateList(String s) {
		domainListModel.clear();
		Iterator it = frame.domainsById.keySet().iterator();
		if (s.length()==3) {
			domain = true;
			while (it.hasNext()) {
				String id = ((String) it.next());
				if (id.regionMatches(0,"DUF",0,3)) 
					domainListModel.addElement(id);
			}
		}
		else if (s.length()==5) {
			domain = false;
			it = frame.clans.listIterator();
			while (it.hasNext()) {
				PfamClan clan = (PfamClan)it.next();
				domainListModel.addElement(clan.clan_id);
			}
		}
		else if (s.length()==7) {
			domain = true;
			while (it.hasNext()) {
				String id = ((String) it.next());
				if (id.charAt(0)<='9') 
					domainListModel.addElement(id);
			}
		}
		else {
			domain = true;
			while (it.hasNext()) {
				String id = ((String) it.next());
				if (id.toUpperCase().startsWith(s) && (!id.startsWith("DUF")))  
					domainListModel.addElement(id);
			}
		}
	}
	
	public void actionPerformed(ActionEvent e) {
		if (e.getActionCommand().equals("Nifas")) {
			if (domainPanel.id == null)
				return;
			String type;
			if (showFull.isSelected())
				type = "full";
			else
				type = "seed";
			frame.applet.showURL(frame.nifasPrefix+"acc="+domainPanel.acc+"&name="+domainPanel.id+"&type="+type);		
		}
		if (e.getActionCommand().equals("Browser")) {
			if (domainPanel.id == null)
				return;
			frame.applet.showURL(frame.domainBrowserPrefix+domainPanel.acc);	
		}
		if (e.getActionCommand().equals("Seed alignment") || e.getActionCommand().equals("Full alignment")) 
			alignPanel.newDomain();
	}
	
	public void itemStateChanged(ItemEvent e) {
		String sel = (String)e.getItem();
		updateList(sel);
	}	
	
	public void updateDomain(String id) {
		domainPanel.loadDescription(id, domain);
		if (domain)
			alignPanel.newDomain();
		else
			alignPanel.noDomain();
		validate();
		repaint();
	}
	
}
