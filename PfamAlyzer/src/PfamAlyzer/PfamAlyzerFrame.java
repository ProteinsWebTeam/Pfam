/*
 * Created on 2003-nov-26
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package PfamAlyzer;

import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import java.util.*;
import java.io.*;
/**
 * @author volker
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class PfamAlyzerFrame extends JFrame {

	public PfamAlyzer applet;
	//public FontsizeMenu fmenu;
	
	public class PfamAlyzerFrameMenuBar extends JMenuBar implements ActionListener, ItemListener {

		public PfamAlyzerFrame parent;
		public JCheckBoxMenuItem viewProteinDescription;
		public JCheckBoxMenuItem viewShadow;
		public JCheckBoxMenuItem viewFragments;
		public JCheckBoxMenuItem viewClans;
		public JCheckBoxMenuItem viewCondTaxonomy;
		
		public PfamAlyzerFrameMenuBar(PfamAlyzerFrame frame) {
			super();

			parent = frame;
			
			JMenu menu = new JMenu("Views");
			JMenuItem menuItem = new JMenuItem("Protein HMM search");
			menuItem.addActionListener(this);
			//menu.
			add(menuItem);
			menuItem = new JMenuItem("Domain Query");
			menuItem.addActionListener(this);
			//menu.
			add(menuItem);
			menuItem = new JMenuItem("Browse SwissPfam");
			menuItem.addActionListener(this);
			//menu.
			add(menuItem);
			menuItem = new JMenuItem("Browse Pfam");
			menuItem.addActionListener(this);
			//menu.
			add(menuItem);
			menuItem = new JMenuItem("Exit");
			menuItem.setEnabled(true);
			menuItem.addActionListener(this);
			//menu.
			//add(menuItem);
			//add(menu);

			menu = new JMenu("Options");
			viewProteinDescription = new JCheckBoxMenuItem("Protein description");
			viewProteinDescription.setSelected(true);
			viewProteinDescription.addItemListener(this);
			menu.add(viewProteinDescription);
			viewShadow = new JCheckBoxMenuItem("Shadows");
			viewShadow.setSelected(true);
			viewShadow.addItemListener(this);
			menu.add(viewShadow);
			/*viewClans = new JCheckBoxMenuItem("Colour clans");
			viewClans.setSelected(false);
			viewClans.addItemListener(this);
			menu.add(viewClans);*/
			/*viewFragments = new JCheckBoxMenuItem("Fragments");
			viewFragments.setSelected(true);
			viewFragments.addItemListener(this);
			menu.add(viewFragments);*/
			viewCondTaxonomy = new JCheckBoxMenuItem("Extended taxonomy");
			viewCondTaxonomy.setSelected(false);
			viewCondTaxonomy.addItemListener(this);
			menu.add(viewCondTaxonomy);
			/* fmenu = new FontsizeMenu();
			fmenu.addSize("8");
			fmenu.addSize("10");
			fmenu.addSize("12");
			fmenu.addPreferredSize("14");
			fmenu.addSize("16");
			fmenu.addSize("20");
			fmenu.addSize("24");
			fmenu.addSize("28");
			fmenu.addSize("32");
			fmenu.addActionListener(this);
			menu.add(fmenu); */
			add(menu);
			
			menu = new JMenu("Help");
			menuItem = new JMenuItem("Display help");
			menuItem.setEnabled(true);
			menuItem.addActionListener(this);
			menu.add(menuItem);
			menuItem = new JMenuItem("About");
			menuItem.setEnabled(true);
			menuItem.addActionListener(this);
			menu.add(menuItem);
			add(menu);

		}
		
		public void actionPerformed(ActionEvent e) {
			
			String command = e.getActionCommand();
			if (command=="Exit") {
				parent.setVisible(false);
				parent.dispose();
				parent.applet.exit();
				//System.exit(0);
			}
			else if (command=="Display help") {
				parent.applet.showURL(applet.baseURL + "search/pfamalyzer/help");
			}
			else if (command=="About") {
				PfamAlyzerInfoDialog od = new PfamAlyzerInfoDialog(parent);
			}
			else if (command=="Font") {
				//parent.doRepaint();
			}
			else if (command=="Protein HMM search") {
				parent.showHMMSearch();
			}
			else if (command=="Browse Pfam") {
				parent.showBrowsePfam();
			}
			else if (command=="Browse SwissPfam") {
				parent.showBrowseSwissPfam();
			}
			else if (command=="Domain Query") {
				parent.showDomainQuery();
			}
		}

		public void itemStateChanged(ItemEvent e) {
			if (e.getItemSelectable()==viewProteinDescription) {
				parent.viewProteinDescription = parent.viewProteinDescription?false:true;
				parent.repaint();
			}
			else if (e.getItemSelectable()==viewShadow) {
				parent.viewShadow = parent.viewShadow?false:true;
				parent.repaint();
			}
			else if (e.getItemSelectable()==viewClans) {
				parent.viewClans = parent.viewClans?false:true;
			}
			else if (e.getItemSelectable()==viewCondTaxonomy) {
				parent.viewCondTaxonomy = viewCondTaxonomy.isSelected();
				parent.dqp.redisplay();
				parent.repaint();
			}		
		}
	}	
	
	public class PfamAlyzerFrameEventHandler extends WindowAdapter {
		
		public PfamAlyzerFrameEventHandler() {
			super();
		}
		
		public void windowClosing(WindowEvent e) {
			
			PfamAlyzerFrame frame = (PfamAlyzerFrame)e.getSource();
			frame.setVisible(false);
			frame.dispose();
			frame.applet.exit();
			//System.exit(0);
		}		
	}
	static ImageIcon logo;
	static {
		try {
			Class c = Class.forName("PfamAlyzer.SplashScreen");
			logo = new ImageIcon(c.getResource("resources/PfamAlyzer.gif"));
		}
		catch (ClassNotFoundException cnfe) {}
	}
	
	public boolean viewProteinDescription;
	public boolean viewShadow;
	public boolean viewClans = false;;
	public boolean viewFragments;
	public boolean viewCondTaxonomy = false;
	private DomainQueryPanel dqp;
	private BrowsePfamPanel bpp;
	private BrowseSwissPfamPanel bsp;
	private HMMSearchPanel hsp;
	public TreeMap domainsByAcc;
	public TreeMap domainsById;
	public LinkedList clans;
	public int hmmSearchTimeFactor = -1; 
	public String domainBrowserPrefix = null;
	public String proteinBrowserPrefix = null;
	public String alignmentPrefix = null;
	public String literaturePrefix = null;
	public String prositePrefix = null;
	public String homestradPrefix = null;
	public String printsPrefix = null;
	public String goPrefix = null;
	public String interproPrefix = null;
	public String scopPrefix = null;
	public String nifasPrefix = null;
	
	public PfamAlyzerFrame(PfamAlyzer a) {
		super("PfamAlyzer");
		applet = a;
		SplashScreen sp = new SplashScreen();
		setSize(800, 500);
		setLocation(150, 100);
		addWindowListener(new PfamAlyzerFrameEventHandler());	
		
		proteinBrowserPrefix = applet.baseURL + "protein/";

		viewProteinDescription = true;
		viewShadow = true;
		if (getStartupNotice()) {

		        loadClans();
		    
		        loadDomains();
			
			PfamAlyzerFrameMenuBar mb = new PfamAlyzerFrameMenuBar(this);
			setJMenuBar(mb);
			
			dqp = new DomainQueryPanel(this);
			bsp = new BrowseSwissPfamPanel(this);
			bpp = new BrowsePfamPanel(this);
			hsp = new HMMSearchPanel(this);
			
			getContentPane().setBackground(Color.WHITE);
			JLabel l = new JLabel(logo);
			getContentPane().add(l, BorderLayout.CENTER);
			
			sp.discard();
			show();
		}
		else {
			sp.discard();
			dispose();
			applet.exit();
		}

		showDomainQuery();

	}
	
	public void showDomainQuery() {
		getContentPane().removeAll();
		getContentPane().add(dqp);
		dqp.revalidate();
		getContentPane().repaint();
	}
	
	public void showDomainQuery(PfamAlyzerProtein protein) {
		dqp.proteinControl.newQuery(protein);
		dqp.resultPanel.newList(new Vector());	
		dqp.proteintree.newTree(new Vector(), viewCondTaxonomy);
		getContentPane().removeAll();
		getContentPane().add(dqp);
		dqp.revalidate();
		getContentPane().repaint();
	}
	
	public void showBrowsePfam() {
		getContentPane().removeAll();
		getContentPane().add(bpp);
		bpp.revalidate();
		getContentPane().repaint();
	}
	
	public void showBrowsePfam(String id) {
		getContentPane().removeAll();
		getContentPane().add(bpp);
		bpp.domainPanel.loadDescription(id, true);
		bpp.alignPanel.newDomain();
		bpp.revalidate();
		getContentPane().repaint();
	}

	public void showBrowsePfamClan(String acc) {
		getContentPane().removeAll();
		getContentPane().add(bpp);
		bpp.domainPanel.loadDescription(acc, false);
		bpp.alignPanel.newDomain();
		bpp.revalidate();
		getContentPane().repaint();
	}
	
	public void showBrowseSwissPfam() {
		getContentPane().removeAll();
		getContentPane().add(bsp);
		bsp.revalidate();
		getContentPane().repaint();
	}
	
	public void showBrowseSwissPfam(String acc) {
		getContentPane().removeAll();
		getContentPane().add(bsp);
		bsp.proteinPanel.loadProtein(acc);
		bsp.revalidate();
		getContentPane().repaint();
	}
	
	public void showHMMSearch() {
			getContentPane().removeAll();
			getContentPane().add(hsp);
			hsp.revalidate();
			getContentPane().repaint();
		}
	
	private boolean getStartupNotice() {
		boolean exit = false;
		try {
			String display = "";
			BufferedReader buf = applet.pfamServer.doRequest("search/pfamalyzer/startup", applet.version);
			String s = null;
			while ((s = buf.readLine()) != null) {
				if (s.startsWith("DIS:"))
					display += s.substring(4);
				else if (s.startsWith("EXT:"))
					exit = true;
				else if (s.startsWith("PAR:")) {
					if (s.startsWith("PAR:PFA")) {
						domainBrowserPrefix = s.substring(8);
					}
					else if (s.startsWith("PAR:PRO")) {
						proteinBrowserPrefix = s.substring(8);
					}
					else if (s.startsWith("PAR:ALG")) {
						alignmentPrefix = s.substring(8);
					}
					else if (s.startsWith("PAR:LIT")) {
						literaturePrefix = s.substring(8);
					}
					else if (s.startsWith("PAR:PST")) {
						prositePrefix = s.substring(8);
					}
					else if (s.startsWith("PAR:HSD")) {
						homestradPrefix = s.substring(8);
					}
					else if (s.startsWith("PAR:GOP")) {
						goPrefix = s.substring(8);
					}
					else if (s.startsWith("PAR:PTS")) {
						printsPrefix = s.substring(8);
					}
					else if (s.startsWith("PAR:IPR")) {
						interproPrefix = s.substring(8);
					}
					else if (s.startsWith("PAR:SCO")) {
						scopPrefix = s.substring(8);
					}
					else if (s.startsWith("PAR:NIF")) {
						nifasPrefix = s.substring(8);
					}
					else if (s.startsWith("PAR:TIF")) {
						try {
							hmmSearchTimeFactor = Integer.parseInt(s.substring(8));
						} catch (NumberFormatException Ex) {
							hmmSearchTimeFactor = -1;
						}
					}
				}
			}
			if (display.length()>0) 
				new PfamAlyzerStartupNotice(this, display);
		} catch (Exception ex) {
      PfamAlyzerExceptionDialog ed = new PfamAlyzerExceptionDialog(this, ex.toString()); 
      ex.printStackTrace();
      exit=true;
    }
		if (exit)
			return false;
		return true;
	}

	public String getAcc(String id) {
		if (domainsById==null)
			return (String) null;
		PfamADomain domain = (PfamADomain) domainsById.get(id);
		if (domain != null)
			return domain.pfamA_acc;
		else
			return (String) null;
	}
	
	public String getId(String acc) {
		if (domainsByAcc==null)
			return (String) null;
		PfamADomain domain = (PfamADomain) domainsByAcc.get(acc);
		if (domain != null)
			return domain.pfamA_id;
		else
			return (String) null;
	}
		
	private void loadDomains() {
			ServerCommunication server = applet.pfamServer;
			domainsById = new TreeMap();
			domainsByAcc = new TreeMap();
			try {

				BufferedReader buf = applet.pfamServer.doRequest("families", "output=pfamalyzer");
				
				String line = null;
				while ((line = buf.readLine()) != null) {
					StringTokenizer st = new StringTokenizer(line,"	");
					PfamADomain domain = new PfamADomain(); 
          domain.pfamA_id = st.nextToken();
          domain.pfamA_acc = st.nextToken();

					if (st.hasMoreTokens()) {
						String clan_acc = st.nextToken();
						for (int i=0; i<clans.size(); i++) {
							PfamClan clan = (PfamClan) clans.get(i);
							if (clan.clan_acc.equals(clan_acc)) {
								domain.clan_member = clan;
								clan.members.add(domain);
							}
						}
					} 
					else
						domain.clan_member = null;
					domainsByAcc.put(domain.pfamA_acc, domain);
					domainsById.put(domain.pfamA_id, domain);
					}
				} catch (Exception e) {
          PfamAlyzerExceptionDialog ed = new PfamAlyzerExceptionDialog(this, e.toString());
          e.printStackTrace();
        }
		}
	
	private void loadClans() {
		ServerCommunication server = applet.pfamServer;
		clans = new LinkedList();
		PfamClan clan = null;
		try {
			BufferedReader buf = applet.pfamServer.doRequest("clans", "output=pfamalyzer");
			String line = null;
			while ((line = buf.readLine()) != null) {
				clan = new PfamClan();
        /*
				clans.add(clan);
        String[] tokens = line.split("\\t");
        clan.clan_acc  = tokens[0];
        clan.clan_id   = tokens[1];
        clan.clan_desc = tokens[2];
				clan.members = new LinkedList();
        */
				StringTokenizer st = new StringTokenizer(line,"	");
				clans.add(clan);
				clan.clan_acc  = st.nextToken();
				clan.clan_id   = st.nextToken();
				clan.clan_desc = st.nextToken();
				clan.members = new LinkedList();
			}
		} catch (Exception e) {
      PfamAlyzerExceptionDialog ed = new PfamAlyzerExceptionDialog(this, e.toString());
      e.printStackTrace();
    }
	}
}
