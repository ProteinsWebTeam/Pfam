/*
 * Created on 2003-dec-04
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package PfamAlyzer;

import java.awt.*;
import java.util.*;

/**
 * @author volker
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class PfamAlyzerProtein {
	
	private static String [] condTaxonomy = {"Bacteria", "Eukaryota", "Metazoa", "Fungi", "Viriplantae", "Chordata", "Vertebrata", "Mollusca",
	"Arthropoda", "Platyhelminthes", "Porifera", "Nematoda", "Mammalia", "Amphibia", "Aves", "Teleostei", "Cnidaria", "Echinodermata", "Hexapoda",
	"Crustacea", "Chelicerata", "Urchordata", "Cephalochordata"};

	public Vector domains;
	public int startX;
	public int posYname;
	public int posY;
	public double scaleX;
	public int height;
	public String id;
	public String acc;
	public String description;
	public String species;
	public int length;
	public String taxonomy;
	public String condensed_taxonomy;
	public boolean longNames;
	public boolean is_fragment;
	
	PfamAlyzerProtein() {
		domains = new Vector();
		longNames = true;
	}
	
	public void condenseTaxonomy() {
		condensed_taxonomy = "";
		StringTokenizer st = new StringTokenizer(taxonomy, ";");
		while (st.hasMoreTokens()) {
			String s = st.nextToken();
			int k = s.length()-1;
			while (k>0 && s.charAt(k) == ' ')
				k--;
			s = s.substring(0,k+1);
			k = 0;
			while (s.length()-1>k && s.charAt(k) == ' ')
				k++;
			s = s.substring(k);
			if (st.hasMoreTokens()) {
				for (int i=0; i!= condTaxonomy.length; i++)
					if (s.equals(condTaxonomy[i]))
						condensed_taxonomy += s +"; ";
			}
			else
				condensed_taxonomy += s;
		}
	}
	
	public void insertDistances() {
		int lastPos=0;
		PfamAlyzerDomain d;
		for (int i=0; i<domains.size(); i++) {
			d = (PfamAlyzerDomain) domains.get(i);
			if (lastPos+1<d.seq_start) {
				PfamAlyzerDomain s = new PfamAlyzerDomain();
				s.type = PfamAlyzerDomain.DISTANCE;
				s.seq_start = lastPos+1;
				s.seq_end = d.seq_start-1;
				domains.add(i, s);
				i++;
			}
			lastPos= lastPos<d.seq_end?d.seq_end:lastPos;
		}
		if (lastPos<length) {
			PfamAlyzerDomain s = new PfamAlyzerDomain();
			s.type = PfamAlyzerDomain.DISTANCE;
			s.seq_start = lastPos+1;
			s.seq_end = length;
			domains.add(s);
		}
	}	
	
	/*public void load(ServerCommunication server, String a) {
		domains = new Vector();
		try {
			StringTokenizer st2 = new StringTokenizer(server.doRequest("loadProtein.pl", "acc="+a), "\n");
			while (st2.hasMoreTokens()) {
				StringTokenizer st = new StringTokenizer(st2.nextToken(), "\t");
				if (st.hasMoreTokens()) {
					String type = st.nextToken();
					if (type.equals("P")) {
						acc = a;
						id = st.nextToken();
						description = st.nextToken();
						length = Integer.parseInt(st.nextToken());
						species = st.nextToken();
						taxonomy = st.nextToken();
						if (st.nextToken().equals("0"))
							is_fragment = false;
						else 
							is_fragment = true;
					}
					else if (type.equals("A")) {
						PfamAlyzerDomain domain = new PfamAlyzerDomain();
						domain.id = st.nextToken();
						domain.acc = st.nextToken();
						domain.seq_start = Integer.parseInt(st.nextToken());
						domain.seq_end = Integer.parseInt(st.nextToken());
						domain.type = PfamAlyzerDomain.PFAMA;
						int k=0;
						while (k<domains.size() && ((PfamAlyzerDomain) domains.get(k)).seq_start<domain.seq_start)
							k++;
						domains.add(k, domain);
					}
					else if (type.equals("B")) {
						PfamAlyzerDomain domain = new PfamAlyzerDomain();
						domain.id = st.nextToken();
						domain.acc = st.nextToken();
						domain.seq_start = Integer.parseInt(st.nextToken());
						domain.seq_end = Integer.parseInt(st.nextToken());
						domain.type = PfamAlyzerDomain.PFAMB;
						int k=0;
						while (k<domains.size() && ((PfamAlyzerDomain) domains.get(k)).seq_start<domain.seq_start)
							k++;
						domains.add(k, domain);
					}
				}
			}
		} catch (Exception ex) {PfamAlyzerExceptionDialog ed = new PfamAlyzerExceptionDialog(ex.toString());}
		
		int lastPos=1;
		PfamAlyzerDomain d;
		for (int i=0; i<domains.size(); i++) {
			d = (PfamAlyzerDomain) domains.get(i);
			if (lastPos+1<d.seq_start) {
				PfamAlyzerDomain s = new PfamAlyzerDomain();
				s.type = PfamAlyzerDomain.DISTANCE;
				s.seq_start = lastPos+1;
				s.seq_end = d.seq_start-1;
				domains.add(i, s);
				i++;
			}
			lastPos= lastPos<d.seq_end?d.seq_end:lastPos;
		}
		d = (PfamAlyzerDomain) domains.lastElement();
		if (d.seq_end<length) {
			PfamAlyzerDomain s = new PfamAlyzerDomain();
			s.type = PfamAlyzerDomain.DISTANCE;
			s.seq_start = lastPos;
			s.seq_end = length;
			domains.add(s);
		}
	}
	*/
	static public PfamAlyzerProtein copy (PfamAlyzerProtein protein) {
		return null;
	}
	
	public void recalculate(int x, int y, double scale, int h) {
		startX = x;
		height = h;
		scaleX = scale;
		PfamAlyzerDomain d;
		PfamAlyzerDomain d0;
		posYname = y - h; 
		posY = y;
		int posX =x;
		for (int i=0; i<domains.size(); i++) {
			d = (PfamAlyzerDomain) domains.get(i);
			d.startX = x+(new Double((new Double(d.seq_start)).doubleValue() / scale)).intValue();	
			d.stopX = x+(new Double((new Double(d.seq_end)).doubleValue() / scale)).intValue();
			d.leftOverlap = 0;
		}
		for (int i=1; i<domains.size(); i++) {
					d = (PfamAlyzerDomain) domains.get(i);
					d0 = (PfamAlyzerDomain) domains.get(i-1);
					if (d.seq_start <= d0.seq_end) {
						/* System.out.println("!!!"); */
						d.leftOverlap = d.stopX<d0.stopX?d.stopX:d0.stopX;
					}
				}
	}
	
	public void paintComponent(Graphics2D g, int x, int y, boolean longNames, boolean shadow) {
		if (domains==null)
			return;
		g.setColor(Color.LIGHT_GRAY);
		String str = id;
		if (longNames) {
			str += " ["+ species + "] " + description + ", length " + (new Integer(length)).toString() + "aa";
			if (is_fragment)
				str += ", fragment";
		}
		FontMetrics fm = g.getFontMetrics();
		fm.stringWidth(str);
		if (shadow)
			g.drawString(str, startX+4, posYname+2);
		g.setColor(Color.BLACK);
		g.drawString(str, startX, posYname);
		for (int i=0; i<domains.size(); i++) 
			((PfamAlyzerDomain) domains.get(i)).paintComponent(g, posY, height, shadow);		
	}
		
	public int paintComponent(Graphics2D g, boolean longNames, boolean shadow) {
		if (domains==null)
			return 0;
		if (id != null) {
			g.setColor(Color.LIGHT_GRAY);
			String str = id;
			if (longNames) {
				str += " ["+ species + "] " + description + ", length " + (new Integer(length)).toString() + "aa";
				if (is_fragment)
					str += ", fragment";
			}
			FontMetrics fm = g.getFontMetrics();
			fm.stringWidth(str);
			if (shadow)
				g.drawString(str, startX+4, posYname+2);
			g.setColor(Color.BLACK);
			g.drawString(str, startX, posYname); 
			
			for (int i=0; i<domains.size(); i++) 
				((PfamAlyzerDomain) domains.get(i)).paintComponent(g, posY, height, shadow);
			
			return fm.stringWidth(str);
		}
		else {
			for (int i=0; i<domains.size(); i++) 
				((PfamAlyzerDomain) domains.get(i)).paintComponent(g, posY, height, shadow);
			return 500;
		}
	}
}
