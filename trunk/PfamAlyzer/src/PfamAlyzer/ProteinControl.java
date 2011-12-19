/*
 * Created on 1-apr-2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package PfamAlyzer;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.awt.datatransfer.*;
import java.awt.dnd.*;

/**
 * @author volker
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class ProteinControl extends JPanel implements DropTargetListener, DragGestureListener, DragSourceListener, MouseListener, MouseMotionListener {
	
	public class DistanceDialog extends JDialog implements ActionListener {
		
		public class DistanceGraphic extends JComponent {
			public void paint(Graphics g) {
				super.paint(g);
				Dimension d = getSize();
				g.drawLine(1,1,1,d.height);
				g.drawLine(d.width-1,1,d.width-1,d.height);
				g.drawLine(1,d.height/2,d.width-1,d.height/2);
			}
		}		
		
		public JTextField min;
		public JTextField max;	
		public QueryEntry qe;	
		
		public DistanceDialog (PfamAlyzerFrame frame, QueryEntry q, Point loc) {
			super(frame, "Distances", true);
			qe = q;
			setLocation(loc);
			setSize(280, 190);
			SpringLayout layout = new SpringLayout();
			getContentPane().setLayout(layout);
			
			min = new JTextField(qe.min==-1?"*":Integer.toString(qe.min));
			SpringLayout.Constraints constr = layout.getConstraints(min);
			constr.setX(Spring.constant(30));
			constr.setY(Spring.constant(30));
			constr.setHeight(Spring.constant(20));
			constr.setWidth(Spring.constant(60));
			getContentPane().add(min);
			max = new JTextField(qe.max==-1?"*":Integer.toString(qe.max));
			constr = layout.getConstraints(max);
			constr.setX(Spring.constant(170));
			constr.setY(Spring.constant(30));
			constr.setHeight(Spring.constant(20));
			constr.setWidth(Spring.constant(60));
			
			getContentPane().add(max);
			DistanceGraphic dg = new DistanceGraphic();
			constr = layout.getConstraints(dg);
			constr.setX(Spring.constant(30));
			constr.setY(Spring.constant(60));
			constr.setHeight(Spring.constant(20));
			constr.setWidth(Spring.constant(200));
			getContentPane().add(dg);
			
			JButton button = new JButton("Ok");
			button.addActionListener(this);
			constr = layout.getConstraints(button);
			constr.setX(Spring.constant(100));
			constr.setY(Spring.constant(100));
			constr.setHeight(Spring.constant(20));
			constr.setWidth(Spring.constant(60));
			getContentPane().add(button);
			
			show();
		}
		
		public void actionPerformed(ActionEvent e) {
			try {
				int minInt = -1;
				if (!min.getText().equals("*"))
					minInt = (new Integer(min.getText())).intValue();
				int maxInt = -1;
				if (!max.getText().equals("*"))
					maxInt = (new Integer(max.getText())).intValue();
				if (minInt <=maxInt || maxInt==-1) {
					qe.min = minInt;
					qe.max = maxInt;
				}					
			} catch (Exception ex) {}
			setVisible(false);
			dispose();			
		}		
	}
	
	public class ProteinControlPopupMenu extends JPopupMenu implements ActionListener{
		
		public QueryEntry qe;
		public ProteinControl qp;
		Point loc = null;
		
		ProteinControlPopupMenu(QueryEntry q, ProteinControl p) {
			super();
			qe = q;
			qp = p;
			JMenuItem it;
			if (qe.notAcc) {
				it = new JMenuItem("Include");
				it.addActionListener(this);
				add(it);
			}
			else {
				it = new JMenuItem("Exclude");
				it.addActionListener(this);
				add(it);
			}
			if (qe.type==QueryEntry.GIVENDOMAIN || qe.type==QueryEntry.CLAN) {
				it = new JMenuItem("Short info");
				it.addActionListener(this);
				add(it);
				it = new JMenuItem("Domain viewer");
				it.addActionListener(this);
				add(it);
			}
			if (qe.type==QueryEntry.GIVENDOMAIN) {
				if (qp.frame.frame.domainBrowserPrefix != null) {
					it = new JMenuItem("Browser");
					it.addActionListener(this);
					add(it);
				}
			}
		}
		
		public void paint(Graphics g) {
			super.paint(g);
			if (loc == null)
				loc = getLocationOnScreen();
		}
		
		public void actionPerformed(ActionEvent e) {
			String command = e.getActionCommand();
			if (command=="Include") {
				qe.notAcc = false;
				qp.repaint();
			}
			else if (command=="Exclude") {
				qe.notAcc = true;
				qp.repaint();
			}
			else if (command=="Short info") {
				frame.setCursor(new Cursor(Cursor.WAIT_CURSOR));
				if (qe.type==QueryEntry.GIVENDOMAIN) {
					PfamAlyzerFamilyInfo fi = new PfamAlyzerFamilyInfo(qp.frame.frame.applet.pfamServer, qe.id, loc);
				}
				else {
					PfamAlyzerClanInfo ci = new PfamAlyzerClanInfo(qp.frame.frame.applet.pfamServer, qe.id, loc);
				}
				frame.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			}
			else if (command=="Domain viewer") {
				if (qe.type==QueryEntry.GIVENDOMAIN) 
					qp.frame.frame.showBrowsePfam(qe.id);
				else 
					qp.frame.frame.showBrowsePfamClan(qe.id);
			}
			else if (command=="Browser") {
				TreeMap t = qp.frame.frame.domainsById;
				PfamADomain domain = (PfamADomain) t.get(qe.id);
				if (domain != null)
					qp.frame.frame.applet.showURL(qp.frame.frame.domainBrowserPrefix+domain.pfamA_acc);
			}
		}
	}
	
	
	private DragSource dragSource;
	private DropTarget dropTarget;
	private final int proteinX = 20;
	private int proteinY = 80;
	private final int proteinLength = 800;
	private int tolerance = 10;
	private Vector domains; 
	private DomainQueryPanel frame; 
	
	ProteinControl(DomainQueryPanel f) {
		super();
		frame = f;
		domains = new Vector();
		QueryEntry qe = new QueryEntry();
		qe.type = QueryEntry.DISTANCE;
		qe.min = -1;
		qe.max = -1;
		qe.startX = proteinX;
		qe.stopX = proteinLength + proteinX;
		qe.distanceType = QueryEntry.AA;
		domains.add(qe);
		dropTarget = new DropTarget(this, DnDConstants.ACTION_COPY_OR_MOVE, this, true);
		dragSource = new DragSource();
		dragSource.createDefaultDragGestureRecognizer(this, DnDConstants.ACTION_MOVE, this);
		addMouseListener(this);
		addMouseMotionListener(this);
		setBackground(Color.WHITE);
	}
	
	public Dimension getPreferredSize() {
		return new Dimension(proteinX*2+proteinLength, proteinY*2);
	}
	
	public void mouseClicked(MouseEvent e) {
		Point loc = e.getPoint();
		if (loc.y > proteinY - tolerance && loc.y < proteinY + tolerance) {	
			for (int i=0; i<domains.size(); i++) {
				QueryEntry qe = (QueryEntry) domains.get(i);
				if (loc.x+tolerance > qe.startX && loc.x-tolerance < qe.stopX) {
					if (qe.type==QueryEntry.GIVENDOMAIN || qe.type==QueryEntry.CLAN) {
						if (e.getModifiers()==MouseEvent.BUTTON1_MASK) {
							qe.notAcc = qe.notAcc==true?false:true; 
							repaint();
						}
						else {
							(new ProteinControlPopupMenu(qe, this)).show(this, loc.x, loc.y);
						}
					}
					else if (qe.type==QueryEntry.DISTANCE) {
						Point p = this.getLocationOnScreen();
						p.x += loc.x;
						p.y += loc.y;
						DistanceDialog dd = new DistanceDialog(frame.frame, qe, p);
						repaint();
					}
				}
			}
		}
	}
	
	public void mouseDragged(MouseEvent e) {}
	
	public void mouseMoved(MouseEvent e) {
		Point loc = e.getPoint();
		if (loc.y > proteinY - tolerance && loc.y < proteinY + tolerance) {	
			for (int i=0; i<domains.size(); i++) {
				QueryEntry qe = (QueryEntry) domains.get(i);
				if (loc.x+tolerance > qe.startX && loc.x-tolerance < qe.stopX) {
					if (qe.type==QueryEntry.GIVENDOMAIN) {
						frame.setCursor(new Cursor(Cursor.HAND_CURSOR));
						return;
					}
					else if (qe.type==QueryEntry.DISTANCE) {
						frame.setCursor(new Cursor(Cursor.HAND_CURSOR));
						return;
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
	
	public void dragGestureRecognized (DragGestureEvent dge) {
		Point loc = dge.getDragOrigin();
		if (loc.y > proteinY - tolerance && loc.y < proteinY + tolerance) {	
			for (int i=0; i<domains.size(); i++) {
				QueryEntry qe = (QueryEntry) domains.get(i);
				if (loc.x+tolerance > qe.startX && loc.x-tolerance < qe.stopX && qe.type!=QueryEntry.DISTANCE) {
					StringSelection text = new StringSelection(Integer.toString(i));
					dge.startDrag(DragSource.DefaultMoveNoDrop, text, this);
				}
			}
		}
	}
	
	public void dragEnter(DragSourceDragEvent dsde) {}
	
	public void dragOver(DragSourceDragEvent dsde) {}
	
	public void dragExit(DragSourceEvent dse) {
		dse.getDragSourceContext().setCursor(null);
	}
	
	public void dragDropEnd(DragSourceDropEvent dsde) {}
	
	public void dropActionChanged(DragSourceDragEvent e) {}
	
	public void dragEnter(DropTargetDragEvent dtde) {}
	
	public void dragOver(DropTargetDragEvent dtde) {
		Point loc = dtde.getLocation();
		if (dtde.getDropAction()==DnDConstants.ACTION_MOVE) {
			if (loc.y + tolerance < proteinY || loc.y - tolerance > proteinY) {
				dtde.acceptDrag(dtde.getDropAction());
				return;
			}
			boolean ok = false;
			for (int i=0; i<domains.size(); i++) {
				QueryEntry qe = (QueryEntry) domains.get(i);
				if (loc.x+tolerance > qe.startX && loc.x-tolerance < qe.stopX && qe.type==QueryEntry.DISTANCE) 
					ok = true;				
			}
			if (ok) 
				dtde.acceptDrag(dtde.getDropAction());
			else
				dtde.rejectDrag();
		}
		else {
			if (loc.y - tolerance > proteinY || loc.y + tolerance < proteinY) {
				dtde.rejectDrag();
				return;
			}
			boolean ok = false;
			for (int i=0; i<domains.size(); i++) {
				QueryEntry qe = (QueryEntry) domains.get(i);
				if (loc.x+tolerance > qe.startX && loc.x-tolerance < qe.stopX && qe.type==QueryEntry.DISTANCE) 
					ok = true;				
			}
			if (ok) 
				dtde.acceptDrag(dtde.getDropAction());
			else 
				dtde.rejectDrag();
		}
	}
	
	public void dragExit(DropTargetEvent dte) {}
	
	public void drop(DropTargetDropEvent dtde) {
		Point loc = dtde.getLocation();
		try {
			Transferable  t =  dtde.getTransferable();
			if (t.isDataFlavorSupported(DataFlavor.stringFlavor)) {
				if (dtde.getDropAction()==DnDConstants.ACTION_MOVE) {	
					String no = (String) t.getTransferData(DataFlavor.stringFlavor);
					int entryNo = Integer.parseInt(no);	 
					if (loc.y + tolerance < proteinY || loc.y - tolerance > proteinY) {
						QueryEntry qe = (QueryEntry) domains.get(entryNo);
						domains.remove(entryNo);
						domains.remove(entryNo);
						boolean lastID = true;
						for (int i=0; i<domains.size(); i++) {
							QueryEntry qe1 = (QueryEntry) domains.get(i);
							if (qe1.type==QueryEntry.GIVENDOMAIN && qe.id.equals(qe1.id))
								lastID = false;
						}
						if (lastID)
							frame.coloursA.freeID(qe.id);
						dtde.acceptDrop(dtde.getDropAction());
					}
					else for (int i=0; i<domains.size(); i++) {
						QueryEntry qe = (QueryEntry) domains.get(i);
						if (loc.x-tolerance > qe.startX && loc.x+tolerance < qe.stopX && qe.type==QueryEntry.DISTANCE) {
							QueryEntry qe1 = (QueryEntry) domains.get(entryNo);
							domains.remove(entryNo);
							QueryEntry qe2 = (QueryEntry) domains.get(entryNo);
							domains.remove(entryNo);
							if (i>entryNo)
								i-=2;
							domains.add(i, qe1);
							domains.add(i, qe2);
							i = domains.size();
							dtde.acceptDrop(dtde.getDropAction());	
						}		
					}
				}
				else if (dtde.getDropAction()==DnDConstants.ACTION_COPY) {
					String id = (String) t.getTransferData(DataFlavor.stringFlavor);
					QueryEntry qe = new QueryEntry();
					//if (id.startsWith("Clan_"))
					if (id.endsWith("\t")) {
						qe.type = QueryEntry.CLAN;
						qe.id = id.substring(0,id.length()-1);
					}
					else {
						qe.type = QueryEntry.GIVENDOMAIN;
						qe.id = id;
					}
					qe.notAcc = false;
					qe.colour = -1;
					
					for (int i=0; i<domains.size(); i++) {
						QueryEntry qe1 = (QueryEntry) domains.get(i);
						if (loc.x+tolerance > qe1.startX && loc.x-tolerance < qe1.stopX && qe1.type==QueryEntry.DISTANCE) {
							domains.add(i+1, qe);
							qe = new QueryEntry();
							qe.type = QueryEntry.DISTANCE;
							qe.distanceType = QueryEntry.AA;
							qe.min = -1;
							qe.max = -1;
							domains.add(i+2, qe);
						}	
					}
					dtde.acceptDrop(dtde.getDropAction());	
				}
				recalculate();
				repaint();
			}
		} catch (Exception e) {}
	}
	
	public void dropActionChanged(DropTargetDragEvent e) {}
	
	public void recalculate() {
		int dEntries = (domains.size()-1)/2;
		int dLength = (proteinLength-dEntries) / (dEntries*3+1);
		int x = proteinX;
		for (int i=0; i<domains.size(); i++) {
			QueryEntry qe = (QueryEntry) domains.get(i);
			if (qe.colour == -1 && (qe.type==QueryEntry.GIVENDOMAIN || qe.type==QueryEntry.CLAN))
				qe.colour = frame.coloursA.getColour(qe.id, 0);
			qe.startX = x;
			if (qe.type==QueryEntry.DISTANCE)
				qe.stopX = x+dLength;
			else
				qe.stopX = x+2*dLength;
			x = qe.stopX+1;
		}
	}
	
	public void paintComponent(Graphics g) {
		super.paintComponent(g);
		Graphics2D g2 = (Graphics2D) g;
		g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		Dimension d = getSize();
		proteinY = d.height/2;
		for (int i=0; i<domains.size(); i++) {
			QueryEntry qe = (QueryEntry) domains.get(i);
			qe.paintComponent(g2, proteinY, frame.frame.viewShadow);
		}
	}	
	
	public void clearQuery() {
		frame.speciesControl.selectAll();
		frame.coloursA = new PfamAColours();
		domains = new Vector();
		QueryEntry qe = new QueryEntry();
		qe.type = QueryEntry.DISTANCE;
		qe.min = -1;
		qe.max = -1;
		qe.startX = proteinX;
		qe.stopX = proteinLength + proteinX;
		qe.distanceType = QueryEntry.AA;
		domains.add(qe);
		
		recalculate();	
		repaint();
	}
	
	public void newQuery(PfamAlyzerProtein p) {
		frame.speciesControl.selectAll();
		frame.coloursA = new PfamAColours();
		domains = new Vector();
		for (int i=0; i<p.domains.size();  i++) {
			PfamAlyzerDomain d = (PfamAlyzerDomain) p.domains.get(i);
			if (d.type==PfamAlyzerDomain.PFAMA) {
				QueryEntry qe = new QueryEntry();
				qe.id = d.id;
				qe.colour = -1;
				qe.type = QueryEntry.GIVENDOMAIN;
				qe.notAcc = false;
				domains.add(qe);
			}
		}
		QueryEntry qe;
		for (int i=0; i<domains.size(); i+=2) {
			qe = new QueryEntry();
			qe.min = -1;
			qe.max = -1;
			qe.type = QueryEntry.DISTANCE;
			domains.add(i, qe);
		}
		qe = new QueryEntry();
		qe.min = -1;
		qe.max = -1;
		qe.type = QueryEntry.DISTANCE;
		domains.add(qe);
		
		recalculate();	
	}
	
	public String getQueryString() {
		if (domains.size()==1)
			return null;
		QueryEntry qe = (QueryEntry) domains.get(0);
		String queryString = "N_term_min=";
		if (qe.min!=-1)
			queryString += qe.min; 
		queryString += "&N_term_max="; 
		if (qe.max!=-1)
			queryString += qe.max; 
		int i; int k=0;
		for (i=1; i<domains.size()-1; i++) {
			qe = (QueryEntry) domains.get(i);
			if (qe.type==QueryEntry.GIVENDOMAIN) {
				k++;
				queryString += "&domain"+k+"=";
				if (qe.notAcc == true)
					queryString += "Not+"; 
				queryString += qe.id; 
			}
			else if (qe.type==QueryEntry.DISTANCE) {
				queryString += "&min"+k+"=";
				if (qe.min!=-1)
					queryString += qe.min; 
				queryString += "&max"+k+"=";
				if (qe.max!=-1)
					queryString += qe.max; 
			}
			else if (qe.type==QueryEntry.CLAN) {
				k++;
				queryString += "&domain"+k+"=";
				if (qe.notAcc == true)
				 queryString += "Not+"; 
				queryString += "Clan_"+qe.id;
			}
		}
		qe = (QueryEntry) domains.get(i);
		queryString += "&C_term_min="; 
		if (qe.min!=-1)
			queryString += qe.min; 
		queryString += "&C_term_max="; 
		if (qe.max!=-1)
			queryString += qe.max; 
		queryString += "&number_of_boxes="+k;
		if (frame.order.isSelected()) 
			queryString += "&toggle_order=on";
		if (frame.unit.getSelectedIndex () == 1)
			queryString += "&unit=aa";
		else
			queryString += "&unit=domains";
		//if (frame.clans.isSelected())
		//	queryString += "&clans=on";
		queryString += "&max_list="+(String) frame.resultLimit.getSelectedItem();
		queryString += "&subtaxa=" + frame.speciesControl.getSelectedString();
		//System.out.println(queryString);
		return queryString;
	}
}
