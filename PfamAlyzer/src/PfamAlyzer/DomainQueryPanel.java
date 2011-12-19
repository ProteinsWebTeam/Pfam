/*
 * Created on 2003-nov-26
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
public class DomainQueryPanel extends JPanel implements ItemListener, ActionListener, TSCCaller {
	
	public class AlphabeticList extends JComboBox {
		
		AlphabeticList () {
			setBackground(Color.WHITE);
			addItem("A");	addItem("B");	addItem("C");	addItem("D");	addItem("E");	addItem("F");	addItem("G");	addItem("H");	addItem("I");		addItem("J");
			addItem("K");	addItem("L");	addItem("M");	addItem("N");	addItem("O");	addItem("P");	addItem("Q");	addItem("R");	addItem("S");	addItem("T");
			addItem("U");	addItem("V");	addItem("W");	addItem("X");	addItem("Y");	addItem("Z");	addItem("Numbers");	addItem("DUF");	addItem("Clans");
		}	
	}
	
	public class ResultLimit extends JComboBox {
		
			ResultLimit () {
				setBackground(Color.WHITE);
				addItem("50");	addItem("100");	addItem("250");	addItem("500"); addItem("1000"); addItem("2000");
			}	
		}
	
	public class DomainList extends JList implements DragGestureListener, DragSourceListener  {
		
		private DragSource dragSource;
		
		public DomainList(ListModel listModel){
			super(listModel);
			dragSource = new DragSource();
			dragSource.createDefaultDragGestureRecognizer(this, DnDConstants.ACTION_COPY, this);	
		}
		 
		public void dragGestureRecognized (DragGestureEvent dge) {
			Object selected = getSelectedValue();
			if (selected!=null) {
				StringSelection text = new StringSelection(selected.toString());
				dge.startDrag(DragSource.DefaultCopyNoDrop, text, this);
			}
		}
		
		public void dragEnter(DragSourceDragEvent dsde) {}
		
		public void dragOver(DragSourceDragEvent dsde) {}
		
		public void dragExit(DragSourceEvent dse) {
			dse.getDragSourceContext().setCursor(null);
		}
		
		public void dragDropEnd(DragSourceDropEvent dsde) {}
		
		public void dropActionChanged(DragSourceDragEvent e) {}
	}
	

	
	public Vector pfamADomains;
	public AlphabeticList alphabeticList;
	public DomainList domainList;
	public PfamAlyzerFrame frame;
	public DefaultListModel domainListModel;
	public ProteinControl proteinControl;
	public SpeciesControl speciesControl;
	public JPanel domainPanel;
	public DomainQueryResultPanel resultPanel;
	public JSplitPane split1;
	public JSplitPane split2;
	public JScrollPane sp2;
	public JScrollPane sp3;
	public JCheckBox order;
	public JCheckBox clans;
	public JComboBox unit;
	public JButton okBtn;
	public JButton clearBtn;
	public ResultLimit resultLimit;
	public PfamAColours coloursA;
	public JTabbedPane result;
	public JTabbedPane query;
	public ProteinTree proteintree;
	private ProgressIndicator progress;
	
	DomainQueryPanel(PfamAlyzerFrame f) {
		setLayout(new BorderLayout());
		frame = f;
		
		coloursA = new PfamAColours();
		domainPanel = new JPanel();
		SpringLayout layout = new SpringLayout();
		domainPanel.setLayout(layout);
		domainPanel.setBackground(Color.WHITE);
		
		JLabel descLabel = new JLabel ("<html>Drag and drop domains on the<p>line in the box to the right,<p>then click Search.</html>");
		descLabel.setBackground(Color.WHITE);		
		domainPanel.add(descLabel);
		SpringLayout.Constraints constr = layout.getConstraints(descLabel);
		constr.setX(Spring.constant(5));
		constr.setY(Spring.constant(5));

		alphabeticList = new AlphabeticList();
		alphabeticList.addItemListener(this);
		domainPanel.add(alphabeticList);
		constr = layout.getConstraints(alphabeticList);
		constr.setX(Spring.constant(35));
		constr.setHeight(Spring.constant(25));
		constr.setY(Spring.constant(60));
		
		clearBtn = new JButton("Clear");
		clearBtn.addActionListener(this);
		domainPanel.add(clearBtn);
		constr = layout.getConstraints(clearBtn);
		constr.setHeight(Spring.constant(25));
		constr.setWidth(Spring.constant(90));
		constr.setX(Spring.constant(110));
		layout.putConstraint(SpringLayout.SOUTH, domainPanel, 15,SpringLayout.SOUTH, clearBtn);
		
		
		okBtn = new JButton("Search");
		okBtn.addActionListener(this);
		domainPanel.add(okBtn);
		constr = layout.getConstraints(okBtn);
		constr.setHeight(Spring.constant(25));
		constr.setWidth(Spring.constant(95));
		constr.setX(Spring.constant(10));
		layout.putConstraint(SpringLayout.SOUTH, clearBtn, 0,SpringLayout.SOUTH, okBtn);
		
		JLabel label = new JLabel("Limit results");
		domainPanel.add(label);
		constr = layout.getConstraints(label);
		constr.setHeight(Spring.constant(25));
		constr.setX(Spring.constant(10));
		layout.putConstraint(SpringLayout.NORTH, okBtn, 10,SpringLayout.SOUTH, label);
		resultLimit = new ResultLimit();
		domainPanel.add(resultLimit);
		constr = layout.getConstraints(resultLimit);
		constr.setHeight(Spring.constant(25));
		constr.setWidth(Spring.constant(60));
		constr.setX(Spring.constant(100));
		layout.putConstraint(SpringLayout.NORTH, label, -25,SpringLayout.SOUTH, resultLimit);
				
		order = new JCheckBox("Order sensitivity");
		layout.putConstraint(SpringLayout.NORTH, resultLimit, 10,SpringLayout.SOUTH, order);
		order.setSelected(true);
		constr = layout.getConstraints(order);
		constr.setX(Spring.constant(15));
		order.setBackground(Color.WHITE);
		domainPanel.add(order);
		
		/*clans = new JCheckBox("Include clans");
		layout.putConstraint(SpringLayout.NORTH, order, 10,SpringLayout.SOUTH, clans);
		clans.setSelected(false);
		constr = layout.getConstraints(clans);
		constr.setX(Spring.constant(15));
		clans.setBackground(Color.WHITE);
		domainPanel.add(clans);*/

		JPanel unitPanel = new JPanel ();
		unitPanel.setBackground(Color.WHITE);		
		layout.putConstraint(SpringLayout.NORTH, order, 5,SpringLayout.SOUTH, unitPanel);
		domainPanel.add(unitPanel);

		JLabel unitLabel = new JLabel ("Gap range units:");
		unitLabel.setBackground(Color.WHITE);
		unitPanel.add(unitLabel);

		String [] units = {"Domains", "AA"};
		unit = new JComboBox (units);
		unit.setSelectedIndex (0);
		constr = layout.getConstraints(unit);
		constr.setX(Spring.constant(15));
		constr.setHeight(Spring.constant(25));
		unit.setBackground(Color.WHITE);
		unitPanel.add(unit);
		
		domainListModel = new DefaultListModel();
		domainList = new DomainList(domainListModel);
		JScrollPane sp = new JScrollPane(domainList); 
		updateList("A");
		domainPanel.add(sp);
		constr = layout.getConstraints(sp);
		constr.setX(Spring.constant(15));
		constr.setY(Spring.constant(90));
		layout.putConstraint(SpringLayout.NORTH, unitPanel, 15,SpringLayout.SOUTH, sp);
		layout.putConstraint(SpringLayout.EAST, domainPanel, 15,SpringLayout.EAST, sp);
		
		proteinControl = new ProteinControl(this);
		speciesControl = new SpeciesControl();
		JScrollPane sp1 = new JScrollPane(proteinControl);
		JScrollPane sp4 = new JScrollPane(speciesControl);
		query = new JTabbedPane();
		query.insertTab("Domain architecture", null, sp1, "", 0);
		query.insertTab("Species", null, sp4, "", 1);
		resultPanel = new DomainQueryResultPanel(this);
		sp2 = new JScrollPane(resultPanel);
		proteintree = new ProteinTree();
		sp3 = new JScrollPane(proteintree);
		result = new JTabbedPane();
		result.insertTab("Proteins", null, sp2, "", 0);
		result.insertTab("Species distribution", null, sp3, "", 1);

		split2 = new JSplitPane(JSplitPane.VERTICAL_SPLIT, query, result);
		split2.setDividerLocation(200);
		split1 = new JSplitPane (JSplitPane.HORIZONTAL_SPLIT, domainPanel, split2);
		split1.setDividerLocation(230);
		add(split1, BorderLayout.CENTER);
	}
	
	private void updateList(String s) {
		domainListModel.clear();
		Iterator it = frame.domainsById.keySet().iterator();
		if (s.equals("DUF")) {
			while (it.hasNext()) {
				String id = ((String) it.next());
				if (id.regionMatches(0,"DUF",0,3)) 
					domainListModel.addElement(id);
			}
		}
		else if (s.equals("Numbers")) {
			while (it.hasNext()) {
	
				String id = ((String) it.next());
				if (id.charAt(0)<='9') 
					domainListModel.addElement(id);
			}
		}
		else if (s.equals("Clans")) {
			it = frame.clans.listIterator();
			while (it.hasNext()) {
				PfamClan clan = (PfamClan) it.next();
				domainListModel.addElement(clan.clan_id+"\t");
			}
		}
		else  {
			while (it.hasNext()) {
				String id = ((String) it.next());
				if (id.toUpperCase().startsWith(s) && (!id.startsWith("DUF")))  
					domainListModel.addElement(id);
			}	
		}
	}
	
	public void requestDone(String result) {
		progress.setText("Fetching proteins...");
		Vector proteinList = new Vector();
		StringTokenizer st = new StringTokenizer(result, "\n");
		while (st.hasMoreTokens()) {
		    proteinList.add(st.nextToken());
	  	}	
		//for (int h=0; h<proteinList.size(); h++)
		//	System.out.println(proteinList.elementAt(h));
		//proteinList.remove(0);
		if (proteinList.size() == 0) {

			PfamAlyzerExceptionDialog ed = new PfamAlyzerExceptionDialog(frame, "Query returned no matches.");
		}
		resultPanel.newList(proteinList);
		proteintree.newTree(resultPanel.getProteinList(), frame.viewCondTaxonomy);
		setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		sp2.revalidate();
		progress.done();
		repaint();

    System.err.println( "end of 'requestDone'" );
	}
	
	public void redisplay() {
		proteintree.newTree(resultPanel.getProteinList(), frame.viewCondTaxonomy);
		repaint();
	}
	
	public void requestException(Exception ex) {
		setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		progress.done();
		PfamAlyzerExceptionDialog ed = new PfamAlyzerExceptionDialog(frame, ex.toString());
    ex.printStackTrace();
	}
	
	public void actionPerformed(ActionEvent e) {
    System.err.println( "start of 'actionPerformed'" );
		if (e.getActionCommand().equals("Search")) {
			String queryString = proteinControl.getQueryString();
			if (queryString != null) {
				//System.out.println(queryString);
				progress = new ProgressIndicator(this.frame);
				progress.setText("Processing query ...");
        System.err.println( "queryString: " + queryString );
				ThreadedServerCommunication thread = new ThreadedServerCommunication("search/pfamalyzer/query", queryString, this, this.frame.applet);
			}
		}
		else if (e.getActionCommand().equals("Clear")) {
			proteinControl.clearQuery();
		}
    System.err.println( "end of 'actionPerformed'" );
	}
	
	public void itemStateChanged(ItemEvent e) {
		String sel = (String)e.getItem();
		updateList(sel);
	}	
}
