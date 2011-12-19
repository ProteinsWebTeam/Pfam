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
import javax.swing.tree.*;


/**
 * @author volker
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class ProteinTree extends JPanel implements MouseListener {
	
	public class ProteinTreePopupMenu extends JPopupMenu implements ActionListener{
		
		public PfamAlyzerProtein protein;
		int row;

		ProteinTreePopupMenu(int r) {
			super();
			JMenuItem it;
			it = new JMenuItem("Expand all");
			it.addActionListener(this);
			add(it);
			it = new JMenuItem("Expand children");
			it.addActionListener(this);
			add(it);
			it = new JMenuItem("Collapse all");
			it.addActionListener(this);
			add(it);
			/*it = new JMenuItem("Collapse children");
			it.addActionListener(this);
			add(it);*/
			row = r;
		}

		public void actionPerformed(ActionEvent e) {
			String command = e.getActionCommand();
			if (command=="Expand all") 
				expandAll();
			else if (command=="Collapse all")
				collapseAll();
			else if (command=="Collapse children")
				collapseBeneath(row);
			else if (command=="Expand children")
				expandBeneath(row);
		}
	}
	
	public class IconNode extends DefaultMutableTreeNode  {

	  protected Icon   icon;
	  protected String iconName;

	  public IconNode() {
		this(null);
	  }

	  public IconNode(Object userObject) {
		this(userObject, true, null);
	  }

	  public IconNode(Object userObject, boolean allowsChildren, Icon icon) {
		super(userObject, allowsChildren);
		this.icon = icon;
	  }

	  public void setIcon(Icon icon) {
		this.icon = icon;
	  }

	  public Icon getIcon() {
		return icon;
	  }

	  public String getIconName() {
		if (iconName != null) {
		  return iconName;
		} else {
		  String str = userObject.toString();
		  int index = str.lastIndexOf(".");
		  if (index != -1) {
			return str.substring(++index);
		  } else {
			return null;
		  }
		}
	  }

	  public void setIconName(String name) {
		iconName = name;
	  }
	}

	public class ProteinTreeCellRenderer extends DefaultTreeCellRenderer {

	  public Component getTreeCellRendererComponent(JTree tree, Object value,
		  boolean sel, boolean expanded, boolean leaf,
		  int row, boolean hasFocus) {

		super.getTreeCellRendererComponent(tree, value,
		   sel, expanded, leaf, row, hasFocus);

		Icon icon = ((IconNode)value).getIcon();

		if (icon == null) {
		  Hashtable icons = (Hashtable)tree.getClientProperty("JTree.icons");
		  String name = ((IconNode)value).getIconName();
		  if ((icons != null) && (name != null)) {
			icon = (Icon)icons.get(name);
			if (icon != null) {
			  setIcon(icon);
			}
		  }
		} else {
		  setIcon(icon);
		}

		return this;
	  }
	}
	
	private int height = 20;
	private Vector proteinList;
	private IconNode root;
	private JTree tree;
	
	ProteinTree() {
		super();
		setBackground(Color.WHITE);
		this.setLayout(new BorderLayout());
	}
	
	public void newTree(Vector p, boolean condensed) {
		removeAll();
		proteinList = p;
		root = new IconNode("total");
		buildTree(condensed);
		tree = new JTree(root);
		tree.setCellRenderer( new ProteinTreeCellRenderer());
		tree.addMouseListener(this);
		add(tree, BorderLayout.WEST);
	}
	
	public void mouseEntered(MouseEvent e) {}
	
	public void mouseExited(MouseEvent e) {}
	
	public void mousePressed(MouseEvent e) {}
	
	public void mouseReleased(MouseEvent e) {}	
	
	public void mouseClicked(MouseEvent e) {
		if (e.getModifiers()!=MouseEvent.BUTTON1_MASK)
			(new ProteinTreePopupMenu(tree.getRowForLocation(e.getX(), e.getY()))).show(this, e.getX(), e.getY());
	}
	
	public void expandAll() {
		int row = 0; 
		while (row < tree.getRowCount()) {
			tree.expandRow(row);
			row++;
		}
	}
	
	public void collapseBeneath(int row) {
		IconNode n = (IconNode) tree.getPathForRow(row).getLastPathComponent();
		Enumeration en = n.breadthFirstEnumeration();
		while (en.hasMoreElements()) {
			IconNode n1 = (IconNode) en.nextElement();
			TreeNode[] p = n1.getPath();
			/*int i=0;
			while (!p[i].equals(n))
				i++;
			System.out.println(i);
			TreeNode[] p1 = new TreeNode[p.length-i];
			for (int k=0; k<p1.length; k++)
				p1[k] = p[k+i];*/
			tree.collapsePath(new TreePath(p));
		}
	}
	
	public void expandBeneath(int row) {
		IconNode n = (IconNode) tree.getPathForRow(row).getLastPathComponent();
		Enumeration en = n.breadthFirstEnumeration();
		while (en.hasMoreElements()) {
			IconNode n1 = (IconNode) en.nextElement();
			tree.expandPath(new TreePath(n1.getPath()));
		}
	}
	
	public void collapseAll() {
		int row = tree.getRowCount() - 1;
		while (row > 0) {
			tree.collapseRow(row);
			row--;
		}
	}
	
	private void buildTree(boolean extended) {
		PfamAlyzerProtein p;
		PfamAlyzerProtein p1;
		IconNode n;
		IconNode n1;
		String s1;
		boolean ok;
		for (int i=0; i<proteinList.size(); i++) {
			p = (PfamAlyzerProtein) proteinList.get(i);
			n = root;
			String tax = "";
			if (!extended)
				tax = p.condensed_taxonomy;
			else
				tax = p.taxonomy;
			StringTokenizer st = new StringTokenizer(tax, ";");
			while (st.hasMoreTokens()) {
				ok = false;
				String s = st.nextToken();
				int k = s.length()-1;
				while (k>0 && s.charAt(k) == ' ')
					k--;
				s = s.substring(0,k+1);
				k = 0;
				while (s.length()-1>k && s.charAt(k) == ' ')
					k++;
				s = s.substring(k);
				for (k=0; k<n.getChildCount(); k++) {
					n1 = (IconNode) n.getChildAt(k);
					if (!n1.isLeaf()) {
						s1 = (String) n1.getUserObject();
						if (s1.equals(s)) {
							n = n1;
							ok = true;
						}
					}		
				}
				if (!ok) {
					n1 = new IconNode();
					n1.setUserObject(s);
					n.add(n1);
					n = n1;
				}
			}
			n1 = new IconNode();
			//n1.setUserObject(p.id + " ["+p.species+"]");
			n1.setIcon(new ProteinIcon(p));
			n.add(n1);
		}
		Enumeration en = root.breadthFirstEnumeration();
		while (en.hasMoreElements()) {
			n = (IconNode) en.nextElement();
			if (!n.isLeaf()) {
				s1 = (String) n.getUserObject();
				s1 += " ("+n.getLeafCount()+")";
				n.setUserObject(s1);
			}
		} 
	}
}
	
	

