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
public class SpeciesControl extends JPanel {
	
	class FileRender extends DefaultTreeCellRenderer {
		JCheckBox checkBox = new JCheckBox();
		JLabel label = new JLabel();
		
		public FileRender() {
			checkBox.setOpaque(false);
		}
		
		public Component getTreeCellRendererComponent(JTree tree, Object value, boolean selected, boolean expanded, 
				boolean leaf, int row, boolean hasFocus) {
			DefaultMutableTreeNode node = (DefaultMutableTreeNode)value;
			if (!node.isRoot()) {
				MyObject mo = (MyObject) node.getUserObject();
				checkBox.setText(mo.toString());
				checkBox.setSelected(mo.isChecked());
				return checkBox;
			}
			else {
				label.setText(node.getUserObject().toString());
				return label;
			}
		}
	}
	
	class EditRender extends AbstractCellEditor implements TreeCellEditor, ItemListener {
		JLabel label = new JLabel();
		JCheckBox checkBox = new JCheckBox();
		DefaultMutableTreeNode node;
		MyObject mo;
		boolean leaf;
		
		public EditRender() {
			checkBox.addItemListener(this);
			checkBox.setOpaque(false);
		}
		
		public Component getTreeCellEditorComponent(JTree tree, Object value, boolean isSelected, 
				boolean expanded, boolean leaf,int row) {
			node = (DefaultMutableTreeNode)value;
			this.leaf = leaf;
			if (!node.isRoot()) {
				mo = (MyObject) node.getUserObject();
				checkBox.setText(mo.toString());
				checkBox.setSelected(mo.isChecked());
				setBackground(Color.WHITE);
				return checkBox;
			}
			else {
				label.setText(node.getUserObject().toString());
				return label;
			}
		}
		
		public void itemStateChanged(ItemEvent e) {
			DefaultMutableTreeNode n;
			boolean b =(e.getStateChange()==ItemEvent.SELECTED);
			//checkBox.setSelected(b);
			mo.checked = b;
			if (!leaf) {
				Enumeration en = node.breadthFirstEnumeration();
				while (en.hasMoreElements()) {
					n = (DefaultMutableTreeNode) en.nextElement();
					MyObject mo = (MyObject) n.getUserObject();
					mo.checked = b;
					/* System.out.println(mo.text); */
				} 
			}
			tree.repaint();
			//getSelectedString();
		}
		
		public Object getCellEditorValue(){
			//mo.setChecked(checkBox.isSelected());
			return mo;
		}
	}
	
	class MyObject {
		private String text;
		boolean checked = false;
		public MyObject(String text) {
			this.text=text;
		}
		public String toString() {
			return text;
		}
		public boolean isChecked() {
			return checked;
		}
		public void setChecked(boolean b) {
			checked=b;
		}
	}
	
	private DefaultMutableTreeNode root;
	private JTree tree;
	
	SpeciesControl(){
		super();
		setBackground(Color.WHITE);
		this.setLayout(new BorderLayout());
		
		root = new DefaultMutableTreeNode(new MyObject("O"));
		buildTree();
		tree = new JTree(root);
		FileRender render = new FileRender();
		EditRender er = new EditRender();
		
		tree.setEditable(true);
		tree.setCellRenderer(render);
		tree.setCellEditor(er);
		add(tree, BorderLayout.WEST);
		selectAll();
	}
	
	private void buildTree() {
		DefaultMutableTreeNode n;
		DefaultMutableTreeNode n1;
		n = new DefaultMutableTreeNode(new MyObject("Bacteria"));
		root.add(n);
		n = new DefaultMutableTreeNode(new MyObject("Archaea"));
		root.add(n);
		n = new DefaultMutableTreeNode(new MyObject("Eukaryota"));
		root.add(n);
		n1 = new DefaultMutableTreeNode(new MyObject("Fungi"));
		n.add(n1);
		n1 = new DefaultMutableTreeNode(new MyObject("Viridiplantae"));
		n.add(n1);
		n1 = new DefaultMutableTreeNode(new MyObject("Metazoa"));
		n.add(n1);
		n = new DefaultMutableTreeNode(new MyObject("Chordata"));
		n1.add(n);
		n = new DefaultMutableTreeNode(new MyObject("Nematoda"));
		n1.add(n);
		n = new DefaultMutableTreeNode(new MyObject("Arthropoda"));
		n1.add(n);		
	}
	
	public String getSelectedString() {
		String s = "";
		Enumeration en = root.breadthFirstEnumeration();
		DefaultMutableTreeNode n;
		MyObject mo;
		boolean first = true;
		while (en.hasMoreElements()) {
			n = (DefaultMutableTreeNode) en.nextElement();
			if (!n.isRoot()) {
				mo = (MyObject) n.getUserObject();
				if (mo.checked) {
				 
				    if (first) {

					first = false;

				    }

				    else {

					s += ",";
					
				    }


				    s += mo.text;

				}
			}
		} 
		//System.out.println(s);
		return s;
	}
	
	public void selectAll() {
		Enumeration en = root.breadthFirstEnumeration();
		DefaultMutableTreeNode n;
		MyObject mo;
		while (en.hasMoreElements()) {
			n = (DefaultMutableTreeNode) en.nextElement();
			if (!n.isRoot()) {
				mo = (MyObject) n.getUserObject();
				mo.checked = true;
			}
		} 
	}
}
	
	

