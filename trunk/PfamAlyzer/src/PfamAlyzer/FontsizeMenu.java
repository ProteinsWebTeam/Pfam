package PfamAlyzer;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
/**
 * Insert the type's description here.
 * Creation date: (2001-07-02 17.35.42)
 * @author: 
 */
public class FontsizeMenu extends JMenu implements ItemListener {
	ActionListener actionListener;
/**
 * Insert the method's description here.
 * Creation date: (2001-07-02 17.37.07)
 */
public FontsizeMenu() {
	
	super("Font size");
		
}
/**
 * Insert the method's description here.
 * Creation date: (07/13/2001 %r)
 * @param al java.awt.event.ActionListener
 */
public void addActionListener(ActionListener al) {
	
	actionListener = AWTEventMulticaster.add(actionListener, al);	
	
}
/**
 * Insert the method's description here.
 * Creation date: (2001-07-02 18.42.52)
 * @param s java.lang.String
 */
public void addPreferredSize(String s) {
	
	JCheckBoxMenuItem item = new JCheckBoxMenuItem(s);
	item.setState(true);
	item.addItemListener(this);
	add(item);
		
}
/**
 * Insert the method's description here.
 * Creation date: (2001-07-02 17.44.39)
 * @param s java.lang.String
 */
public void addSize(String s) {
	
	JCheckBoxMenuItem item = new JCheckBoxMenuItem(s);
	item.setState(false);
	item.addItemListener(this);
	add(item);
	
}
/**
 * Insert the method's description here.
 * Creation date: (2001-07-02 18.50.45)
 * @return java.lang.String
 */
public String getSelectedSize() {
	JCheckBoxMenuItem item;
	
	for (int i=0; i<getItemCount(); i++) {
		item = (JCheckBoxMenuItem) getItem(i);
		if (item.getState()==true) return item.getText();
	}
	
	return null;
}
/**
 * Insert the method's description here.
 * Creation date: (2001-07-02 17.53.02)
 * @param e java.awt.event.ItemEvent
 */
public void itemStateChanged(ItemEvent e) {
	JCheckBoxMenuItem item;
	
	if (e.getStateChange()==ItemEvent.DESELECTED) {
		((JCheckBoxMenuItem)e.getItemSelectable()).setState(true);
	}
	else {
		for (int i=0; i<getItemCount(); i++) {
			item = (JCheckBoxMenuItem) getItem(i);
			if (item!=e.getItemSelectable()) item.setState(false);
		}
		ActionEvent ev = new ActionEvent(this, ActionEvent.ACTION_PERFORMED, "Font");
		if (actionListener != null) {
			actionListener.actionPerformed(ev);
		}
	}
	
}
}
