/*
 * Created on 9-feb-2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package PfamAlyzer;

import javax.swing.*;
import java.awt.*;

/**
 * @author volker
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class ProgressIndicator extends JDialog {
	private JLabel text;
	private JProgressBar progressBar;
	
	public ProgressIndicator(JFrame ow) {
		super(ow, "PfamAlyzer");
		setCursor(new Cursor(Cursor.WAIT_CURSOR));
		setSize(320, 120);
		Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
		Point loc = ow.getLocation();
		setLocation(loc.x+100, loc.y+200);
		progressBar = new JProgressBar();
		progressBar.setIndeterminate(true);
		progressBar.setBounds(30, 35, 250, 20);
		text = new JLabel("                                         ");
		getContentPane().add(progressBar);
		text.setBounds(30, 10, 250, 20);
		getContentPane().add(text);
		JLabel label = new JLabel("");
		label.setBounds(40, 130, 250, 20);
		getContentPane().add(label);	
		show();
	}
	
	public void setText(String s) {
		text.setText(s);
	}
	
	public void done() {
		dispose();
	}

}
