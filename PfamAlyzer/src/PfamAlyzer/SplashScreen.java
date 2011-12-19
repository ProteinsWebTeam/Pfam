/*
 * Created on 7-jan-2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package PfamAlyzer;

import java.awt.*;
import javax.swing.*;

/**
 * @author volker
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class SplashScreen extends JWindow {
	static ImageIcon logo;
	static {
		try {
			Class c = Class.forName("PfamAlyzer.SplashScreen");
			logo = new ImageIcon(c.getResource("resources/PfamAlyzer.gif"));
		}
		catch (ClassNotFoundException cnfe) {}
	}
	
	public SplashScreen() {
		super();
		JLabel l = new JLabel(logo);
		getContentPane().add(l, BorderLayout.CENTER);
		pack();
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Dimension labelSize = l.getPreferredSize();
		setLocation(screenSize.width/2 - (labelSize.width/2), screenSize.height/2 - (labelSize.height/2));
		
		setVisible(true);
	}
		
	public void discard() {
		setVisible(false);
		dispose();
	}		
}

