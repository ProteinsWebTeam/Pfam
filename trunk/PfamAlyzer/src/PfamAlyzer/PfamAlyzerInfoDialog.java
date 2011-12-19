package PfamAlyzer;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

/**
 * Insert the type's description here.
 * Creation date: (2001-06-27 19.39.20)
 * @author: 
 */
public class PfamAlyzerInfoDialog extends JDialog implements ActionListener {

	public class PfamAlyzerInfoDialogEventHandler extends WindowAdapter {

		public PfamAlyzerInfoDialogEventHandler() {
			super();
		}
		
		public void windowClosing(WindowEvent e) {
	
			PfamAlyzerInfoDialog od = (PfamAlyzerInfoDialog)e.getSource();
			od.setVisible(false);
			od.dispose();
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
	
	
public PfamAlyzerInfoDialog(JFrame ow) {
	super(ow, "About PfamAlyzer", true);
	
	Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
	Point loc = ow.getLocation();

	setSize(500, 270);
	setLocation(loc.x+100, loc.y+200);
	addWindowListener(new PfamAlyzerInfoDialogEventHandler());
	//setLayout(null);
	JLabel l = new JLabel(logo);
	l.setBounds(10, 10, 300, 200);
	getContentPane().add(l);

	JButton button = new JButton("Ok");
	button.addActionListener(this);
	button.setBounds(370, 175, 75, 25);
	getContentPane().add(button);
	
	JLabel label = new JLabel("PfamAlyzer");
	label.setBounds(315, 10, 250, 20);
	getContentPane().add(label);
	label = new JLabel("2011: V 0.5");
	label.setBounds(315, 30, 250, 20);
	getContentPane().add(label);	
	label = new JLabel("Volker Hollich");
	label.setBounds(315, 50, 250, 20);
	getContentPane().add(label);
	label = new JLabel("Kristoffer Forslund");
	label.setBounds(315, 70, 250, 20);
	getContentPane().add(label);
	label = new JLabel("Erik Sonnhammer");
	label.setBounds(315, 90, 250, 20);
	getContentPane().add(label);
	label = new JLabel("Stockholm Bioinformatics");
	label.setBounds(315, 110, 250, 20);
	getContentPane().add(label);
	label = new JLabel("Centre, SU");
	label.setBounds(315, 130, 250, 20);
	getContentPane().add(label);
	label = new JLabel("Karolinska Institutet");
	label.setBounds(315, 150, 250, 20);
	getContentPane().add(label);
	label = new JLabel("");
	label.setBounds(315, 150, 250, 20);
	getContentPane().add(label);

	show();
}
/**
 * Insert the method's description here.
 * Creation date: (2001-06-27 19.49.09)
 */
public void actionPerformed(ActionEvent e) {
	setVisible(false);
	dispose();			
}

}
