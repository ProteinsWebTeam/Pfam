package PfamAlyzer;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/**
 * Insert the type's description here.
 * Creation date: (2001-06-27 19.39.20)
 * @author: 
 */
public class PfamAlyzerExceptionDialog extends JDialog implements ActionListener {
	
	public class PfamAlyzerInfoDialogEventHandler extends WindowAdapter {
		
		public PfamAlyzerInfoDialogEventHandler() {
			super();
		}
		
		public void windowClosing(WindowEvent e) {
			
			PfamAlyzerExceptionDialog od = (PfamAlyzerExceptionDialog)e.getSource();
			od.setVisible(false);
			od.dispose();
		}		
	}
	
	public String exceptionText;
	
	public PfamAlyzerExceptionDialog(Frame frame, String text) {
		super(frame, "Exception", true);
		
		exceptionText = text;
		Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
		setSize(300, 250);
		setLocation(dim.width/2, dim.height/2-200);
		addWindowListener(new PfamAlyzerInfoDialogEventHandler());
		getContentPane().setLayout(null);
		
		JButton button = new JButton("Ok");
		button.addActionListener(this);
		button.setBounds(110, 180, 75, 25);
		getContentPane().add(button);
		
		/*JLabel label = new JLabel("PfamAlyzer");
		label.setBounds(100, 40, 150, 25);
		getContentPane().add(label);
		label = new JLabel("Exception:");
		label.setBounds(40, 80, 250, 20);
		getContentPane().add(label);	
		label = new JLabel(exceptionText);
		label.setBounds(40, 100, 250, 20);
		getContentPane().add(label); */
		
		show();
	}
	
	public PfamAlyzerExceptionDialog(String exceptionText) {
		super();
		setName("Exception");
		
		Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
		setSize(300, 250);
		setLocation(dim.width/2, dim.height/2-200);
		addWindowListener(new PfamAlyzerInfoDialogEventHandler());
		getContentPane().setLayout(null);
		
		JButton button = new JButton("Ok");
		button.addActionListener(this);
		button.setBounds(110, 180, 75, 25);
		getContentPane().add(button);
		
		show();
	}
	
	public int paintText(Graphics g, String s, int x1, int x2, int y, int height) {
		String str = s;
		if (s==null)
			return y;
		if (s.length()==0)
			return y + height;
		int i;
		FontMetrics fm = g.getFontMetrics();
		if (str.charAt(0)==' ')
			str = str.substring(1);
		while (str.length()>0) {
			i = str.length()-1;
			while (fm.stringWidth(str.substring(0,i))>x2-x1 && i>0)
				i--;
			if (i==str.length()-1) {
				g.drawString(str, x1, y);
				str = "";
				y += height;
			}
			else {
				g.drawString(str.substring(0,i), x1, y);
				str = str.substring(i);
				if (str.charAt(0)==' ')
					str = str.substring(1);
				y += height;
			}
		}
		return  y;
	}
	
	public void paint (Graphics g) {
		super.paint(g);
		
		Dimension dim = getSize();
		Font f;
		FontMetrics fm;
		
		int h = 20;
		int y = 40;
		f = new Font("Times Roman", Font.BOLD, 14);
		g.setFont(f);
		g.drawString("PfamAlyzer", dim.width/2-50, y);
		f = new Font("Times Roman", Font.PLAIN, 12);
		y = 70;
		y = paintText(g, exceptionText, 10, dim.width-10, y, h);
		
	}
	
	public void actionPerformed(ActionEvent e) {
		setVisible(false);
		dispose();			
	}
	
}
