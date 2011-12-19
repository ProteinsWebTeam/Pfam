package PfamAlyzer;

import java.applet.*;
//import java.lang.System;
import java.net.*;

/**
 * Insert the type's description here.
 * Creation date: (2001-06-13 12.40.08)
 * @author: 
 */
public class PfamAlyzer extends Applet {
	ServerCommunication pfamServer;
	String baseURL;
	String version = "0.1";
/**
 * Insert the method's description here.
 * Creation date: (2001-06-20 20.05.15)
 */
	public void exit() {
		super.stop();
		super.destroy();
		//System.exit(0);
	}

/**
 * Initializes the applet.
 */
	public void init() {
		super.init();
	
		if (getParameter("baseURL")!="")
			baseURL = getParameter("baseURL");
		pfamServer = new ServerCommunication(baseURL); 

		PfamAlyzerFrame hw = new PfamAlyzerFrame(this);

	}

	public void showURL(String s) {
		//System.out.println(s);
		try {
			URL url = new URL(s);
			getAppletContext().showDocument(url, "_blank");
		} catch (Exception ex) {}
	}
	
}
