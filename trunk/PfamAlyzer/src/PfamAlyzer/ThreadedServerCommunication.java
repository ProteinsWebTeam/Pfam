/*
 * Created on 10-feb-2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package PfamAlyzer;

import java.net.*;
import java.io.*;


/**
 * @author volker
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class ThreadedServerCommunication extends Thread{
	
	TSCCaller callback;
	String script;
	String request;
	PfamAlyzer applet;
	String pfamServerScriptPrefix = "/pfamalyserbackend/";
	
	public ThreadedServerCommunication(String  script, String request, TSCCaller callback, PfamAlyzer applet){
		this.callback = callback;
		this.script = script;
		this.request = request;
		this.applet = applet;
		start();
	}
	
	public void run() {
		try {
			/* String s = applet.baseURL+pfamServerScriptPrefix+script; */
			String s = applet.baseURL + script;
			URL url = new URL(s);

			URLConnection uc = url.openConnection();
			
			uc.setDoOutput(true);
			uc.setDoInput(true);
			uc.setUseCaches(false);
			uc.setDefaultUseCaches(false);
			uc.setAllowUserInteraction(false);
			uc.setRequestProperty("Content-Type", "application/x-www-form-urlencoded;");
			DataOutputStream dos = new DataOutputStream(uc.getOutputStream());
			
			dos.writeBytes(request);
			dos.flush();
			dos.close();
			InputStreamReader in = new InputStreamReader(uc.getInputStream());	
			
			int bufferlength = 255;
			char [] buffer  = new char [bufferlength];
			String result = "";
			int num_read = 0;
			while (num_read !=-1) {
				num_read=in.read(buffer, 0, bufferlength);
				if (num_read>0)
					result += new String(buffer, 0, num_read);
			}
			in.close();
			

			callback.requestDone(result);
		} catch (IOException ex) {
			callback.requestException(ex);
		}
	}
	
}
