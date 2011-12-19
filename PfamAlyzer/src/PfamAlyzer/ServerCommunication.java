package PfamAlyzer;

/**
 * Insert the type's description here.
 * Creation date: (2001-06-13 13.43.58)
 * @author: 
 */

import java.net.*;
import java.io.*;


public class ServerCommunication {
	/**
	 * Insert the method's description here.
	 * Creation date: (2001-06-13 13.45.54)
	 */
	
	private String baseURL;
	/* private String pfamServerScriptPrefix; */
	
	public ServerCommunication(String aBaseURL) {
		baseURL = aBaseURL;
		/* pfamServerScriptPrefix = "/pfamalyserbackend/"; */
	}

	/* public BufferedReader doRequest(String script, String request) throws IOException{ */

	/*     return doRequest (script, request, pfamServerScriptPrefix); */

	/* } */
	
    /* public BufferedReader doRequest(String script, String request, String prefix) throws IOException{ */
    public BufferedReader doRequest(String script, String request ) throws IOException{
		InputStreamReader in;

		/* String s = baseURL+prefix+script; */
    String s = baseURL + script;

		if (request == null) {
			URL url = new URL(s);
			in = new InputStreamReader(url.openStream());
		}
		else {
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
			in = new InputStreamReader(uc.getInputStream());	
		}
				
		return new BufferedReader(in);
	}
	
	public String doUnbufferedRequest(String script, String request) throws IOException{

	    return doUnbufferedRequest (script, request );

	}
	
    public String doUnbufferedRequest(String script, String request, String prefix) throws IOException{

		/* String s = baseURL+prefix+script; */
		String s = baseURL + script;
		
		int bufferlength = 255;
		char [] buffer  = new char [bufferlength];
		
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
		String result = "";
		int num_read = 0;
		while (num_read !=-1) {
			num_read=in.read(buffer, 0, bufferlength);
			if (num_read>0)
				result += new String(buffer, 0, num_read);
		}
		in.close();
		
		return result;
	}
	
}
