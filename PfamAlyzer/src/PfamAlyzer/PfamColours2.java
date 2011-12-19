package PfamAlyzer;

import java.awt.Color;
import java.util.*;

abstract class PfamColours2 {
	private Vector names;
	private Vector colours;
	private Vector temporary;
	private static Color[] allColours;
	
	public PfamColours2() {
		names = new Vector();
		colours = new Vector();
		temporary = new Vector();
	}
	
	public int getColour(String id, int temp) {
		for (int i=0; i<names.size(); i++)
			if (((String)names.get(i)).equals(id))
				return ((Integer) colours.get(i)).intValue();
		boolean[] a = new boolean[allColours.length];
		for (int i=0; i<colours.size(); i++)
			a[((Integer) colours.get(i)).intValue()] = true;
		int i = 0;
		while (i<allColours.length && a[i]==true)
			i++;
		colours.add(new Integer(i));
		names.add(id);
		temporary.add(new Integer(temp));
		return i;
	}
	
	public void freeTemporary() {
		for (int i=0; i<names.size(); i++)
			if (((Integer)temporary.get(i)).intValue()==1) {
				names.remove(i);
				colours.remove(i);
				temporary.remove(i);
			}
	}
	
	public void freeID(String id) {
		for (int i=0; i<names.size(); i++)
			if (((String)names.get(i)).equals(id)) {
				names.remove(i);
				colours.remove(i);
				temporary.remove(i);
			}
	}
	
	public static Color getColor(int number) {
		if (allColours == null)
			return Color.darkGray; 
		if (number < allColours.length)
			return allColours[number];
		else
			return Color.darkGray; 
	}
	
}
