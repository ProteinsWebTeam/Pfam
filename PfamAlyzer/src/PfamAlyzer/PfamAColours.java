package PfamAlyzer;

import java.awt.Color;
import java.util.*;

class PfamAColours 
{
	protected Vector names;
	protected Vector colours;
	protected Vector temporary;
	private static Color allColours[] = {new Color(78, 137, 200), new Color(150, 178, 221), new Color(76, 180, 231), new Color(89, 180, 190),
			new Color(200, 204, 67), new Color(242, 213, 121), new Color(213, 173, 134),  new Color(204, 94, 69), new Color(63, 186, 141), 
			new Color(192, 133, 164), new Color(160, 150, 191), new Color(199, 76, 94), new Color(81, 143, 237), new Color(86, 195, 104),
			new Color(217, 164, 61), new Color(192, 200, 68), new Color(0, 103, 180), new Color(255, 155, 0), new Color(179, 170, 236), 
			new Color(250, 202, 87), new Color(157, 93, 166), new Color(192, 156, 201), new Color(180, 32, 192), new Color(31, 192, 31),
			new Color(192, 15, 15), new Color(189, 192, 0), new Color(192, 8, 174), new Color(0, 186, 192), new Color(132, 132, 192), 
			new Color(147, 192, 144), new Color(192, 175, 146), new Color(133, 133, 230), new Color(142, 37, 17), new Color(242, 146, 66),
			new Color(0, 135, 0), new Color(255, 135, 250), new Color(235, 235, 48),new Color(0, 100, 244), new Color(69, 69, 69), new Color(255, 135, 164)};

public PfamAColours() {
	names = new Vector();
	colours = new Vector();
	temporary = new Vector();
}

public int getColour(String id, int temp) {
	for (int i=0; i<names.size(); i++)
		if (((String)names.get(i)).equals(id))
				return ((Integer) colours.get(i)).intValue();
	boolean[] a = new boolean[allColours.length+1];
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

public static Color getBrighter(Color change) 
{
	Color brighterColor;
	double red = (double) change.getRed();
	double green = (double) change.getGreen();
	double blue = (double) change.getBlue();
	red += red/20;
	green += green/20;
	blue += blue/20;
	red = (red > 255) ? 255 : red;
	green = (green > 255) ? 255 : green;
	blue = (blue > 255) ? 255 : blue;
	brighterColor = new Color((int) red,(int) green,(int) blue);
	return brighterColor;
}

public static Color getColour(int number)
{
	if (number >= allColours.length)
		return Color.darkGray;
	return allColours[number];
}

public static Color getDarker(Color change) 
{
	Color darkerColor;
	double red = (double) change.getRed();
	double green = (double) change.getGreen();
	double blue = (double) change.getBlue();
	red -= red/25;
	green -= green/25;
	blue -= blue/25;
	red = (red < 0) ? 0 : red;
	green = (green < 0) ? 0 : green;
	blue = (blue < 0) ? 0 : blue;
	darkerColor = new Color((int) red,(int) green,(int) blue);
	return darkerColor;
}
}
