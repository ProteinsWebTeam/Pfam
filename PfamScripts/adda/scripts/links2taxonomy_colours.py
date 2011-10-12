"""add colours to a list of nids together with labels.

variation of links2colours.py: use taxonomic identifiers
Archea (blue), Bacteria (red), Eukarya (green)
"""


import sys,os,string,getopt

colors={ "archaea" :"red",
         "bacteria" : "blue",
         "eukaryota" : "green"}

param_default_colour = "white"

if __name__ == "__main__":

    try:
        optlist, args = getopt.getopt(sys.argv[1:],
                                      "m",
                                      ["multi_labels"])

    except getopt.error, msg:
        print USAGE
        sys.exit(2)

    while 1:

        line = sys.stdin.readline()
        if not line: break

        id, label = string.split(line[:-1], "\t")[:2]

        label = string.lower(label)

        if colors.has_key(label):
            color = colors[label]
        else:
            color = param_default_colour
            
        print "%s\t%s" % (id, color)




