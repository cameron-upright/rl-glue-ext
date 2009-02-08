import sys

theDir="wiki/current";
theFiles=['CandCPP.wiki.header','CandCPP.wiki.fromsource','RLGlueCore.wiki.macbinary','CandCPP.wiki.footer'];

outfileName="wiki/current/CandCPP.wiki"
outfile = file(outfileName, 'w')

for whichFile in theFiles:
	f = file(theDir+'/'+whichFile)
	for line in f:
		outfile.writelines(line)
