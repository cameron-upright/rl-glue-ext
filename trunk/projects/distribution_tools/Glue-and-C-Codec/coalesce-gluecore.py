import sys

theDir="wiki/current";
theFiles=['RLGlueCore.wiki.header','RLGlueCore.wiki.fromsource','RLGlueCore.wiki.macbinary','RLGlueCore.wiki.windows','RLGlueCore.wiki.debian','RLGlueCore.wiki.footer'];

outfileName="wiki/current/RLGlueCore.wiki"
outfile = file(outfileName, 'w')

for whichFile in theFiles:
	f = file(theDir+'/'+whichFile)
	for line in f:
		outfile.writelines(line)
