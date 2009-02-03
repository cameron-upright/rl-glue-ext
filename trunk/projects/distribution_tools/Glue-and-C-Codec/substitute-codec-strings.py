import sys


theVersion=sys.argv[1]
uploadedDevFileBase=sys.argv[2]
uploadedDevFileName=uploadedDevFileBase+".tar.gz"
uploadedMacFileBase=sys.argv[3]

devLink="http://rl-glue-ext.googlecode.com/files/"+uploadedDevFileName
devDetailsLink="http://code.google.com/p/rl-glue-ext/downloads/detail?name="+uploadedDevFileName

macLink="http://rl-glue-ext.googlecode.com/files/"+uploadedMacFileBase
macDetailsLink="http://code.google.com/p/rl-glue-ext/downloads/detail?name="+uploadedMacFileBase

fileName="CandCPP.wiki.template"
outfileName="CandCPP.wiki"

subs={}
subs['CODEC-DEV-VERSION']=theVersion
subs['CODEC-DEV-LINK']=devLink
subs['CODEC-DEV-DETAILS-LINK']=devDetailsLink
subs['CODEC-DEV-FILE-BASE']=uploadedDevFileBase
subs['MAC-LINK']=macLink
subs['MAC-DETAILS-LINK']=macDetailsLink
f = file(fileName)
newlines = []
for line in f:
	for key,value in subs.iteritems():
		if key in line:
			line=line.replace(key,value)
	newlines.append(line)

outfile = file(outfileName, 'w')
outfile.writelines(newlines)