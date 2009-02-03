import sys


theVersion=sys.argv[1]
uploadedFileBase=sys.argv[2]
uploadedFileName=uploadedFileBase+".tar.gz"
theLink="http://rl-glue-ext.googlecode.com/files/"+uploadedFileName
detailsLink="http://code.google.com/p/rl-glue-ext/downloads/detail?name="+uploadedFileName

fileName="Lisp.wiki.template"
outfileName="Lisp.wiki"

subs={}
subs['LISP-USER-DEV-VERSION']=theVersion
subs['LISP-USER-DEV-LINK']=theLink
subs['LISP-USER-DEV-DETAILS-LINK']=detailsLink
subs['LISP-USER-DEV-FILE-BASE']=uploadedFileBase
f = file(fileName)
newlines = []
for line in f:
	for key,value in subs.iteritems():
		if key in line:
			line=line.replace(key,value)
	newlines.append(line)

outfile = file(outfileName, 'w')
outfile.writelines(newlines)