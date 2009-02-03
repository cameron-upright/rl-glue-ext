import sys


theVersion=sys.argv[1]
theLink=sys.argv[2]

fileName="Matlab.wiki.template"
outfileName="Matlab.wiki"

subs={}
subs['MATLAB-USER-DEV-VERSION']=theVersion
subs['MATLAB-USER-DEV-LINK']=theLink

f = file(fileName)
newlines = []
for line in f:
	for key,value in subs.iteritems():
		if key in line:
			line=line.replace(key,value)
	newlines.append(line)

outfile = file(outfileName, 'w')
outfile.writelines(newlines)