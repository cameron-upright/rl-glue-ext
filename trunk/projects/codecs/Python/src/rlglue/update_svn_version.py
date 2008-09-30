import os,signal,time,sys
from subprocess import Popen,PIPE


def update_svn_version():
	svnVersionDir=os.path.abspath(os.path.dirname(__file__));
		# Get revision from svn command line on repository
	revision = 0
	try:
		output = Popen(["svnversion", "-n"], stdout=PIPE).communicate()[0]
		revisionNumber=output
		#Write it to a file
		svnVersionFileName=svnVersionDir+"/"+"get_svn_version.py";
		f = open(svnVersionFileName, 'w')
		f.write("#This fill was generated automatically.  It will be replaced automatically.  Don't change it.\n")
		f.write("def get_svn_glue_version():\n")
		f.write("\treturn \""+str(revisionNumber)+"\"\n")
		f.close()
	except:
		pass