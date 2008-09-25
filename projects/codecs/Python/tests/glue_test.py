class glue_test:
	callerName = "not set"
	failCount = 0
	testCount = 0
	def __init__(self, callerName):
		self.callerName = callerName
		self.failCount = 0
		self.testCount = 0
		
	def check_fail(self, didFail):
		self.testCount=self.testCount+1
		if didFail:
			self.failCount=self.failCount+1
			print "Failed test "+str(self.testCount)
			
	def getFailCount(self):
		return self.failCount
		
	def get_summary(self):
		if self.failCount>0:
			return "Failed "+str(self.failCount)+" / "+str(self.testCount)+" in "+self.callerName;
		else:
	   		return "Passed all "+str(self.testCount)+" checks in "+self.callerName;
	    
