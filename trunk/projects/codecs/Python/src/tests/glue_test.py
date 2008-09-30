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

# class TestUtility:
# 	def set_k_ints_in_abstract_type(self, the_struct,num_ints):
# 		the_struct.intArray[=]
# 		for(int i=0;i<num_ints;i++) the_struct.intArray[i]=i;
# 			}
# 			public static void set_k_doubles_in_abstract_type(RL_abstract_type the_struct, int num_doubles){
# 			        the_struct.doubleArray=new double[num_doubles];
# 				for(int i=0;i<num_doubles;i++) the_struct.doubleArray[i]=(double)i/(double)num_doubles;
# 			}
# 			public static void set_k_chars_in_abstract_type(RL_abstract_type the_struct, int num_chars){
# 			        the_struct.charArray=new char[num_chars];
# 				for(int i=0;i<num_chars;i++)
# 			            the_struct.charArray[i]=(char)(((int)'a')+i);
# 			}
# 
# 			public static void clean_abstract_type(RL_abstract_type the_struct){
# 			    the_struct.intArray=new int[0];
# 			    the_struct.doubleArray=new double[0];
# 			    the_struct.charArray=new char[0];
# 			}
