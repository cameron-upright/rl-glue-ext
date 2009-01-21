try:
    import psyco
    psyco.full()
except ImportError:
    pass

class TaskSpecParser:
    """
       RL-Glue TaskSpec Sparser V3
    """
    w = ["VERSION","PROBLEMTYPE","DISCOUNTFACTOR", "OBSERVATIONS","ACTIONS","REWARDS","EXTRA"]
    v = ["INTS","DOUBLES","CHARCOUNT"]

    def __init__(self,ts):
        self.ts = ts

    def getValue(self,i,ts,w):
        try:
            a = ts.index(w[i]) + len(w[i]) + 1
        except ValueError:
            raise AttributeError("Malformed TaskSpec String: could not find the "+w[i]+" keyword")
        b=None
        if (i+1)<len(w):
            try:
                b = ts.index(w[i+1])-1
            except ValueError:
                raise AttributeError("Malformed TaskSpec String: could not find the "+w[i+1]+" keyword")

        return ts[a:b]


    def getVersion(self):
        return self.getValue(0,self.ts,self.w)

    def getProblemType(self):
        return self.getValue(1,self.ts,self.w)

    def getDiscountFactor(self):
        return self.getValue(2,self.ts,self.w)

    def CompleteVars(self,str_in):
        """ forces the vars to have ints doubles and charcount
        """
        if self.v[0] not in str_in:
            str_in = self.v[0]+" (0 0 0) " + str_in
        if self.v[2] not in str_in:
            str_in+=self.v[2]+" 0 "
        if self.v[1] not in str_in:
            i = str_in.find(self.v[2])
            str_in= str_in[0:i]+self.v[1]+" (0 0 0) "+str_in[i:]
        return str_in

    def getObservations(self):
        str_o = self.getValue(3,self.ts,self.w)
        return self.CompleteVars(str_o)

    def getActions(self):
        str_a = self.getValue(4,self.ts,self.w)
        return self.CompleteVars(str_a)

    def getReward(self):
        return self.getValue(5,self.ts,self.w)

    def getExtra(self):
        return self.getValue(6,self.ts,self.w)

    def getRange(self,str_input):
        str_input = str_input.replace("UNSPEC","'UNSPEC'")
        str_input = str_input.replace("NEGINF","'NEGINF'")
        str_input = str_input.replace("POSINF","'POSINF'")
        str_input = str_input.replace(" ",",")
        r = eval(str_input)
        if len(r)==2:
            return [list(r)]

        out = r[0]*([[r[1],r[2]]])
        return out

    def getRewardRange(self):
        str_reward = self.getReward()
        return self.getRange(str_reward)

    def getVarInfoRange(self,i,ts,w):
        a = ts.index(w[i])
        b = ts.index(w[i+1])+1
        return ts[a:b]

    def GetVarValue(self,i,str_o):
        str_r = self.getValue(i,str_o,self.v)
        str_r = str_r.replace(") (",")#(")
        parts = str_r.split("#")
        obs=[]
        for p in parts:
            obs.extend(self.getRange(p))
        return obs

    def getIntObservations(self):
            return self.GetVarValue(0,self.getObservations())

    def getDoubleObservations(self):
            return self.GetVarValue(1,self.getObservations())

    def getCharCountObservations(self):
        str_o = self.getObservations()
        return int(self.getValue(2,str_o,self.v))

    def getIntActions(self):
        return self.GetVarValue(0,self.getActions())

    def getDoubleActions(self):
        return self.GetVarValue(1,self.getActions())

    def getCharCountActions(self):
        str_a = self.getActions()
        return int(self.getValue(2,str_a,self.v))


if __name__=="__main__":
    # you can cut the taskspec by the main words with new line
    ts= """VERSION RL-Glue-3.0 PROBLEMTYPE episodic DISCOUNTFACTOR 1 OBSERVATIONS INTS (3 0 1) DOUBLES (2 -1.2 0.5) (-.07 .07) CHARCOUNT 1024
         ACTIONS INTS (2 0 4) CHARCOUNT 1024 REWARDS (-5.0 UNSPEC) EXTRA some other stuff goes here"""
    print ts
    print
    print
    TaskSpec = TaskSpecVRLGLUE3(ts)
    print "Version: ["+TaskSpec.getVersion()+"]"
    print "ProblemType: ["+TaskSpec.getProblemType()+"]"
    print "DiscountFactor: ["+TaskSpec.getDiscountFactor()+"]"
    print "======================================================================================================="
    print "\t \t \t \t Observations"
    print "======================================================================================================="
    #print "Observations: ["+TaskSpec.getObservations()+"]"
    print "Integers:",TaskSpec.getIntObservations()
    print "Doubles: ",TaskSpec.getDoubleObservations()
    print "Chars:   ",TaskSpec.getCharCountObservations()
    print "======================================================================================================="
    print "======================================================================================================="
    print "\t \t \t \t Actions"
    print "======================================================================================================"
    #print "Observations: ["+TaskSpec.getActions()+"]"
    print "Integers:",TaskSpec.getIntActions()
    print "Doubles: ",TaskSpec.getDoubleActions()
    print "Chars:   ",TaskSpec.getCharCountActions()
    print "======================================================================================================="
    #print "Actions: ["+TaskSpec.getActions()+"]"
    #print "Reward :["+TaskSpec.getReward()+"]")
    print "Extra: ["+TaskSpec.getExtra()+"]"
    print "Reward Range:",TaskSpec.getRewardRange()



