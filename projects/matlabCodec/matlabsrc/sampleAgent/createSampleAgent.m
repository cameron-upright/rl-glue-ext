sampleAgentStruct.init='sampleAgent_init';
sampleAgentStruct.start='sampleAgent_start';
sampleAgentStruct.step='sampleAgent_step';
sampleAgentStruct.end='sampleAgent_end';
%Do this for all of the agent functions

theAgentCell=agentCellFromStruct(sampleAgentStruct);

theJavaAgent=org.rlcommunity.rlglue.ext.codecs.matlab.MatlabCodec(theAgentCell);