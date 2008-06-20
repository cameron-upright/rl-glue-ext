randomAgentStruct.init='randomAgent_init';
randomAgentStruct.start='randomAgent_start';
randomAgentStruct.step='randomAgent_step';
randomAgentStruct.end='randomAgent_end';
randomAgentStruct.message='randomAgent_message';
randomAgentStruct.cleanup='randomAgent_cleanup';
randomAgentStruct.freeze='randomAgent_freeze';
%Create a mapping of functions to names, to be passed into Java. This is
%so java knows the name of the functions to call for each of the
%rl-glue functions.
theAgentCell=agentCellFromStruct(randomAgentStruct);

theJavaAgent=org.rlcommunity.rlglue.ext.codecs.matlab.MatlabAgentCodec(theAgentCell);