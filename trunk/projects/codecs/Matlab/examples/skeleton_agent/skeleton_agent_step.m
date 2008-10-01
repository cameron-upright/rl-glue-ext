function theAction=skeleton_agent_step(theReward, theObservation)
	%This is a persistent struct we will use to store things
	%that we want to keep around
	    global skeleton_agent_struct;

	    theAction = org.rlcommunity.rlglue.codec.types.Action();
		theAction.intArray=[round(rand(1))];

		%Make copies (using Java methods) of the observation and action
		%Store in our persistent struct
		skeleton_agent_struct.lastAction=theAction.duplicate();
		skeleton_agent_struct.lastObservation=theObservation.duplicate();
end