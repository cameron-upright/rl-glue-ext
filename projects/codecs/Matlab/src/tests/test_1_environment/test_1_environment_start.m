function theObservation=test_1_environment_start()
    global test_1_environment_struct;
    test_1_environment_struct.stepCount=0;
    theObservation = org.rlcommunity.rlglue.codec.types.Observation();
	theObservation.intArray=[0];
	theObservation.doubleArray=[0/2 1/2];
	theObservation.charArray=['abc'];
end