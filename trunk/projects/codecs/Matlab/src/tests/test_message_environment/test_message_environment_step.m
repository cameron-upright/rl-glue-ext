function rewardObservation=test_message_environment_step(theAction)
    global test_message_environment_struct;

	rewardObservation=org.rlcommunity.rlglue.codec.types.Reward_observation();
	rewardObservation.o=test_message_environment_struct.emptyObservation;
end
	