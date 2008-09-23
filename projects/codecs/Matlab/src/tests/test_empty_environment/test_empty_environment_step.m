function rewardObservation=test_empty_environment_step(theAction)
    global test_empty_environment_struct;

	rewardObservation=org.rlcommunity.rlglue.codec.types.Reward_observation();

	if mod(test_empty_environment_struct.whichEpisode,2) == 0
		rewardObservation.o=test_empty_environment_struct.emptyObservation;
	else
		rewardObservation.o=test_empty_environment_struct.nonEmptyObservation;
	end
end
	