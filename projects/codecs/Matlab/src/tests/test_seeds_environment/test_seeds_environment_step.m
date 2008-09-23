function rewardObservation=test_seeds_environment_step(theAction)
    global test_seeds_environment_struct;

	rewardObservation=org.rlcommunity.rlglue.codec.types.Reward_observation();
	rewardObservation.o=test_seeds_environment_struct.emptyObservation;
end
	