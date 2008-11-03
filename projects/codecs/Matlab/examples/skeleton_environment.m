function theEnvironment=skeleton_environment()
%Assign members of the returning struct to be function pointers
	theEnvironment.env_init=@skeleton_environment_init;
	theEnvironment.env_start=@skeleton_environment_start;
	theEnvironment.env_step=@skeleton_environment_step;
	theEnvironment.env_load_state=@skeleton_environment_load_state;
	theEnvironment.env_save_state=@skeleton_environment_save_state;
	theEnvironment.env_load_random_seed=@skeleton_environment_load_random_seed;
	theEnvironment.env_save_random_seed=@skeleton_environment_save_random_seed;
	theEnvironment.env_cleanup=@skeleton_environment_cleanup;
	theEnvironment.env_message=@skeleton_environment_message;
end

	% This is a very simple environment with discrete observations corresponding to states labeled {0,1,...,19,20}
	%     The starting state is 10.
	% 
	%     There are 2 actions = {0,1}.  0 decrements the state, 1 increments the state.
	% 
	%     The problem is episodic, ending when state 0 or 20 is reached, giving reward -1 or +1, respectively.  The reward is 0 on 
	%     all other steps.
function taskSpec=skeleton_environment_init()
	global skeleton_environment_struct;
	skeleton_environment_struct.currentState=10;
    taskSpec='2:e:1_[i]_[0,20]:1_[i]_[0,1]:[-1,1]';
end    

function theObservation=skeleton_environment_start()
	global skeleton_environment_struct;
	skeleton_environment_struct.currentState=10;
	
	theObservation = org.rlcommunity.rlglue.codec.types.Observation();
	theObservation.intArray=[skeleton_environment_struct.currentState];
end

function rewardObservation=skeleton_environment_step(thisAction)
global skeleton_environment_struct;

	episodeOver=0;
	theReward=0;
	
	if thisAction.intArray(1)==0
		skeleton_environment_struct.currentState=skeleton_environment_struct.currentState-1;
	end

	if thisAction.intArray(1)==1
		skeleton_environment_struct.currentState=skeleton_environment_struct.currentState+1;
	end
	
	if skeleton_environment_struct.currentState <=0
		skeleton_environment_struct.currentState=0;
		theReward=-1;
		episodeOver=1;
	end

	if skeleton_environment_struct.currentState >=20
		skeleton_environment_struct.currentState=20;
		theReward=1;
		episodeOver=1;
	end

	theObservation = org.rlcommunity.rlglue.codec.types.Observation();
	theObservation.intArray=[skeleton_environment_struct.currentState];


	rewardObservation=org.rlcommunity.rlglue.codec.types.Reward_observation_terminal(theReward,theObservation,episodeOver);
end

function returnMessage=skeleton_environment_message(theMessageJavaObject)
%Java strings are objects, and we want a Matlab string
	inMessage=char(theMessageJavaObject);

	if strcmp(inMessage,'what is your name?')==1
		returnMessage='my name is skeleton_environment, Matlab edition!';
	else
		returnMessage='I don\''t know how to respond to your message';
    end
end

function skeleton_environment_cleanup()
	global skeleton_environment_struct;
	skeleton_environment_struct=rmfield(skeleton_environment_struct,'currentState');
	clear skeleton_environment_struct;

end

function randomSeed=skeleton_environment_save_random_seed()
%Not really implemented because of advanced nature and example is 
%supposed to be simple
	randomSeed=org.rlcommunity.rlglue.codec.types.Random_seed_key();
end

function skeleton_environment_load_random_seed(randomSeed)
	%Not really implemented because of advanced nature and example is 
	%supposed to be simple
end

function theState=skeleton_environment_save_state()
	%Not really implemented because of advanced nature and example is 
	%supposed to be simple
	theState=org.rlcommunity.rlglue.codec.types.State_key();
end

function skeleton_environment_load_seed(theState)
	%Not really implemented because of advanced nature and example is 
	%supposed to be simple
end