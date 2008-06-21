function Reward_observation=mountainCar_step(theAction)
global mountainCar__internalState;


action = theAction.intArray(1);

%read in the state
position = mountainCar__internalState.theState(1);
velocity = mountainCar__internalState.theState(2);

%update velocity
velocity = velocity +((double(action) - 1)) * 0.001 + cos(3*position) * (-0.0025);

if (velocity > mountainCar__internalState.maxVel)
    velocity = mountainCar__internalState.maxVel;
end
if (velocity < mountainCar__internalState.minVel)
    velocity = mountainCar__internalState.minVel;
end

%update position
position = position + velocity;
 if (position > mountainCar__internalState.maxPos)
    position = mountainCar__internalState.maxPos;
end
if (position < mountainCar__internalState.minPos) 
    position = mountainCar__internalState.minPos;
end
if (position == mountainCar__internalState.minPos)
    if(velocity < 0) 
        velocity = 0;
    end
end

mountainCar__internalState.theState(1) = position;
mountainCar__internalState.theState(2) = velocity;

%Sensible defaults
theReward=-1;
terminal=0;

%If we finished the episode
if(position > mountainCar__internalState.goalPos)
    theReward = 0;
    terminal = 1;
end


Observation.intArray=[];
Observation.doubleArray=[mountainCar__internalState.theState(1) mountainCar__internalState.theState(2)];

Reward_observation.Observation=Observation;
Reward_observation.reward=theReward;
Reward_observation.terminal=terminal;

end



