function Reward_observation=mountainCar_step(intAction, doubleAction)

global action theState minVel minPos maxVel maxPos goalPos;
action = intAction(1);

if(action > 2 || action < 0)
    action = int32(rand()*3);
end
%read in the state
position = theState(1);
velocity = theState(2);

%update velocity
velocity = velocity +((double(action) - 1)) * 0.001 + cos(3*position) * (-0.0025);

if (velocity > maxVel)
    velocity = maxVel;
end
if (velocity < minVel)
    velocity = minVel;
end

%update position
position = position + velocity;
 if (position > maxPos)
    position = maxPos;
end
if (position < minPos) 
    position = minPos;
end
if (position == minPos)
    if(velocity < 0) 
        velocity = 0;
    end
end

theState(1) = position;
theState(2) = velocity;

Reward_observation{1} = int32([]);
Reward_observation{2} = [theState(1) theState(2)];

if(position > goalPos)
    Reward_observation{3} = 0;
else
    Reward_observation{3} = -1.0;
end

