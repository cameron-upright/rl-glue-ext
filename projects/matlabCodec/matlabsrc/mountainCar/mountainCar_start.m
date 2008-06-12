function Observation=mountainCar_start()
    %theState(1) = position, theState(2) = velocity
    global theState;
    theState(1) = -0.5;
    theState(2) = 0;
    
    Observation{1} = int32([]);
    Observation{2} = [theState(1) theState(2)];
end

