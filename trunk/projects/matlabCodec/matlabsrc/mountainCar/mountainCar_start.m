function Observation=mountainCar_start()
    %theState(1) = position, theState(2) = velocity
    global mountainCar__internalState;
    mountainCar__internalState.theState(1) = -0.5;
    mountainCar__internalState.theState(2) = 0;
    
    Observation.intArray=[];
    Observation.doubleArray=[mountainCar__internalState.theState(1) mountainCar__internalState.theState(2)];
end

