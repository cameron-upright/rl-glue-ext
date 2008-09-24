function disconnectGlue()
    global p__rlglueStruct;
    
   
    if(isfield(p__rlglueStruct,'network'))
       p__rlglueStruct.network.close();
       p__rlglueStruct=rmfield(p__rlglueStruct,'network');
    end
end