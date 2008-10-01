function disconnectGlue()
    global p__rlglueStruct;
    
   
    if(isfield(p__rlglueStruct,'network'))
        %We do this in case network is null so that we still rm the field
       networkHolder=p__rlglueStruct.network;
       p__rlglueStruct=rmfield(p__rlglueStruct,'network');
       
      networkHolder.close();
    end
end