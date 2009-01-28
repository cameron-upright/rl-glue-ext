%
%  This function will add the Matlab Codec functions into the path so that
%  you can call them directly.
%
function reloadPath()
    addpath(sprintf('%s%s',pwd(),'/src'));
    addpath(sprintf('%s%s',pwd(),'/src/glue'));
    addpath(sprintf('%s%s',pwd(),'/src/agent'));
    addpath(sprintf('%s%s',pwd(),'/src/environment'));
end
