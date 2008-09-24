function reloadPath()
    clear java;javaaddpath('/Users/btanner/Documents/Java-Projects/rl-glue-ext/projects/codecs/Java/products/JavaRLGlueCodec.jar')
    addpath(sprintf('%s%s',pwd(),'/src/glue'));
    addpath(sprintf('%s%s',pwd(),'/src/agent'));
    addpath(sprintf('%s%s',pwd(),'/src/environment'));
end
