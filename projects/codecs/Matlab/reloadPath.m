function reloadPath(pathToUse)
%     clear java
%     
%     defaultJarExists=2;
%     %by default we look into ../Java/products/JavaRLGlueCodec.jar   
%     defaultJavaCodecPath=sprintf('%s/../Java/products/JavaRLGlueCodec.jar',pwd);
%     javaExistCommand=sprintf('exist %s',defaultJavaCodecPath);
%     
%     defaultJarExistsCode=eval(javaExistCommand);
%     if defaultJarExistsCode == 2
%         defaultJarExists=true;
%         
%     javaaddpath(JavaCodecPath);
    addpath(sprintf('%s%s',pwd(),'/src'));
    addpath(sprintf('%s%s',pwd(),'/src/glue'));
    addpath(sprintf('%s%s',pwd(),'/src/agent'));
    addpath(sprintf('%s%s',pwd(),'/src/environment'));
end
