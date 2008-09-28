# LaTeX2HTML 2002-2-1 (1.71)
# Associate internals original text with physical files.


$key = q/sec:custom-flags/;
$ref_files{$key} = "$dir".q|doc.html|; 
$noresave{$key} = "$nosave";

$key = q/sec:agent/;
$ref_files{$key} = "$dir".q|doc.html|; 
$noresave{$key} = "$nosave";

$key = q/sec:structure-types/;
$ref_files{$key} = "$dir".q|doc.html|; 
$noresave{$key} = "$nosave";

1;

