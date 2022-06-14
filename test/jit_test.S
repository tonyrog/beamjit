{module, jit_test}.  %% version = 0

{exports, [{add,2}]}.

{attributes, []}.

{labels, 2}.

{function, add, 2, 2}.
  {label,1}.
    {line,[]}.
    {func_info,{atom,jit_test},{atom,add},2}.
  {label,2}.
    {gc_bif,'+',{f,0},2,[{x,0},{x,1}],{x,0}}.
    return.
