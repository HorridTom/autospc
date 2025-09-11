get_algorithm_flow_chart_string <- function() {
  
  diagram_string <- "
      digraph {
      
  # graph attributes
  graph [overlap = true]
  rankdir=TB

  # node attributes
  node [shape = box,
        fontname = Helvetica,
        color = black]

  # edge attributes
  edge [color = black]

  # node statements
  A [label = '@@1', width = 3, group = c];
  B [shape = diamond, label = '@@2', width = 3, group = c];
  C [label = '@@3', width = 1];
  D [shape = diamond, label = '@@4', width = 3, group = c];
  E [shape = diamond, label = '@@5', width = 3, group = c];
  F [label = '@@6', width = 3, group = c];
  G [shape = diamond, label = '@@7', width = 3, group = c];
  H [label = '@@8', width = 3, group = c];
  I [shape = diamond, label = '@@9', width = 3, group = c];
  J [label = '@@10', width = 3, group = c];
  K [shape = diamond, label = '@@11', width = 3, group = c];
  L [label = '@@12', width = 3];
  M [label = '@@13', width = 3];
  N [label = '@@14', width = 3]

  # edge statements
  A -> B;
  B -> C [label = 'N (2b)']; B -> D [label = 'Y (2a)'];
  D -> C [label = 'N (3b)']; D -> E [label = 'Y (3a)'];
  E -> C [label = 'N (4b)']; E -> F [label = 'Y (4a)'];
  F -> G;
  G -> C [label = 'N (5b)']; G -> H [label = 'Y (5a)'];
  H -> I;
  I -> C [label = 'N (6b)']; I -> J [label = 'Y (6a)'];
  J -> K;
  K -> L [label = 'N (7a)']; K -> M [label = 'Y (7b)'];
  L -> N;
  M -> N;
  N -> D
  
  {rank = same; L; M}
  
      }
  
   [1]: paste0('(1) Initialise Algorithm\\n','Counter = 1')
   [2]: paste0('(2) Sufficient data for\\n', 'at least one period?')
   [3]: paste0('End')
   [4]: paste0('(3) Loop while\\n', 'remaining data')
   [5]: paste0('(4) Sufficient data remaining\\n', 'to form a new period?')
   [6]: paste0('Identify subsequent\\n', 'rule-breaking runs')
   [7]: paste0('(5) Is there at least\\n','one subsequent rule break?')
   [8]: paste0('Set counter to next rule break\\n','and record its direction')
   [9]: paste0('(6) Sufficient data remaining\\n', 'to form a new period?')
  [10]: paste0('Establish candidate limits')
  [11]: paste0('(7) Is there an opposing rule break\\n','or does the final run prevent\\n','establishment of new limits?')
  [12]: paste0('Accept candidate limits:\\n','re-establish limits at the counter')
  [13]: paste0('Reject candidate limits')
  [14]: paste0('Set counter to next\\n','rule-breaking run')
  "
  
  return(diagram_string)
  
}


get_log_explanation_table <- function() {
  
  log_exp_tab <- tibble::tribble(
    ~algorithm_step, ~shorthand, ~explanation,
    "1"            , "0100"    , "Initialise algorithm. Counter = 1.",
    "2"            , "02xx"    , paste0("Check whether there are sufficient",
                                        " data to form at least one set of",
                                        " limits."),
    ""             , ""        , "x = 00: Yes",
    ""             , ""        , "x = 10: No",
    "3"            , "0300"    , "Main algorithm loop begins.",
    "4"            , "04xxyy"  , paste0("Check whether there are sufficient",
                                        " data to proceed (Yes/No), and find",
                                        " subsequent rule breaks."),
    ""             , ""        , paste0("xx = 00: Yes - next rule break within",
                                        " current run"),
    ""             , ""        , paste0("xx = 01: Yes - next rule break",
                                        " beyond current run"),
    ""             , ""        , "xx = 10: No",
    ""             , ""        , "yy = position of next rule break.",
    "5"            , "05xxyy"  , paste0("Check whether there are any",
                                        " subsequent rule breaks."),
    ""             , ""        ,  "xx = 00: Yes",
    ""             , ""        ,  "xx = 10: No",
    ""             , ""        ,  "yy = 01: Next rule break downwards",
    ""             , ""        ,  "yy = 10: Next rule break upwards",
    "6"            , "06xxyz"  , paste0("Check whether there are sufficient",
                                        " data to proceed."),
    ""             , ""        , "xx = 00: Yes",
    ""             , ""        , "xx =10: No",
    ""             , ""        , "Examine candidate limits.",
    ""             , ""        , "y = 0: No opposing rule break",
    ""             , ""        , "y = 1: At least one opposing rule break.",
    ""             , ""        , paste0("z = 0: Final run does not prevent",
                                        " re-establishment of limits"),
    ""             , ""        , paste0("z = 1: Final run prevents",
                                        " re-establishment of limits."),
    "7"            , "07xx"    , paste0("Decide whether to re-establish",
                                        " limits. xx = 00: Yes, xx = 10: No.")
  )
  
}
