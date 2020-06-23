BOTH    agda2-solveOne                  agda2-maybe-normalised 
        agda2-solveAll
LOCAL   agda2-give                      agda2-goal-cmd                      want='goal
LOCAL   agda2-elaborate-give            agda2-maybe-normalised              want="expression to elaborate and give"
LOCAL   agda2-refine                    agda2-goal-cmd                      want='goal
BOTH    agda2-autoOne                   agda2-goal-cmd                      want='goal
        agda2-autoAll                   agda2-go                            
LOCAL   agda2-make-case                 agda2-goal-cmd                      want="pattern variables to case (empty for split on result)"
LOCAL   agda2-goal-type                 agda2-maybe-normalised              want=nil
LOCAL   agda2-show-context              agda2-maybe-normalised              want=nil
LOCAL   agda2-helper-function-type      agda2-maybe-normalised-asis         prompt="Expression"
BOTH    agda2-infer-type                agda2-maybe-normalised              want="expression to type"
        agda2-infer-type-toplevel       agda2-maybe-normalised-toplevel     prompt="Expression"
BOTH    agda2-why-in-scope              agda2-goal-cmd                      save=nil want="Name"
        agda2-why-in-scope-toplevel     agda2-go    
LOCAL   agda2-goal-and-context          agda2-maybe-normalised              want=nil 
LOCAL   agda2-goal-and-context-and-inferred     agda2-maybe-normalised      want=nil 
LOCAL   agda2-goal-and-context-and-checked      agda2-maybe-normalised      want=nil 
BOTH    agda2-search-about-toplevel     agda2-maybe-normalised-toplevel     prompt="Name"
BOTH    agda2-module-contents           agda2-maybe-normalised              want="Module name"
        agda2-module-contents-toplevel  agda2-maybe-normalised-toplevel     prompt="Module name"
BOTH    agda2-compute-normalised        agda2-goal-cmd                      save=nil want="expression to normalise"
        agda2-compute-normalised-toplevel       agda2-go

(cmd save &optional want ask &rest args)