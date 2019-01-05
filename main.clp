(deftemplate MAIN::equation
   (slot name (type SYMBOL))
   (multislot reactant-ratio (type INTEGER))
   (multislot reactants)
   (multislot product-ratio (type INTEGER))
   (multislot products)
   (multislot catalyzers)
   (multislot condition (allowed-values heating ignite))
   (slot checking-generation (type NUMBER) (default 0)))

(deftemplate MAIN::material
   (slot name (type STRING))
   (slot amount (type NUMBER) (default 0)))

(deftemplate MAIN::environment
   (slot checking-generation (type NUMBER))
   (multislot catalyzers (type STRING))
   (multislot action (type SYMBOL)))

(deftemplate MAIN::checking-process
   (slot equation (type SYMBOL))
   (multislot remainder (type STRING))
   (slot pending (allowed-values TRUE FALSE) (default TRUE))
   (slot ratio (type NUMBER) (default 100))
   (slot checking-generation (type NUMBER) (default 0))
   (slot reacted (allowed-values TRUE FALSE) (default FALSE)))

(deftemplate MAIN::reaction-process
   (slot equation (type SYMBOL))
   (multislot r-reactants (type STRING))
   (multislot r-products (type STRING))
   (slot ratio (type NUMBER))
   (slot checking-generation (type NUMBER)))

(deftemplate MAIN::action
   (slot target (default none)))

(deffacts MAIN::initial-bundle
   (environment (checking-generation 1) (catalyzers) (action)))

(defrule MAIN::start-checking
   (environment (checking-generation ?gen) (catalyzers $?catas) (action $?actions))
   (not (checking-process (checking-generation ?gen) (pending TRUE)))
   (not (checking-process (checking-generation ?gen) (remainder) (pending FALSE) (reacted FALSE)))
   (not (reaction-process (checking-generation ?gen)))
   (equation (name ?name) (reactants $?reactants) (catalyzers $?catalyzers&:(subsetp $?catalyzers $?catas)) (condition $?condition&:(subsetp $?condition $?actions)) (checking-generation ?equgen&:(< ?equgen ?gen)))
   =>
   (assert (checking-process (equation ?name) (remainder $?reactants) (checking-generation ?gen))))

(defrule MAIN::donot-start-checking
   (environment (checking-generation ?gen) (catalyzers $?catas) (action $?actions))
   ?equ <- (equation (catalyzers $?catalyzers) (condition $?condition) (checking-generation ?equgen&:(< ?equgen ?gen)))
   (not (and (subsetp $?catalyzers $?catas)
             (subsetp $?condition $?actions)))
   =>
   (modify ?equ (checking-generation ?gen)))

(defrule MAIN::checking-exist
   ?p <- (checking-process (equation ?equation) (remainder ?f $?remainder) (pending TRUE) (ratio ?ratio))
   (material (name ?f) (amount ?amount&:(> ?amount 0)))
   (equation (name ?equation) (reactants $?reactants) (reactant-ratio $?ratios))
   =>
   (bind ?x (/ ?amount (nth$ (member$ ?f $?reactants) $?ratios)))
   (modify ?p (remainder $?remainder) (ratio (min ?x ?ratio))))

(defrule MAIN::checking-nonexist
   ?p <- (checking-process (equation ?equation) (remainder ?f $?) (pending TRUE) (checking-generation ?gen))
   ?e <- (equation (name ?equation) (checking-generation ?equgen&:(neq ?equgen ?gen)))
   (not (material (name ?f) (amount ?amount&:(> ?amount 0))))
   =>
   (modify ?p (pending FALSE))
   (modify ?e (checking-generation ?gen)))

(defrule MAIN::end-checking
   ?p <- (checking-process (remainder) (pending TRUE))
   =>
   (modify ?p (pending FALSE)))

(defrule MAIN::clear-checking
   (environment (checking-generation ?envgen))
   ?cp <- (checking-process (checking-generation ?gen&:(< ?gen ?envgen)))
   =>
   (retract ?cp))

(defrule MAIN::start-reaction
   ?cp <- (checking-process (equation ?equation) (remainder) (pending FALSE) (ratio ?ratio) (checking-generation ?gen) (reacted FALSE))
   ?e <- (equation (name ?equation) (reactants $?reactants) (products $?products))
   (not (reaction-process (checking-generation ?gen)))
   =>
   (assert (reaction-process (equation ?equation) (r-reactants ?reactants) (r-products ?products) (ratio ?ratio) (checking-generation ?gen)))
   (modify ?e (checking-generation ?gen))
   (modify ?cp (reacted TRUE)))

(defrule MAIN::reaction-cost
   ?rp <- (reaction-process (equation ?equation) (r-reactants ?f $?remainder) (ratio ?ratio))
   ?m <- (material (name ?f) (amount ?amount))
   (equation (name ?equation) (reactant-ratio $?ratios) (reactants $?reactants))
   =>
   (modify ?m (amount (- ?amount (* ?ratio (nth$ (member ?f ?reactants) $?ratios)))))
   (modify ?rp (r-reactants ?remainder)))

(defrule MAIN::reaction-increase
   ?rp <- (reaction-process (equation ?equation) (r-products ?f $?remainder) (ratio ?ratio) (checking-generation ?gen))
   ?m <- (material (name ?f) (amount ?amount))
   (equation (name ?equation) (product-ratio $?ratios) (products $?products))
   =>
   (modify ?m (amount (+ ?amount (* ?ratio (nth$ (member ?f ?products) $?ratios)))))
   (modify ?rp (r-products ?remainder)))

(defrule MAIN::reaction-generate
   ?rp <- (reaction-process (equation ?equation) (r-products ?f $?remainder) (ratio ?ratio) (checking-generation ?gen))
   (not (material (name ?f)))
   (equation (name ?equation) (product-ratio $?ratios) (products $?products))
   =>
   (assert (material (name ?f) (amount (* ?ratio (nth$ (member ?f ?products) $?ratios)))))
   (modify ?rp (r-products ?remainder)))

(defrule MAIN::end-reaction
   ?rp <- (reaction-process (r-reactants) (r-products))
   =>
   (retract ?rp))

(defrule MAIN::auto-generation
   ?env <- (environment (checking-generation ?gen) (catalyzers $?catas) (action $?actions))
   (not (reaction-process))
   (checking-process (reacted TRUE) (checking-generation ?gen))
   (not (equation (checking-generation ?equgen&:(< ?equgen ?gen))))
   =>
   (modify ?env (checking-generation (+ ?gen 1))))

(defrule MAIN::default-action
   (not (action))
   =>
   (assert (action)))

(defrule MAIN::resolve
   (declare (salience -1))
   ?a <- (action (target ?target))
   =>
   (switch ?target
      (case addm then
         (printout t "MATERIAL> ")
         (assert (add-material (explode$ (readline)))))
      (case printm then
         (do-for-all-facts ((?m material))
            TRUE
            (printout t "m(" ?m:name ") = " ?m:amount " mol" crlf))
         (retract ?a))
      (case savem then
         (save-facts "material.dat" local material)
         (printout t "done" crlf)
         (retract ?a))
      (case loadm then
         (load-facts "material.dat")
         (retract ?a))
      (case run then
         (do-for-fact ((?e environment))
            TRUE
            (modify ?e (checking-generation (+ ?e:checking-generation 5))))
         (retract ?a))
      (case adde then
         (bind ?name (gensym*))
         (do-for-fact ((?e environment))
            TRUE
            (bind ?gen ?e:checking-generation))
         (printout t "REACTANTS> ")
         (bind ?r (explode$ (readline)))
         (printout t "REACTANT RATIOS> ")
         (bind ?r-r (explode$ (readline)))
         (printout t "PRODUCTS> ")
         (bind ?p (explode$ (readline)))
         (printout t "PRODUCT RATIOS> ")
         (bind ?r-p (explode$ (readline)))
         (printout t "CATALYZERS> ")
         (bind ?catalyzers (explode$ (readline)))
         (printout t "CONDITIONS> ")
         (bind ?condition (explode$ (readline)))
         (assert (equation (name ?name) (reactants ?r) (reactant-ratio ?r-r) (products ?p) (product-ratio ?r-p) (catalyzers ?catalyzers) (condition ?condition) (checking-generation ?gen)))
         (retract ?a))
      (case printe then
         (do-for-all-facts ((?e equation))
            TRUE
            (printout t ?e:reactants " * " ?e:reactant-ratio " = " ?e:products " * " ?e:product-ratio " ,catalyzed by " ?e:catalyzers " ,with " ?e:condition crlf))
         (retract ?a))
      (case env then
         (printout t "ACTION> ")
         (bind ?condition (explode$ (readline)))
         (printout t "CATALYZERS> ")
         (bind ?catalyzers (explode$ (readline)))
         (do-for-fact ((?e environment))
            TRUE
            (modify ?e (action ?condition) (catalyzers ?catalyzers)))
         (retract ?a))
      (case printenv then
         (do-for-fact ((?env environment))
            TRUE
            (printout t "action = " ?env:action crlf)
            (printout t "catalyzers = " ?env:catalyzers crlf))
         (retract ?a))
      (case saverule then
         (save "main.clp")
         (printout t "done" crlf)
         (retract ?a))
      (case halt then
         none)
      (case exit then
         (bind ?list (find-all-facts ((?equ equation))
            TRUE))
         (foreach ?equ ?list
            (modify ?equ (checking-generation 0)))
         (save-facts "equation.dat" local equation))
      (case help then
         (printout t "[help] - to view help information" crlf)
         (printout t "[addm] - to add materials to the laboratory" crlf)
         (printout t "[printm] - to view current materials in the laboratory" crlf)
         (printout t "[savem] - to save current material information to material.dat" crlf)
         (printout t "[loadm] - to load material information from material.dat" crlf)
         (printout t "[run] - to start reaction" crlf)
         (printout t "[adde] - to add chemical equation" crlf)
         (printout t "[env] - to change environment condition" crlf)
         (printout t "[printenv] - to printout environment information" crlf)
         (printout t "[halt] - to exit without saving" crlf)
         (printout t "[saverule] - to save the whole program structure into main.clp" crlf)
         (printout t "[exit] - to save the equation facts to equation.dat and exit" crlf)
         (retract ?a))
      (default 
         (printout t "USER> ")
         (modify ?a (target (read))))))

(defrule MAIN::material-exist
   ?a <- (action (target addm))
   ?am <- (add-material ?name ?amount)
   ?m <- (material (name ?name) (amount ?prev))
   =>
   (modify ?m (amount (+ ?prev ?amount)))
   (retract ?am ?a))

(defrule MAIN::material-nonexist
   ?a <- (action (target addm))
   ?am <- (add-material ?name ?amount)
   (not (material (name ?name)))
   =>
   (assert (material (name ?name) (amount ?amount)))
   (retract ?am ?a))

