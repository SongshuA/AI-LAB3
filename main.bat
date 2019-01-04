(clear)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; reaction part
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(deftemplate equation
    (slot name (type STRING))
    (multislot reactant-ratio (type INTEGER))
    (multislot reactants)
    (multislot product-ratio (type INTEGER))
    (multislot products)
    (multislot catalyzers)
    (multislot condition (allowed-values heating ignite))
    (slot checking-generation (type NUMBER) (default 0))
)

(deftemplate material
    (slot name (type STRING))
    (slot amount (type NUMBER) (default 0))
)


(deftemplate environment
    (slot checking-generation (type NUMBER))
    (multislot catalyzers (type STRING))
    (multislot action (type SYMBOL))
)



(deftemplate checking-process
    (slot equation)
    (multislot remainder (type STRING))
    (slot pending (allowed-values TRUE FALSE) (default TRUE))
    (slot ratio (type NUMBER) (default 100))
    (slot checking-generation (type NUMBER) (default 0))
    (slot reacted (allowed-values TRUE FALSE) (default FALSE))
)


(deftemplate reaction-process
    (slot equation (type STRING))
    (multislot r-reactants (type STRING))
    (multislot r-products (type STRING))
    (slot ratio (type NUMBER))
    (slot checking-generation (type NUMBER))
)


(defrule start-checking
    (environment (checking-generation ?gen) (catalyzers $?catas) (action $?actions))
    (not (checking-process (checking-generation ?gen) (pending TRUE)))
    (not (checking-process (checking-generation ?gen) (remainder) (pending FALSE) (reacted FALSE)))
    (not (reaction-process (checking-generation ?gen)))
    (equation
        (name ?name)
        (reactants $?reactants)
        (catalyzers $?catalyzers&:(subsetp $?catalyzers $?catas))
        (condition $?condition&:(subsetp $?condition $?actions))
        (checking-generation ?equgen&:(< ?equgen ?gen))
    )
    =>
    (assert (checking-process
        (equation ?name)
        (remainder $?reactants)
        (checking-generation ?gen)
    ))
)


(defrule checking-exist
    ?p <- (checking-process (equation ?equation) (remainder ?f $?remainder) (pending TRUE) (ratio ?ratio))
    (material (name ?f) (amount ?amount&:(> ?amount 0)))
    (equation (name ?equation) (reactants $?reactants) (reactant-ratio $?ratios))
    =>
    (bind ?x (/ ?amount (nth$ (member$ ?f $?reactants) $?ratios)))
    (modify ?p (remainder $?remainder) (ratio (min ?x ?ratio)))
)


(defrule checking-nonexist
    ?p <- (checking-process (equation ?equation) (remainder ?f $?) (pending TRUE) (checking-generation ?gen))
    ?e <- (equation (name ?equation) (checking-generation ?equgen&:(neq ?equgen ?gen)))
    (not (material (name ?f) (amount ?amount&:(> ?amount 0))))
    =>
    (modify ?p (pending FALSE))
    (modify ?e (checking-generation ?gen))
)


(defrule end-checking
    ?p <- (checking-process (remainder) (pending TRUE))
    =>
    (modify ?p (pending FALSE))
)


(defrule clear-checking
    (environment (checking-generation ?envgen))
    ?cp <- (checking-process (checking-generation ?gen&:(< ?gen ?envgen)))
    =>
    (retract ?cp)
)


(defrule start-reaction
    ?cp <- (checking-process (equation ?equation) (remainder) (pending FALSE) (ratio ?ratio) (checking-generation ?gen) (reacted FALSE))
    ?e <- (equation (name ?equation) (reactants $?reactants) (products $?products))
    (not (reaction-process (checking-generation ?gen)))
    =>
    (assert (reaction-process (equation ?equation) (r-reactants ?reactants) (r-products ?products) (ratio ?ratio) (checking-generation ?gen)))
    (modify ?e (checking-generation ?gen))
    (modify ?cp (reacted TRUE))
)

(defrule reaction-cost
    ?rp <- (reaction-process (equation ?equation) (r-reactants ?f $?remainder) (ratio ?ratio) (checking-generation ?gen))
    ?m <- (material (name ?f) (amount ?amount))
    (equation (name ?equation) (reactant-ratio $?ratios) (reactants $?reactants))
    =>
    (modify ?m (amount (- ?amount (* ?ratio (nth$ (member ?f ?reactants) $?ratios)))))
    (modify ?rp (r-reactants ?remainder))
)


(defrule reaction-increase
    ?rp <- (reaction-process (equation ?equation) (r-products ?f $?remainder) (ratio ?ratio) (checking-generation ?gen))
    ?m <- (material (name ?f) (amount ?amount))
    (equation (name ?equation) (product-ratio $?ratios) (products $?products))
    =>
    (modify ?m (amount (+ ?amount (* ?ratio (nth$ (member ?f ?products) $?ratios)))))
    (modify ?rp (r-products ?remainder))
)


(defrule reaction-generate
    ?rp <- (reaction-process (equation ?equation) (r-products ?f $?remainder) (ratio ?ratio) (checking-generation ?gen))
    (not (material (name ?f)))
    (equation (name ?equation) (product-ratio $?ratios) (products $?products))
    =>
    (assert (material (name ?f) (amount (* ?ratio (nth$ (member ?f ?products) $?ratios)))))
    (modify ?rp (r-products ?remainder))
)


(defrule end-reaction
    ?rp <- (reaction-process (r-reactants) (r-products))
    =>
    (retract ?rp)
)


(defrule auto-generation
    ?env <- (environment (checking-generation ?gen))
    (not (reaction-process))
    (checking-process (reacted TRUE) (checking-generation ?gen))
    (not (equation (checking-generation ?equgen&:(< ?equgen ?gen))))
    =>
    (modify ?env (checking-generation (+ ?gen 1)))
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; flow control part
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(deftemplate action
    (slot target (default none))
)


(defrule default-action
    (not (action))
    =>
    (assert (action))
)


(defrule resolve
    (declare (salience -1))
    ?a <- (action (target ?target))
    =>
    (switch ?target
        (case material then 
            (printout t "MATERIAL> ")
            (assert (add-material (explode$ (readline))))
        )
        
        (case print then 
            (do-for-all-facts ((?m material)) (= 1 1)
                (printout t "m(" ?m:name ") = " ?m:amount " mol" crlf)
            )
            (retract ?a)
        )

        (case run then
            (do-for-fact ((?e environment)) (= 1 1)
                (modify ?e (checking-generation (+ ?e:checking-generation 1)))
            )
            (retract ?a)
        )

        (case equation then
            (bind ?name (gensym))

            (do-for-fact ((?e environment)) (= 1 1)
                (bind ?gen ?e:checking-generation)
            )

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

            (assert (equation
                (name ?name)
                (reactants ?r)
                (reactant-ratio ?r-r)
                (products ?p)
                (product-ratio ?r-p)
                (catalyzers ?catalyzers)
                (condition ?condition)
                (checking-generation ?gen)
            ))
            (retract ?a)
        )

        (case save then 
            (printout t "PATH> ")
            (bind ?path (read))
            (save ?path)
            (retract ?a)
        )

        (case exit then none)

        (default
            (printout t "USER> ")
            (modify ?a (target (read)))
        )
    )
)



(defrule material-exist
    ?a <- (action (target material))
    ?am <- (add-material ?name ?amount)
    ?m <- (material (name ?name) (amount ?prev))
    =>
    (modify ?m (amount (+ ?prev ?amount)))
    (retract ?am ?a)
)

(defrule material-nonexist
    ?a <- (action (target material))
    ?am <- (add-material ?name ?amount)
    (not (material (name ?name)))
    =>
    (assert (material (name ?name) (amount ?amount)))
    (retract ?am ?a)
)


(deffacts initial-bundle
    (environment
        (checking-generation 1)
        (catalyzers)
        (action heating ignite)
    )
)

; (assert 
;     (environment
;         (checking-generation 1)
;         (catalyzers)
;         (action heating ignite)
;     )

;     (material
;         (name "C")
;         (amount 3)
;     )
;     (material
;         (name "S")
;         (amount 3)
;     )
;     (material
;         (name "O2")
;         (amount 2)
;     )

;     (equation
;         (name "E1")
;         (reactant-ratio 1 1)
;         (reactants "C" "O2")
;         (product-ratio 1)
;         (products "CO2")
;         (condition ignite)
;     )
;     (equation
;         (name "E2")
;         (reactant-ratio 1 1)
;         (reactants "S" "O2")
;         (product-ratio 1)
;         (products "SO2")
;         (condition ignite)
;     )
; )


(run)
