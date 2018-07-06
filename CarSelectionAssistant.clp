;Alias:Jasmine
(deftemplate question
    (slot text)
    (slot type)
    (slot x))

(deftemplate answer
    (slot x)
    (slot text))

(deftemplate recommendation
    (slot car)
    (slot description))

(deffacts queinfo
    (question (text "What is the price range? (1)<35,000 (2)35,001-50,000 (3)50,000+
            (1/2/3)") 
        (type number)
        (x price))
    (question (text "What is the primary model intended for the car? sedan/suv/sports/pickup")
        (type string)
        (x model))
    (question (text "What is your higher priority? (1)Gas milege (2) Resale value? 1/2")
        (type number)
        (x sedanchoice))
    (question (text "What is your higher prioroty? (1)Seating(7 seater) (2)Drive(All wheel)? 1/2")
        (type number)
        (x suvchoice))
    (question (text "What is your higher priority? (1)Transmission(Manual) (2)Luxary 1/2")
        (type number)
        (x sportschoice))
        (question (text "What is your higher priority? (1)Single Cab (2)Double Cab 1/2")
        (type number)
        (x pickupchoice))
    )

(defmodule ask)

(deffunction typeChecking (?answer ?type)
    (if (eq ?type yes-no) then
        (return (or (eq ?answer yes) (eq ?answer no)))
     elif (eq ?type number) then
        (return (numberp ?answer))
     else (return (> (str-length ?answer)0))))

(deffunction askmodelr (?question ?type)
    (bind ?answer "")
    (while (not (typeChecking ?answer ?type)) do
        (printout t ?question " ")
        (if (eq ?type yes-no) then
            (printout t "(yes or no)"))
        (bind ?answer (read)))
    (return ?answer))

(defrule ask::ask-question-by-id
    (declare (auto-focus TRUE))
    (MAIN::question (x ?id) (text ?text) (type ?type))
    (not (MAIN::answer (x ?id)))
    ?ask <- (MAIN::ask ?id)
    =>
    (bind ?answer (askmodelr ?text ?type))
    (assert (answer (x ?id) (text ?answer)))
    (retract ?ask)
    (return))

(defmodule interview)

(defrule price
    =>
    (assert (ask price)))

(defrule badchoiceprice
    ?choice <- (answer (x price) (text ?cs&~1&~2&~3))
    =>
    (retract ?choice)
    (assert (ask price))
    (printout t "You have chosen a wrong selection. chose 1/2/3"))

(defrule request-model
    =>
    (assert (ask model)))

(defrule badchoice2
    ?choice <- (answer (x model) (text ?cs&~sedan&~suv&~sports&~pickup))
    =>
    (retract ?choice)
    (assert (ask model))
    (printout t "You have chosen a wrong selection. chose sedan/suv/sports/pickup"))

(defrule request-sedanchoice
    (answer (x model) (text ?cc&:(eq ?cc sedan)))
    =>
    (assert(ask sedanchoice)))

(defrule badchoicesuvchoice
    ?choice <- (answer (x sedanchoice) (text ?cs&~1&~2))
    =>
    (retract ?choice)
    (assert (ask sedanchoice))
    (printout t "You have chosen a wrong selection. chose 1/2"))

(defrule request-suvchoice
    (answer (x model) (text ?cc&:(eq ?cc suv)))
    =>
    (assert(ask suvchoice)))

(defrule badchoicesuvchoice
    ?choice <- (answer (x suvchoice) (text ?cs&~1&~2))
    =>
    (retract ?choice)
    (assert (ask suvchoice))
    (printout t "You have chosen a wrong selection. chose 1/2"))

(defrule request-sportschoice
    (answer (x model) (text ?cc&:(eq ?cc sports)))
    =>
    (assert(ask sportschoice)))

(defrule badchoicesportschoice
    ?choice <- (answer (x sportschoice) (text ?cs&~1&~2))
    =>
    (retract ?choice)
    (assert (ask sportschoice))
    (printout t "You have chosen a wrong selection. chose 1/2"))

(defrule request-pickupchoice
    (answer (x model) (text ?cc&:(eq ?cc pickup)))
    =>
    (assert(ask pickupchoice)))

(defrule badchoicepickupchoice
    ?choice <- (answer (x pickupchoice) (text ?cs&~1&~2))
    =>
    (retract ?choice)
    (assert (ask pickupchoice))
    (printout t "You have chosen a wrong selection. chose 1/2"))


(defmodule final)

(defrule car1
    (and (answer (x price) (text 1))
        (answer (x model) (text sedan))
        (answer (x sedanchoice) (text 1)))
    =>
    (assert (recommendation (car "2017 Kia Optima Plug-In Hybrid") (description " You can find more about this car at https://www.kia.com/us/en/vehicle/optima-plug-in-hybrid/2017  
               "))))

(defrule car2
    (and (answer (x price) (text 1))
        (answer (x model) (text sedan))
        (answer (x sedanchoice) (text 2)))
    =>
    (assert (recommendation (car "2017 Honda Accord") (description " You can find more about this car at https://automobiles.honda.com/accord-sedan  "))))
    
(defrule car3
    (and (answer (x price) (text 2))
        (answer (x model) (text sedan))
        (answer (x sedanchoice) (text 1)))
    =>
    (assert (recommendation (car "2017 Fusion Energi Platinum") (description " You can find more about this car at https://www.ford.com/cars/fusion/2017/models/fusion-energi-platinum/"))))

    
(defrule car4
    (and (answer (x price) (text 2))
        (answer (x model) (text sedan))
        (answer (x sedanchoice) (text 2)))
    =>
    (assert (recommendation (car "BMW 3 series") (description " You can find more about this car at https://www.bmwusa.com/vehicles/3series.html"))))
    
(defrule car5
    (and (answer (x price) (text 3))
        (answer (x model) (text sedan))
        (answer (x sedanchoice) (text 1)))
    =>
    (assert (recommendation (car "2017 Lexus ES 300h") (description " You can find more about this car at http://www.lexus.com/models/ES-hybrid"))))
    
    
    
(defrule car6
    (and (answer (x price) (text 3))
        (answer (x model) (text sedan))
        (answer (x sedanchoice) (text 2)))
    =>
    (assert (recommendation (car "2016 porsche panamera") (description " You can find more about this car at https://www.porsche.com/usa/models/panamera/"))))
 
    
(defrule car7
    (and (answer (x price) (text 1 ))
        (answer (x model) (text suv))
        (answer (x suvchoice) (text 1)))
    =>
    (assert (recommendation (car "2017 Toyota Highlander") (description " You can find more about this car at https://www.toyota.com/highlander/"))))
    
 
(defrule car8
    (and (answer (x price) (text 1))
        (answer (x model) (text suv))
        (answer (x suvchoice) (text 2)))
    =>
    (assert (recommendation (car "2017 Chevrolet Traverse") (description " You can find more about this car at http://www.chevrolet.com/suvs/traverse-mid-size-suv"))))
 
    
(defrule car9
    (and (answer (x price) (text 2))
        (answer (x model) (text suv))
        (answer (x suvchoice) (text 1)))
    =>
    (assert (recommendation (car "2017 Audi Q7") (description " You can find more about this car at https://www.audiusa.com/models/audi-q7"))))
 
    
    
(defrule car10
    (and (answer (x price) (text 2))
        (answer (x model) (text suv))
        (answer (x suvchoice) (text 2)))
    =>
    (assert (recommendation (car "2017 Acura MDX") (description " You can find more about this car at https://www.acura.com/mdx"))))
 
    
(defrule car11
    (and (answer (x price) (text 3))
        (answer (x model) (text suv))
        (answer (x suvchoice) (text 1)))
    =>
    (assert (recommendation (car "2017 BMW X5") (description " You can find more about this car at https://www.bmwusa.com/vehicles/xmodels/x5.html"))))
 
    
    
(defrule car12
    (and (answer (x price) (text 3))
        (answer (x model) (text suv))
        (answer (x suvchoice) (text 2)))
    =>
    (assert (recommendation (car "2017 Mercedes-Benz GLS-Class") (description " You can find more about this car at https://www.mbusa.com/mercedes/vehicles/class/class-GLS/bodystyle-suv"))))
 
    
    
(defrule car13
    (and (answer (x price) (text 1))
        (answer (x model) (text sports))
        (answer (x sportschoice) (text 1)))
    =>
    (assert (recommendation (car "2017 Mazda MX-5 Miata") (description " You can find more about this car at https://www.mazdausa.com/vehicles/mx-5-miata-rf"))))
 
(defrule car14
    (and (answer (x price) (text 1))
        (answer (x model) (text sports))
        (answer (x sportschoice) (text 2)))
    =>
    (assert (recommendation (car "None") (description " There is no such car in this price range"))))
 
    
(defrule car15
    (and (answer (x price) (text 2))
        (answer (x model) (text sports))
        (answer (x sportschoice) (text 1)))
    =>
    (assert (recommendation (car "BMW Z4") (description " You can find more about this car at https://www.bmwusa.com/vehicles/z4.html"))))
 
    
(defrule car16
    (and (answer (x price) (text 2))
        (answer (x model) (text sports))
        (answer (x sportschoice) (text 2)))
    =>
    (assert (recommendation (car "Audi TT") (description " You can find more about this car at https://www.audiusa.com/models/audi-tt-coupe"))))
 
    
(defrule car17
    (and (answer (x price) (text 3))
        (answer (x model) (text sports))
        (answer (x sportschoice) (text 1)))
    =>
    (assert (recommendation (car "Alfa Romeo 4C") (description " You can find more about this car at https://www.alfaromeousa.com/cars/alfa-romeo-4c-coupe"))))
 
    
(defrule car18
    (and (answer (x price) (text 3))
        (answer (x model) (text sports))
        (answer (x sportschoice) (text 2)))
    =>
    (assert (recommendation (car "2017 Lamborghini Huracan") (description " You can find more about this car at http://driving.ca/lamborghini/huracan"))))

(defrule car19
    (and (answer (x price) (text 1))
        (answer (x model) (text pickup))
        (answer (x pickupchoice) (text 1)))
    =>
    (assert (recommendation (car "Toyota Tundra SR") (description "You can find more about this car at https://www.toyota.com/tundra/"))))

(defrule car20
    (and (answer (x price) (text 1))
        (answer (x model) (text pickup))
        (answer (x pickupchoice) (text 2)))
    =>
    (assert (recommendation (car "2017 GMC Sierra 3500HD") (description "You can find more about this car at http://www.gmc.com/trucks/sierra-3500hd-pickup-truck.html"))))

(defrule car21
    (and (answer (x price) (text 2))
        (answer (x model) (text pickup))
        (answer (x pickupchoice) (text 1)))
    =>
    (assert (recommendation (car "None") (description " No car in this price range"))))

(defrule car22
    (and (answer (x price) (text 2))
        (answer (x model) (text pickup))
        (answer (x pickupchoice) (text 2)))
    =>
    (assert (recommendation (car "Tacoma SR") (description " You can find more about this car at https://www.toyota.com/tacoma/"))))

(defrule car23
    (and (answer (x price) (text 3))
        (answer (x model) (text pickup))
        (answer (x pickupchoice) (text 1)))
    =>
    (assert (recommendation (car "Toyota Tundra TRD Pro") (description " You can find more about this car athttps://www.toyota.com/trdpro/"))))

(defrule car24
    (and (answer (x price) (text 3))
        (answer (x model) (text pickup))
        (answer (x pickupchoice) (text 2)))
    =>
    (assert (recommendation (car "2017 017 Nissan Titan PRO-4X ") (description " You can find more about this car at https://www.nissanusa.com/trucks/titan-xd/versions-specs/version.pro-4x.html"))))
 
  
(defmodule startup)

(defrule intro
    =>
    (printout t " Welcome to the Car Selection Assistant" crlf)
    (printout t " Please answer the following questions and we will suggest you the car that best" crlf 
        "  suits your needs." crlf))

(defmodule report)

(defrule sort-and-print
    ?r1 <- (recommendation (car ?f1) (description ?e))
    (not (recommendation (car ?f2&:(< (str-compare ?f2 ?f1) 0))))
    =>
    (printout t crlf " " crlf)
    (printout t " Suggestion :" ?f1 crlf)
    (printout t " Description:"  crlf ?e crlf)
    (retract ?r1))
(deffunction run-system ()
    (reset)
    (focus startup interview final report)
    (run))

(while TRUE
    (run-system))

