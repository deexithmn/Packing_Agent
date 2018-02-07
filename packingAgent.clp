(defglobal ?*name* = nil)
(deftemplate user
    "User inputs"
    (slot state)
    (slot gender)
    (slot purpose)
    (slot stay-place)
    (slot month-of-travel))

(deftemplate question
  (slot text)
  (slot type)
  (slot ident))

(deftemplate answer
  (slot ident)
  (slot answer))


(deffunction checking-in ()
    (printout t  crlf crlf crlf)
	(printout t "Hello, I will he assisting you with what stuffs you need to pack. But I need some info on your itenary" crlf)
	(printout t "Shall we proceed? Y/N" crlf)
	(bind ?agree (read) crlf)
	(if (or (eq ?agree Y)(eq ?agree y))then
    	(printout t "Yay..! ok. Lets get started then, there is so much to pack!" crlf)
        (printout t "Kindly type in the options given only!!" crlf)
        (printout t "I'm implemented on rule based logic! Not on machine learning!" crlf)
        (printout t "EX: Month -> for 'Jan' type '1', 'Feb' tpye '2' " crlf  crlf)
        (return TRUE))
		else (printout t "It's okay! Let me help you next time then! Bon Voyage!!" crlf)
    			(return FALSE))

(defrule print-banner
    (test (eq TRUE (checking-in))) 
	=>
(printout t " Ok let's get started then! :) " crlf crlf)
(printout t " Can you type in your name? :) " crlf)
(bind ?name (read))
(printout t crlf "***************************************" crlf)
(printout t " -----  Hello, " ?name " ----- " crlf)
(bind ?*name* ?name)
(printout t "***************************************" crlf crlf)
(printout t crlf " ---- Welcome to packing helper agent ---- " crlf)
(printout t crlf " Please answer the questions and " crlf)
(printout t crlf " I will help you out to pack your stuffs! " crlf))



(deffunction is-type (?answer ?type) 
    "Checking the answer has correct type"
    (if (eq ?type male-female )then
        (return (or (eq ?answer male)(eq ?answer female)))      
    	  else (if(eq ?type B-P)then
                (return(or (or (eq ?answer B)(eq ?answer B))(or (eq ?answer P)(eq ?answer P))))
           	else(if (eq ?type number1)then
        			(return(numberp ?answer))
                else(if (eq ?type hotel-airbnb)then
        			(return(or (eq ?answer hotel)(eq ?answer airbnb)))
                        else(if (eq ?type five-states)then
        			(return(or (eq ?answer CA)(eq ?answer IL)(eq ?answer FL)(eq ?answer WA)))
                    	else(if (eq ?type number2)then
                            ;(printout t "in months")
        					(return (or(eq ?answer 1)(eq ?answer 2)(eq ?answer 3)(eq ?answer 4)(eq ?answer 5)
                                    (eq ?answer 6)(eq ?answer 7)(eq ?answer 8)(eq ?answer 9)(eq ?answer 10)
                                    (eq ?answer 11)(eq ?answer 12))))
                        )
                    )
                )
            )
        )
    )

(deffunction ask-user(?question ?type)
    "Ask questiona and return answer"
    (bind ?answer "") 
    (while (not (is-type ?answer ?type))do
        (printout t ?question " " )
        (if (eq ?type male-female )then
            (printout t  crlf)
            (printout t " (male or female) "))
        (if (eq ?type B-P)then
            (printout t crlf " Business or Personal? (B/P)" crlf ))
        (if (eq ?type hotel-airbnb)then
            (printout t  crlf)
            (printout t crlf " hotel or airbnb? "))
        (if (eq ?type number2)then 
            (printout t crlf)
            (printout t crlf " Jan-1 , Feb-2, .. Dec-12 "))
        (if (eq ?type five-states)then
            (printout t  crlf)
            (printout t crlf " California-CA" crlf " Florida-FL" crlf " Illinois-IL" crlf " Washington-WA" crlf ))
        (bind ?answer (read))
     )        
    (return ?answer)
 )




(defrule request-gender
    (test (eq TRUE (checking-in)))
=>
(assert (ask gender)))


(defrule request-purpose
    (test (eq TRUE (checking-in)))
=>
(assert (ask purpose)))

(defrule request-stay-place
    (test (eq TRUE (checking-in)))
=>
(assert (ask stay-place)))

(defrule request-month-of-travel
    (test (eq TRUE (checking-in)))
=>
(assert (ask month-of-travel)))

(defrule request-state
    (test (eq TRUE (checking-in)))
=>
(assert (ask state)))


;rule that binds asserts the answer into the memeory
(defrule ask-question-by-id
 "Ask a question and assert the answer" 
(question (ident  ?id)(text ?text)(type ?type))
(not (answer (ident ?id)))
 ?ask <- (ask ?id)
 =>                        
 (bind ?answer (ask-user ?text ?type))
 (assert (answer (ident ?id)(answer ?answer)))
 (retract ?ask)
 (return)            
  )



;california, male , business ,hotel

(defrule CA-JAN-MALE-B-HOTEL 
  (user(gender male)(state CA)(purpose B)(stay-place hotel)(month-of-travel 1) )  
  =>       
 (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, tank tops, vneck tops."  
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule CA-FEB-MALE-B-HOTEL
  (user(gender male)(state CA)(purpose B)(stay-place hotel)(month-of-travel 2) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule CA-MAR-MALE-B-HOTEL
  (user(gender male)(state CA)(purpose B)(stay-place hotel)(month-of-travel 3) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule CA-APR-MALE-B-HOTEL
  (user(gender male)(state CA)(purpose B)(stay-place hotel)(month-of-travel 4) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf  )
   ) 
(defrule CA-MAY-MALE-B-HOTEL
  (user(gender male)(state CA)(purpose B)(stay-place hotel)(month-of-travel 5) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule CA-JUN-MALE-B-HOTEL
  (user(gender male)(state CA)(purpose B)(stay-place hotel)(month-of-travel 6) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)" 
        ;crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."  
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule CA-JULY-MALE-B-HOTEL
  (user(gender male)(state CA)(purpose B)(stay-place hotel)(month-of-travel 7) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
       ; crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank." 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule CA-AUG-MALE-B-HOTEL
  (user(gender male)(state CA)(purpose B)(stay-place hotel)(month-of-travel 8) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
       ; crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank." 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule CA-SEPT-MALE-B-HOTEL
  (user(gender male)(state CA)(purpose B)(stay-place hotel)(month-of-travel 9) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"     
        ;crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule CA-OCT-MALE-B-HOTEL
  (user(gender male)(state CA)(purpose B)(stay-place hotel)(month-of-travel 10) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)" 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule CA-NOV-MALE-B-HOTEL
  (user(gender male)(state CA)(purpose B)(stay-place hotel)(month-of-travel 11) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule CA-DEC-MALE-B-HOTEL
  (user(gender male)(state CA)(purpose B)(stay-place hotel)(month-of-travel 12) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 


;california, male , personal, hotel



(defrule CA-JAN-MALE-P-HOTEL
  (user(gender male)(state CA)(purpose P)(stay-place hotel)(month-of-travel 1) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"  	 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf )
   ) 
(defrule CA-FEB-MALE-P-HOTEL
  (user(gender male)(state CA)(purpose P)(stay-place hotel)(month-of-travel 2) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf )
   ) 
(defrule CA-MAR-MALE-P-HOTEL
  (user(gender male)(state CA)(purpose P)(stay-place hotel)(month-of-travel 3) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf )
   ) 
(defrule CA-APR-MALE-P-HOTEL
  (user(gender male)(state CA)(purpose P)(stay-place hotel)(month-of-travel 4) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf )
   ) 
(defrule CA-MAY-MALE-P-HOTEL
  (user(gender male)(state CA)(purpose P)(stay-place hotel)(month-of-travel 5) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf )
    )
(defrule CA-JUN-MALE-P-HOTEL
  (user(gender male)(state CA)(purpose P)(stay-place hotel)(month-of-travel 6) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        ;crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank." 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."  
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-JULY-MALE-P-HOTEL
  (user(gender male)(state CA)(purpose P)(stay-place hotel)(month-of-travel 7) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
       ; crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank." 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-AUG-MALE-P-HOTEL
  (user(gender male)(state CA)(purpose P)(stay-place hotel)(month-of-travel 8) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"    
        ;crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-SEPT-MALE-P-HOTEL
  (user(gender male)(state CA)(purpose P)(stay-place hotel)(month-of-travel 9) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf )
   ) 
(defrule CA-OCT-MALE-P-HOTEL
  (user(gender male)(state CA)(purpose P)(stay-place hotel)(month-of-travel 10) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-NOV-MALE-P-HOTEL
  (user(gender male)(state CA)(purpose P)(stay-place hotel)(month-of-travel 11) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"   
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-DEC-MALE-P-HOTEL
  (user(gender male)(state CA)(purpose P)(stay-place hotel)(month-of-travel 12) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."  
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 






;california, female , business, hotel



(defrule CA-JAN-FEMALE-B-HOTEL
  (user(gender female)(state CA)(purpose B)(stay-place hotel)(month-of-travel 1) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->tank tops, vneck tops, jorts, maxidresses." 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule CA-FEB-FEMALE-B-HOTEL
  (user(gender female)(state CA)(purpose B)(stay-place hotel)(month-of-travel 2) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule CA-MAR-FEMALE-B-HOTEL
  (user(gender female)(state CA)(purpose B)(stay-place hotel)(month-of-travel 3) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule CA-APR-FEMALE-B-HOTEL
  (user(gender female)(state CA)(purpose B)(stay-place hotel)(month-of-travel 4) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule CA-MAY-FEMALE-B-HOTEL
  (user(gender female)(state CA)(purpose B)(stay-place hotel)(month-of-travel 5) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule CA-JUN-FEMALE-B-HOTEL
  (user(gender female)(state CA)(purpose B)(stay-place hotel)(month-of-travel 6) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"      
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."
				crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
				crlf "->Office documents(if needed!)."
				crlf "->Laptop, charger, earphone, power bank."
				crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule CA-JULY-FEMALE-B-HOTEL
  (user(gender female)(state CA)(purpose B)(stay-place hotel)(month-of-travel 7) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"     
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank." 
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule CA-AUG-FEMALE-B-HOTEL
  (user(gender female)(state CA)(purpose B)(stay-place hotel)(month-of-travel 8) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"      
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."x
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule CA-SEPT-FEMALE-B-HOTEL
  (user(gender female)(state CA)(purpose B)(stay-place hotel)(month-of-travel 9) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"    
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule CA-OCT-FEMALE-B-HOTEL
  (user(gender female)(state CA)(purpose B)(stay-place hotel)(month-of-travel 10) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank." 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule CA-NOV-FEMALE-B-HOTEL
  (user(gender female)(state CA)(purpose B)(stay-place hotel)(month-of-travel 11) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule CA-DEC-FEMALE-B-HOTEL
  (user(gender female)(state CA)(purpose B)(stay-place hotel)(month-of-travel 12) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   )

;california, female , personal, hotel


(defrule CA-JAN-FEMALE-P-HOTEL
  (user(gender female)(state CA)(purpose P)(stay-place hotel)(month-of-travel 1) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->tank tops, vneck tops, jorts, maxidresses." 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."  
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-FEB-FEMALE-P-HOTEL
  (user(gender female)(state CA)(purpose P)(stay-place hotel)(month-of-travel 2) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-MAR-FEMALE-P-HOTEL
  (user(gender female)(state CA)(purpose P)(stay-place hotel)(month-of-travel 3) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-APR-FEMALE-P-HOTEL
  (user(gender female)(state CA)(purpose P)(stay-place hotel)(month-of-travel 4) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-MAY-FEMALE-P-HOTEL
  (user(gender female)(state CA)(purpose P)(stay-place hotel)(month-of-travel 5) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-JUN-FEMALE-P-HOTEL
  (user(gender female)(state CA)(purpose P)(stay-place hotel)(month-of-travel 6) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"    
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."  
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-JULY-FEMALE-P-HOTEL
  (user(gender female)(state CA)(purpose P)(stay-place hotel)(month-of-travel 7) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"    
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."  
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-AUG-FEMALE-P-HOTEL
  (user(gender female)(state CA)(purpose P)(stay-place hotel)(month-of-travel 8) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank." 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf )
   ) 
(defrule CA-SEPT-FEMALE-P-HOTEL
  (user(gender female)(state CA)(purpose P)(stay-place hotel)(month-of-travel 9) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."  
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."  
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-OCT-FEMALE-P-HOTEL
  (user(gender female)(state CA)(purpose P)(stay-place hotel)(month-of-travel 10) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank." 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-NOV-FEMALE-P-HOTEL
  (user(gender female)(state CA)(purpose P)(stay-place hotel)(month-of-travel 11) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-DEC-FEMALE-P-HOTEL
  (user(gender female)(state CA)(purpose P)(stay-place hotel)(month-of-travel 12) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   )



;Illinois, male , business, hotel


(defrule IL-JAN-MALE-B-HOTEL
  (user(gender male)(state IL)(purpose B)(stay-place hotel)(month-of-travel 1) )  
  =>       
    (printout t crlf "Its going to be very cold!!"
        crlf "->Pack your thermals,cold creams, snow boots(if you have one!)"
        crlf "->gloves, balaclava or beanie, an umbrella, scarves, pair of shoes(water resistant would be good!)." 
        crlf "->(won't suggest you to carry sandals), a normal jacket, a show jacket as well!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule IL-FEB-MALE-B-HOTEL
  (user(gender male)(state IL)(purpose B)(stay-place hotel)(month-of-travel 2) )  
  =>       
    (printout t crlf "Its going to be very cold!!"
        crlf "->Pack your thermals,cold creams, snow boots(if you have one!)"
        crlf "->gloves, balaclava or beanie, an umbrella, scarves, pair of shoes(water resistant would be good!)." 
        crlf "->(won't suggest you to carry sandals), a normal jacket, a show jacket as well!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule IL-MAR-MALE-B-HOTEL 
  (user(gender male)(state IL)(purpose B)(stay-place hotel)(month-of-travel 3) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule IL-APR-MALE-B-HOTEL
  (user(gender male)(state IL)(purpose B)(stay-place hotel)(month-of-travel 4) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket " 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule IL-MAY-MALE-B-HOTEL
  (user(gender male)(state IL)(purpose B)(stay-place hotel)(month-of-travel 5) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule IL-JUN-MALE-B-HOTEL
  (user(gender male)(state IL)(purpose B)(stay-place hotel)(month-of-travel 6) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule IL-JULY-MALE-B-HOTEL
  (user(gender male)(state IL)(purpose B)(stay-place hotel)(month-of-travel 7) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule IL-AUG-MALE-B-HOTEL
  (user(gender male)(state IL)(purpose B)(stay-place hotel)(month-of-travel 8) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule IL-SEPT-MALE-B-HOTEL
  (user(gender male)(state IL)(purpose B)(stay-place hotel)(month-of-travel 9) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule IL-OCT-MALE-B-HOTEL
  (user(gender male)(state IL)(purpose B)(stay-place hotel)(month-of-travel 10) )  
  =>       
    (printout t crlf "Its going to be a pleasant trip!! You'll enjoy the wather there!"
        crlf "->Carry a pair of shoes,(not bad if you can carry sandals if you prefer),"
        crlf "->sweatshirt or pullover, a simple jacket,jeans,shirts,tee-shirts " 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
    	crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule IL-NOV-MALE-B-HOTEL
  (user(gender male)(state IL)(purpose B)(stay-place hotel)(month-of-travel 11) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule IL-DEC-MALE-B-HOTEL
  (user(gender male)(state IL)(purpose B)(stay-place hotel)(month-of-travel 12) )  
  =>       
    (printout t crlf "Its going to be very cold!!"
        crlf "->Pack your thermals,cold creams, snow boots(if you have one!)"
        crlf "->gloves, balaclava or beanie, an umbrella, scarves, pair of shoes(water resistant would be good!)." 
        crlf "->(won't suggest you to carry sandals), a normal jacket, a show jacket as well!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 



;Illinois, male , personal, hotel


(defrule IL-JAN-MALE-P-HOTEL
  (user(gender male)(state IL)(purpose P)(stay-place hotel)(month-of-travel 1) )  
  =>       
    (printout t crlf "Its going to be very cold!!"
        crlf "->Pack your thermals,cold creams, snow boots(if you have one!)"
        crlf "->gloves, balaclava or beanie, an umbrella, scarves, pair of shoes(water resistant would be good!)." 
        crlf "->(won't suggest you to carry sandals), a normal jacket, a show jacket as well!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-FEB-MALE-P-HOTEL
  (user(gender male)(state IL)(purpose P)(stay-place hotel)(month-of-travel 2) )  
  =>       
    (printout t crlf "Its going to be very cold!!"
        crlf "->Pack your thermals,cold creams, snow boots(if you have one!)"
        crlf "->gloves, balaclava or beanie, an umbrella, scarves, pair of shoes(water resistant would be good!)." 
        crlf "->(won't suggest you to carry sandals), a normal jacket, a show jacket as well!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-MAR-MALE-P-HOTEL
  (user(gender male)(state IL)(purpose P)(stay-place hotel)(month-of-travel 3) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf )
   ) 
(defrule IL-APR-MALE-P-HOTEL
  (user(gender male)(state IL)(purpose P)(stay-place hotel)(month-of-travel 4) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."  
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-MAY-MALE-P-HOTEL
  (user(gender male)(state IL)(purpose P)(stay-place hotel)(month-of-travel 5) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf )
   ) 
(defrule IL-JUN-MALE-P-HOTEL
  (user(gender male)(state IL)(purpose P)(stay-place hotel)(month-of-travel 6) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-JULY-MALE-P-HOTEL
  (user(gender male)(state IL)(purpose P)(stay-place hotel)(month-of-travel 7) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."  
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-AUG-MALE-P-HOTEL
  (user(gender male)(state IL)(purpose P)(stay-place hotel)(month-of-travel 8) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf )
   ) 
(defrule IL-SEPT-MALE-P-HOTEL
  (user(gender male)(state IL)(purpose P)(stay-place hotel)(month-of-travel 9) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-OCT-MALE-P-HOTEL
  (user(gender male)(state IL)(purpose P)(stay-place hotel)(month-of-travel 10) )  
  =>       
    (printout t crlf "Its going to be a pleasant trip!! You'll enjoy the wather there!"
        crlf "->Carry a pair of shoes,(not bad if you can carry sandals if you prefer),"
        crlf "->sweatshirt or pullover, a simple jacket,jeans,shirts,tee-shirts "
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf )
   ) 
(defrule IL-NOV-MALE-P-HOTEL
  (user(gender male)(state IL)(purpose P)(stay-place hotel)(month-of-travel 11) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf )
   ) 
(defrule IL-DEC-MALE-P-HOTEL
  (user(gender male)(state IL)(purpose P)(stay-place hotel)(month-of-travel 12) )  
  =>      
    (printout t crlf "Its going to be very cold!!"
        crlf "->Pack your thermals,cold creams, snow boots(if you have one!)"
        crlf "->gloves, balaclava or beanie, an umbrella, scarves, pair of shoes(water resistant would be good!)." 
        crlf "->(won't suggest you to carry sandals), a normal jacket, a show jacket as well!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 




;Illinois, female , business, hotel


(defrule IL-JAN-FEMALE-B-HOTEL
  (user(gender female)(state IL)(purpose B)(stay-place hotel)(month-of-travel 1) )  
  =>       
    (printout t crlf "Its going to be very cold!!"
        crlf "->Pack your thermals,cold creams, snow boots(if you have one!)"
        crlf "->gloves, balaclava or beanie, an umbrella, scarves, pair of shoes(water resistant would be good!)." 
        crlf "->(won't suggest you to carry sandals), a normal jacket, a show jacket as well!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule IL-FEB-FEMALE-B-HOTEL
  (user(gender female)(state IL)(purpose B)(stay-place hotel)(month-of-travel 2) )  
  =>       
    (printout t crlf "Its going to be very cold!!"
        crlf "->Pack your thermals,cold creams, snow boots(if you have one!)"
        crlf "->gloves, balaclava or beanie, an umbrella, scarves, pair of shoes(water resistant would be good!)." 
        crlf "->(won't suggest you to carry sandals), a normal jacket, a show jacket as well!" 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule IL-MAR-FEMALE-B-HOTEL
  (user(gender female)(state IL)(purpose B)(stay-place hotel)(month-of-travel 3) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule IL-APR-FEMALE-B-HOTEL
  (user(gender female)(state IL)(purpose B)(stay-place hotel)(month-of-travel 4) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule IL-MAY-FEMALE-B-HOTEL
  (user(gender female)(state IL)(purpose B)(stay-place hotel)(month-of-travel 5) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."     
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule IL-JUN-FEMALE-B-HOTEL
  (user(gender female)(state IL)(purpose B)(stay-place hotel)(month-of-travel 6) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank." 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule IL-JULY-FEMALE-B-HOTEL
  (user(gender female)(state IL)(purpose B)(stay-place hotel)(month-of-travel 7) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"    
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule IL-AUG-FEMALE-B-HOTEL
  (user(gender female)(state IL)(purpose B)(stay-place hotel)(month-of-travel 8) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"    
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule IL-SEPT-FEMALE-B-HOTEL
  (user(gender female)(state IL)(purpose B)(stay-place hotel)(month-of-travel 9) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule IL-OCT-FEMALE-B-HOTEL
  (user(gender female)(state IL)(purpose B)(stay-place hotel)(month-of-travel 10) )  
  =>       
    (printout t crlf "Its going to be a pleasant trip!! You'll enjoy the wather there!"
        crlf "->Carry a pair of shoes,(not bad if you can carry sandals if you prefer),"
        crlf "->sweatshirt or pullover, a simple jacket,jeans,shirts,tee-shirts " 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule IL-NOV-FEMALE-B-HOTEL
  (user(gender female)(state IL)(purpose B)(stay-place hotel)(month-of-travel 11) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule IL-DEC-FEMALE-B-HOTEL
  (user(gender female)(state IL)(purpose B)(stay-place hotel)(month-of-travel 12) )  
  =>       
    (printout t crlf "Its going to be very cold!!"
        crlf "->Pack your thermals,cold creams, snow boots(if you have one!)"
        crlf "->gloves, balaclava or beanie, an umbrella, scarves, pair of shoes(water resistant would be good!)." 
        crlf "->(won't suggest you to carry sandals), a normal jacket, a show jacket as well!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   )



;Illinois, female , personal, hotel



(defrule IL-JAN-FEMALE-P-HOTEL
  (user(gender female)(state IL)(purpose P)(stay-place hotel)(month-of-travel 1) )  
  =>       
    (printout t crlf "Its going to be very cold!!"
        crlf "->Pack your thermals,cold creams, snow boots(if you have one!)"
        crlf "->gloves, balaclava or beanie, an umbrella, scarves, pair of shoes(water resistant would be good!)." 
        crlf "->(won't suggest you to carry sandals), a normal jacket, a show jacket as well!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-FEB-FEMALE-P-HOTEL
  (user(gender female)(state IL)(purpose P)(stay-place hotel)(month-of-travel 2) )  
  =>       
    (printout t crlf "Its going to be very cold!!"
        crlf "->Pack your thermals,cold creams, snow boots(if you have one!)"
        crlf "->gloves, balaclava or beanie, an umbrella, scarves, pair of shoes(water resistant would be good!)." 
        crlf "->(won't suggest you to carry sandals), a normal jacket, a show jacket as well!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-MAR-FEMALE-P-HOTEL
  (user(gender female)(state IL)(purpose P)(stay-place hotel)(month-of-travel 3) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf  
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-APR-FEMALE-P-HOTEL
  (user(gender female)(state IL)(purpose P)(stay-place hotel)(month-of-travel 4) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."  
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-MAY-FEMALE-P-HOTEL
  (user(gender female)(state IL)(purpose P)(stay-place hotel)(month-of-travel 5) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses." 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-JUN-FEMALE-P-HOTEL
  (user(gender female)(state IL)(purpose P)(stay-place hotel)(month-of-travel 6) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"    
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."  
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-JULY-FEMALE-P-HOTEL
  (user(gender female)(state IL)(purpose P)(stay-place hotel)(month-of-travel 7) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"    
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-AUG-FEMALE-P-HOTEL
  (user(gender female)(state IL)(purpose P)(stay-place hotel)(month-of-travel 8) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."  
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."  
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-SEPT-FEMALE-P-HOTEL
  (user(gender female)(state IL)(purpose P)(stay-place hotel)(month-of-travel 9) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."  
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-OCT-FEMALE-P-HOTEL
  (user(gender female)(state IL)(purpose P)(stay-place hotel)(month-of-travel 10) )  
  =>       
    (printout t crlf "Its going to be a pleasant trip!! You'll enjoy the wather there!"
        crlf "->Carry a pair of shoes,(not bad if you can carry sandals if you prefer),"
        crlf "->sweatshirt or pullover, a simple jacket,jeans,shirts,tee-shirts " 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-NOV-FEMALE-P-HOTEL
  (user(gender female)(state IL)(purpose P)(stay-place hotel)(month-of-travel 11) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."  
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-DEC-FEMALE-P-HOTEL
  (user(gender female)(state IL)(purpose P)(stay-place hotel)(month-of-travel 12) )  
  =>       
    (printout t crlf "Its going to be very cold!!"
        crlf "->Pack your thermals,cold creams, snow boots(if you have one!)"
        crlf "->gloves, balaclava or beanie, an umbrella, scarves, pair of shoes(water resistant would be good!)." 
        crlf "->(won't suggest you to carry sandals), a normal jacket, a show jacket as well!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf )
   )



;Florida, male , business, hotel


(defrule FL-JAN-MALE-B-HOTEL
  (user(gender male)(state FL)(purpose B)(stay-place hotel)(month-of-travel 1) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule FL-FEB-MALE-B-HOTEL
  (user(gender male)(state FL)(purpose B)(stay-place hotel)(month-of-travel 2) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule FL-MAR-MALE-B-HOTEL
  (user(gender male)(state FL)(purpose B)(stay-place hotel)(month-of-travel 3) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule FL-APR-MALE-B-HOTEL
  (user(gender male)(state FL)(purpose B)(stay-place hotel)(month-of-travel 4) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule FL-MAY-MALE-B-HOTEL
  (user(gender male)(state FL)(purpose B)(stay-place hotel)(month-of-travel 5) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule FL-JUN-MALE-B-HOTEL
  (user(gender male)(state FL)(purpose B)(stay-place hotel)(month-of-travel 6) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule FL-JULY-MALE-B-HOTEL
  (user(gender male)(state FL)(purpose B)(stay-place hotel)(month-of-travel 7) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!" 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule FL-AUG-MALE-B-HOTEL
  (user(gender male)(state FL)(purpose B)(stay-place hotel)(month-of-travel 8) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule FL-SEPT-MALE-B-HOTEL
  (user(gender male)(state FL)(purpose B)(stay-place hotel)(month-of-travel 9) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule FL-OCT-MALE-B-HOTEL
  (user(gender male)(state FL)(purpose B)(stay-place hotel)(month-of-travel 10) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule FL-NOV-MALE-B-HOTEL
  (user(gender male)(state FL)(purpose B)(stay-place hotel)(month-of-travel 11) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule FL-DEC-MALE-B-HOTEL
  (user(gender male)(state FL)(purpose B)(stay-place hotel)(month-of-travel 12) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 



;Florida, male , personal, hotel



(defrule FL-JAN-MALE-P-HOTEL
  (user(gender male)(state FL)(purpose P)(stay-place hotel)(month-of-travel 1) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-FEB-MALE-P-HOTEL
  (user(gender male)(state FL)(purpose P)(stay-place hotel)(month-of-travel 2) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-MAR-MALE-P-HOTEL
  (user(gender male)(state FL)(purpose P)(stay-place hotel)(month-of-travel 3) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-APR-MALE-P-HOTEL
  (user(gender male)(state FL)(purpose P)(stay-place hotel)(month-of-travel 4) )  
  =>       
    (printout t  crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-MAY-MALE-P-HOTEL
  (user(gender male)(state FL)(purpose P)(stay-place hotel)(month-of-travel 5) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."  
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-JUN-MALE-P-HOTEL
  (user(gender male)(state FL)(purpose P)(stay-place hotel)(month-of-travel 6) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-JULY-MALE-P-HOTEL
  (user(gender male)(state FL)(purpose P)(stay-place hotel)(month-of-travel 7) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf )
   ) 
(defrule FL-AUG-MALE-P-HOTEL
  (user(gender male)(state FL)(purpose P)(stay-place hotel)(month-of-travel 8) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-SEPT-MALE-P-HOTEL
  (user(gender male)(state FL)(purpose P)(stay-place hotel)(month-of-travel 9) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf )
   ) 
(defrule FL-OCT-MALE-P-HOTEL
  (user(gender male)(state FL)(purpose P)(stay-place hotel)(month-of-travel 10) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."  
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-NOV-MALE-P-HOTEL
  (user(gender male)(state FL)(purpose P)(stay-place hotel)(month-of-travel 11) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."  
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-DEC-MALE-P-HOTEL
  (user(gender male)(state FL)(purpose P)(stay-place hotel)(month-of-travel 12) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf )
   ) 



;Florida, female , business, hotel


(defrule FL-JAN-FEMALE-B-HOTEL
  (user(gender female)(state FL)(purpose B)(stay-place hotel)(month-of-travel 1) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule FL-FEB-FEMALE-B-HOTEL
  (user(gender female)(state FL)(purpose B)(stay-place hotel)(month-of-travel 2) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule FL-MAR-FEMALE-B-HOTEL
  (user(gender female)(state FL)(purpose B)(stay-place hotel)(month-of-travel 3) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule FL-APR-FEMALE-B-HOTEL
  (user(gender female)(state FL)(purpose B)(stay-place hotel)(month-of-travel 4) )  
  =>       
    (printout t  crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"    
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule FL-MAY-FEMALE-B-HOTEL
  (user(gender female)(state FL)(purpose B)(stay-place hotel)(month-of-travel 5) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"    
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule FL-JUN-FEMALE-B-HOTEL
  (user(gender female)(state FL)(purpose B)(stay-place hotel)(month-of-travel 6) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule FL-JULY-FEMALE-B-HOTEL
  (user(gender female)(state FL)(purpose B)(stay-place hotel)(month-of-travel 7) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule FL-AUG-FEMALE-B-HOTEL
  (user(gender female)(state FL)(purpose B)(stay-place hotel)(month-of-travel 8) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule FL-SEPT-FEMALE-B-HOTEL
  (user(gender female)(state FL)(purpose B)(stay-place hotel)(month-of-travel 9) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule FL-OCT-FEMALE-B-HOTEL
  (user(gender female)(state FL)(purpose B)(stay-place hotel)(month-of-travel 10) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule FL-NOV-FEMALE-B-HOTEL
  (user(gender female)(state FL)(purpose B)(stay-place hotel)(month-of-travel 11) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"     
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule FL-DEC-FEMALE-B-HOTEL
  (user(gender female)(state FL)(purpose B)(stay-place hotel)(month-of-travel 12) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   )


;Florida, female , personal, hotel


(defrule FL-JAN-FEMALE-P-HOTEL
  (user(gender female)(state FL)(purpose P)(stay-place hotel)(month-of-travel 1) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf )
   ) 
(defrule FL-FEB-FEMALE-P-HOTEL
  (user(gender female)(state FL)(purpose P)(stay-place hotel)(month-of-travel 2) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."  
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-MAR-FEMALE-P-HOTEL
  (user(gender female)(state FL)(purpose P)(stay-place hotel)(month-of-travel 3) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-APR-FEMALE-P-HOTEL
  (user(gender female)(state FL)(purpose P)(stay-place hotel)(month-of-travel 4) )  
  =>       
    (printout t  crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."  
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf )
   ) 
(defrule FL-MAY-FEMALE-P-HOTEL
  (user(gender female)(state FL)(purpose P)(stay-place hotel)(month-of-travel 5) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank." 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-JUN-FEMALE-P-HOTEL
  (user(gender female)(state FL)(purpose P)(stay-place hotel)(month-of-travel 6) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-JULY-FEMALE-P-HOTEL
  (user(gender female)(state FL)(purpose P)(stay-place hotel)(month-of-travel 7) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-AUG-FEMALE-P-HOTEL
  (user(gender female)(state FL)(purpose P)(stay-place hotel)(month-of-travel 8) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."  
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-SEPT-FEMALE-P-HOTEL
  (user(gender female)(state FL)(purpose P)(stay-place hotel)(month-of-travel 9) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-OCT-FEMALE-P-HOTEL
  (user(gender female)(state FL)(purpose P)(stay-place hotel)(month-of-travel 10) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!" 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-NOV-FEMALE-P-HOTEL
  (user(gender female)(state FL)(purpose P)(stay-place hotel)(month-of-travel 11) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank." 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-DEC-FEMALE-P-HOTEL
  (user(gender female)(state FL)(purpose P)(stay-place hotel)(month-of-travel 12) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   )



;Washington, male , business, hotel


(defrule WA-JAN-MALE-B-HOTEL
  (user(gender male)(state WA)(purpose B)(stay-place hotel)(month-of-travel 1) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule WA-FEB-MALE-B-HOTEL
  (user(gender male)(state WA)(purpose B)(stay-place hotel)(month-of-travel 2) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule WA-MAR-MALE-B-HOTEL
  (user(gender male)(state WA)(purpose B)(stay-place hotel)(month-of-travel 3) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule WA-APR-MALE-B-HOTEL
  (user(gender male)(state WA)(purpose B)(stay-place hotel)(month-of-travel 4) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule WA-MAY-MALE-B-HOTEL
  (user(gender male)(state WA)(purpose B)(stay-place hotel)(month-of-travel 5) )  
  =>       
    (printout t crlf "Its going to be a pleasant trip!! You'll enjoy the wather there!"
        crlf "->Carry a pair of shoes,(not bad if you can carry sandals if you prefer),"
        crlf "->sweatshirt or pullover, a simple jacket,jeans,shirts,tee-shirts "
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule WA-JUN-MALE-B-HOTEL
  (user(gender male)(state WA)(purpose B)(stay-place hotel)(month-of-travel 6) )  
  =>       
    (printout t crlf "It will be bit  Pleasant over there! But it might get cold."
        crlf "->Pack pull overs or sweatshirt just be on safer side."
        crlf "->Carry moisturiser, cold cream, jeans, shoes or sandals, a pair of shorts." 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule WA-JULY-MALE-B-HOTEL
  (user(gender male)(state WA)(purpose B)(stay-place hotel)(month-of-travel 7) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule WA-AUG-MALE-B-HOTEL
  (user(gender male)(state WA)(purpose B)(stay-place hotel)(month-of-travel 8) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule WA-SEPT-MALE-B-HOTEL
  (user(gender male)(state WA)(purpose B)(stay-place hotel)(month-of-travel 9) )  
  =>       
    (printout t crlf "It will be bit Pleasant over there! But it might get cold."
        crlf "->Pack pull overs or sweatshirt just be on safer side."
        crlf "->Carry moisturiser, cold cream, jeans, shoes or sandals, a pair of shorts."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule WA-OCT-MALE-B-HOTEL
  (user(gender male)(state WA)(purpose B)(stay-place hotel)(month-of-travel 10) )  
  =>       
    (printout t crlf "Its going to be a pleasant trip!! You'll enjoy the wather there!"
        crlf "->Carry a pair of shoes,(not bad if you can carry sandals if you prefer),"
        crlf "->sweatshirt or pullover, a simple jacket,jeans,shirts,tee-shirts " 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule WA-NOV-MALE-B-HOTEL
  (user(gender male)(state WA)(purpose B)(stay-place hotel)(month-of-travel 11) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule WA-DEC-MALE-B-HOTEL
  (user(gender male)(state WA)(purpose B)(stay-place hotel)(month-of-travel 12) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   )


;Washington, male , personal, hotel



(defrule WA-JAN-MALE-P-HOTEL
  (user(gender male)(state WA)(purpose P)(stay-place hotel)(month-of-travel 1) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-FEB-MALE-P-HOTEL
  (user(gender male)(state WA)(purpose P)(stay-place hotel)(month-of-travel 2) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."  
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-MAR-MALE-P-HOTEL
  (user(gender male)(state WA)(purpose P)(stay-place hotel)(month-of-travel 3) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket " 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-APR-MALE-P-HOTEL
  (user(gender male)(state WA)(purpose P)(stay-place hotel)(month-of-travel 4) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket " 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-MAY-MALE-P-HOTEL
  (user(gender male)(state WA)(purpose P)(stay-place hotel)(month-of-travel 5) )  
  =>       
    (printout t crlf "Its going to be a pleasant trip!! You'll enjoy the wather there!"
        crlf "->Carry a pair of shoes,(not bad if you can carry sandals if you prefer),"
        crlf "->sweatshirt or pullover, a simple jacket,jeans,shirts,tee-shirts " 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-JUN-MALE-P-HOTEL
  (user(gender male)(state WA)(purpose P)(stay-place hotel)(month-of-travel 6) )  
  =>       
    (printout t crlf "It will be bit  Pleasant over there! But it might get cold."
        crlf "->Pack pull overs or sweatshirt just be on safer side."
        crlf "->Carry moisturiser, cold cream, jeans, shoes or sandals, a pair of shorts." 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-JULY-MALE-P-HOTEL
  (user(gender male)(state WA)(purpose P)(stay-place hotel)(month-of-travel 7) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-AUG-MALE-P-HOTEL
  (user(gender male)(state WA)(purpose P)(stay-place hotel)(month-of-travel 8) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-SEPT-MALE-P-HOTEL
  (user(gender male)(state WA)(purpose P)(stay-place hotel)(month-of-travel 9) )  
  =>       
    (printout t crlf "It will be bit  Pleasant over there! But it might get cold."
        crlf "->Pack pull overs or sweatshirt just be on safer side."
        crlf "->Carry moisturiser, cold cream, jeans, shoes or sandals, a pair of shorts."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-OCT-MALE-P-HOTEL
  (user(gender male)(state WA)(purpose P)(stay-place hotel)(month-of-travel 10) )  
  =>       
    (printout t crlf "Its going to be a pleasant trip!! You'll enjoy the wather there!"
        crlf "->Carry a pair of shoes,(not bad if you can carry sandals if you prefer),"
        crlf "->sweatshirt or pullover, a simple jacket,jeans,shirts,tee-shirts " 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-NOV-MALE-P-HOTEL
  (user(gender male)(state WA)(purpose P)(stay-place hotel)(month-of-travel 11) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."  
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-DEC-MALE-P-HOTEL
  (user(gender male)(state WA)(purpose P)(stay-place hotel)(month-of-travel 12) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."  
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   )


;Washington, female , business, hotel


(defrule WA-JAN-FEMALE-B-HOTEL
  (user(gender female)(state WA)(purpose B)(stay-place hotel)(month-of-travel 1) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule WA-FEB-FEMALE-B-HOTEL
  (user(gender female)(state WA)(purpose B)(stay-place hotel)(month-of-travel 2) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket " 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule WA-MAR-FEMALE-B-HOTEL
  (user(gender female)(state WA)(purpose B)(stay-place hotel)(month-of-travel 3) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket " 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule WA-APR-FEMALE-B-HOTEL
  (user(gender female)(state WA)(purpose B)(stay-place hotel)(month-of-travel 4) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket " 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule WA-MAY-FEMALE-B-HOTEL
  (user(gender female)(state WA)(purpose B)(stay-place hotel)(month-of-travel 5) )  
  =>       
    (printout t crlf "Its going to be a pleasant trip!! You'll enjoy the wather there!"
        crlf "->Carry a pair of shoes,(not bad if you can carry sandals if you prefer),"
        crlf "->sweatshirt or pullover, a simple jacket,jeans,shirts,tee-shirts "
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule WA-JUN-FEMALE-B-HOTEL
  (user(gender female)(state WA)(purpose B)(stay-place hotel)(month-of-travel 6) )  
  =>       
    (printout t crlf "It will be bit  Pleasant over there! But it might get cold."
        crlf "->Pack pull overs or sweatshirt just be on safer side."
        crlf "->Carry moisturiser, cold cream, jeans, shoes or sandals, a pair of shorts."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule WA-JULY-FEMALE-B-HOTEL
  (user(gender female)(state WA)(purpose B)(stay-place hotel)(month-of-travel 7) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule WA-AUG-FEMALE-B-HOTEL
  (user(gender female)(state WA)(purpose B)(stay-place hotel)(month-of-travel 8) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule WA-SEPT-FEMALE-B-HOTEL
  (user(gender female)(state WA)(purpose B)(stay-place hotel)(month-of-travel 9) )  
  =>       
    (printout t crlf "It will be bit Pleasant over there! But it might get cold."
        crlf "->Pack pull overs or sweatshirt just be on safer side."
        crlf "->Carry moisturiser, cold cream, jeans, shoes or sandals, a pair of shorts."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule WA-OCT-FEMALE-B-HOTEL
  (user(gender female)(state WA)(purpose B)(stay-place hotel)(month-of-travel 10) )  
  =>       
    (printout t crlf "Its going to be a pleasant trip!! You'll enjoy the wather there!"
        crlf "->Carry a pair of shoes,(not bad if you can carry sandals if you prefer),"
        crlf "->sweatshirt or pullover, a simple jacket,jeans,shirts,tee-shirts "
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule WA-NOV-FEMALE-B-HOTEL
  (user(gender female)(state WA)(purpose B)(stay-place hotel)(month-of-travel 11) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule WA-DEC-FEMALE-B-HOTEL
  (user(gender female)(state WA)(purpose B)(stay-place hotel)(month-of-travel 12) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf  
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   )



;Washington, female , personal, hotel


(defrule WA-JAN-FEMALE-P-HOTEL
  (user(gender female)(state WA)(purpose P)(stay-place hotel)(month-of-travel 1) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf  
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-FEB-FEMALE-P-HOTEL
  (user(gender female)(state WA)(purpose P)(stay-place hotel)(month-of-travel 2) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-MAR-FEMALE-P-HOTEL
  (user(gender female)(state WA)(purpose P)(stay-place hotel)(month-of-travel 3) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-APR-FEMALE-P-HOTEL
  (user(gender female)(state WA)(purpose P)(stay-place hotel)(month-of-travel 4) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-MAY-FEMALE-P-HOTEL
  (user(gender female)(state WA)(purpose P)(stay-place hotel)(month-of-travel 5) )  
  =>       
    (printout t crlf "Its going to be a pleasant trip!! You'll enjoy the wather there!"
        crlf "->Carry a pair of shoes,(not bad if you can carry sandals if you prefer),"
        crlf "->sweatshirt or pullover, a simple jacket,jeans,shirts,tee-shirts "
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-JUN-FEMALE-P-HOTEL
  (user(gender female)(state WA)(purpose P)(stay-place hotel)(month-of-travel 6) )  
  =>       
    (printout t crlf "It will be bit Pleasant over there! But it might get cold."
        crlf "->Pack pull overs or sweatshirt just be on safer side."
        crlf "->Carry moisturiser, cold cream, jeans, shoes or sandals, a pair of shorts."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)."  
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-JULY-FEMALE-P-HOTEL
  (user(gender female)(state WA)(purpose P)(stay-place hotel)(month-of-travel 7) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-AUG-FEMALE-P-HOTEL
  (user(gender female)(state WA)(purpose P)(stay-place hotel)(month-of-travel 8) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf )
   ) 
(defrule WA-SEPT-FEMALE-P-HOTEL
  (user(gender female)(state WA)(purpose P)(stay-place hotel)(month-of-travel 9) )  
  =>       
    (printout t crlf "It will be bit Pleasant over there! But it might get cold."
        crlf "->Pack pull overs or sweatshirt just be on safer side."
        crlf "->Carry moisturiser, cold cream, jeans, shoes or sandals, a pair of shorts."
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-OCT-FEMALE-P-HOTEL
  (user(gender female)(state WA)(purpose P)(stay-place hotel)(month-of-travel 10) )  
  =>       
    (printout t crlf "Its going to be a pleasant trip!! You'll enjoy the wather there!"
        crlf "->Carry a pair of shoes,(not bad if you can carry sandals if you prefer),"
        crlf "->sweatshirt or pullover, a simple jacket,jeans,shirts,tee-shirts "
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-NOV-FEMALE-P-HOTEL
  (user(gender female)(state WA)(purpose P)(stay-place hotel)(month-of-travel 11) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   )
(defrule WA-DEC-FEMALE-P-HOTEL
  (user(gender female)(state WA)(purpose P)(stay-place hotel)(month-of-travel 12) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->No need to carry toiletries(But if you need any personal care it's better to pack it as well!!)." 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf )
   )



;California, male , business, airbnb

(defrule CA-JAN-MALE-B-AIRBNB
  (user(gender male)(state CA)(purpose B)(stay-place airbnb)(month-of-travel 1) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!"
		crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"  
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule CA-FEB-MALE-B-AIRBNB
  (user(gender male)(state CA)(purpose B)(stay-place airbnb)(month-of-travel 2) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule CA-MAR-MALE-B-AIRBNB
  (user(gender male)(state CA)(purpose B)(stay-place airbnb)(month-of-travel 3) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule CA-APR-MALE-B-AIRBNB
  (user(gender male)(state CA)(purpose B)(stay-place airbnb)(month-of-travel 4) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule CA-MAY-MALE-B-AIRBNB
  (user(gender male)(state CA)(purpose B)(stay-place airbnb)(month-of-travel 5) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule CA-JUN-MALE-B-AIRBNB
  (user(gender male)(state CA)(purpose B)(stay-place airbnb)(month-of-travel 6) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule CA-JULY-MALE-B-AIRBNB
  (user(gender male)(state CA)(purpose B)(stay-place airbnb)(month-of-travel 7) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule CA-AUG-MALE-B-AIRBNB
  (user(gender male)(state CA)(purpose B)(stay-place airbnb)(month-of-travel 8) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule CA-SEPT-MALE-B-AIRBNB
  (user(gender male)(state CA)(purpose B)(stay-place airbnb)(month-of-travel 9) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule CA-OCT-MALE-B-AIRBNB
  (user(gender male)(state CA)(purpose B)(stay-place airbnb)(month-of-travel 10) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule CA-NOV-MALE-B-AIRBNB
  (user(gender male)(state CA)(purpose B)(stay-place airbnb)(month-of-travel 11) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule CA-DEC-MALE-B-AIRBNB
  (user(gender male)(state CA)(purpose B)(stay-place airbnb)(month-of-travel 12) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 



;California, male , personal, airbnb


(defrule CA-JAN-MALE-P-AIRBNB
  (user(gender male)(state CA)(purpose P)(stay-place airbnb)(month-of-travel 1) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"   
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-FEB-MALE-P-AIRBNB
  (user(gender male)(state CA)(purpose P)(stay-place airbnb)(month-of-travel 2) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-MAR-MALE-P-AIRBNB
  (user(gender male)(state CA)(purpose P)(stay-place airbnb)(month-of-travel 3) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-APR-MALE-P-AIRBNB
  (user(gender male)(state CA)(purpose P)(stay-place airbnb)(month-of-travel 4) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-MAY-MALE-P-AIRBNB
  (user(gender male)(state CA)(purpose P)(stay-place airbnb)(month-of-travel 5) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-JUN-MALE-P-AIRBNB
  (user(gender male)(state CA)(purpose P)(stay-place airbnb)(month-of-travel 6) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-JULY-MALE-P-AIRBNB
  (user(gender male)(state CA)(purpose P)(stay-place airbnb)(month-of-travel 7) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-AUG-MALE-P-AIRBNB
  (user(gender male)(state CA)(purpose P)(stay-place airbnb)(month-of-travel 8) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-SEPT-MALE-P-AIRBNB
  (user(gender male)(state CA)(purpose P)(stay-place airbnb)(month-of-travel 9) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."  
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-OCT-MALE-P-AIRBNB
  (user(gender male)(state CA)(purpose P)(stay-place airbnb)(month-of-travel 10) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf )
   ) 
(defrule CA-NOV-MALE-P-AIRBNB
  (user(gender male)(state CA)(purpose P)(stay-place airbnb)(month-of-travel 11) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-DEC-MALE-P-AIRBNB
  (user(gender male)(state CA)(purpose P)(stay-place airbnb)(month-of-travel 12) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 



;California, female , business, airbnb

(defrule CA-JAN-FEMALE-B-AIRBNB
  (user(gender female)(state CA)(purpose B)(stay-place airbnb)(month-of-travel 1) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses." 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule CA-FEB-FEMALE-B-AIRBNB
  (user(gender female)(state CA)(purpose B)(stay-place airbnb)(month-of-travel 2) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule CA-MAR-FEMALE-B-AIRBNB
  (user(gender female)(state CA)(purpose B)(stay-place airbnb)(month-of-travel 3) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule CA-APR-FEMALE-B-AIRBNB
  (user(gender female)(state CA)(purpose B)(stay-place airbnb)(month-of-travel 4) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule CA-MAY-FEMALE-B-AIRBNB
  (user(gender female)(state CA)(purpose B)(stay-place airbnb)(month-of-travel 5) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule CA-JUN-FEMALE-B-AIRBNB
  (user(gender female)(state CA)(purpose B)(stay-place airbnb)(month-of-travel 6) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"    
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule CA-JULY-FEMALE-B-AIRBNB
  (user(gender female)(state CA)(purpose B)(stay-place airbnb)(month-of-travel 7) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."  
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule CA-AUG-FEMALE-B-AIRBNB
  (user(gender female)(state CA)(purpose B)(stay-place airbnb)(month-of-travel 8) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank." 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule CA-SEPT-FEMALE-B-AIRBNB
  (user(gender female)(state CA)(purpose B)(stay-place airbnb)(month-of-travel 9) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."  
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule CA-OCT-FEMALE-B-AIRBNB
  (user(gender female)(state CA)(purpose B)(stay-place airbnb)(month-of-travel 10) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"    
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule CA-NOV-FEMALE-B-AIRBNB
  (user(gender female)(state CA)(purpose B)(stay-place airbnb)(month-of-travel 11) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   ) 
(defrule CA-DEC-FEMALE-B-AIRBNB
  (user(gender female)(state CA)(purpose B)(stay-place airbnb)(month-of-travel 12) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf)
   )



;California, female , prsonal, airbnb


(defrule CA-JAN-FEMALE-P-AIRBNB
  (user(gender female)(state CA)(purpose P)(stay-place airbnb)(month-of-travel 1) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->tank tops, vneck tops, jorts, maxidresses." 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-FEB-FEMALE-P-AIRBNB
  (user(gender female)(state CA)(purpose P)(stay-place airbnb)(month-of-travel 2) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-MAR-FEMALE-P-AIRBNB
  (user(gender female)(state CA)(purpose P)(stay-place airbnb)(month-of-travel 3) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."  
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-APR-FEMALE-P-AIRBNB
  (user(gender female)(state CA)(purpose P)(stay-place airbnb)(month-of-travel 4) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-MAY-FEMALE-P-AIRBNB
  (user(gender female)(state CA)(purpose P)(stay-place airbnb)(month-of-travel 5) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-JUN-FEMALE-P-AIRBNB
  (user(gender female)(state CA)(purpose P)(stay-place airbnb)(month-of-travel 6) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."  
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-JULY-FEMALE-P-AIRBNB
  (user(gender female)(state CA)(purpose P)(stay-place airbnb)(month-of-travel 7) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."  
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-AUG-FEMALE-P-AIRBNB
  (user(gender female)(state CA)(purpose P)(stay-place airbnb)(month-of-travel 8) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"    
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-SEPT-FEMALE-P-AIRBNB
  (user(gender female)(state CA)(purpose P)(stay-place airbnb)(month-of-travel 9) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank." 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-OCT-FEMALE-P-AIRBNB
  (user(gender female)(state CA)(purpose P)(stay-place airbnb)(month-of-travel 10) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."  
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-NOV-FEMALE-P-AIRBNB
  (user(gender female)(state CA)(purpose P)(stay-place airbnb)(month-of-travel 11) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule CA-DEC-FEMALE-P-AIRBNB
  (user(gender female)(state CA)(purpose P)(stay-place airbnb)(month-of-travel 12) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   )



;Illinois, male , business, airbnb


(defrule IL-JAN-MALE-B-AIRBNB
  (user(gender male)(state IL)(purpose B)(stay-place airbnb)(month-of-travel 1) )  
  =>       
    (printout t crlf "Its going to be very cold!!"
        crlf "->Pack your thermals,cold creams, snow boots(if you have one!)"
        crlf "->gloves, balaclava or beanie, an umbrella, scarves, pair of shoes(water resistant would be good!)." 
        crlf "->(won't suggest you to carry sandals), a normal jacket, a show jacket as well!"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule IL-FEB-MALE-B-AIRBNB
  (user(gender male)(state IL)(purpose B)(stay-place airbnb)(month-of-travel 2) )  
  =>       
    (printout t crlf "Its going to be very cold!!"
        crlf "->Pack your thermals,cold creams, snow boots(if you have one!)"
        crlf "->gloves, balaclava or beanie, an umbrella, scarves, pair of shoes(water resistant would be good!)." 
        crlf "->(won't suggest you to carry sandals), a normal jacket, a show jacket as well!"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule IL-MAR-MALE-B-AIRBNB
  (user(gender male)(state IL)(purpose B)(stay-place airbnb)(month-of-travel 3) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule IL-APR-MALE-B-AIRBNB
  (user(gender male)(state IL)(purpose B)(stay-place airbnb)(month-of-travel 4) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf  )
   ) 
(defrule IL-MAY-MALE-B-AIRBNB
  (user(gender male)(state IL)(purpose B)(stay-place airbnb)(month-of-travel 5) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule IL-JUN-MALE-B-AIRBNB
  (user(gender male)(state IL)(purpose B)(stay-place airbnb)(month-of-travel 6) )  
  =>       
    (printout t "Its going to be bit hot there! 
        Carry sunsreen, light colour tee-shirts, sandals, flipflops, shorts,
         few jeans, shades, a pair of shoes " crlf 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule IL-JULY-MALE-B-AIRBNB
  (user(gender male)(state IL)(purpose B)(stay-place airbnb)(month-of-travel 7) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule IL-AUG-MALE-B-AIRBNB
  (user(gender male)(state IL)(purpose B)(stay-place airbnb)(month-of-travel 8) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule IL-SEPT-MALE-B-AIRBNB
  (user(gender male)(state IL)(purpose B)(stay-place airbnb)(month-of-travel 9) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule IL-OCT-MALE-B-AIRBNB
  (user(gender male)(state IL)(purpose B)(stay-place airbnb)(month-of-travel 10) )  
  =>       
    (printout t crlf "Its going to be a pleasant trip!! You'll enjoy the weather there!"
        crlf "->Carry a pair of shoes,(not bad if you can carry sandals if you prefer),"
        crlf "->sweatshirt or pullover, a simple jacket,jeans,shirts,tee-shirts "
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf  )
   ) 
(defrule IL-NOV-MALE-B-AIRBNB
  (user(gender male)(state IL)(purpose B)(stay-place airbnb)(month-of-travel 11) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule IL-DEC-MALE-B-AIRBNB
  (user(gender male)(state IL)(purpose B)(stay-place airbnb)(month-of-travel 12) )  
  =>       
    (printout t crlf "Its going to be very cold!!"
        crlf "->Pack your thermals,cold creams, snow boots(if you have one!)"
        crlf "->gloves, balaclava or beanie, an umbrella, scarves, pair of shoes(water resistant would be good!)." 
        crlf "->(won't suggest you to carry sandals), a normal jacket, a show jacket as well!" 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 


;Illinois, male , personal, airbnb



(defrule IL-JAN-MALE-P-AIRBNB
  (user(gender male)(state IL)(purpose P)(stay-place airbnb)(month-of-travel 1) )  
  =>       
    (printout t crlf "Its going to be very cold!!"
        crlf "->Pack your thermals,cold creams, snow boots(if you have one!)"
        crlf "->gloves, balaclava or beanie, an umbrella, scarves, pair of shoes(water resistant would be good!)." 
        crlf "->(won't suggest you to carry sandals), a normal jacket, a show jacket as well!"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."  
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-FEB-MALE-P-AIRBNB
  (user(gender male)(state IL)(purpose P)(stay-place airbnb)(month-of-travel 2) )  
  =>       
    (printout t crlf "Its going to be very cold!!"
        crlf "->Pack your thermals,cold creams, snow boots(if you have one!)"
        crlf "->gloves, balaclava or beanie, an umbrella, scarves, pair of shoes(water resistant would be good!)." 
        crlf "->(won't suggest you to carry sandals), a normal jacket, a show jacket as well!"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-MAR-MALE-P-AIRBNB
  (user(gender male)(state IL)(purpose P)(stay-place airbnb)(month-of-travel 3) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-APR-MALE-P-AIRBNB
  (user(gender male)(state IL)(purpose P)(stay-place airbnb)(month-of-travel 4) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-MAY-MALE-P-AIRBNB
  (user(gender male)(state IL)(purpose P)(stay-place airbnb)(month-of-travel 5) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-JUN-MALE-P-AIRBNB
  (user(gender male)(state IL)(purpose P)(stay-place airbnb)(month-of-travel 6) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-JULY-MALE-P-AIRBNB
  (user(gender male)(state IL)(purpose P)(stay-place airbnb)(month-of-travel 7) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."  
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-AUG-MALE-P-AIRBNB
  (user(gender male)(state IL)(purpose P)(stay-place airbnb)(month-of-travel 8) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."  
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-SEPT-MALE-P-AIRBNB
  (user(gender male)(state IL)(purpose P)(stay-place airbnb)(month-of-travel 9) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-OCT-MALE-P-AIRBNB
  (user(gender male)(state IL)(purpose P)(stay-place airbnb)(month-of-travel 10) )  
  =>       
    (printout t crlf "Its going to be a pleasant trip!! You'll enjoy the weather there!"
        crlf "->Carry a pair of shoes,(not bad if you can carry sandals if you prefer),"
        crlf "->sweatshirt or pullover, a simple jacket,jeans,shirts,tee-shirts " 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-NOV-MALE-P-AIRBNB
  (user(gender male)(state IL)(purpose P)(stay-place airbnb)(month-of-travel 11) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-DEC-MALE-P-AIRBNB
  (user(gender male)(state IL)(purpose P)(stay-place airbnb)(month-of-travel 12) )  
  =>      
    (printout t crlf "Its going to be very cold!!"
        crlf "->Pack your thermals,cold creams, snow boots(if you have one!)"
        crlf "->gloves, balaclava or beanie, an umbrella, scarves, pair of shoes(water resistant would be good!)." 
        crlf "->(won't suggest you to carry sandals), a normal jacket, a show jacket as well!"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf )
   ) 



;Illinois, female , business, airbnb


(defrule IL-JAN-FEMALE-B-AIRBNB
  (user(gender female)(state IL)(purpose B)(stay-place airbnb)(month-of-travel 1) )  
  =>       
    (printout t crlf "Its going to be very cold!!"
        crlf "->Pack your thermals,cold creams, snow boots(if you have one!)"
        crlf "->gloves, balaclava or beanie, an umbrella, scarves, pair of shoes(water resistant would be good!)." 
        crlf "->(won't suggest you to carry sandals), a normal jacket, a show jacket as well!" 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule IL-FEB-FEMALE-B-AIRBNB
  (user(gender female)(state IL)(purpose B)(stay-place airbnb)(month-of-travel 2) )  
  =>       
    (printout t crlf "Its going to be very cold!!"
        crlf "->Pack your thermals,cold creams, snow boots(if you have one!)"
        crlf "->gloves, balaclava or beanie, an umbrella, scarves, pair of shoes(water resistant would be good!)." 
        crlf "->(won't suggest you to carry sandals), a normal jacket, a show jacket as well!"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule IL-MAR-FEMALE-B-AIRBNB
  (user(gender female)(state IL)(purpose B)(stay-place airbnb)(month-of-travel 3) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule IL-APR-FEMALE-B-AIRBNB
  (user(gender female)(state IL)(purpose B)(stay-place airbnb)(month-of-travel 4) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf  )
   ) 
(defrule IL-MAY-FEMALE-B-AIRBNB
  (user(gender female)(state IL)(purpose B)(stay-place airbnb)(month-of-travel 5) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule IL-JUN-FEMALE-B-AIRBNB
  (user(gender female)(state IL)(purpose B)(stay-place airbnb)(month-of-travel 6) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."  
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule IL-JULY-FEMALE-B-AIRBNB
  (user(gender female)(state IL)(purpose B)(stay-place airbnb)(month-of-travel 7) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."  
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule IL-AUG-FEMALE-B-AIRBNB
  (user(gender female)(state IL)(purpose B)(stay-place airbnb)(month-of-travel 8) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."  
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule IL-SEPT-FEMALE-B-AIRBNB
  (user(gender female)(state IL)(purpose B)(stay-place airbnb)(month-of-travel 9) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule IL-OCT-FEMALE-B-AIRBNB
  (user(gender female)(state IL)(purpose B)(stay-place airbnb)(month-of-travel 10) )  
  =>       
    (printout t crlf "Its going to be a pleasant trip!! You'll enjoy the weather there!"
        crlf "->Carry a pair of shoes,(not bad if you can carry sandals if you prefer),"
        crlf "->sweatshirt or pullover, a simple jacket,jeans,shirts,tee-shirts "
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf  )
   ) 
(defrule IL-NOV-FEMALE-B-AIRBNB
  (user(gender female)(state IL)(purpose B)(stay-place airbnb)(month-of-travel 11) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule IL-DEC-FEMALE-B-AIRBNB
  (user(gender female)(state IL)(purpose B)(stay-place airbnb)(month-of-travel 12) )  
  =>       
    (printout t crlf "Its going to be very cold!!"
        crlf "->Pack your thermals,cold creams, snow boots(if you have one!)"
        crlf "->gloves, balaclava or beanie, an umbrella, scarves, pair of shoes(water resistant would be good!)." 
        crlf "->(won't suggest you to carry sandals), a normal jacket, a show jacket as well!" 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   )



;Illinois, female , personal, airbnb



(defrule IL-JAN-FEMALE-P-AIRBNB
  (user(gender female)(state IL)(purpose P)(stay-place airbnb)(month-of-travel 1) )  
  =>       
    (printout t crlf "Its going to be very cold!!"
        crlf "->Pack your thermals,cold creams, snow boots(if you have one!)"
        crlf "->gloves, balaclava or beanie, an umbrella, scarves, pair of shoes(water resistant would be good!)." 
        crlf "->(won't suggest you to carry sandals), a normal jacket, a show jacket as well!"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."  
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-FEB-FEMALE-P-AIRBNB
  (user(gender female)(state IL)(purpose P)(stay-place airbnb)(month-of-travel 2) )  
  =>       
    (printout t crlf "Its going to be very cold!!"
        crlf "->Pack your thermals,cold creams, snow boots(if you have one!)"
        crlf "->gloves, balaclava or beanie, an umbrella, scarves, pair of shoes(water resistant would be good!)." 
        crlf "->(won't suggest you to carry sandals), a normal jacket, a show jacket as well!"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."  
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-MAR-FEMALE-P-AIRBNB
  (user(gender female)(state IL)(purpose P)(stay-place airbnb)(month-of-travel 3) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-APR-FEMALE-P-AIRBNB
  (user(gender female)(state IL)(purpose P)(stay-place airbnb)(month-of-travel 4) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-MAY-FEMALE-P-AIRBNB
  (user(gender female)(state IL)(purpose P)(stay-place airbnb)(month-of-travel 5) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-JUN-FEMALE-P-AIRBNB
  (user(gender female)(state IL)(purpose P)(stay-place airbnb)(month-of-travel 6) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."  
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-JULY-FEMALE-P-AIRBNB
  (user(gender female)(state IL)(purpose P)(stay-place airbnb)(month-of-travel 7) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"    
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-AUG-FEMALE-P-AIRBNB
  (user(gender female)(state IL)(purpose P)(stay-place airbnb)(month-of-travel 8) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank." 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-SEPT-FEMALE-P-AIRBNB
  (user(gender female)(state IL)(purpose P)(stay-place airbnb)(month-of-travel 9) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-OCT-FEMALE-P-AIRBNB
  (user(gender female)(state IL)(purpose P)(stay-place airbnb)(month-of-travel 10) )  
  =>       
    (printout t crlf "Its going to be a pleasant trip!! You'll enjoy the weather there!"
        crlf "->Carry a pair of shoes,(not bad if you can carry sandals if you prefer),"
        crlf "->sweatshirt or pullover, a simple jacket,jeans,shirts,tee-shirts "
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."  
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-NOV-FEMALE-P-AIRBNB
  (user(gender female)(state IL)(purpose P)(stay-place airbnb)(month-of-travel 11) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule IL-DEC-FEMALE-P-AIRBNB
  (user(gender female)(state IL)(purpose P)(stay-place airbnb)(month-of-travel 12) )  
  =>       
    (printout t crlf "Its going to be very cold!!"
        crlf "->Pack your thermals,cold creams, snow boots(if you have one!)"
        crlf "->gloves, balaclava or beanie, an umbrella, scarves, pair of shoes(water resistant would be good!)." 
        crlf "->(won't suggest you to carry sandals), a normal jacket, a show jacket as well!" 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   )



;Florida, male , business, airbnb



(defrule FL-JAN-MALE-B-AIRBNB
  (user(gender male)(state FL)(purpose B)(stay-place airbnb)(month-of-travel 1) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule FL-FEB-MALE-B-AIRBNB
  (user(gender male)(state FL)(purpose B)(stay-place airbnb)(month-of-travel 2) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule FL-MAR-MALE-B-AIRBNB
  (user(gender male)(state FL)(purpose B)(stay-place airbnb)(month-of-travel 3) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule FL-APR-MALE-B-AIRBNB
  (user(gender male)(state FL)(purpose B)(stay-place airbnb)(month-of-travel 4) )  
  =>       
    (printout t  crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule FL-MAY-MALE-B-AIRBNB
  (user(gender male)(state FL)(purpose B)(stay-place airbnb)(month-of-travel 5) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf  )
   ) 
(defrule FL-JUN-MALE-B-AIRBNB
  (user(gender male)(state FL)(purpose B)(stay-place airbnb)(month-of-travel 6) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule FL-JULY-MALE-B-AIRBNB
  (user(gender male)(state FL)(purpose B)(stay-place airbnb)(month-of-travel 7) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule FL-AUG-MALE-B-AIRBNB
  (user(gender male)(state FL)(purpose B)(stay-place airbnb)(month-of-travel 8) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!" 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule FL-SEPT-MALE-B-AIRBNB
  (user(gender male)(state FL)(purpose B)(stay-place airbnb)(month-of-travel 9) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule FL-OCT-MALE-B-AIRBNB
  (user(gender male)(state FL)(purpose B)(stay-place airbnb)(month-of-travel 10) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule FL-NOV-MALE-B-AIRBNB
  (user(gender male)(state FL)(purpose B)(stay-place airbnb)(month-of-travel 11) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule FL-DEC-MALE-B-AIRBNB
  (user(gender male)(state FL)(purpose B)(stay-place airbnb)(month-of-travel 12) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 



;Florida, male , personal, airbnb



(defrule FL-JAN-MALE-P-AIRBNB
  (user(gender male)(state FL)(purpose P)(stay-place airbnb)(month-of-travel 1) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-FEB-MALE-P-AIRBNB
  (user(gender male)(state FL)(purpose P)(stay-place airbnb)(month-of-travel 2) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-MAR-MALE-P-AIRBNB
  (user(gender male)(state FL)(purpose P)(stay-place airbnb)(month-of-travel 3) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."  
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-APR-MALE-P-AIRBNB
  (user(gender male)(state FL)(purpose P)(stay-place airbnb)(month-of-travel 4) )  
  =>       
    (printout t  crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-MAY-MALE-P-AIRBNB
  (user(gender male)(state FL)(purpose P)(stay-place airbnb)(month-of-travel 5) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-JUN-MALE-P-AIRBNB
  (user(gender male)(state FL)(purpose P)(stay-place airbnb)(month-of-travel 6) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-JULY-MALE-P-AIRBNB
  (user(gender male)(state FL)(purpose P)(stay-place airbnb)(month-of-travel 7) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf )
   ) 
(defrule FL-AUG-MALE-P-AIRBNB
  (user(gender male)(state FL)(purpose P)(stay-place airbnb)(month-of-travel 8) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."  
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-SEPT-MALE-P-AIRBNB
  (user(gender male)(state FL)(purpose P)(stay-place airbnb)(month-of-travel 9) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."  
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-OCT-MALE-P-AIRBNB
  (user(gender male)(state FL)(purpose P)(stay-place airbnb)(month-of-travel 10) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!" 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-NOV-MALE-P-AIRBNB
  (user(gender male)(state FL)(purpose P)(stay-place airbnb)(month-of-travel 11) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-DEC-MALE-P-AIRBNB
  (user(gender male)(state FL)(purpose P)(stay-place airbnb)(month-of-travel 12) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 



;Florida, female , business, airbnb


(defrule FL-JAN-FEMALE-B-AIRBNB
  (user(gender female)(state FL)(purpose B)(stay-place airbnb)(month-of-travel 1) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule FL-FEB-FEMALE-B-AIRBNB
  (user(gender female)(state FL)(purpose B)(stay-place airbnb)(month-of-travel 2) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule FL-MAR-FEMALE-B-AIRBNB
  (user(gender female)(state FL)(purpose B)(stay-place airbnb)(month-of-travel 3) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule FL-APR-FEMALE-B-AIRBNB
  (user(gender female)(state FL)(purpose B)(stay-place airbnb)(month-of-travel 4) )  
  =>       
    (printout t  crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank." 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule FL-MAY-FEMALE-B-AIRBNB
  (user(gender female)(state FL)(purpose B)(stay-place airbnb)(month-of-travel 5) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank." 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule FL-JUN-FEMALE-B-AIRBNB
  (user(gender female)(state FL)(purpose B)(stay-place airbnb)(month-of-travel 6) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!" 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule FL-JULY-FEMALE-B-AIRBNB
  (user(gender female)(state FL)(purpose B)(stay-place airbnb)(month-of-travel 7) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule FL-AUG-FEMALE-B-AIRBNB
  (user(gender female)(state FL)(purpose B)(stay-place airbnb)(month-of-travel 8) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule FL-SEPT-FEMALE-B-AIRBNB
  (user(gender female)(state FL)(purpose B)(stay-place airbnb)(month-of-travel 9) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule FL-OCT-FEMALE-B-AIRBNB
  (user(gender female)(state FL)(purpose B)(stay-place airbnb)(month-of-travel 10) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf  )
   ) 
(defrule FL-NOV-FEMALE-B-AIRBNB
  (user(gender female)(state FL)(purpose B)(stay-place airbnb)(month-of-travel 11) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"    
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule FL-DEC-FEMALE-B-AIRBNB
  (user(gender female)(state FL)(purpose B)(stay-place airbnb)(month-of-travel 12) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   )



;Florida, female , personal, airbnb


(defrule FL-JAN-FEMALE-P-AIRBNB
  (user(gender female)(state FL)(purpose P)(stay-place airbnb)(month-of-travel 1) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-FEB-FEMALE-P-AIRBNB
  (user(gender female)(state FL)(purpose P)(stay-place airbnb)(month-of-travel 2) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."  
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-MAR-FEMALE-P-AIRBNB
  (user(gender female)(state FL)(purpose P)(stay-place airbnb)(month-of-travel 3) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."  
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-APR-FEMALE-P-AIRBNB
  (user(gender female)(state FL)(purpose P)(stay-place airbnb)(month-of-travel 4) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"  
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."   
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)"  
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-MAY-FEMALE-P-AIRBNB
  (user(gender female)(state FL)(purpose P)(stay-place airbnb)(month-of-travel 5) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, " 
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
        crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank." 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-JUN-FEMALE-P-AIRBNB
  (user(gender female)(state FL)(purpose P)(stay-place airbnb)(month-of-travel 6) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-JULY-FEMALE-P-AIRBNB
  (user(gender female)(state FL)(purpose P)(stay-place airbnb)(month-of-travel 7) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."  
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-AUG-FEMALE-P-AIRBNB
  (user(gender female)(state FL)(purpose P)(stay-place airbnb)(month-of-travel 8) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."  
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-SEPT-FEMALE-P-AIRBNB
  (user(gender female)(state FL)(purpose P)(stay-place airbnb)(month-of-travel 9) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."  
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-OCT-FEMALE-P-AIRBNB
  (user(gender female)(state FL)(purpose P)(stay-place airbnb)(month-of-travel 10) )  
  =>       
    (printout t crlf "It going to be a tiring trip!! Weather there gonna drain you out!"
        crlf "->Carry sunscreen, tee shirt, sandals,filpflops, shorts, less jeans, a pair of shoes, two or more shades!"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-NOV-FEMALE-P-AIRBNB
  (user(gender female)(state FL)(purpose P)(stay-place airbnb)(month-of-travel 11) )  
  =>       
    (printout t crlf "Its going to be bit hot there!" crlf "->Carry sunsreen, an umbrella. " 
				crlf "->Cotton material non-dark colored cloths, sandals, "      
        		crlf "->Sun dress, muscle tees, bandeau tops, printed flowy pants, plain white tee, slouchy sweater, baseball tee, v-neck tank."
				crlf "->flipflops, shorts, few jeans, shades, a pair of shoes (loafers preferably)"   
		        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
		        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
		        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
		        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule FL-DEC-FEMALE-P-AIRBNB
  (user(gender female)(state FL)(purpose P)(stay-place airbnb)(month-of-travel 12) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."  
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   )



;Washington, male , business, airbnb


(defrule WA-JAN-MALE-B-AIRBNB
  (user(gender male)(state WA)(purpose B)(stay-place airbnb)(month-of-travel 1) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule WA-FEB-MALE-B-AIRBNB
  (user(gender male)(state WA)(purpose B)(stay-place airbnb)(month-of-travel 2) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule WA-MAR-MALE-B-AIRBNB
  (user(gender male)(state WA)(purpose B)(stay-place airbnb)(month-of-travel 3) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf  )
   ) 
(defrule WA-APR-MALE-B-AIRBNB
  (user(gender male)(state WA)(purpose B)(stay-place airbnb)(month-of-travel 4) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule WA-MAY-MALE-B-AIRBNB
  (user(gender male)(state WA)(purpose B)(stay-place airbnb)(month-of-travel 5) )  
  =>       
    (printout t crlf "Its going to be a pleasant trip!! You'll enjoy the weather there!"
        crlf "->Carry a pair of shoes,(not bad if you can carry sandals if you prefer),"
        crlf "->sweatshirt or pullover, a simple jacket,jeans,shirts,tee-shirts "
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule WA-JUN-MALE-B-AIRBNB
  (user(gender male)(state WA)(purpose B)(stay-place airbnb)(month-of-travel 6) )  
  =>       
    (printout t crlf "It will be bit Pleasant over there! But it might get cold."
        crlf "->Pack pull overs or sweatshirt just be on safer side."
        crlf "->Carry moisturiser, cold cream, jeans, shoes or sandals, a pair of shorts."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule WA-JULY-MALE-B-AIRBNB
  (user(gender male)(state WA)(purpose B)(stay-place airbnb)(month-of-travel 7) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule WA-AUG-MALE-B-AIRBNB
  (user(gender male)(state WA)(purpose B)(stay-place airbnb)(month-of-travel 8) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule WA-SEPT-MALE-B-AIRBNB
  (user(gender male)(state WA)(purpose B)(stay-place airbnb)(month-of-travel 9) )  
  =>       
    (printout t crlf "It will be bit Pleasant over there! But it might get cold."
        crlf "->Pack pull overs or sweatshirt just be on safer side."
        crlf "->Carry moisturiser, cold cream, jeans, shoes or sandals, a pair of shorts."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule WA-OCT-MALE-B-AIRBNB
  (user(gender male)(state WA)(purpose B)(stay-place airbnb)(month-of-travel 10) )  
  =>       
    (printout t crlf "Its going to be a pleasant trip!! You'll enjoy the weather there!"
        crlf "->Carry a pair of shoes,(not bad if you can carry sandals if you prefer),"
        crlf "->sweatshirt or pullover, a simple jacket,jeans,shirts,tee-shirts " 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule WA-NOV-MALE-B-AIRBNB
  (user(gender male)(state WA)(purpose B)(stay-place airbnb)(month-of-travel 11) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule WA-DEC-MALE-B-AIRBNB
  (user(gender male)(state WA)(purpose B)(stay-place airbnb)(month-of-travel 12) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   )



;Washington, male , personal, airbnb


(defrule WA-JAN-MALE-P-AIRBNB
  (user(gender male)(state WA)(purpose P)(stay-place airbnb)(month-of-travel 1) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-FEB-MALE-P-AIRBNB
  (user(gender male)(state WA)(purpose P)(stay-place airbnb)(month-of-travel 2) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."  
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-MAR-MALE-P-AIRBNB
  (user(gender male)(state WA)(purpose P)(stay-place airbnb)(month-of-travel 3) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-APR-MALE-P-AIRBNB
  (user(gender male)(state WA)(purpose P)(stay-place airbnb)(month-of-travel 4) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket " 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-MAY-MALE-P-AIRBNB
  (user(gender male)(state WA)(purpose P)(stay-place airbnb)(month-of-travel 5) )  
  =>       
    (printout t crlf "Its going to be a pleasant trip!! You'll enjoy the weather there!"
        crlf "->Carry a pair of shoes,(not bad if you can carry sandals if you prefer),"
        crlf "->sweatshirt or pullover, a simple jacket,jeans,shirts,tee-shirts " 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-JUN-MALE-P-AIRBNB
  (user(gender male)(state WA)(purpose P)(stay-place airbnb)(month-of-travel 6) )  
  =>       
    (printout t crlf "It will be bit Pleasant over there! But it might get cold."
        crlf "->Pack pull overs or sweatshirt just be on safer side."
        crlf "->Carry moisturiser, cold cream, jeans, shoes or sandals, a pair of shorts."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-JULY-MALE-P-AIRBNB
  (user(gender male)(state WA)(purpose P)(stay-place airbnb)(month-of-travel 7) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."  
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-AUG-MALE-P-AIRBNB
  (user(gender male)(state WA)(purpose P)(stay-place airbnb)(month-of-travel 8) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."  
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-SEPT-MALE-P-AIRBNB
  (user(gender male)(state WA)(purpose P)(stay-place airbnb)(month-of-travel 9) )  
  =>       
    (printout t crlf "It will be bit Pleasant over there! But it might get cold."
        crlf "->Pack pull overs or sweatshirt just be on safer side."
        crlf "->Carry moisturiser, cold cream, jeans, shoes or sandals, a pair of shorts."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-OCT-MALE-P-AIRBNB
  (user(gender male)(state WA)(purpose P)(stay-place airbnb)(month-of-travel 10) )  
  =>       
    (printout t crlf "Its going to be a pleasant trip!! You'll enjoy the weather there!"
        crlf "->Carry a pair of shoes,(not bad if you can carry sandals if you prefer),"
        crlf "->sweatshirt or pullover, a simple jacket,jeans,shirts,tee-shirts "
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."  
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-NOV-MALE-P-AIRBNB
  (user(gender male)(state WA)(purpose P)(stay-place airbnb)(month-of-travel 11) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."  
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-DEC-MALE-P-AIRBNB
  (user(gender male)(state WA)(purpose P)(stay-place airbnb)(month-of-travel 12) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   )



;Washington, female , business, airbnb


(defrule WA-JAN-FEMALE-B-AIRBNB
  (user(gender female)(state WA)(purpose B)(stay-place airbnb)(month-of-travel 1) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule WA-FEB-FEMALE-B-AIRBNB
  (user(gender female)(state WA)(purpose B)(stay-place airbnb)(month-of-travel 2) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule WA-MAR-FEMALE-B-AIRBNB
  (user(gender female)(state WA)(purpose B)(stay-place airbnb)(month-of-travel 3) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule WA-APR-FEMALE-B-AIRBNB
  (user(gender female)(state WA)(purpose B)(stay-place airbnb)(month-of-travel 4) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket " 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule WA-MAY-FEMALE-B-AIRBNB
  (user(gender female)(state WA)(purpose B)(stay-place airbnb)(month-of-travel 5) )  
  =>       
    (printout t crlf "Its going to be a pleasant trip!! You'll enjoy the weather there!"
        crlf "->Carry a pair of shoes,(not bad if you can carry sandals if you prefer),"
        crlf "->sweatshirt or pullover, a simple jacket,jeans,shirts,tee-shirts "
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule WA-JUN-FEMALE-B-AIRBNB
  (user(gender female)(state WA)(purpose B)(stay-place airbnb)(month-of-travel 6) )  
  =>       
    (printout t crlf "It will be bit Pleasant over there! But it might get cold."
        crlf "->Pack pull overs or sweatshirt just be on safer side."
        crlf "->Carry moisturiser, cold cream, jeans, shoes or sandals, a pair of shorts."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule WA-JULY-FEMALE-B-AIRBNB
  (user(gender female)(state WA)(purpose B)(stay-place airbnb)(month-of-travel 7) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule WA-AUG-FEMALE-B-AIRBNB
  (user(gender female)(state WA)(purpose B)(stay-place airbnb)(month-of-travel 8) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts" 
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule WA-SEPT-FEMALE-B-AIRBNB
  (user(gender female)(state WA)(purpose B)(stay-place airbnb)(month-of-travel 9) )  
  =>       
    (printout t crlf "It will be bit Pleasant over there! But it might get cold."
        crlf "->Pack pull overs or sweatshirt just be on safer side."
        crlf "->Carry moisturiser, cold cream, jeans, shoes or sandals, a pair of shorts."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule WA-OCT-FEMALE-B-AIRBNB
  (user(gender female)(state WA)(purpose B)(stay-place airbnb)(month-of-travel 10) )  
  =>       
    (printout t crlf "Its going to be a pleasant trip!! You'll enjoy the weather there!"
        crlf "->Carry a pair of shoes,(not bad if you can carry sandals if you prefer),"
        crlf "->sweatshirt or pullover, a simple jacket,jeans,shirts,tee-shirts "
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   ) 
(defrule WA-NOV-FEMALE-B-AIRBNB
  (user(gender female)(state WA)(purpose B)(stay-place airbnb)(month-of-travel 11) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf  )
   ) 
(defrule WA-DEC-FEMALE-B-AIRBNB
  (user(gender female)(state WA)(purpose B)(stay-place airbnb)(month-of-travel 12) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Office documents(if needed!)."
        crlf "->Laptop, charger, earphone, power bank."
        crlf "->Formal shirts, trousers, tie, formal shoe, belt, blazer.(Company ID if neccessary!)." crlf )
   )


;Washington, female , personal, airbnb



(defrule WA-JAN-FEMALE-P-AIRBNB
  (user(gender female)(state WA)(purpose P)(stay-place airbnb)(month-of-travel 1) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-FEB-FEMALE-P-AIRBNB
  (user(gender female)(state WA)(purpose P)(stay-place airbnb)(month-of-travel 2) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-MAR-FEMALE-P-AIRBNB
  (user(gender female)(state WA)(purpose P)(stay-place airbnb)(month-of-travel 3) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-APR-FEMALE-P-AIRBNB
  (user(gender female)(state WA)(purpose P)(stay-place airbnb)(month-of-travel 4) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."  
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-MAY-FEMALE-P-AIRBNB
  (user(gender female)(state WA)(purpose P)(stay-place airbnb)(month-of-travel 5) )  
  =>       
    (printout t crlf "Its going to be a pleasant trip!! You'll enjoy the weather there!"
        crlf "->Carry a pair of shoes,(not bad if you can carry sandals if you prefer),"
        crlf "->sweatshirt or pullover, a simple jacket,jeans,shirts,tee-shirts "
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-JUN-FEMALE-P-AIRBNB
  (user(gender female)(state WA)(purpose P)(stay-place airbnb)(month-of-travel 6) )  
  =>       
    (printout t crlf "It will be bit Pleasant over there! But it might get cold."
        crlf "->Pack pull overs or sweatshirt just be on safer side."
        crlf "->Carry moisturiser, cold cream, jeans, shoes or sandals, a pair of shorts."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-JULY-FEMALE-P-AIRBNB
  (user(gender female)(state WA)(purpose P)(stay-place airbnb)(month-of-travel 7) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-AUG-FEMALE-P-AIRBNB
  (user(gender female)(state WA)(purpose P)(stay-place airbnb)(month-of-travel 8) )  
  =>       
    (printout t crlf "It’s a good time for this trip! It’s a pleasant weather for you out there!" 
        crlf "->Pack your sunscreen, a simple jacket, a pair of sandals, shorts, jeans, few tee-shirts"
        crlf "->tank tops, vneck tops, jorts, maxidresses."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf )
   ) 
(defrule WA-SEPT-FEMALE-P-AIRBNB
  (user(gender female)(state WA)(purpose P)(stay-place airbnb)(month-of-travel 9) )  
  =>       
    (printout t crlf "It will be bit Pleasant over there! But it might get cold."
        crlf "->Pack pull overs or sweatshirt just be on safer side."
        crlf "->Carry moisturiser, cold cream, jeans, shoes or sandals, a pair of shorts."
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   ) 
(defrule WA-OCT-FEMALE-P-AIRBNB
  (user(gender female)(state WA)(purpose P)(stay-place airbnb)(month-of-travel 10) )  
  =>       
    (printout t crlf "Its going to be a pleasant trip!! You'll enjoy the weather there!"
        crlf "->Carry a pair of shoes,(not bad if you can carry sandals if you prefer),"
        crlf "->sweatshirt or pullover, a simple jacket,jeans,shirts,tee-shirts "
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf )
   ) 
(defrule WA-NOV-FEMALE-P-AIRBNB
  (user(gender female)(state WA)(purpose P)(stay-place airbnb)(month-of-travel 11) )  
  =>       
    (printout t crlf "It's gonna be bit cold!"
        crlf "->Carry your cold creams to be on safer side, a pair of shoes,"
        crlf "->sweat shirts, a pair of shoes, shades, beanie, a jacket "
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)." 
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf )
   )
(defrule WA-DEC-FEMALE-P-AIRBNB
  (user(gender female)(state WA)(purpose P)(stay-place airbnb)(month-of-travel 12) )  
  =>       
    (printout t crlf "It's goin to be cold! But, I'll help you to manage the cold!"
        crlf "->Pack your cold cream, a pair of shoes, beanie, sweat shirt or a pull over, jacket, gloves" crlf 
        crlf "->It's better to carry some basic toiletries(Every Airbnb doesn't provide the toiletries)."
        crlf "->If you prefer to cook, you can carry some special ingredients you need!(Masala's or your special ingredient! :)" 
        crlf "->Power bank, charger, earphone or headphones, camera.(Laptop, if needed!)"
        crlf "->Shirts, tee-shirts, jeans, shoes.(State ID if needed!)" crlf)
   )




(defrule assert-user-fact
  (answer (ident gender)(answer ?gen))
  (answer (ident purpose)(answer ?pur))
  (answer (ident stay-place)(answer ?sp))
  (answer (ident month-of-travel)(answer ?mot))
  (answer (ident state)(answer ?st))        
    =>   
   (assert (user (gender ?gen)(purpose ?pur)(stay-place ?sp)(month-of-travel ?mot)(state ?st)))
 )



(deffacts question-data
"The questions the system can ask."
(question (ident gender) (type male-female)
(text " What is you gender?" ))
(question (ident purpose) (type B-P)
(text " what is your purpose? "))  
(question (ident stay-place) (type hotel-airbnb)
(text " where are you are planning to stay? "))
(question (ident state) (type five-states)
(text " which state are you planning to visit? "))  
(question (ident month-of-travel) (type number2)
(text " which month are you planning to travel? ")))

    
(reset)
(run)