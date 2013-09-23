;; This is two files cat'ed together:
;; perfect-number-data.lisp
;; mersenne-data.lisp

(defvar *mersenne-exponents* '(
 2
 3
 5
 7
 13
 17
 19
 31
 61
 89
 107
 127
 521
 607
 1279
 2203
 2281
 3217
 4253
 4423
 9689
 9941
 11213
 19937
 21701
 23209
 44497
 86243
 110503
 132049
 216091
 756839
 859433
 1257787
 1398269
 2976221
 3021377
 6972593
 13466917
 20996011
 24036583
 25964951
 30402457
 32582657
 37156667
 42643801
 43112609
 57885161))

(defvar *perfect-numbers-num-digits* '(
 1
 2
 3
 4
 8
 10
 12
 19
 37
 54
 65
 77
 314
 366
 770
 1327
 1373
 1937
 2561
 2663
 5834
 5985
 6751
 12003
 13066
 13973
 26790
 51924
 66530
 79502
 130100
 455663
 517430
 757263
 841842
 1791864
 1819050
 4197919
 8107892
 12640858
 14471465
 15632458
 18304103
 19616714
 22370543
 25674127
 25956377
 34850340))

(defvar *mersenne-numbers-year* '(
  nil 
 nil 
 nil 
 nil 
 1456 
 1588 
 1588 
 1772 
 1883 
 1911 
 1914 
 1876 
 1952 
 1952 
 1952 
 1952 
 1952 
 1957 
 1961 
 1961 
 1963 
 1963 
 1963 
 1971 
 1978 
 1979 
 1979 
 1982 
 1988 
 1983 
 1985 
 1992 
 1994 
 1996 
 1996 
 1997 
 1998 
 1999 
 2001 
 2003 
 2004 
 2005 
 2005 
 2006 
 2008 
 2009 
 2008 
 2013 
))

(defvar *mersenne-numbers-discoverer* '(
  "Known to the Greeks"
 "Known to the Greeks"
 "Known to the Greeks"
 "Known to the Greeks"
 "First seen in a medieval manuscript, Codex Lat. Monac."
 "Cataldi"
 "Cataldi"
 "Euler"
 "Pervushin"
 "Powers"
 "Powers"
 "Lucas"
 "Robinson"
 "Robinson"
 "Robinson"
 "Robinson"
 "Robinson"
 "Riesel"
 "Hurwitz"
 "Hurwitz"
 "Gillies"
 "Gillies"
 "Gillies"
 "Tuckerman"
 "Noll and Nickel"
 "Noll"
 "Nelson and Slowinski"
 "Slowinski"
 "Colquitt and Welsh"
 "Slowinski"
 "Slowinski"
 "Slowinski and Gage"
 "Slowinski and Gage"
 "Slowinski and Gage"
 "Armengaud, Woltman, et al."
 "Spence, Woltman, et al."
 "Clarkson, Woltman, Kurowski, et al."
 "Hajratwala, Woltman, Kurowski, et al."
 "Cameron, Woltman, Kurowski, et al."
 "Shafer, Woltman, Kurowski, et al."
 "Findley, Woltman, Kurowski, et al."
 "Nowak, Woltman, Kurowski, et al."
 "Cooper, Boone, Woltman, Kurowski, et al."
 "Cooper, Boone, Woltman, Kurowski, et al."
 "Elvenich, Woltman, Kurowski, et al."
 "Strindmo, Woltman, Kurowski, et al."
 "Smith, Woltman, Kurowski, et al."
 "Cooper, Woltman, Kurowski, et al."
))

(defvar *mersenne-prime-num-digits* '(
 1
 1
 2
 3
 4
 6
 6
 10
 19
 27
 33
 39
 157
 183
 386
 664
 687
 969
 1281
 1332
 2917
 2993
 3376
 6002
 6533
 6987
 13395
 25962
 33265
 39751
 65050
 227832
 258716
 378632
 420921
 895932
 909526
 2098960
 4053946
 6320430
 7235733
 7816230
 9152052
 9808358
 11185272
 12837064
 12978189
 17425170))

(defvar *mersenne-numbers-full-date* '(
  "c. 430 BC" 
 "c. 430 BC" 
 "c. 300 BC" 
 "c. 300 BC" 
 "1456" 
 "1588" 
 "1588" 
 "1772" 
 "1883 November" 
 "1911 June" 
 "1914 June 1" 
 "1876 January 10" 
 "1952 January 30" 
 "1952 January 30" 
 "1952 June 25" 
 "1952 October 7" 
 "1952 October 9" 
 "1957 September 8" 
 "1961 November 3" 
 "1961 November 3" 
 "1963 May 11" 
 "1963 May 16" 
 "1963 June 2" 
 "1971 March 4" 
 "1978 October 30" 
 "1979 February 9" 
 "1979 April 8" 
 "1982 September 25" 
 "1988 January 29" 
 "1983 September 19" 
 "1985 September 1" 
 "1992 February 17" 
 "1994 January 4" 
 "1996 September 3" 
 "1996 November 13" 
 "1997 August 24" 
 "1998 January 27" 
 "1999 June 1" 
 "2001 November 14" 
 "2003 November 17" 
 "2004 May 15" 
 "2005 February 18" 
 "2005 December 15" 
 "2006 September 4" 
 "2008 September 6" 
 "2009 April 12" 
 "2008 August 23" 
 "2013 January 25" 
))

(defvar *mersenne-numbers-full-discoverer* '(
  "Ancient Greek mathematicians" 
 "Ancient Greek mathematicians" 
 "Ancient Greek mathematicians" 
 "Ancient Greek mathematicians" 
 "Anonymous" 
 "Pietro Cataldi" 
 "Pietro Cataldi" 
 "Leonhard Euler" 
 "I. M. Pervushin" 
 "R. E. Powers" 
 "R. E. Powers" 
 "Édouard Lucas" 
 "Raphael M. Robinson" 
 "Raphael M. Robinson" 
 "Raphael M. Robinson" 
 "Raphael M. Robinson" 
 "Raphael M. Robinson" 
 "Hans Riesel" 
 "Alexander Hurwitz" 
 "Alexander Hurwitz" 
 "Donald B. Gillies" 
 "Donald B. Gillies" 
 "Donald B. Gillies" 
 "Bryant Tuckerman" 
 "Landon Curt Noll and Laura Nickel" 
 "Landon Curt Noll" 
 "Harry Lewis Nelson and David Slowinski" 
 "David Slowinski" 
 "Walter Colquitt and Luke Welsh" 
 "David Slowinski" 
 "David Slowinski" 
 "David Slowinski and Paul Gage" 
 "David Slowinski and Paul Gage" 
 "David Slowinski and Paul Gage" 
 "GIMPS / Joel Armengaud" 
 "GIMPS / Gordon Spence" 
 "GIMPS / Roland Clarkson" 
 "GIMPS / Nayan Hajratwala" 
 "GIMPS / Michael Cameron" 
 "GIMPS / Michael Shafer" 
 "GIMPS / Josh Findley" 
 "GIMPS / Martin Nowak" 
 "GIMPS / Curtis Cooper and Steven Boone" 
 "GIMPS / Curtis Cooper and Steven Boone" 
 "GIMPS / Hans-Michael Elvenich" 
 "GIMPS / Odd M. Strindmo" 
 "GIMPS / Edson Smith" 
 "GIMPS / Curtis Cooper" 
))

(defvar *mersenne-numbers-method* '(
  "" 
 "" 
 "" 
 "" 
 "Trial division" 
 "Trial division" 
 "Trial division" 
 "Enhanced trial division" 
 "Lucas sequences" 
 "Lucas sequences" 
 "Lucas sequences" 
 "Lucas sequences" 
 "LLT / SWAC" 
 "LLT / SWAC" 
 "LLT / SWAC" 
 "LLT / SWAC" 
 "LLT / SWAC" 
 "LLT / BESK" 
 "LLT / IBM 7090" 
 "LLT / IBM 7090" 
 "LLT / ILLIAC II" 
 "LLT / ILLIAC II" 
 "LLT / ILLIAC II" 
 "LLT / IBM 360/91" 
 "LLT / CDC Cyber 174" 
 "LLT / CDC Cyber 174" 
 "LLT / Cray 1" 
 "LLT / Cray 1" 
 "LLT / NEC SX-2" 
 "LLT / Cray X-MP" 
 "LLT / Cray X-MP/24" 
 "LLT / Maple on Harwell Lab Cray-2" 
 "LLT / Cray C90" 
 "LLT / Cray T94" 
 "LLT / Prime95 on 90 MHz Pentium PC" 
 "LLT / Prime95 on 100 MHz Pentium PC" 
 "LLT / Prime95 on 200 MHz Pentium PC" 
 "LLT / Prime95 on 350 MHz Pentium II IBM Aptiva" 
 "LLT / Prime95 on 800 MHz Athlon T-Bird" 
 "LLT / Prime95 on 2 GHz Dell Dimension" 
 "LLT / Prime95 on 2.4 GHz Pentium 4 PC" 
 "LLT / Prime95 on 2.4 GHz Pentium 4 PC" 
 "LLT / Prime95 on 2 GHz Pentium 4 PC" 
 "LLT / Prime95 on 3 GHz Pentium 4 PC" 
 "LLT / Prime95 on 2.83 GHz Core 2 Duo PC" 
 "LLT / Prime95 on 3 GHz Core 2 PC" 
 "LLT / Prime95 on Dell Optiplex 745" 
 "LLT / Prime95 on 3 GHz Core 2 Duo PC" 
))
