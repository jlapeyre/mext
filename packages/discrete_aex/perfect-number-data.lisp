;;;  Copyright (C) (2012,2013) John Lapeyre. Licensed under GPL, v3 or greater. See the file
;;;  `LICENSE' in this directory.

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
