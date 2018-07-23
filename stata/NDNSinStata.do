// Program of fitting level 1 LCA model in stata 
// date created: 20th July 2018


import delimited /home/wangcc-me/Documents/LSHTMproject/stata/ndnsNNN.dat, clear 


 gsem (h12 h18 h21 <-, logit), lclass(C 2)
 
 h7 h8 h9 h10 h11 h12 h13 h14 h15 h16 h17 h18 h19 h20 h21 h22 h23 
