options nodate nocenter linesize=78 pagesize=500;
data all;
input rep trt $ mgt $ stand;
cards;
1	C2	PRE	32409
1	C2	POST	31102
2	C2	PRE	32409
2	C2	POST	31015
3	C2	PRE	32409
3	C2	POST	30231
4	C2	PRE	31276
4	C2	POST	31929
1	C3	PRE	32409
1	C3	POST	29098
2	C3	PRE	33018
2	C3	POST	30405
3	C3	PRE	31189
3	C3	POST	30710
4	C3	PRE	31537
4	C3	POST	31799
1	C4	PRE	32409
1	C4	POST	28924
2	C4	PRE	33106
2	C4	POST	32234
3	C4	PRE	31886
3	C4	POST	31276
4	C4	PRE	32844
4	C4	POST	30383

run;
 proc sort data=all; by mgt;
proc glm data=all;
  class rep mgt;
  model stand = rep mgt;
    means trt / LSD; title '2011 Marsden Corn Stands';  run;
