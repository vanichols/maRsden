options nodate nocenter linesize=78 pagesize=500;
data all;
input rep trt $ stand;
cards;
1	S2	145490
2	S2	148540
3	S2	147668
4	S2	149846
1	S3	139828
2	S3	115870
3	S3	133729
4	S3	133729
1	S4	134165
2	S4	125453
3	S4	128938
4	S4	130244
run;
 proc sort data=all; by trt;
proc glm data=all;
  class rep trt;
  model stand = rep trt;
  contrast 'S3/S4 vs. S2' trt 2 -1 -1;
  means trt / LSD; title '2007 Marsden Soybean stands'; run;
