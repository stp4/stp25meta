require(stp25tools)
require(stp25meta)
require(meta)
dat_BMI <- stp25tools::get_data("
id author            year	tm1	tsd1	tn	 tm2	tsd2	cm1	csd1	cn	cm2	csd2	 tm	tsd	cm	csd	md	se
1 'Benson'           2008	24	5.4	31	24	5.5	22.1	3.6	36	22.5	3.5	-0.01	0.8	0.4	0.7 NA NA
2 'Costigan (AEP)'   2015	21.72	4.75	21	21.8	4.67	22.29	4.42	22	22.63	4.34	NA	NA	NA	NA	-0.27	  2.39
2 'Costigan (RAP)'   2015	22	  4.42	22	22.07	4.33	22.29	4.42	22	22.63	4.34	NA	NA	NA	NA	  -0.28	  2.312
2 'Costigan'        2015	21.9  4.54    43  21.9  4.45  22.3  4.37    44  22.6  4.29	NA	NA	NA	NA	-0.275	2.351
3 'Dorgo (MRT)'     2009	24.4	6.35	63	24.5	6.35	24.9	4.96	129	24.6	4.96	NA	NA	NA	NA	NA	NA
3 'Dorgo (MRT + E)' 2009	24.8	6.57	30	24.7	6.57	24.9	4.96	129	24.6	4.96	NA	NA	NA	NA	NA	NA
3 'Dorgo'           2009	24.6  6.45    93  24.6  6.39  24.9  4.95   258  24.6  4.93	NA	NA	NA	NA	NA	NA
4 'Eather'   2016	21.6	3	49	21.5	3	20.6	3.7	34	21.7	1.3	NA	NA	NA	NA	-1.3	1.28
6 'Kennedy'   2018	21.92	2.14	294	22.01	2.14	22.57	2.34	211	22.55	2.34	NA	NA	NA	NA	0.11	0.168
7 'McMurray'   2002	22.5	5.358	319	NA	NA	21.4	4.714	247	NA	NA	0.21	0.893	0.23	0.942	NA	NA
8 'Muros'   2015	19.7	3.4	28	19.7	3.3	20.9	4.1	41	21.1	4	NA	NA	NA	NA	NA	NA
9 'Sadowsky'   1999	20.5	3.3	39	20.3	3.3	20.3	2.2	22	20.35	2.2	-0.197	0.472	0.0409	0.4027	NA	NA
10 'Torbeyns'   2017	19.7	3.5	21	19.9	3.2	20.1	3.7	23	20.5	3.5	NA	NA	NA	NA	NA	NA
11 'Velez'   2010	24.6	4.7	13	24.9	4.9	22.1	2.8	15	22	2.4	NA	NA	NA	NA	NA	NA
13 'Weeks'   2012	20	3.5	47	20.5	3.3	20	3.5	52	20.4	3.3	NA	NA	NA	NA	NA	NA
",  na.strings="NA")


combining( dat_BMI[c(2,3),] )
combining( dat_BMI[c(5,6),] )


dat_BMI<- dat_BMI[-c(2,3,5,6),]
dat_BMI

BMI <- convert(dat_BMI)


m_bmi <- meta::metacont(
  tn,     tm,   tsd,
  cn,     cm,   csd,
  sm = "MD",
  studlab = paste(author, year),
  data = BMI
)

rslt <- as.data.frame(m_bmi)

#x<- stp25stat2::render_f(rslt[ c( "studlab",    "TE" ,"seTE" )],2)
rslt$TE <- ifelse(is.na(BMI$md),  rslt$TE, BMI$md)
rslt$seTE <- ifelse(is.na(BMI$se), rslt$seTE, BMI$se)


m.gen.bmi <- metagen(
  TE = TE,
  seTE = seTE,
  studlab = studlab,
  data = rslt,
  sm = "MD",
  fixed = FALSE,
  random = TRUE,
  method.tau = "REML",
  hakn = TRUE,
  title = "BMI"
)


x2<-stp25stat2::render_f(as.data.frame(m.gen.bmi), 2)

#summary(m.gen.bmi)

#meta_text(m.gen.bmi) |> Text()
#windows(7,4)
oldset <- settings.meta(smbin = "RD", smprop = "PLN")



# Forest plot using JAMA style
#
# settings.meta("JAMA")
#
# settings.meta("RevMan5")
settings.meta(reset =TRUE)
forest(m.gen.bmi,  leftcols = c('studlab'),
       digits.tau2 = 2,
       digits.tau=2,
       xlim=c(-2.5, 2.5),
       title = "Body weight")

