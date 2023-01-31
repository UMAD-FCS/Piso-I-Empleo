cd "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos"
global bases "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO I_MICRODATOS\ECH\Microdatos\Compatibilizadas Iecon\En uso"


*2006
use "$bases\Fusionada_personasyhogares_6.dta", clear

g ipc_ene19=.
replace ipc_ene19 =36.7461609947569 if bc_mes==1
replace ipc_ene19 =37.248034365621 if bc_mes==2
replace ipc_ene19 =37.4963661719656 if bc_mes==3
replace ipc_ene19 =37.616190609992 if bc_mes==4
replace ipc_ene19 =37.8124248345859 if bc_mes==5
replace ipc_ene19 =38.0503371245803 if bc_mes==6
replace ipc_ene19 =38.171898148665 if bc_mes==7
replace ipc_ene19 =38.4966397415771 if bc_mes==8
replace ipc_ene19 =38.8005423017889 if bc_mes==9
replace ipc_ene19 =39.0089326287913 if bc_mes==10
replace ipc_ene19 =38.9307862561654 if bc_mes==11
replace ipc_ene19 =38.9446789446323 if bc_mes==12

save "$bases\Fusionada_personasyhogares_6.dta", replace

*2007
use "$bases\Fusionada_personasyhogares_7.dta", clear

g ipc_ene19=.
replace ipc_ene19 =39.090552173534 if bc_mes==1
replace ipc_ene19 =39.7834500108169 if bc_mes==2
replace ipc_ene19 =40.0265720589864 if bc_mes==3
replace ipc_ene19 =40.3860453730655 if bc_mes==4
replace ipc_ene19 =40.8792358136379 if bc_mes==5
replace ipc_ene19 =41.1900847180831 if bc_mes==6
replace ipc_ene19 =41.2456554719505 if bc_mes==7
replace ipc_ene19 =41.5860263393877 if bc_mes==8
replace ipc_ene19 =42.304972967546 if bc_mes==9
replace ipc_ene19 =42.4803681594397 if bc_mes==10
replace ipc_ene19 =42.3831193401719 if bc_mes==11
replace ipc_ene19 =42.2858705209041 if bc_mes==12

save "$bases\Fusionada_personasyhogares_7.dta", replace

*2008
use "$bases\Fusionada_personasyhogares_8.dta", clear

g ipc_ene19=.
replace ipc_ene19 =42.4143778892223 if bc_mes==1
replace ipc_ene19 =42.7443292403094 if bc_mes==2
replace ipc_ene19 =43.1350611034389 if bc_mes==3
replace ipc_ene19 =43.6282515440113 if bc_mes==4
replace ipc_ene19 =43.7723881868546 if bc_mes==5
replace ipc_ene19 =44.1544371196923 if bc_mes==6
replace ipc_ene19 =44.7205641747155 if bc_mes==7
replace ipc_ene19 =44.9202715714262 if bc_mes==8
replace ipc_ene19 =45.3769937047731 if bc_mes==9
replace ipc_ene19 =45.6496377159346 if bc_mes==10
replace ipc_ene19 =45.8007207030113 if bc_mes==11
replace ipc_ene19 =45.8861086395005 if bc_mes==12

save "$bases\Fusionada_personasyhogares_8.dta", replace

*2009
use "$bases\Fusionada_personasyhogares_9.dta", clear

g ipc_ene19=.
replace ipc_ene19 =46.3130135902255 if bc_mes==1
replace ipc_ene19 =46.6794332485381 if bc_mes==2
replace ipc_ene19 =46.5543990523367 if bc_mes==3
replace ipc_ene19 =46.9121357803575 if bc_mes==4
replace ipc_ene19 =46.8930333337156 if bc_mes==5
replace ipc_ene19 =47.0840578001344 if bc_mes==6
replace ipc_ene19 =47.6189263061073 if bc_mes==7
replace ipc_ene19 =48.089541127921 if bc_mes==8
replace ipc_ene19 =48.6817169738195 if bc_mes==9
replace ipc_ene19 =48.7945950676125 if bc_mes==10
replace ipc_ene19 =48.7893853094374 if bc_mes==11
replace ipc_ene19 =48.8171706863711 if bc_mes==12

save "$bases\Fusionada_personasyhogares_9.dta", replace

*2010
use "$bases\Fusionada_personasyhogares_10.dta", clear

g ipc_ene19=.
replace ipc_ene19 =49.0464000460737 if bc_mes==1
replace ipc_ene19 =49.504858765479 if bc_mes==2
replace ipc_ene19 =49.7809759487572 if bc_mes==3
replace ipc_ene19 =50.2533273566293 if bc_mes==4
replace ipc_ene19 =50.3418932456053 if bc_mes==5
replace ipc_ene19 =50.4217762042896 if bc_mes==6
replace ipc_ene19 =50.5641762610746 if bc_mes==7
replace ipc_ene19 =51.1129374555142 if bc_mes==8
replace ipc_ene19 =51.7242157480546 if bc_mes==9
replace ipc_ene19 =51.878771907248 if bc_mes==10
replace ipc_ene19 =52.2121964304519 if bc_mes==11
replace ipc_ene19 =52.1722549511098 if bc_mes==12

save "$bases\Fusionada_personasyhogares_10.dta", replace

*2011
use "$bases\Fusionada_personasyhogares_11.dta", clear

g ipc_ene19=.
replace ipc_ene19 =52.4466355483296 if bc_mes==1
replace ipc_ene19 =53.1022184926837 if bc_mes==2
replace ipc_ene19 =53.6004615303928 if bc_mes==3
replace ipc_ene19 =54.3609377458436 if bc_mes==4
replace ipc_ene19 =54.5445009702628 if bc_mes==5
replace ipc_ene19 =54.7228195311271 if bc_mes==6
replace ipc_ene19 =54.9168720826559 if bc_mes==7
replace ipc_ene19 =55.3312005034877 if bc_mes==8
replace ipc_ene19 =55.6406356532229 if bc_mes==9
replace ipc_ene19 =55.9238474851838 if bc_mes==10
replace ipc_ene19 =56.3224419153511 if bc_mes==11
replace ipc_ene19 =56.5584517753186 if bc_mes==12

save "$bases\Fusionada_personasyhogares_11.dta", replace

*2012
use "$bases\Fusionada_personasyhogares_12.dta", clear

g ipc_ene19=.
replace ipc_ene19 =56.9570462054859 if bc_mes==1
replace ipc_ene19 =57.3766192898726 if bc_mes==2
replace ipc_ene19 =57.8538836733624 if bc_mes==3
replace ipc_ene19 =58.4255520008391 if bc_mes==4
replace ipc_ene19 =58.902816384329 if bc_mes==5
replace ipc_ene19 =59.1335815807416 if bc_mes==6
replace ipc_ene19 =59.3119001416059 if bc_mes==7
replace ipc_ene19 =59.4692400482509 if bc_mes==8
replace ipc_ene19 =60.0251743850632 if bc_mes==9
replace ipc_ene19 =60.754182619185 if bc_mes==10
replace ipc_ene19 =61.4517228719778 if bc_mes==11
replace ipc_ene19 =61.6667540777259 if bc_mes==12

save "$bases\Fusionada_personasyhogares_12.dta", replace

*2013
use "$bases\Fusionada_personasyhogares_13.dta", clear

g ipc_ene19=.
replace ipc_ene19 =61.2157130120103 if bc_mes==1
replace ipc_ene19 =62.3800283211832 if bc_mes==2
replace ipc_ene19 =62.9988986206535 if bc_mes==3
replace ipc_ene19 =63.4132270414853 if bc_mes==4
replace ipc_ene19 =63.6964388734463 if bc_mes==5
replace ipc_ene19 =63.9009807520848 if bc_mes==6
replace ipc_ene19 =64.1789479204909 if bc_mes==7
replace ipc_ene19 =64.6719462946452 if bc_mes==8
replace ipc_ene19 =65.3432632296638 if bc_mes==9
replace ipc_ene19 =66.2348560339854 if bc_mes==10
replace ipc_ene19 =66.7803010436881 if bc_mes==11
replace ipc_ene19 =66.9166622961137 if bc_mes==12


save "$bases\Fusionada_personasyhogares_13.dta", replace

*2014
use "$bases\Fusionada_personasyhogares_14.dta", clear

g ipc_ene19=.
replace ipc_ene19 =66.4341532490691 if bc_mes==1
replace ipc_ene19 =68.0547542875125 if bc_mes==2
replace ipc_ene19 =69.1823569518016 if bc_mes==3
replace ipc_ene19 =69.5861960455237 if bc_mes==4
replace ipc_ene19 =69.544238737085 if bc_mes==5
replace ipc_ene19 =69.764514606388 if bc_mes==6
replace ipc_ene19 =70.0057691299103 if bc_mes==7
replace ipc_ene19 =70.5302354853936 if bc_mes==8
replace ipc_ene19 =71.0599465044317 if bc_mes==9
replace ipc_ene19 =71.773220747889 if bc_mes==10
replace ipc_ene19 =72.1980384958305 if bc_mes==11
replace ipc_ene19 =72.3029317669272 if bc_mes==12

save "$bases\Fusionada_personasyhogares_14.dta", replace

*2015
use "$bases\Fusionada_personasyhogares_15.dta", clear

g ipc_ene19=.
replace ipc_ene19 =71.9200713274244 if bc_mes==1
replace ipc_ene19 =73.5144490480936 if bc_mes==2
replace ipc_ene19 =74.3221272355378 if bc_mes==3
replace ipc_ene19 =74.8413489274663 if bc_mes==4
replace ipc_ene19 =75.2661666754078 if bc_mes==5
replace ipc_ene19 =75.6332931242461 if bc_mes==6
replace ipc_ene19 =75.9741962553102 if bc_mes==7
replace ipc_ene19 =76.892012377406 if bc_mes==8
replace ipc_ene19 =77.7993391723921 if bc_mes==9
replace ipc_ene19 =78.3342948549851 if bc_mes==10
replace ipc_ene19 =78.80631457492 if bc_mes==11
replace ipc_ene19 =79.1419730424293 if bc_mes==12

save "$bases\Fusionada_personasyhogares_15.dta", replace

*2016
use "$bases\Fusionada_personasyhogares_16.dta", clear

g ipc_ene19=.
replace ipc_ene19 =78.7066659673782 if bc_mes==1
replace ipc_ene19 =80.6314574920019 if bc_mes==2
replace ipc_ene19 =81.9216447264908 if bc_mes==3
replace ipc_ene19 =82.7712802223737 if bc_mes==4
replace ipc_ene19 =83.1488959983217 if bc_mes==5
replace ipc_ene19 =83.9513295222112 if bc_mes==6
replace ipc_ene19 =84.2869879897205 if bc_mes==7
replace ipc_ene19 =84.6174017936749 if bc_mes==8
replace ipc_ene19 =85.0999108407196 if bc_mes==9
replace ipc_ene19 =85.3096973829129 if bc_mes==10
replace ipc_ene19 =85.4670372895579 if bc_mes==11
replace ipc_ene19 =85.5509519064352 if bc_mes==12

save "$bases\Fusionada_personasyhogares_16.dta", replace

*2017
use "$bases\Fusionada_personasyhogares_17.dta", clear

g ipc_ene19=.
replace ipc_ene19 =85.0841768500551 if bc_mes==1
replace ipc_ene19 =87.2974248701946 if bc_mes==2
replace ipc_ene19 =87.7327319452457 if bc_mes==3
replace ipc_ene19 =88.3253789269418 if bc_mes==4
replace ipc_ene19 =88.5194314784707 if bc_mes==5
replace ipc_ene19 =88.634814076677 if bc_mes==6
replace ipc_ene19 =88.7659306655478 if bc_mes==7
replace ipc_ene19 =89.0491424975088 if bc_mes==8
replace ipc_ene19 =89.7361934231919 if bc_mes==9
replace ipc_ene19 =90.2187024702365 if bc_mes==10
replace ipc_ene19 =90.6330308910683 if bc_mes==11
replace ipc_ene19 =90.9372213772487 if bc_mes==12

save "$bases\Fusionada_personasyhogares_17.dta", replace

*2018
use "$bases\Fusionada_personasyhogares_18.dta", clear

g ipc_ene19=.
replace ipc_ene19 =90.6592542088425 if bc_mes==1
replace ipc_ene19 =93.1190014160592 if bc_mes==2
replace ipc_ene19 =93.9371689306131 if bc_mes==3
replace ipc_ene19 =94.1994021083548 if bc_mes==4
replace ipc_ene19 =94.2623380710127 if bc_mes==5
replace ipc_ene19 =95.0280589500184 if bc_mes==6
replace ipc_ene19 =95.9668537263335 if bc_mes==7
replace ipc_ene19 =96.5385220538103 if bc_mes==8
replace ipc_ene19 =97.1888603346095 if bc_mes==9
replace ipc_ene19 =97.6713693816542 if bc_mes==10
replace ipc_ene19 =97.896889914512 if bc_mes==11
replace ipc_ene19 =98.2535270362406 if bc_mes==12

save "$bases\Fusionada_personasyhogares_18.dta", replace

*2019
use "$bases\Fusionada_personasyhogares_19.dta", clear

g ipc_ene19=.
replace ipc_ene19 =97.8759112602927 if bc_mes==1
replace ipc_ene19 =100 if bc_mes==2
replace ipc_ene19 =100.975507421199 if bc_mes==3
replace ipc_ene19 =101.531441758011 if bc_mes==4
replace ipc_ene19 =101.966748833062 if bc_mes==5
replace ipc_ene19 =102.370587926785 if bc_mes==6
replace ipc_ene19 =103.026170871139 if bc_mes==7
replace ipc_ene19 =103.812870404364 if bc_mes==8
replace ipc_ene19 =104.730686526459 if bc_mes==9
replace ipc_ene19 =105.270886872607 if bc_mes==10
replace ipc_ene19 =106.062831069387 if bc_mes==11
replace ipc_ene19 =106.508627471548 if bc_mes==12

save "$bases\Fusionada_personasyhogares_19.dta", replace

