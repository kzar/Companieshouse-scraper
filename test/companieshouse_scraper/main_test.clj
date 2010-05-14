(ns companieshouse-scraper.core-test
  (:use [companieshouse-scraper.core] :reload-all)
  (:use [clojure.test]))

(deftest replace-me ;; FIXME: write
  (is false))

; CHEERS (THETFORD) LIMITED
; - CHEERS THETFORD LIMITED
; - CHEERSTIX MARKETING LIMITED
(companieshouse-scraper.main/search "CHEERS (THETFORD) LIMITED" 3
				    #(do (. Thread sleep (rand 1000))
					 (println (:name %))))

; A & J LIMITED, A & J LTD, A & J LLP
(companieshouse-scraper.main/search "A & J LIMITED" 3
				    #(do (. Thread sleep (rand 1000))
					 (println (:name %))))

; A.D.I. SOLUTIONS LTD
; - ADI-SOLUTIONS.COM LTD
; - A D I SOLUTIONS (LTD.)
; - ADISON LIMITED
(companieshouse-scraper.main/search "A.D.I. SOLUTIONS LTD" 4
				    #(do (. Thread sleep (rand 1000))
					 (println (:name %))))


; AASB LIMITED, A * ASBESTOS LTD, A.A.S. BUILDERS LIMITED
(companieshouse-scraper.main/search "AASB LIMITED" 3
				    #(do (. Thread sleep (rand 1000))
					 (println (:name %))))

; ABACUS ENTERPRISES LIMITED
; - ABACUS ENTERPRISES LIMITED
; - ABACUS ENTERTAINMENTS LLP
(companieshouse-scraper.main/search "ABACUS ENTERPRISES LIMITED" 3
				    #(do (. Thread sleep (rand 1000))
					 (println (:name %))))

; ABBEY NATIONAL FINANCIAL INVESTMENTS NO.1 LIMITED,
;  - ABBEY NATIONAL FINANCIAL INVESTMENTS NO.2 LIMITED
;  - ABBEY NATIONAL FINANCIAL INVESTMENTS 3 B.V.
;  - ABBEY NATIONAL FINANCIAL INVESTMENTS 4 B.V.
;  - ABBEY NATIONAL FUNDING PLC
(companieshouse-scraper.main/search "ABBEY NATIONAL FINANCIAL INVESTMENTS NO.1 LIMITED" 5
				    #(do (. Thread sleep (rand 1000))
					 (println (:name %))))

; ABRAKADABRI LIMITED
; - ABRA*KEBAB*RA LIMITED
; - ABRAKEBABRA UK LIMITED
(companieshouse-scraper.main/search "ABRAKADABRI LIMITED" 3
				    #(do (. Thread sleep (rand 1000))
					 (println (:name %))))

; ACTONE RECORDS LTD
; - ACT*1 RECRUITING LIMITED
; - ACT*1 SERVICES LIMITED
; - ACTON ESTATES LIMITED
(companieshouse-scraper.main/search "ACTONE RECORDS LTD" 4
				    #(do (. Thread sleep (rand 1000))
					 (println (:name %))))

;ABERDEEN COMPOSITE COMPANY (1342) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1367) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1307) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1395) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1383) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1316) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1318) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1355) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1345) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1394) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1301) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1382) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1317) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1315) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1358) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1327) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1399) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1396) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1371) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1397) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1325) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1368) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1303) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1323) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1324) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1344) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1357) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1350) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1330) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1340) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1314) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1338) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1374) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1337) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1346) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1347) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1300) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1378) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1328) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1390) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1284) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1288) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1285) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1283) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1280) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1286) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1252) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1258) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1257) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1254) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1256) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1253) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1250) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1255) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1242) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1246) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1241) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1247) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1243) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1248) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1240) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1295) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1290) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1297) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1294) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1291) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1293) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1299) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1296) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1204) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1211) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1215) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1209) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1212) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1214) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1210) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1200) LTD.
; - ABERDEEN COMPOSITE COMPANY (1207) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1206) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1203) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1275) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1276) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1274) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1278) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1272) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1277) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1262) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1268) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1269) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1264) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1239) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1232) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1238) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1224) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1223) LIMITED
; - ABERDEEN COMPOSITE COMPANY (1222) LIMITED
; - ABERDEEN COMPUTER SERVICES LIMITED
(companieshouse-scraper.main/search "ABERDEEN COMPOSITE COMPANY (1342) LIMITED" 97
				    #(do (. Thread sleep (rand 1000))
					 (println (:name %))))

;@A COMPANY LIMITE
;A LIMITE
;@AA LIMITED
;A LIMITE
;AAA LIMITED
;A LT
;A LIMITE
;AAA AA LIMITED
;A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A. LTDissolved
;A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.A.AAMIRI LIMITED
;9 SCHOOL OF MOTORING LIMITE
;A.A.A.A.A!A!A!A!A! SCHLUESSELDIENST BESCHAEDIGUNGSFREIE OEFFNUNG 24H NOTDIENST LTD.	
;AAAAAA AARDVARK PROPERTY SERVICES LIMITED
;AAAAAA ABACADABRA LIMITED
;A ABACUS ACTIONS LTD
;AAAAAA ABATE EMERGENCY LIMITED
;AAAAAA ABATE EMERGENCY PLUMBING LTD
;AAAAAA ABATE PLUMBING EMERGENCY LTD
;A ABBA ACTIONS LIMITED
;A.A.A.A.A.A.A DER PFENNIG-GUTE UMZUG LIMITED
;A.A.A.A.A. ABSICHERUNGEN & SCHLUESSEL - NOTDIENST TAG + NACHT EINBRUCHSCHUTZ REMUS LIMITED
;A AA A CAPITAL MAINTENANCE SERVICES LIMITED
;A CONCEPT BUILDING SOLUTIONS LTD
;AA AAA ADVANCED HOME ASSISTANCE LIMITED
;A GARAGE DOOR AND GATE COMPANY LTD
;A.A.A.A.A.A. GAS-SERVE LIMITED
;AAAAAA H & B PLUMBING AND HEATING LIMITED
;AAA AA ALL AREAS MAINTENANCE LTD.
;AAA AA ALL BIRMINGHAM MAINTENANCE LTD
;AAA.AA.A1.AACTION MAINTENANCE SERVICES LIMITED
;AAA.AA.A1.ACOMPLETE PLUMBING & HEATING SERVICES LIMITEin Liquidation
;AAA AA ASTERICK MAINTENANCE LTD
;AAAAACE TICKET SERVICES STARGREEN BOX OFFICE LIMITED
;.......... AAAA ACTIVE MAINTENANCE SOLUTIONS LIMITED
;AAA A+P LIMITED
;A.A.A.A.A. PEST & VERMIN CONTROL SERVICES LIMITED
;AAA ABACUS INVESTIGATION LIMITED
;THE A.A.A. ACADEMY COMMUNITY INTEREST COMPANY
;AAA ACCESS LIMITED
;AAA ACCOUNTANCY LIMITED
;A.A.A. ACCOUNTANCY SERVICES LIMITED	Dissolved
;AAA ACCOUNTANCY SERVICES LTD
;AAA ACCOUNTANTS LIMITED	Dissolved
;AAA ACCOUNTING LTD
;AAA ACCOUNTING LTD
(companieshouse-scraper.main/search "A" 45
				    #(do (. Thread sleep (rand 1000))
					 (println (:name %))))


;ALBANY HOMES INTERNATIONAL LIMITED
;ALBANY HOMES RENTALS LIMITED
(companieshouse-scraper.main/search "ALBANY HOMES INTERNATIONAL LIMITED" 2
				    #(do (. Thread sleep (rand 1000))
					 (println (:name %))))