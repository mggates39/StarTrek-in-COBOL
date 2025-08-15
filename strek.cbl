       IDENTIFICATION DIVISION.
       PROGRAM-ID. STREK.
       AUTHOR.  KURT WILHELM.
       INSTALLATION.  OAKLAND UNIVERSITY.
       DATE-WRITTEN.  COMPLETED SEPTEMBER 1, 1979.
      *
      *******************************************************
      * STAR_TREK SIMULATES AN OUTER SPACE ADVENTURE GAME   *
      * ON A REMOTE TERMINAL.  THE USER COMMANDS THE U.S.S. *
      * ENTERPRISE, AND THRU VARIOUS OFFENSIVE AND DEFEN-   *
      * SIVE COMMANDS, TRAVELS THROUGHOUT THE GALAXY ON A   *
      * MISSION TO DESTROY ALL KLINGONS, WHICH ALSO MANEU-  *
      * VER AND FIRE ON THE ENTERPRISE.                     *
      *******************************************************
      *

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  V-380.
       OBJECT-COMPUTER.  V-300.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  EOF-FLAG                  PIC X VALUE "N".
       01  STAR-TABLE.
           05  ROW      OCCURS 42 TIMES.
               10  KOLUMN            PIC X OCCURS 42 TIMES.
       01  RCTR                      PIC 99.
       01  KCTR                      PIC 99.
       01  COMMANDS-X.
           05  COMMAND               PIC X(3).
               88  NAVIGATE          VALUE "NAV".
               88  PHASERS           VALUE "PHA".
               88  TORPEDO           VALUE "TOR".
               88  SHIELDS           VALUE "DEF".
               88  DOCK              VALUE "DOC".
               88  LIB-COM           VALUE "COM".
               88  NAV-C             VALUE "NAV".
               88  PHA-C             VALUE "PHA".
               88  TOR-C             VALUE "TOR".
               88  DEF-C             VALUE "DEF".
               88  DOC-C             VALUE "DOC".
               88  COM-C             VALUE "COM".
           05  ENTRY1                PIC 9.
           05  ENTRY2                PIC 9.
       01  MINI-TABLE.
           05  MROW     OCCURS 14 TIMES.
               10  MCOL              PIC X OCCURS 14 TIMES.
       01  RCNTR                     PIC 99.
       01  KCNTR                     PIC 99.
       01  X                         PIC 999.
       01  Y                         PIC 999.
       01  WS-DATE                   PIC 9(4) COMP.
       01  TIME-FLAG                 PIC 9.
           88  TIME-FLAG-SET         VALUE 1.
       01  MAX-NO                    PIC 999.
       01  HQ1                       PIC 9.
       01  HQ2                       PIC 9.
       01  T-STORE                   PIC 9(4) COMP.
       01  ATTACK-FLAG               PIC 9.
           88  KLINGONS-ATTACKING    VALUE 1.
       01  TOO-LATE-FLAG             PIC 9.
           88  TOO-LATE              VALUE 1.
       01  BYE-K                     PIC 99.
       01  VAR1                      PIC 99 VALUE 1.
       01  VAR2                      PIC 9(6) COMP.
       01  VAR3                      PIC 9(6) COMP.
       01  VAR4                      PIC 9(4) COMP.
       01  VAR4-AN                   PIC X(4).
       01  VAR5                      PIC ZZZ999.
       01  VAR6                      PIC ZZZZ99.
       01  RETURN-X                  PIC X.
       01  COMP-COM                  PIC 9.
       01  BASE-CNT                  PIC 9 VALUE 0.
       01  NX                        PIC 99 VALUE 0.
       01  A                         PIC 999.
       01  B                         PIC 999.
       01  WARP1                     PIC 99.
       01  WARP2                     PIC 99.
       01  WARP3                     PIC 99.
       01  WARP4                     PIC 99.
       01  GENERATE-TABLE.
           05  CHAR                  PIC X OCCURS 25 TIMES.
       01  SEED-TABLE                PIC X(25) VALUE
               "A4HFXNC89KD3JXF5DKS3HB3M1".
       01  GENRTE-RESULT             PIC 9.
           88  NO-WAY                VALUE 1.
       01  FUEL-COUNT                PIC S9(5) COMP.
       01  TORPS                     PIC 9 VALUE 5.
       01  PRT-LINES.
           05  CON-RED.
               10  FILLER            PIC X(16) VALUE
                   "*CONDITION RED* ".
               10  KLGNS             PIC 99.
               10  FILLER            PIC X(21) VALUE
                   " KLINGONS IN QUADRANT".
           05  CON-GREEN.
               10  FILLER            PIC X(17) VALUE
                   "*CONDITION GREEN*".
           05  COM-REQ.
               10  FILLER            PIC X(22) VALUE
                   "WHAT IS YOUR COMMAND? ".
       01  MASTER-TBL.
           05  MAROW     OCCURS 126 TIMES.
               10  MACOL             PIC X OCCURS 126 TIMES.
       01  MRCTR                     PIC 999.
       01  MKCTR                     PIC 999.
       01  VAB1                      PIC 9.
       01  VAB2                      PIC 99.
       01  ROLL-X                    PIC 999V.
       01  SHIELD-CNT                PIC S9(4) COMP.
       01  SHIELD-CNT-AN             PIC X(4).
       01  DAMAGE-CNT                PIC 9(6) COMP.
       01  SCAN-KEEP.
           05  CV                    PIC 99 OCCURS 18 TIMES.
       01  SCAN-CTR                  PIC 99.
       01  SCAN-TABLE.
           05  SCAN-ROW     OCCURS 14 TIMES.
               10 SCAN-COL           PIC X OCCURS 14 TIMES.
       01  RX-S                      PIC 99V99.
       01  QT                        PIC 99.
       01  RT                        PIC 99.
       01  QX                        PIC 99.
       01  RX                        PIC 99.
       01  TR1                       PIC 9.
       01  TR2                       PIC 9.
       01  KTCTR                     PIC 99.
       01  RTCTR                     PIC 99.
       01  NAME-VAR.
           05  NAME-X                PIC X(12).
       01  INST-REPLY                PIC X(3).
           88  YES-REPLY             VALUE "YES".
       01  INDICATE-Y                PIC 9.
           88  TRAP-VEC              VALUE 1.
       01  INDICATE-X                PIC 9.
           88  BYE-BYE               VALUE 1.
       01  INDICATE-Z                PIC 9.
           88  JUST-STARTING         VALUE 0.
       01  QUADRANT.
           05  FILLER                PIC X(9) VALUE "QUADRANT ".
           05  Q1                    PIC 9.
           05  FILLER                PIC X VALUE ",".
           05  Q2                    PIC 9.
           05  FILLER                PIC X(15) VALUE
               "    STAR DATE: ".
           05  S-DATE                PIC 9(4).
       01  DS-DATE                   PIC 9(4).
       01  DS-TABLE.
           05  DS-MIN                PIC 99.
           05  DS-SEC                PIC 99.
       01  KLINGONS                  PIC 99.
       01  ROMULONS                  PIC 99.
       01  LST-REPLY                 PIC X(3).
           88  YES-LST               VALUE "YES".
       01  REV-STR                   PIC 9(6) COMP.
       01  SEED-X                    PIC V9(6) COMP.
       01  SEED-AST                  PIC 9(6)V9(6) COMP.
       01  WS-TIME.
           05  WS-HOUR               PIC 99.
           05  WS-MIN                PIC 99.
           05  WS-SEC                PIC 99.
           05  WS-SIXTY              PIC 99.
       01  TIME-REV.
           05  WS-SIXTY              PIC 99.
           05  WS-SEC                PIC 99.
           05  WS-MIN                PIC 99.
       01  WARP-SPEED.
           05  WARP-A                PIC 9.
           05  WARP-PT               PIC X.
           05  WARP-B                PIC 99.
       01  COURSE-X.
           05  COURSE-A              PIC 9.
           05  COURSE-PT             PIC X.
           05  COURSE-B              PIC 99.
       01  VAB5                      PIC 99.
       01  VAB6                      PIC 99.
       01  VAE1                      PIC Z9.
       01  K-OR                      PIC 99.
       01  QS-1                      PIC 9.
       01  QS-2                      PIC 9.
       01  SRCTR                     PIC S999.
       01  SKCTR                     PIC S999.
       01  MOD-CTR                   PIC 99.
       01  MD-ROW.
           05  MD-SUB                PIC X OCCURS 28 TIMES.
       01  DM-VAR4                   PIC 9(4) COMP.
       01  CT-K                      PIC 99.
       01  CT-R                      PIC 99.
       01  DIST-X                    PIC 99.
       01  DIST-R                    PIC 99.
       01  DIST-B                    PIC 99.
       01  TAL4                      PIC 9.
       01  KH-TL                     PIC 9(5) COMP.
       01  STR-A                     PIC 99.
       01  STR-R                     PIC 99.
       01  STR-X                     PIC 99.
       01  CX                        PIC 999 COMP.
       01  DX                        PIC 999 COMP.
       01  CX-1                      PIC 9.
       01  DX-1                      PIC 9.
       01  E1                        PIC 99.
       01  E2                        PIC 99.
       01  R1                        PIC 99.
       01  R2                        PIC 99.
       01  K1                        PIC 99.
       01  K2                        PIC 99.
       01  B1                        PIC 99.
       01  B2                        PIC 99.
       01  STAR-CTR                  PIC 999.
       01  REP-CTR                   PIC 99.
       01  FUEL-CO                   PIC ZZZ99.
       01  SHIELD-CO                 PIC ZZ99.
       01  SBL                       PIC 9.
       01  QT1                       PIC 9.
       01  QT2                       PIC 9.
       01  QT3                       PIC 9.
       01  QT4                       PIC 9.
       01  R9                        PIC 9.
       01  Q9                        PIC 9.
       01  W                         PIC 999.
       01  Z                         PIC 999.
       01  SKILL-LEV                 PIC 9.
       01  DIST-K-STR.
           05  DKC                   PIC 99 OCCURS 45 TIMES.
       01  DIST-R-STR.
           05  DRC                   PIC 99 OCCURS 60 TIMES.
          
       PROCEDURE DIVISION.

       0000-CONTROL SECTION.
       0000-PROGRAM-CONTROL.
           PERFORM 0100-HOUSEKEEPING THRU 0100-EXIT.
           PERFORM 1000-MAINLINE THRU 1000-EXIT.
           PERFORM 9000-END-OF-JOB THRU 9000-EXIT.
           STOP RUN.
       
      ************************************************
      * 0100-HOUSEKEEPING INITIALIZES VARIABLES, AND *
      * ASKS THE USER FOR A NAME AND SKILL LEVEL.    *
      * IT THEN DETERMINES THE QUANTITY OF BASES,    *
      * KLINGONS, AND ROMULONS IN THE GALAXY.        *
      * INSTRUCTIONS ARE A USER OPTION.              *
      ************************************************

       0100-HOUSEKEEPING-SECTION SECTION.
       0100-HOUSEKEEPING.
           MOVE 0 TO SHIELD-CNT.
           MOVE 0 TO DAMAGE-CNT.
           MOVE 40000 TO FUEL-COUNT.
           MOVE 0 TO INDICATE-Z.
           MOVE 0 TO GENRTE-RESULT.
           MOVE SPACES TO MD-ROW.
           MOVE SEED-TABLE TO GENERATE-TABLE.
           MOVE 0 TO INDICATE-X.
           MOVE 0 TO INDICATE-Y.
           MOVE 0 TO ATTACK-FLAG.
           MOVE 0 TO TOO-LATE-FLAG.
           DISPLAY " ".
           DISPLAY " *STAR TREK* ".
           DISPLAY " ".
           DISPLAY "CONGRATULATIONS - YOU HAVE JUST BEEN APPOINTED ".
           DISPLAY "CAPTAIN OF THE U.S.S. ENTERPRISE. ".
           DISPLAY " ".
           DISPLAY "PLEASE ENTER YOUR NAME, CAPTAIN ".
           ACCEPT NAME-X.
           DISPLAY "AND YOUR SKILL LEVEL (1-4)? ".
           ACCEPT SKILL-LEV.
           IF SKILL-LEV NOT NUMERIC OR SKILL-LEV < 1 OR SKILL-LEV > 4
               DISPLAY "INVALID SKILL LEVEL "
               DISPLAY "ENTER YOUR SKILL LEVEL (1-4) "
               ACCEPT SKILL-LEV
               IF SKILL-LEV NOT NUMERIC OR SKILL-LEV < 1 
                       OR SKILL-LEV > 4
                   MOVE 1 TO SKILL-LEV
                   DISPLAY "YOUR SKILL LEVEL MUST BE 1 ".
           MOVE 0 TO VAB5.
           MOVE 0 TO VAB6.
           INSPECT NAME-X TALLYING VAB6 FOR ALL "A".
           INSPECT NAME-X TALLYING VAB6 FOR ALL "E".
           ADD 1 TO VAB6.
           INSPECT NAME-X TALLYING VAB5 FOR ALL " ".
           COMPUTE VAB6 ROUNDED = (VAB5 / 1.75) + (VAB6 / SKILL-LEV).
           COMPUTE K-OR ROUNDED = (SKILL-LEV * 4) + VAB6 + 5.
           COMPUTE VAB1 = 9 - SKILL-LEV.
           COMPUTE VAB2 ROUNDED = (SKILL-LEV / 3) * K-OR.
           MOVE K-OR TO KLINGONS.
           MOVE VAB1 TO VAE1.
           ACCEPT WS-TIME FROM TIME.
           MOVE WS-MIN OF WS-TIME TO DS-MIN.
           MOVE WS-SEC OF WS-TIME TO DS-SEC.
           MOVE DS-TABLE TO S-DATE.
           ADD 16 TO DS-MIN.
           IF DS-MIN > 59
               MOVE 1 TO TIME-FLAG
           ELSE
               MOVE 0 TO TIME-FLAG.
           MOVE DS-TABLE TO DS-DATE.
           DISPLAY " ".
           DISPLAY " *MESSAGE FROM STAR FLEET COMMAND* ".
           DISPLAY " ".
           DISPLAY "ATTENTION - CAPTAIN " NAME-X.
           DISPLAY "YOUR MISSION IS TO DESTROY THE ".
           DISPLAY K-OR " KLINGON SHIPS THAT HAVE INVADED ".
           DISPLAY "THE GALAXY TO ATTACK STAR FLEET HQ ".
           DISPLAY "ON STAR DATE " DS-DATE 
           " - GIVING YOU 16 STAR DATES.".
           PERFORM 1200-INITIALIZE-GALAXY THRU 1200-EXIT.
           DISPLAY " ".
           DISPLAY "DO YOU REQUIRE INSTRUCTIONS? ".
           ACCEPT INST-REPLY.
           IF YES-REPLY
               PERFORM 0500-PRT-INST THRU 0500-EXIT
               PERFORM 0550-ADD-INST THRU 0550-EXIT.
       0100-EXIT.  EXIT.

       0500-PRT-INST.
           DISPLAY " ".
           DISPLAY "YOU MAY USE THE FOLLOWING COMMANDS: ".
           DISPLAY "       NAV  (TO NAVIGATE) ".
           DISPLAY "       PHA  (TO FIRE PHASERS) ".
           DISPLAY "       TOR  (TO FIRE TORPEDO) ".
           DISPLAY "       DEF  (TO RAISE OR LOWER SHIELDS) ".
           DISPLAY "       DOC  (TO DOCK AT A STAR BASE) ".
           DISPLAY "       COM  (TO REQUEST INFO FROM THE LIBRARY COMPUT
      -    "ER) ".
           DISPLAY " ".
           DISPLAY "COURSE PLOT: ".
           DISPLAY "      ".
           DISPLAY "    1 ".
           DISPLAY "  8   2 ".
           DISPLAY "7  -X-  3 ".
           DISPLAY "  6   4 ".
           DISPLAY "    5 ".
           DISPLAY "      ".
       0500-EXIT.  EXIT.

       0550-ADD-INST.
           DISPLAY "THERE ARE " VAE1 " STAR BASES LOCATED SOMEWHERE IN T
      -    "HE GALAXY, ".
           DISPLAY "WHICH IS MADE UP OF 81 QUADRANTS, 1,1 THRU 9,9. ".
           DISPLAY "YOU MAY DOCK AT A STAR BASE TO REFUEL AND EFFECT REP
      -    "AIRS ".
           DISPLAY "WHEN THERE IS A BASE IN YOUR QUADRANT.  YOU ARE AUTH
-     -    "ORIZED ".
           DISPLAY "TO DESTROY ROMULON VESSELS IF THEY INTERFERE WITH YO
-     -    "UR MISSION. ".
           DISPLAY "      ".
           DISPLAY "HIT RETURN ".
           ACCEPT RETURN-X.
       0550-EXIT.  EXIT.
      
       1000-MAINLINE.
           PERFORM 4000-DISPLAY-G THRU 4000-EXIT.
           MOVE 1 TO INDICATE-Z.
           PERFORM 2000-PROCESS THRU 2000-EXIT
               UNTIL KLINGONS < 1 OR BYE-BYE.
           PERFORM 8500-FINISH-GAME THRU 8500-EXIT.
       1000-EXIT.  EXIT.
      
       1100-CHK-GALAXY.
           ADD 1 TO VAR1.
           IF VAR1 = 7
               INSPECT MASTER-TBL REPLACING ALL "      K" BY "K      "
               PERFORM 1120-RESET THRU 1120-EXIT
           ELSE
               IF VAR1 = 12
                   INSPECT MASTER-TBL REPLACING ALL "R      " 
                           BY "      R"
                   PERFORM 1120-RESET THRU 1120-EXIT
               ELSE
                   IF VAR1 = 15
                       INSPECT MASTER-TBL REPLACING ALL "K           "
                          BY "           K"
                       PERFORM 1120-RESET THRU 1120-EXIT
                   ELSE
                       IF VAR1 > 20
                           INSPECT MASTER-TBL REPLACING ALL "         R"
                               BY "R         "
                           PERFORM 1120-RESET THRU 1120-EXIT
                           MOVE 1 TO VAR1.
       1100-EXIT.  EXIT.
      
       1120-RESET.
           PERFORM 5900-TRANS THRU 5900-EXIT.
           MOVE 0 TO KLGNS.
           MOVE 0 TO ROMULONS.
           MOVE 0 TO BASE-CNT.
           INSPECT MINI-TABLE TALLYING KLGNS FOR ALL "K".
           INSPECT MINI-TABLE TALLYING ROMULONS FOR ALL "R".
           INSPECT MINI-TABLE TALLYING BASE-CNT FOR ALL "B".
       1120-EXIT.  EXIT.
      
       1145-CK-FLAG.
           IF TIME-FLAG-SET AND DS-MIN < 40
      
           ADD 60 TO DS-MIN.
       1145-EXIT.  EXIT.
      
       1150-CK-TIME.
           IF KLINGONS > 0
               ACCEPT WS-TIME FROM TIME
               MOVE WS-MIN OF WS-TIME TO DS-MIN
               PERFORM 1145-CK-FLAG THRU 1145-EXIT
               MOVE WS-SEC OF WS-TIME TO DS-SEC
               MOVE DS-TABLE TO S-DATE
           ELSE
               GO TO 1150-EXIT.
           COMPUTE T-STORE = DS-DATE - S-DATE.
           IF T-STORE < 90 AND NOT KLINGONS-ATTACKING
               MOVE 14 TO MAX-NO
               COMPUTE W = ((HQ2 - 1) * 14)
               COMPUTE Z = ((HQ1 - 1) * 14)
               INSPECT MASTER-TBL REPLACING ALL "K" BY " "
               MOVE 0 TO RX
               PERFORM 1170-MOVE-ON-HQ THRU 1170-EXIT
                   VARYING KCTR FROM 1 BY 1 UNTIL KCTR > KLINGONS
               MOVE 1 TO ATTACK-FLAG
               PERFORM 5900-TRANS THRU 5900-EXIT
               IF (Q1 NOT = HQ1 OR Q2 NOT = HQ2)
                   DISPLAY "WARNING - STAR DATE: " S-DATE
                   DISPLAY "SCIENCE OFFICER SPOCK ADVISES"
                   DISPLAY "YOU NAVIGATE TO QUADRANT " HQ1 "," HQ2
                   DISPLAY "TO DEFEND STAR FLEET HEADQUARTERS".
           IF NOT TOO-LATE
               MOVE DS-DATE TO WS-DATE.
           IF S-DATE > WS-DATE AND Q1 = HQ1 AND Q2 = HQ2 
                   AND NOT TOO-LATE
               MOVE 1 TO TOO-LATE-FLAG
               ADD 230 TO WS-DATE
           ELSE
               IF S-DATE > WS-DATE
                   MOVE 1 TO INDICATE-X
                   PERFORM 8200-CK-DONE THRU 8200-EXIT.
       1150-EXIT.  EXIT.
      
       1160-DBL-K.
           PERFORM 1225-DBL-ROLL THRU 1225-EXIT.
           ADD 1 TO RX.
           COMPUTE A = W + A.
           COMPUTE B = Z + B.
       1160-EXIT.  EXIT.
      
       1170-MOVE-ON-HQ.
           MOVE 0 TO A.
           PERFORM 1160-DBL-K THRU 1160-EXIT
               UNTIL MACOL (A , B) = " " AND A > 0.
           MOVE "K" TO MACOL (A , B).
       1170-EXIT.  EXIT.
      
      **********************************************
      * 1200-INITIALIZE-GALAXY MOVES STARS, KLING- *
      * ONS, ROMULONS, BASES, AND FINALLY, THE EN- *
      * TERPRISE TO MASTER-TBL IN RANDOM POSITION, *
      * AND IN THE QUANTITIES DETERMINED IN 0100-  *
      * HOUSEKEEPING.                              *
      **********************************************
      
       1200-INITIALIZE-GALAXY.
           MOVE SPACES TO MASTER-TBL.
           ACCEPT WS-TIME FROM TIME.
           MOVE CORRESPONDING WS-TIME TO TIME-REV.
           MOVE TIME-REV TO REV-STR.
           COMPUTE SEED-X = (REV-STR / 1000000).
           MOVE 126 TO MAX-NO.
           PERFORM 1230-MOVE-STARS THRU 1230-EXIT
               VARYING STAR-CTR FROM 1 BY 1 UNTIL STAR-CTR > 275.
           PERFORM 1240-MOVE-ROMULONS THRU 1240-EXIT
               VARYING STAR-CTR FROM 1 BY 1 UNTIL STAR-CTR > VAB2.
           PERFORM 1250-MOVE-KLINGONS THRU 1250-EXIT
               VARYING STAR-CTR FROM 1 BY 1 UNTIL STAR-CTR > K-OR.
           PERFORM 1260-MOVE-BASE THRU 1260-EXIT
               VARYING STAR-CTR FROM 1 BY 1 UNTIL STAR-CTR > VAB1.
           PERFORM 1270-MOVE-E THRU 1270-EXIT.
           PERFORM 1280-MOVE-HQ THRU 1280-EXIT.
       1200-EXIT.  EXIT.
      
       1220-ROLL.
           COMPUTE SEED-AST = (262147.0 * SEED-X).
           MOVE SEED-AST TO SEED-X.
           COMPUTE ROLL-X = (SEED-X * MAX-NO) + 1.
       1220-EXIT.  EXIT.
      
       1225-DBL-ROLL.
           PERFORM 1220-ROLL THRU 1220-EXIT.
           MOVE ROLL-X TO A.
           PERFORM 1220-ROLL THRU 1220-EXIT.
           MOVE ROLL-X TO B.
       1225-EXIT.  EXIT.
      
       1230-MOVE-STARS.
           PERFORM 1225-DBL-ROLL THRU 1225-EXIT.
           MOVE "*" TO MACOL (A , B).
       1230-EXIT.  EXIT.
      
       1240-MOVE-ROMULONS.
           PERFORM 1225-DBL-ROLL THRU 1225-EXIT.
           MOVE "R" TO MACOL (A , B).
       1240-EXIT.  EXIT.
      
       1250-MOVE-KLINGONS.
           PERFORM 1225-DBL-ROLL THRU 1225-EXIT
               UNTIL MACOL (A , B) = " ".
           MOVE "K" TO MACOL (A , B).
       1250-EXIT.  EXIT.
      
       1260-MOVE-BASE.
           PERFORM 1225-DBL-ROLL THRU 1225-EXIT
               UNTIL MACOL (A , B) = " ".
           MOVE "B" TO MACOL (A , B).
       1260-EXIT.  EXIT.
      
       1270-MOVE-E.
           PERFORM 1225-DBL-ROLL THRU 1225-EXIT
               UNTIL MACOL (A , B) = " ".
           MOVE A TO MRCTR.
           MOVE B TO MKCTR.
           MOVE "E" TO MACOL (MRCTR , MKCTR).
       1270-EXIT.  EXIT.
      
       1280-MOVE-HQ.
           PERFORM 1225-DBL-ROLL THRU 1225-EXIT
               UNTIL MACOL (A , B) = " ".
           MOVE "H" TO MACOL (A , B).
           COMPUTE HQ1 = (B - 1) / 14 + 1.
           COMPUTE HQ2 = (A - 1) / 14 + 1.
       1280-EXIT.  EXIT.
      
       1700-CK-VAR-WARP.
           INSPECT COURSE-B REPLACING ALL " " BY ZEROS.
           INSPECT WARP-A REPLACING ALL " " BY ZEROS.
           INSPECT WARP-B REPLACING ALL " " BY ZEROS.
           IF COURSE-B NOT NUMERIC
               MOVE ZERO TO COURSE-B.
           IF WARP-A NOT NUMERIC
               MOVE ZERO TO WARP-A.
           IF WARP-B NOT NUMERIC
               MOVE ZERO TO WARP-B.
       1700-EXIT.  EXIT.
      
      ********************************************
      * 2000-PROCESS IS AN ITERATIVE LOOP THAT   *
      * REQUESTS AND EXECUTES A COMMAND UNTIL    *
      * ALL KLINGONS ARE DESTROYED, OR THE EN-   *
      * TERPRISE IS NO LONGER ABLE TO CONTINUE.  *
      ********************************************
       
       2000-PROCESS.
           PERFORM 8400-GENERATE THRU 8400-EXIT.
           IF NO-WAY OR KLGNS > 1
               ADD 4 TO NX.
           DISPLAY COM-REQ.
           ACCEPT COMMANDS-X.
           IF NAVIGATE OR NAV-C
               IF ENTRY1 NOT NUMERIC OR ENTRY1 < 1 OR ENTRY1 > 8 
                       OR ENTRY2 NOT NUMERIC
                   DISPLAY "WHAT COURSE (1 - 8.99)? "
                   ACCEPT COURSE-X
                   DISPLAY "WHAT WARP FACTOR (0 - 9.99)? "
                   ACCEPT WARP-SPEED
                   PERFORM 1700-CK-VAR-WARP THRU 1700-EXIT
                   PERFORM 7100-NAV THRU 7100-EXIT
                   PERFORM 4000-DISPLAY-G THRU 4000-EXIT
               ELSE
                   MOVE ENTRY1 TO COURSE-A
                   MOVE ENTRY2 TO WARP-A
                   MOVE 0 TO COURSE-B
                   MOVE 0 TO WARP-B
                   PERFORM 7100-NAV THRU 7100-EXIT
                   PERFORM 4000-DISPLAY-G THRU 4000-EXIT
           ELSE
               IF PHASERS OR PHA-C
                   PERFORM 7200-PHA THRU 7200-EXIT
               ELSE
                   IF TORPEDO OR TOR-C
                       PERFORM 7300-TOR THRU 7300-EXIT
                   ELSE
                       IF SHIELDS OR DEF-C
                           PERFORM 7500-DEF THRU 7500-EXIT
                       ELSE
                           IF DOCK OR DOC-C
                               PERFORM 7600-DOC THRU 7600-EXIT
                           ELSE
                               IF LIB-COM OR COM-C
                                   PERFORM 3000-COM-FUN THRU 3000-EXIT
                               ELSE
                                   DISPLAY "INVALID COMMAND - DO YOU WAN
      -    "T A LIST OF COMMANDS? "
                                   ACCEPT LST-REPLY
                                   IF YES-LST
                                       PERFORM 0500-PRT-INST 
                                           THRU 0500-EXIT.
           PERFORM 1150-CK-TIME THRU 1150-EXIT.
           PERFORM 1100-CHK-GALAXY THRU 1100-EXIT.
       2000-EXIT.  EXIT.
      
      ***************************************
      * 3000-COM-FUN SIMULATES THE OPERA-   *
      * TION OF AN ON-BOARD LIBRARY COMPU-  *
      * TER, AND RESPONDS TO NUMERIC COM-   *
      * MANDS , RANGE 1 - 6.                *
      ***************************************
       
       3000-COM-FUN.
           DISPLAY "      ".
           IF ENTRY1 NOT NUMERIC OR ENTRY1 < 1 OR ENTRY1 > 6
               DISPLAY "*COMPUTER ACTIVE AND AWAITING COMMAND* "
               ACCEPT COMP-COM
           ELSE
               MOVE ENTRY1 TO COMP-COM.
           IF COMP-COM NOT NUMERIC OR COMP-COM < 1 OR COMP-COM > 6
               DISPLAY "INVALID COMPUTER COMMAND "
               DISPLAY "DO YOU WANT A LIST  OF COMPUTER COMMANDS? "
      
               ACCEPT LST-REPLY
               IF YES-LST
                   DISPLAY "FUNCTIONS AVAILABLE FROM THE LIBRARY COMPUTE
-     -    "R: "
                   DISPLAY "     1  TO REQUEST SHIP STATUS "
                   DISPLAY "     2  TO REQUEST SHORT RANGE SCAN OF QUADR
-     -    "ANT "
                   DISPLAY "     3  TO REQUEST LONG RANGE SCAN "
                   DISPLAY "     4  TO REQUEST TALLY OF KLINGONS "
                   DISPLAY "     5  TO REQUEST INTELLIGENCE REPORT "
                   DISPLAY "     6  TO TERMINATE PROGRAM EXECUTION "
                   DISPLAY "      "
                   DISPLAY "*COMPUTER ACTIVE AND AWAITING COMMAND* "
                   ACCEPT COMP-COM
               ELSE
                   DISPLAY "COMPUTER COMMAND?"
                   ACCEPT COMP-COM.
           GO TO
               3010-COM
               3020-COM
               3030-COM
               3040-COM
               3050-COM
               3060-COM
                   DEPENDING ON COMP-COM.
           DISPLAY " INVALID COMPUTER COMMAND ".
           GO TO 3000-EXIT.
      
       3010-COM.
           PERFORM 7400-STA THRU 7400-EXIT.
           GO TO 3000-EXIT.
      
       3020-COM.
           PERFORM 4000-DISPLAY-G THRU 4000-EXIT.
           GO TO 3000-EXIT.
      
       3030-COM.
           PERFORM 7700-LRS THRU 7700-EXIT.
           GO TO 3000-EXIT.
      
       3040-COM.
           COMPUTE BYE-K = K-OR - KLINGONS.
           DISPLAY "      ".
           DISPLAY BYE-K " KLINGONS DESTROYED, " KLINGONS " REMAIN ".
           DISPLAY "ATTACK DATE: " DS-DATE.
           DISPLAY "STAR DATE: " S-DATE.
           DISPLAY "      ".
           PERFORM 8100-DMG-COM THRU 8100-EXIT.
           GO TO 3000-EXIT.
      
       3050-COM.
           PERFORM 7800-INT THRU 7800-EXIT.
           GO TO 3000-EXIT.
      
       3060-COM.
           MOVE 1 TO INDICATE-X.
           DISPLAY "      ".
           DISPLAY "*ENTERPRISE STRANDED - CAPTAIN BOOKED* ".
           DISPLAY "      ".
           PERFORM 8200-CK-DONE THRU 8200-EXIT.
           GO TO 3000-EXIT.
      
       3000-EXIT.  EXIT.
      
      *******************************************
      * 4000-DISPLAY-G DETERMINES WHAT QUADRANT *
      * THE ENTERPRISE IS IN, AND DISPLAYS THE  *
      * QUADRANT, NOTIFYING USER OF PRESENCE OF *
      * KLINGONS IN QUADRANT.                   *
      *******************************************
       
       4000-DISPLAY-G.
           MOVE 0 TO KLGNS.
           MOVE 0 TO ROMULONS.
           MOVE 0 TO BASE-CNT.
           MOVE Q1 TO QS-1.
           MOVE Q2 TO QS-2.
           COMPUTE Q1 = (MKCTR - 1) / 14 + 1.
           COMPUTE Q2 = (MRCTR - 1) / 14 + 1.
           IF Q1 NOT = QS-1 OR Q2 NOT = QS-2
               MOVE 0 TO KH-TL.
           COMPUTE X = (Q1 - 1) * 14.
           COMPUTE Y = (Q2 - 1) * 14.
           PERFORM 5900-TRANS THRU 5900-EXIT.
           INSPECT MINI-TABLE TALLYING KLGNS FOR ALL "K".
           INSPECT MINI-TABLE TALLYING ROMULONS FOR ALL "R".
           INSPECT MINI-TABLE TALLYING BASE-CNT FOR ALL "B".
           DISPLAY "      ".
           IF JUST-STARTING
               DISPLAY "YOU BEGIN IN QUADRANT " Q1 "," Q2 " WITH 40,000"
               DISPLAY "UNITS OF FUEL AND 5 PHOTON TORPEDOES. "
               DISPLAY "      "
               DISPLAY "GOOD LUCK, CAPTAIN " NAME-X
               DISPLAY "      "
               IF KLGNS > 0
                   DISPLAY CON-RED
               ELSE
                   DISPLAY CON-GREEN
           ELSE
               IF KLGNS > 0
                   DISPLAY CON-RED
                   COMPUTE VAR2 = KLGNS * FUEL-COUNT / (SHIELD-CNT + 27)
                   PERFORM 4200-TEST-VAR THRU 4200-EXIT
                   COMPUTE VAR3 = .75 * VAR2
                   ADD VAR2 TO DAMAGE-CNT
                   SUBTRACT VAR3 FROM SHIELD-CNT
                   DISPLAY "*ENTERPRISE ENCOUNTERING KLINGON FIRE* "
                   PERFORM 4500-DISP-HIT THRU 4500-EXIT
               ELSE
                   DISPLAY CON-GREEN.
           DISPLAY QUADRANT.
           PERFORM 6500-DISPLAY-MT THRU 6500-EXIT
               VARYING RCNTR FROM 1 BY 1 UNTIL RCNTR > 14.
           DISPLAY "      ".
           PERFORM 8300-CK-FUEL-DAMAGE THRU 8300-EXIT.
           PERFORM 8200-CK-DONE THRU 8200-EXIT.
       4000-EXIT.  EXIT.
      
       4200-TEST-VAR.
           IF VAR2 < 1776 AND KLGNS > 0
               ADD 223 TO VAR2
               COMPUTE VAR2 = (KLGNS * VAR2 / 3.5) + (VAR2 * DAMAGE-CNT
-          / 760) + (NX * 17).
       4200-EXIT.  EXIT.
      
       4500-DISP-HIT.
           MOVE VAR2 TO VAR5.
           DISPLAY VAR5 " UNIT HIT ON ENTERPRISE ".
       4500-EXIT.  EXIT.
      
       4700-DISP-HIT.
           MOVE VAR4 TO VAR5.
           DISPLAY VAR5 " UNIT HIT ON KLINGON ".
       4700-EXIT.  EXIT.
      
       5400-TRANS-BACK.
           PERFORM 5500-TRANSFER-BACK THRU 5500-EXIT
               VARYING KCNTR FROM 1 BY 1 UNTIL KCNTR > 14
               AFTER RCNTR FROM 1 BY 1 UNTIL RCNTR > 14.
       5400-EXIT.  EXIT.
      
       5500-TRANSFER-BACK.
           COMPUTE A = Y + RCNTR.
           COMPUTE B = X + KCNTR.
           MOVE MCOL (RCNTR , KCNTR) TO MACOL (A , B).
       5500-EXIT.  EXIT.
      
       5900-TRANS.
           PERFORM 6000-TRANSFER THRU 6000-EXIT
               VARYING KCNTR FROM 1 BY 1 UNTIL KCNTR > 14
               AFTER RCNTR FROM 1 BY 1 UNTIL RCNTR > 14.
       5900-EXIT.  EXIT.
      
       6000-TRANSFER.
           COMPUTE A = Y + RCNTR.
           COMPUTE B = X + KCNTR.
           MOVE MACOL (A , B) TO MCOL (RCNTR , KCNTR).
       6000-EXIT.  EXIT.
      
       6500-DISPLAY-MT.
           DISPLAY "= = = = = = = = = = = = = = = =".
           PERFORM 6600-MINI-DIS THRU 6600-EXIT
               VARYING RCNTR FROM 1 BY 1 UNTIL RCNTR > 14.
           DISPLAY "= = = = = = = = = = = = = = = =".
       6500-EXIT.  EXIT.
      
       6600-MINI-DIS.
           PERFORM 6700-MINI-MOD THRU 6700-EXIT
               VARYING KCNTR FROM 1 BY 1 UNTIL KCNTR > 14.
           DISPLAY "=" MD-ROW " =".
       6600-EXIT.  EXIT.
      
       6700-MINI-MOD.
           COMPUTE MOD-CTR = 2 * KCNTR.
           MOVE MCOL (RCNTR , KCNTR) TO MD-SUB (MOD-CTR).
       6700-EXIT.  EXIT.
      
       7000-NAV-CK.
           IF SRCTR < 1 OR SRCTR > 126 OR SKCTR < 1 OR SKCTR > 126
               DISPLAY "WARP DRIVE SHUT DOWN - "
               DISPLAY "UNAUTHORIZED ATTEMPT TO LEAVE GALAXY "
               PERFORM 8100-DMG-COM THRU 8100-EXIT
               GO TO 2000-EXIT
           ELSE
               MOVE " " TO MACOL (MRCTR , MKCTR)
               MOVE SRCTR TO MRCTR.
               MOVE SKCTR TO MKCTR.
               IF MACOL (MRCTR , MKCTR) = "K" OR MACOL (MRCTR , MKCTR) =
                  "R" OR MACOL (MRCTR , MKCTR) = "B"
                   PERFORM 8000-BOMB THRU 8000-EXIT
               ELSE
                   MOVE "E" TO MACOL (MRCTR , MKCTR).
       7000-EXIT.  EXIT.
      
      ********************************************
      * 7100-NAV THRU 7800-INT EXECUTE VARIOUS   *
      * COMMANDS FROM THE USER, AND PRESENT THE  *
      * RESULTS AND CONSEQUENCES OF EACH COMMAND *
      *                                          *
      * CALLED FROM 2000-PROCESS OR 3000-COM-FUN *
      ********************************************
       
       7100-NAV.
           PERFORM 8340-CK-FL THRU 8340-EXIT.
           COMPUTE FUEL-COUNT = FUEL-COUNT - (200 * WARP-A).
           IF WARP-A > 0
               MOVE WARP-A TO RX-S
           ELSE
               COMPUTE RX-S ROUNDED = WARP-B / 100.
           MOVE MRCTR TO SRCTR.
           MOVE MKCTR TO SKCTR.
           COMPUTE WARP1 ROUNDED = (WARP-A * 5) + (WARP-B * .05).
           COMPUTE WARP2 ROUNDED = (WARP-A * 8) + (WARP-B * .08).
           COMPUTE WARP3 ROUNDED = (COURSE-B * .05) * RX-S.
           COMPUTE WARP4 ROUNDED = (COURSE-B * .03) * RX-S.
           GO TO
               7110-NAV
               7120-NAV
               7130-NAV
               7140-NAV
               7150-NAV
               7160-NAV
               7170-NAV
               7180-NAV
                   DEPENDING ON COURSE-A.
           DISPLAY "INVALID COURSE".
           GO TO 7100-EXIT.
       7110-NAV.
           COMPUTE SRCTR = SRCTR - WARP2 + WARP4.
           COMPUTE SKCTR = SKCTR + WARP3.
           PERFORM 7000-NAV-CK THRU 7000-EXIT.
           GO TO 7100-EXIT.
       7120-NAV.
           COMPUTE SRCTR = SRCTR - WARP1 + WARP3.
           COMPUTE SKCTR = SKCTR + WARP1 + WARP4.
           PERFORM 7000-NAV-CK THRU 7000-EXIT.
           GO TO 7100-EXIT.
       7130-NAV.
           COMPUTE SRCTR = SRCTR + WARP3.
           COMPUTE SKCTR = SKCTR + WARP2 - WARP4.
           PERFORM 7000-NAV-CK THRU 7000-EXIT.
           GO TO 7100-EXIT.
       7140-NAV.
           COMPUTE SRCTR = SRCTR + WARP1 + WARP4.
           COMPUTE SKCTR = SKCTR + WARP1 - WARP3.
           PERFORM 7000-NAV-CK THRU 7000-EXIT.
           GO TO 7100-EXIT.
       7150-NAV.
           COMPUTE SRCTR = SRCTR + WARP2 - WARP4.
           COMPUTE SKCTR = SKCTR - WARP3.
           PERFORM 7000-NAV-CK THRU 7000-EXIT.
           GO TO 7100-EXIT.
       7160-NAV.
           COMPUTE SRCTR = SRCTR + WARP1 - WARP3.
           COMPUTE SKCTR = SKCTR - WARP1 - WARP4.
           PERFORM 7000-NAV-CK THRU 7000-EXIT.
           GO TO 7100-EXIT.
       7170-NAV.
           COMPUTE SRCTR = SRCTR - WARP3.
           COMPUTE SKCTR = SKCTR - WARP2 + WARP4.
           PERFORM 7000-NAV-CK THRU 7000-EXIT.
           GO TO 7100-EXIT.
       7180-NAV.
           COMPUTE SRCTR = SRCTR - WARP1 - WARP4.
           COMPUTE SKCTR = SKCTR - WARP1 + WARP3.
           PERFORM 7000-NAV-CK THRU 7000-EXIT.
           GO TO 7100-EXIT.
       7100-EXIT.  EXIT.
      
       7200-PHA.
           IF KLGNS < 1 AND ROMULONS < 1
               DISPLAY "SCIENCE OFFICER SPOCK REPORTS NO ENEMY "
               DISPLAY "VESSELS IN THIS QUADRANT, " NAME-X
               GO TO 7200-EXIT.
           PERFORM 8340-CK-FL THRU 8340-EXIT.
           IF FUEL-COUNT < 9999
               MOVE FUEL-COUNT TO FUEL-CO
               DISPLAY "MAXIMUM OF " FUEL-CO " UNITS AVAILABLE TO PHASER
      -    "S ".
           DISPLAY "HOW MANY UNITS TO PHASER BANKS? ".
           ACCEPT VAR4-AN.
           PERFORM 7210-REP-BL THRU 7210-EXIT.
           PERFORM 7220-COMPUTE-DIST THRU 7220-EXIT.
           COMPUTE VAR2 = 450000 / (SHIELD-CNT + 100).
           PERFORM 8150-TEST-AGN THRU 8150-EXIT.
           IF KLGNS > 1 AND TRAP-VEC
               DISPLAY "*ENTERPRISE DESTROYED* "
               DISPLAY "DIRECT HITS FROM " KLGNS " KLINGONS "
               MOVE 1 TO INDICATE-X.
               PERFORM 8200-CK-DONE THRU 8200-EXIT.
           COMPUTE DM-VAR4 = VAR4 - (DAMAGE-CNT / 15).
           COMPUTE VAR3 = VAR2 / 2.
           IF ROMULONS > 0
               DISPLAY "*ROMULON VESSELS PRESENT IN QUADRANT* "
               DISPLAY "DO YOU WANT TO FIRE ON ROMULONS? "
               ACCEPT LST-REPLY
               IF YES-LST
                   PERFORM 7250-ROMULON-CK THRU 7250-EXIT
                   GO TO 7200-EXIT.
           IF KLGNS > 0
               COMPUTE VAR2 = VAR2 / (DIST-X / 10)
               SUBTRACT VAR4 FROM FUEL-COUNT
               MOVE DM-VAR4 TO VAR4
               ADD KH-TL TO VAR4
               IF VAR4 < 400
                   PERFORM 4700-DISP-HIT THRU 4700-EXIT
                   DISPLAY "*KLINGON DISABLED* "
                   PERFORM 4500-DISP-HIT THRU 4500-EXIT
                   COMPUTE VAR4 = .75 * VAR4
                   ADD VAR4 TO KH-TL
                   ADD VAR2 TO DAMAGE-CNT
                   SUBTRACT VAR3 FROM SHIELD-CNT
               ELSE
                   PERFORM 7201-REPLACE THRU 7201-EXIT
                       VARYING REP-CTR FROM 1 BY 1 UNTIL REP-CTR > RX
                   INSPECT MINI-TABLE REPLACING FIRST "K" BY " "
                   INSPECT MINI-TABLE REPLACING ALL "X" BY "K"
                   COMPUTE VAR4 = VAR4 / (DIST-X ** .224)
                   PERFORM 4700-DISP-HIT THRU 4700-EXIT
                   DISPLAY "*KLINGON DESTROYED* "
                   MOVE 0 TO KH-TL
                   SUBTRACT 1 FROM KLGNS
                   SUBTRACT 1 FROM KLINGONS
                   PERFORM 5400-TRANS-BACK THRU 5400-EXIT
                   IF KLGNS > 0
                       PERFORM 4500-DISP-HIT THRU 4500-EXIT
                       ADD VAR2 TO DAMAGE-CNT
                       COMPUTE VAR2 = .75 * VAR2
                       SUBTRACT VAR2 FROM SHIELD-CNT
                   ELSE
                       MOVE VAR3 TO VAR2
                       PERFORM 4500-DISP-HIT THRU 4500-EXIT
                       ADD VAR3 TO DAMAGE-CNT
                       SUBTRACT VAR3 FROM SHIELD-CNT
           ELSE
               DISPLAY "THERE ARE 0 KLINGONS IN THIS QUADRANT, " NAME-X.
           PERFORM 8120-DAM-COM THRU 8120-EXIT.
           PERFORM 8300-CK-FUEL-DAMAGE THRU 8300-EXIT.
           PERFORM 8200-CK-DONE THRU 8200-EXIT.
       7200-EXIT.  EXIT.
      
       7201-REPLACE.
           INSPECT MINI-TABLE REPLACING FIRST "K" BY "X".
       7201-EXIT.  EXIT.
      
       7202-REPLACE.
           INSPECT MINI-TABLE REPLACING FIRST "R" BY "X".
       7202-EXIT.  EXIT.
      
       7210-REP-BL.
           MOVE 0 TO TAL4.
           INSPECT VAR4-AN TALLYING TAL4 FOR ALL " ".
           IF TAL4 > 0
               INSPECT VAR4-AN REPLACING ALL " " BY ZEROS
               IF VAR4-AN NUMERIC
                   MOVE VAR4-AN TO VAR4
                   COMPUTE VAR4 = VAR4 / (10 ** TAL4)
               ELSE
                   MOVE 300 TO VAR4
           ELSE
               IF VAR4-AN NUMERIC
                   MOVE VAR4-AN TO VAR4
               ELSE
                   MOVE 300 TO VAR4.
           IF VAR4 < 300
               MOVE 300 TO VAR4.
       7210-EXIT.  EXIT.
      
       7220-COMPUTE-DIST.
           MOVE 30 TO DIST-B.
           MOVE 30 TO DIST-X.
           MOVE 30 TO DIST-R.
           MOVE 0 TO CT-K.
           MOVE 0 TO CT-R.
           PERFORM 7225-FIND-E THRU 7225-EXIT
               VARYING RCNTR FROM 1 BY 1 UNTIL RCNTR > 14
               AFTER KCNTR FROM 1 BY 1 UNTIL KCNTR > 14.
           PERFORM 7230-COMPUTE THRU 7230-EXIT
               VARYING RCNTR FROM 1 BY 1 UNTIL RCNTR > 14
               AFTER KCNTR FROM 1 BY 1 UNTIL KCNTR > 14.
           PERFORM 7247-EST-NBR THRU 7247-EXIT.
       7220-EXIT.  EXIT.
      
       7225-FIND-E.
           IF MCOL (RCNTR , KCNTR) = "E"
               MOVE RCNTR TO E1
               MOVE KCNTR TO E2.
       7225-EXIT.  EXIT.
      
       7230-COMPUTE.
           IF MCOL (RCNTR , KCNTR) = "K"
               MOVE RCNTR TO K1
               MOVE KCNTR TO K2
               ADD 1 TO CT-K
               MOVE DIST-X TO STR-A
               PERFORM 7240-DIST-K THRU 7240-EXIT
               IF DIST-X > STR-A
                   MOVE STR-A TO DIST-X.
           IF MCOL (RCNTR , KCNTR) = "R"
               MOVE RCNTR TO R1
               MOVE KCNTR TO R2
               ADD 1 TO CT-R
               MOVE DIST-R TO STR-R
               PERFORM 7243-DIST-R THRU 7243-EXIT
               IF DIST-R > STR-R
                   MOVE STR-R TO DIST-R.
           IF MCOL (RCNTR , KCNTR) = "B"
               MOVE RCNTR TO B1
               MOVE KCNTR TO B2
               MOVE DIST-B TO STR-X
               PERFORM 7245-DIST-B THRU 7245-EXIT
               IF DIST-B > STR-X
                   MOVE STR-X TO DIST-B.
       7230-EXIT.  EXIT.
      
       7240-DIST-K.
           IF K1 > E1
               COMPUTE K1 = K1 - E1
           ELSE
               COMPUTE K1 = E1 - K1.
           IF K2 > E2
               COMPUTE K2 = K2 - E2
           ELSE
               COMPUTE K2 = E2 - K2.
           COMPUTE DIST-X ROUNDED = ((K1 ** 2) + (K2 ** 2)) ** .5.
           MOVE DIST-X TO DKC (CT-K).
       7240-EXIT.  EXIT.
      
       7243-DIST-R.
           IF R1 > E1
               COMPUTE R1 = R1 - E1
           ELSE
               COMPUTE R1 = E1 - R1.
           IF R2 > E2
               COMPUTE R2 = R2 - E2
           ELSE
               COMPUTE R2 = E2 - R2.
           COMPUTE DIST-R ROUNDED = ((R1 ** 2) + (R2 ** 2)) ** .5.
           MOVE DIST-R TO DRC (CT-R).
       7243-EXIT.  EXIT.
      
       7245-DIST-B.
           IF B1 > E1
               COMPUTE B1 = B1 - E1
           ELSE
               COMPUTE B1 = E1 - B1.
           IF B2 > E2
               COMPUTE B2 = B2 - E2
           ELSE
               COMPUTE B2 = E2 - B2.
           COMPUTE DIST-B ROUNDED = ((B1 ** 2) + (B2 ** 2)) ** .5.
       7245-EXIT.  EXIT.
      
       7247-EST-NBR.
           MOVE 30 TO STR-X.
           PERFORM 7248-EST-K THRU 7248-EXIT
      
           VARYING RT FROM 1 BY 1 UNTIL RT > CT-K.
           PERFORM 7249-EST-R THRU 7249-EXIT
               VARYING QT FROM 1 BY 1 UNTIL QT > CT-R.
       7247-EXIT.  EXIT.
      
       7248-EST-K.
           IF DKC (RT) < STR-X
               MOVE DKC (RT) TO STR-X
               COMPUTE RX = RT - 1.
       7248-EXIT.  EXIT.
      
       7249-EST-R.
           IF DRC (QT) < STR-R
               MOVE DRC (QT) TO STR-R
               COMPUTE QX = QT - 1.
       7249-EXIT.  EXIT.
      
       7250-ROMULON-CK.
           IF ROMULONS > 2 AND NO-WAY
               DISPLAY "*ENTERPRISE FIRING ON ROMULONS*"
               DISPLAY "*ROMULONS RETURNING FIRE* "
               DISPLAY "SIMULTANEOUS HITS FROM " ROMULONS " ROMULONS "
               DISPLAY "*ENTERPRISE DESTROYED*"
               MOVE 1 TO INDICATE-X
               PERFORM 8200-CK-DONE THRU 8200-EXIT
           ELSE
               PERFORM 8400-GENERATE THRU 8400-EXIT
               DISPLAY "*ENTERPRISE FIRING ON ROMULONS* "
               SUBTRACT VAR4 FROM FUEL-COUNT
               IF NO-WAY OR VAR4 < 447
                   COMPUTE VAR4 = VAR4 / (DIST-R ** .224)
                   MOVE VAR4 TO VAR5
                   DISPLAY VAR5 " UNIT HIT ON ROMULON "
                   DISPLAY "*ROMULON RETURNING FIRE*"
                   PERFORM 8400-GENERATE THRU 8400-EXIT
                   IF NO-WAY
                       DISPLAY "*ENTERPRISE DESTROYED BY ROMULON
-     -    " TORPEDO* "
                       MOVE 1 TO INDICATE-X
                       PERFORM 8200-CK-DONE THRU 8200-EXIT
                   ELSE
                       COMPUTE VAR2 = 3 * VAR2 / (DIST-R / 10)
                       PERFORM 4500-DISP-HIT THRU 4500-EXIT
                       ADD VAR2 TO DAMAGE-CNT
                       COMPUTE VAR3 = VAR2 / 2
                       IF VAR3 < 9999
                           SUBTRACT VAR3 FROM SHIELD-CNT
                       ELSE
                           MOVE 0 TO SHIELD-CNT
      
               ELSE
                   COMPUTE VAR4 = VAR4 / (DIST-X ** .125)
                   MOVE VAR4 TO VAR5
                   DISPLAY VAR5 " UNIT HIT ON ROMULON "
                   DISPLAY "*ROMULON DESTROYED*"
                   PERFORM 7202-REPLACE THRU 7202-EXIT
                       VARYING REP-CTR FROM 1 BY 1 UNTIL REP-CTR > QX
                   INSPECT MINI-TABLE REPLACING FIRST "R" BY " "
                   INSPECT MINI-TABLE REPLACING ALL "X" BY "R"
                   SUBTRACT 1 FROM ROMULONS
                   PERFORM 5400-TRANS-BACK THRU 5400-EXIT.
           PERFORM 8100-DMG-COM THRU 8100-EXIT.
           PERFORM 8300-CK-FUEL-DAMAGE THRU 8300-EXIT.
           PERFORM 8200-CK-DONE THRU 8200-EXIT.
       7250-EXIT.  EXIT.
      
       7300-TOR.
           IF KLGNS < 1 AND ROMULONS < 1
               DISPLAY "THERE ARE 0 ENEMY VESSELS IN THIS QUADRANT, "
-                  NAME-X
               GO TO 7300-EXIT.
           PERFORM 8400-GENERATE THRU 8400-EXIT.
           COMPUTE VAR2 = 250000 / (SHIELD-CNT + 100).
           PERFORM 8150-TEST-AGN THRU 8150-EXIT.
           PERFORM 7220-COMPUTE-DIST THRU 7220-EXIT.
           IF KLGNS > 2
               COMPUTE VAR2 = VAR2 * (KLGNS + 1) / 2.
           COMPUTE VAR3 = .75 * VAR2.
           IF ROMULONS > 0 AND TORPS > 0
               DISPLAY "*ROMULONS PRESENT IN QUADRANT*"
               DISPLAY "DO YOU WANT TO FIRE ON THEM? "
               ACCEPT LST-REPLY
               IF YES-LST
                   PERFORM 7350-ROMULON-CK THRU 7350-EXIT
                   GO TO 7300-EXIT.
           IF TORPS > 0
               IF KLGNS > 0
                   IF SHIELD-CNT < 475 AND NO-WAY
                       DISPLAY "*ENTERPRISE DESTROYED*"
                       DISPLAY "LOW SHIELDS AT TIME OF ENEMY ATTACK "
                       MOVE 1 TO INDICATE-X
                       PERFORM 8200-CK-DONE THRU 8200-EXIT
                   ELSE
                       IF NO-WAY AND DIST-X > 4
                           COMPUTE VAR2 = VAR2 / (DIST-X / 10)
                           DISPLAY "TORPEDO MISSED "
                           PERFORM 4500-DISP-HIT THRU 4500-EXIT
                           ADD VAR2 TO DAMAGE-CNT
                           SUBTRACT 1 FROM TORPS
                           SUBTRACT VAR3 FROM SHIELD-CNT
                           PERFORM 8120-DAM-COM THRU 8120-EXIT
                       ELSE
                           DISPLAY "*KLINGON DESTROYED*"
                           SUBTRACT VAR3 FROM DAMAGE-CNT
                           PERFORM 7201-REPLACE THRU 7201-EXIT
                               VARYING REP-CTR FROM 1 BY 1 UNTIL REP-CTR
-                                     > RX
                           INSPECT MINI-TABLE REPLACING FIRST "K" BY " "
                           INSPECT MINI-TABLE REPLACING ALL "X" BY "K"
                           SUBTRACT 1 FROM TORPS
                           SUBTRACT 1 FROM KLGNS
                           SUBTRACT 1 FROM KLINGONS
                           PERFORM 5400-TRANS-BACK THRU 5400-EXIT
                           IF KLGNS > 0
                               PERFORM 4500-DISP-HIT THRU 4500-EXIT
                               ADD VAR2 TO DAMAGE-CNT
                               SUBTRACT VAR3 FROM SHIELD-CNT
                               PERFORM 8120-DAM-COM THRU 8120-EXIT
                           ELSE
                               PERFORM 8120-DAM-COM THRU 8120-EXIT
               ELSE
                   DISPLAY "THERE ARE 0 KLINGON VESSELS IN THIS
-     -    " QUADRANT, " NAME-X
           ELSE
               DISPLAY "0 TORPEDOES REMAINING, " NAME-X.
           PERFORM 8300-CK-FUEL-DAMAGE THRU 8300-EXIT.
           PERFORM 8200-CK-DONE THRU 8200-EXIT.
       7300-EXIT.  EXIT.
      
       7350-ROMULON-CK.
           IF ROMULONS > 1 AND NO-WAY
               DISPLAY "*ENTERPRISE FIRING ON ROMULONS*"
               DISPLAY "*ROMULONS RETURNING FIRE*"
               DISPLAY "SIMULTANEOUS HITS FROM " ROMULONS " ROMULONS "
               DISPLAY "*ENTERPRISE DESTROYED*"
               MOVE 1 TO INDICATE-X
               PERFORM 8200-CK-DONE THRU 8200-EXIT
           ELSE
               DISPLAY "*ENTERPRISE FIRING ON ROMULONS*"
               SUBTRACT 1 FROM TORPS
               IF NO-WAY AND DIST-R > 4
                   DISPLAY "TORPEDO MISSED "
                   DISPLAY "*ROMULONS RETURNING FIRE*"
                   PERFORM 8400-GENERATE THRU 8400-EXIT
                   IF NO-WAY AND SHIELD-CNT < 4000
                       DISPLAY "*ENTERPRISE DESTROYED BY ROMULON
-     -    " TORPEDO*"
                       MOVE 1 TO INDICATE-X
                       PERFORM 8200-CK-DONE THRU 8200-EXIT
                   ELSE
                       COMPUTE VAR2 = 3 * VAR2 / (DIST-R / 10)
                       PERFORM 4500-DISP-HIT THRU 4500-EXIT
                       ADD VAR2 TO DAMAGE-CNT
                       COMPUTE VAR3 = VAR2 / 2
                       SUBTRACT VAR3 FROM SHIELD-CNT
               ELSE
                   DISPLAY "*ROMULON DESTROYED*"
                   PERFORM 7202-REPLACE THRU 7202-EXIT
                       VARYING REP-CTR FROM 1 BY 1 UNTIL REP-CTR > QX
                   INSPECT MINI-TABLE REPLACING FIRST "R" BY " "
                   INSPECT MINI-TABLE REPLACING ALL "X" BY "R"
                   SUBTRACT 1 FROM ROMULONS
                   PERFORM 5400-TRANS-BACK THRU 5400-EXIT.
           PERFORM 8300-CK-FUEL-DAMAGE THRU 8300-EXIT.
           PERFORM 8200-CK-DONE THRU 8200-EXIT.
       7350-EXIT.  EXIT.
      
       7400-STA.
           COMPUTE VAR3 = (DAMAGE-CNT / 60).
           MOVE VAR3 TO VAR6.
           MOVE FUEL-COUNT TO FUEL-CO.
           MOVE SHIELD-CNT TO SHIELD-CO.
           DISPLAY "      ".
           DISPLAY "FUEL UNITS   DAMAGE ".
           DISPLAY "REMAINING    LEVEL  ".
           DISPLAY "      ".
           DISPLAY "   " FUEL-CO "  " VAR6 "%".
           DISPLAY "      ".
           DISPLAY "===================".
           DISPLAY "      ".
           DISPLAY " PHOTON      SHIELD ".
           DISPLAY "TORPEDOES    LEVEL ".
           DISPLAY "      ".
           DISPLAY "    " TORPS "         " SHIELD-CO.
           DISPLAY "      ".
           PERFORM 8100-DMG-COM THRU 8100-EXIT.
           PERFORM 8300-CK-FUEL-DAMAGE THRU 8300-EXIT.
           PERFORM 8200-CK-DONE THRU 8200-EXIT.
       7400-EXIT.  EXIT.
      
       7500-DEF.
           ADD SHIELD-CNT TO FUEL-COUNT.
           DISPLAY "HOW MANY UNITS TO SHIELDS (0 - 9999)? ".
           PERFORM 7520-ASC THRU 7520-EXIT.
           IF SHIELD-CNT < FUEL-COUNT
               SUBTRACT SHIELD-CNT FROM FUEL-COUNT
           ELSE
               MOVE FUEL-COUNT TO FUEL-CO
               DISPLAY "MAXIMUM AMOUNT TO SHIELDS: " FUEL-CO
               DISPLAY "HOW MANY UNITS TO SHIELDS? "
               PERFORM 7520-ASC THRU 7520-EXIT
               SUBTRACT SHIELD-CNT FROM FUEL-COUNT.
           MOVE SHIELD-CNT TO SHIELD-CO.
           DISPLAY "SHIELDS AT " SHIELD-CO " PER YOUR COMMAND ".
       7500-EXIT.  EXIT.
      
       7520-ASC.
           ACCEPT SHIELD-CNT-AN.
           MOVE 0 TO SBL.
           INSPECT SHIELD-CNT-AN TALLYING SBL FOR ALL " ".
           INSPECT SHIELD-CNT-AN REPLACING ALL " " BY ZEROS.
           IF SHIELD-CNT-AN NOT NUMERIC
               MOVE 0 TO SBL
               DISPLAY "INVALID - ENTRY MUST BE NUMERIC "
               DISPLAY "HOW MANY UNITS TO SHIELDS? "
               ACCEPT SHIELD-CNT-AN
               INSPECT SHIELD-CNT-AN TALLYING SBL FOR ALL " "
               INSPECT SHIELD-CNT-AN REPLACING ALL " " BY ZEROS
               IF SHIELD-CNT-AN NOT NUMERIC
                   MOVE 0 TO SHIELD-CNT
                   GO TO 7520-EXIT.
           MOVE SHIELD-CNT-AN TO SHIELD-CNT.
           IF SBL > 0
               COMPUTE SHIELD-CNT = SHIELD-CNT / (10 ** SBL).
       7520-EXIT.  EXIT.
      
       7600-DOC.
           PERFORM 8400-GENERATE THRU 8400-EXIT.
           IF BASE-CNT > 0
               PERFORM 7220-COMPUTE-DIST THRU 7220-EXIT
               IF DIST-B < 7
                   IF NO-WAY
                       DISPLAY "*UNSUCCESSFUL DOCKING ATTEMPT* "
                       DISPLAY "STAR BASE REPORTS ALL BAYS IN USE "
                       PERFORM 8100-DMG-COM THRU 8100-EXIT
                   ELSE
                       MOVE 5 TO TORPS
                       MOVE 25000 TO FUEL-COUNT
                       MOVE 0 TO DAMAGE-CNT
                       MOVE 0 TO SHIELD-CNT
                       DISPLAY "SHIELDS DROPPED TO DOCK AT STAR BASE "
                       DISPLAY "*DOCK SUCCESSFUL* "
               ELSE
                   DISPLAY "THE NEAREST STAR BASE IS " DIST-B " PARSECS"
                   DISPLAY "YOU MUST MANEUVER TO WITHIN 6 PARSECS TO DOC
      -     "K "
           ELSE
               DISPLAY "THERE ARE 0 STAR BASES IN THIS QUADRANT, "
                  NAME-X.
           PERFORM 8300-CK-FUEL-DAMAGE THRU 8300-EXIT.
           PERFORM 8200-CK-DONE THRU 8200-EXIT.
       7600-EXIT.  EXIT.
      
       7650-TRANS-STAR.
           IF Q1 = 1
               MOVE 2 TO Q9
           ELSE
               IF Q1 = 9
                   MOVE 8 TO Q9
               ELSE
                   MOVE Q1 TO Q9.
           IF Q2 = 1
               MOVE 2 TO R9
           ELSE
               IF Q2 = 9
                   MOVE 8 TO R9
               ELSE
                   MOVE Q2 TO R9.
           COMPUTE W = (Q9 - 2) * 14.
           COMPUTE Z = (R9 - 2) * 14.
           PERFORM 7670-STRANS THRU 7670-EXIT
               VARYING RCTR FROM 1 BY 1 UNTIL RCTR > 42
               AFTER KCTR FROM 1 BY 1 UNTIL KCTR > 42.
       7650-EXIT.  EXIT.
      
       7670-STRANS.
           COMPUTE A = Z + RCTR.
           COMPUTE B = W + KCTR.
           MOVE MACOL (A , B) TO KOLUMN (RCTR , KCTR).
       7670-EXIT.  EXIT.
      
       7700-LRS.
           PERFORM 7650-TRANS-STAR THRU 7650-EXIT.
           MOVE ZEROS TO SCAN-KEEP.
           MOVE 0 TO SCAN-CTR.
           IF Q1 = 1
               MOVE 1 TO QT1
               MOVE 3 TO QT3
           ELSE
               IF Q1 = 9
                   MOVE 7 TO QT1
                   MOVE 9 TO QT3
               ELSE
                   COMPUTE QT1 = Q1 - 1
                   COMPUTE QT3 = Q1 + 1.
           IF Q2 = 1
               MOVE 1 TO QT2
               MOVE 3 TO QT4
           ELSE
               IF Q2 = 9
                   MOVE 7 TO QT2
                   MOVE 9 TO QT4
               ELSE
                   COMPUTE QT2 = Q2 - 1
                   COMPUTE QT4 = Q2 + 1.
           PERFORM 7730-TRADE-TBL THRU 7730-EXIT
               VARYING TR2 FROM 0 BY 1 UNTIL TR2 > 2
               AFTER TR1 FROM 0 BY 1 UNTIL TR1 > 2.
           DISPLAY "      ".
           DISPLAY "=========================".
           DISPLAY "=       =       =       =".
           DISPLAY "= " CV (1) "," CV (2) " = " CV (3) "," CV (4) " = "
-                CV (5) "," CV (6) " = ".
           DISPLAY "=       =       =       =".
           DISPLAY "=========================".
           DISPLAY "=       =       =       =".
           DISPLAY "= " CV (7) "," CV (8) " = " CV (9) "," CV (10) " = "
                 CV (11) "," CV (12) " =".
           DISPLAY "=       =       =       =".
           DISPLAY "=========================".
           DISPLAY "=       =       =       =".
           DISPLAY "= " CV (13) "," CV (14) " = " CV (15) "," CV (16)
-                 " = " CV (17) "," CV (18) " =".
           DISPLAY "=       =       =       =".
           DISPLAY "=========================".
           DISPLAY "KEY: ".
           DISPLAY "QUADRANTS " QT1 "," QT2 " THRU " QT3 "," QT4.
           DISPLAY "FORMAT - KLINGONS,STAR BASES ".
           IF Q1 = 1 OR Q1 = 9 OR Q2 = 1 OR Q2 = 9
               DISPLAY "*ENTERPRISE ON GALACTIC BOUNDARY*".
           DISPLAY "ENTERPRISE IN QUADRANT " Q1 "," Q2.
           DISPLAY "      ".
           PERFORM 8100-DMG-COM THRU 8100-EXIT.
           PERFORM 8300-CK-FUEL-DAMAGE THRU 8300-EXIT.
           PERFORM 8200-CK-DONE THRU 8200-EXIT.
       7700-EXIT.  EXIT.
      
       7730-TRADE-TBL.
           COMPUTE QT = (TR1 * 14).
           COMPUTE RT = (TR2 * 14).
           PERFORM 7750-TRADE-ACT THRU 7750-EXIT
               VARYING KTCTR FROM 1 BY 1 UNTIL KTCTR > 14
               AFTER RTCTR FROM 1 BY 1 UNTIL RTCTR > 14.
           PERFORM 7770-INSPECT-SCAN THRU 7770-EXIT.
       7730-EXIT.  EXIT.
      
       7750-TRADE-ACT.
           COMPUTE QX = QT + KTCTR.
           COMPUTE RX = RT + RTCTR.
           MOVE KOLUMN (RX , QX) TO SCAN-COL (RTCTR , KTCTR).
       7750-EXIT.  EXIT.
      
       7770-INSPECT-SCAN.
           ADD 1 TO SCAN-CTR.
           INSPECT SCAN-TABLE TALLYING CV (SCAN-CTR) FOR ALL "K".
           ADD 1 TO SCAN-CTR.
           INSPECT SCAN-TABLE TALLYING CV (SCAN-CTR) FOR ALL "B".
       7770-EXIT.  EXIT.
      
       7800-INT.
           IF KLINGONS > 0
               MOVE 1 TO CX
               MOVE 1 TO DX
               PERFORM 7850-SEARCH THRU 7850-EXIT
                   UNTIL MACOL (CX , DX) = "K" OR DX > 126
               COMPUTE CX-1 = (DX - 1) / 14 + 1
               COMPUTE DX-1 = (CX - 1) / 14 + 1
               DISPLAY " "
               DISPLAY "LATEST INTELLIGENCE GATHERING REPORTS "
               DISPLAY "INDICATE 1 OR MORE KLINGON VESSELS "
               DISPLAY "IN THE VICINITY OF QUADRANT " CX-1 "," DX-1
               DISPLAY " "
               DISPLAY "ENTERPRISE IN QUADRANT " Q1 "," Q2
               DISPLAY " ".
       7800-EXIT.  EXIT.
      
       7850-SEARCH.
           ADD 1 TO CX.
           IF CX > 126
               ADD 1 TO DX
               MOVE 1 TO CX.
       7850-EXIT.  EXIT.
      
       8000-BOMB.
           IF MACOL (MRCTR , MKCTR) = "K"
               DISPLAY "*ENTERPRISE DESTROYED IN COLLISION WITH KLINGON*
      -        ""
           ELSE
               IF MACOL (MRCTR , MKCTR) = "R"
                   DISPLAY "*ENTERPRISE DESTROYED IN COLLISION WITH ROMU
-     -            "LON*"
               ELSE
                   DISPLAY "*ENTERPRISE DESTROYED IN COLLISION WITH STAR
-     -            " BASE*".
           MOVE 1 TO INDICATE-X.
           PERFORM 8200-CK-DONE THRU 8200-EXIT.
       8000-EXIT.  EXIT.
      
      ****************************************************
      * 8100-DMG-COM AND 8120-DAM-COM CALCULATE AND DIS- *
      * PLAY HITS ON THE ENTERPRISE FROM ENEMY VESSELS   *
      * IN QUADRANT.  --  CALLED FOLLOWING POSSIBLE CON- *
      * TACT WITH ENEMY VESSELS.                         *
      ****************************************************
       
       8100-DMG-COM.
           IF KLGNS > 0
               COMPUTE VAR2 = (K-OR - KLINGONS) * KLGNS * FUEL-COUNT /
-                 (SHIELD-CNT + 21)
               PERFORM 8150-TEST-AGN THRU 8150-EXIT
               COMPUTE VAR3 = .75 * VAR2
               DISPLAY "*ENTERPRISE ENCOUNTERING KLINGON FIRE*"
               PERFORM 4500-DISP-HIT THRU 4500-EXIT
               ADD VAR2 TO DAMAGE-CNT
               SUBTRACT VAR3 FROM SHIELD-CNT.
       8100-EXIT.  EXIT.
      
       8120-DAM-COM.
           IF ROMULONS > 0
               COMPUTE VAR2 = ROMULONS * FUEL-COUNT / (SHIELD-CNT + 7)
               PERFORM 8160-TEST-AGN THRU 8160-EXIT
               COMPUTE VAR3 = .75 * VAR2
               DISPLAY "*ENTERPRISE ENCOUNTERING ROMULON FIRE*"
               PERFORM 4500-DISP-HIT THRU 4500-EXIT
               ADD VAR2 TO DAMAGE-CNT
               SUBTRACT VAR3 FROM SHIELD-CNT.
       8120-EXIT.  EXIT.
      
       8150-TEST-AGN.
           IF VAR2 < 325 AND KLGNS > 0
               ADD 177 TO VAR2
               COMPUTE VAR2 = (KLGNS * VAR2 / 2.7) + (VAR2 * DAMAGE-CNT
-               / 980).
       8150-EXIT.  EXIT.
      
       8160-TEST-AGN.
           IF VAR2 < 525 AND ROMULONS > 0
               ADD 254 TO VAR2
               COMPUTE VAR2 = (ROMULONS * VAR2 / 4.7) + (VAR2 *
-               DAMAGE-CNT / 365).
       8160-EXIT.  EXIT.
      
       8200-CK-DONE.
           IF BYE-BYE
               GO TO 2000-EXIT.
       8200-EXIT.  EXIT.
      
      *************************************************
      * 8300-CK-FUEL-DAMAGE CHECKS AND NOTIFIES THE   *
      * USER OF LOW SHIELDS, LOW FUEL RESERVES, OR    *
      * HIGH DAMAGE TO THE SHIP, IN ORDER THAT HE MAY *
      * ATTEMPT TO DOCK BEFORE THE ENTERPRISE IS      *
      * DISABLED.   --  CALLED FOLLOWING POSSIBLE     *
      * CONTACT WITH ENEMY VESSELS.                   *
      *************************************************
       
       8300-CK-FUEL-DAMAGE.
           IF FUEL-COUNT < 4500 AND FUEL-COUNT > 0
               DISPLAY "LT. SCOTT REPORTS FUEL IS RUNNING LOW, " NAME-X
           ELSE
               IF FUEL-COUNT NOT > 0
                   DISPLAY "FUEL RESERVES DEPLETED "
                   DISPLAY "THE ENTERPRISE IS DRIFTING IN SPACE "
                   PERFORM 8350-CK-SHIFT THRU 8350-EXIT.
           IF DAMAGE-CNT > 6000
               DISPLAY "ENTERPRISE STRANDED BECAUSE OF HEAVY DAMAGE "
               MOVE 1 TO INDICATE-X
               PERFORM 8200-CK-DONE THRU 8200-EXIT.
           IF DAMAGE-CNT > 4500
               DISPLAY "DAMAGE CONTROL REPORTS HEAVY DAMAGE TO ENTERPRIS
-     -    "E, " NAME-X.
           IF SHIELD-CNT < 800 AND (KLGNS > 0 OR ROMULONS > 0)
               DISPLAY "LT. SULU REPORTS SHIELDS DANGEROUSLY LOW, " 
               NAME-X.
       8300-EXIT.  EXIT.
      
       8340-CK-FL.
           IF FUEL-COUNT NOT > 180
               DISPLAY "*INSUFFICIENT FUEL TO CONTINUE*"
               PERFORM 8350-CK-SHIFT THRU 8350-EXIT
               GO TO 2000-EXIT.
       8340-EXIT.  EXIT.
      
       8350-CK-SHIFT.
           IF SHIELD-CNT > 200
               DISPLAY "LT. SULU ADVISES YOU LOWER SHIELDS "
               DISPLAY "TO INCREASE FUEL SUPPLY, " NAME-X
           ELSE
               MOVE 1 TO INDICATE-X
               PERFORM 8200-CK-DONE THRU 8200-EXIT.
       8350-EXIT.  EXIT.
      
       8400-GENERATE.
           IF NX > 24
               MOVE 0 TO NX.
           ADD 1 TO NX.
      *    NOTE. USED TO BE IF CHAR (NX) NUMERIC
           IF NX EQUAL NX
               MOVE 1 TO GENRTE-RESULT
           ELSE
               MOVE 0 TO INDICATE-Y
               MOVE 0 TO GENRTE-RESULT
               IF CHAR (NX) = "F"
                   MOVE 1 TO INDICATE-Y.
       8400-EXIT.  EXIT.
      
       8500-FINISH-GAME.
           DISPLAY "      ".
           IF BYE-BYE
               IF S-DATE > DS-DATE
                   MOVE KLINGONS TO VAE1
                   MOVE WS-DATE TO DS-DATE
                   DISPLAY "IT IS NOW STAR DATE " S-DATE
                   DISPLAY "STAR DATE " DS-DATE " STAR FLEET HQ"
                   DISPLAY "WAS DESTROYED BY " VAE1 " KLINGON VESSELS"
                   DISPLAY NAME-X " COURT MARTIALED"
               ELSE
                   DISPLAY NAME-X " COURT MARTIALED"
           ELSE
               DISPLAY "CONGRATULATIONS ON A JOB WELL DONE. "
               DISPLAY "THE FEDERATION IS PROUD OF YOU, " NAME-X.
           DISPLAY "      ".
       8500-EXIT.  EXIT.
      
       9000-END-OF-JOB-SECTION SECTION.
       9000-END-OF-JOB.
           DISPLAY "      ".
       9000-EXIT.  EXIT.
      