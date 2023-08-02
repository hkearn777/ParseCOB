       CBL LIB,LANG(EN),LIST,MAP,NODynam
       CBL NumProc(PFD),Trunc(Opt),Optimize(Full),FastSrt
       CBL Char(EBCDIC),PgmName(COMPAT),NoSSR
       IDENTIFICATION DIVISION.
v00879 Replace ==:MaxOBuff:== by ==27990==.
       PROGRAM-ID.            ENGINE.
       AUTHOR.                 HOWARD KEARNEY.
      *Change-History.  (be sure to change VERSION)
      * Date------ Init Ver---- Description of Change-------------------
      * 2016/08/01 HK   v1.0.0  Moved version v0.8.95 to v1
      *                         - add option to DATE() on Field
      *                         - add 'Ratify' routine
      *                         - rename DD name 'CNTRL' to 'DLL'
      *                         - rename LoadCntl routine to LoadDLL
      *                         - rename LoadDLL routine to SumDLL
      *                         - removed old commented code
      *                         - removed old comments
      * ...
      * 2013/09/16 HK   v0.1    Base line
      *---------------------------------------------------------
      * Compatibility. This program uses COMP-5 for many fields.
      *   This type is not compatible with all COBOL compilers such as
      *   the COBOL ILE, but it is the best performer.
      *---------------------------------------------------------
      * Remarks. This program reads a data file and produces
      *  a 'Formatted Data Display'(FDD), 'Data Analysis Report'(DAR),
      *  or a converted target file(MIG).
      * INPUTS:
      *  DLL     - Holds the definitions of the file, record, fields
      *            and directives on what to produce(i.e. DAR, MIG).
      *            This file is also known as the DLL file.
      *            Opened in another program: LoadDLL
      *            Opened in another program: SumDLL
      *  INFILE  - Input (or source) data (in variable or fixed)
      *  OPTIONS - Processing options
      *            Opened in another program: Loadopt
      *  PARM=   - JCL parameters.
      *            'TRACE' turns tracing on, output goes to SYSOUT.
      * OUTPUTS:
      *  LOG     - Audit report of this program
      *  DISCARD - The Input(s) record that had some error
      *  FDD     - Formatted Data Display Report (FDD)
      *  DAR     - Data Analysis Report (DAR)
      *            Created in another program: DAR
      *  DARHTM  - Data Analysis Report (DAR) in HTML format
      *            Created in another program: DAR
      *  REF     - Data Analysis References file
      *            Created in another program: DAR
      *  KEYLIST - Data Analysis list of keys
      *            Created in another program: DAR
      *  MIG     - Converted Target/Migrated File (MIG)
      *  SYSOUT  - Program status information such as filenames
      *            used, program counters, and error conditions.
      *            if TRACE is on then 'TRACE:' messages are
      *            place here as well.
      *  SYSPUNCH- Program Progress is shown here.
      *  TRACEOUT- Detail reporting of a given record.
      *  ALT1    - Alternate 'Business names' for fields file 1
      *  ALT2    - Alternate 'Business names' for fields file 2
      *  PRET    - Unload of the PRET table.
      *            Created in another program: LoadDLL
      *  MAPP    - Show "mapped" fields by Position
      *            Created in another program: MAP
      *  MAPN    - Show "mapped" fields by Name
      *            Created in another program: MAP
      *  SORTWK  - Sort of the MAP fields MAPP and MAPR
      *            Used in another program: MAP
      *  SAMPLE  - Sample test records based only on DLL
      *            Created in another program: SAMPLE
      *  EEOR    - Kampo specific support of Error over ride
      *---------------------------------------------------------
       INSTALLATION.           IBM.
      *MIGRATION ENGINE, (C) IBM CORP. 2006-2013; ALL RIGHTS RESERVED.
       DATE-WRITTEN.           AUGUST 2013.
       DATE-COMPILED.          AUGUST 2013.
       ENVIRONMENT DIVISION.
      *---------------------------------------------------------
      * For PC Environment - Uncomment all 'PCPCPC' lines.
      *                      Comment all 'MFMFMF' lines.
      * For MAINFRAME      - Uncomment all 'MFMFMF' lines.
      *                      Comment all 'PCPCPC' lines.
      *---------------------------------------------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FIX
PCPCPC*        SEQUENTIAL
               ASSIGN to INFILE STATUS IO-STATUS.
           SELECT IN-VAR
PCPCPC*        SEQUENTIAL
               ASSIGN to INFILE STATUS IO-STATUS.
           SELECT LOG-FILE
PCPCPC*        SEQUENTIAL
               ASSIGN to LOG.
           SELECT DISCARD-FIX
PCPCPC*        SEQUENTIAL
               ASSIGN to DISCARD STATUS IO-STATUS.
           SELECT DISCARD-VAR
PCPCPC*        SEQUENTIAL
               ASSIGN to DISCARD STATUS IO-STATUS.
           SELECT MIG-File
PCPCPC*        SEQUENTIAL
               ASSIGN to MIG STATUS mgIO-STATUS.
v00886     SELECT EEO-File
PCPCPC*        SEQUENTIAL
v00886         ASSIGN to EEOR STATUS EEOR-IO-STATUS.
           SELECT FDD-FILE
PCPCPC*        SEQUENTIAL
               ASSIGN to FDD STATUS fddIO-STATUS.
           SELECT Trace-File
PCPCPC*        SEQUENTIAL
               ASSIGN to TRACEOUT STATUS IO-STATUS.

v00890     SELECT Alt1-File
PCPCPC*        SEQUENTIAL
               ASSIGN to Alt1  STATUS IO-STATUS.
v00890     SELECT Alt2-File
PCPCPC*        SEQUENTIAL
               ASSIGN to Alt2  STATUS IO-STATUS.

MFMFMF I-O-CONTROL.
MFMFMF     Apply write-only on Mig-File
MFMFMF                         EEO-File
MFMFMF                         Discard-Var
MFMFMF                         Trace-File
MFMFMF     .

       DATA DIVISION.
       FILE SECTION.
       FD  LOG-FILE IS GLOBAL
           RECORDING MODE IS F
MFMFMF     BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 170 CHARACTERS
MFMFMF     LABEL RECORDS ARE STANDARD
           .
v00894 01  LOG-RECORD PIC X(170)
                      usage is display native.

       FD  IN-FIX
           RECORDING MODE IS F
MFMFMF     LABEL RECORDS ARE STANDARD
MFMFMF     BLOCK CONTAINS 0 RECORDS
MFMFMF     RECORD CONTAINS 0 CHARACTERS
           .
       01  IN-FIX-REC                   PIC X(32756).


       FD  IN-VAR
           RECORDING MODE IS V
MFMFMF     LABEL RECORDS ARE STANDARD
MFMFMF     BLOCK CONTAINS 0 RECORDS
           RECORD varying 1 to 32756   DEPENDING ON IN-LEN
           .
       01  IN-VAR-REC.
           03 FILLER OCCURS 1 to 32756 DEPENDING ON IN-LEN PIC X.


       FD  DISCARD-FIX
           RECORDING MODE IS F
MFMFMF     BLOCK CONTAINS 0 RECORDS
           .
       01  DISCARD-FIX-REC                   PIC X(32756).


       FD  DISCARD-VAR
           RECORDING MODE IS V
MFMFMF     BLOCK CONTAINS 0 RECORDS
           RECORD varying 1 to 32752   DEPENDING ON IN-LEN
           .
       01  DISCARD-VAR-REC.
           03 FILLER OCCURS 1 to 32752 DEPENDING ON IN-LEN PIC X.

       FD  MIG-File
           RECORDING MODE IS V
MFMFMF     BLOCK CONTAINS 0 RECORDS
      *    RECORD varying 1 to 32752   DEPENDING ON Out-Len
           RECORD varying 1 to :MaxOBuff:  DEPENDING ON Out-Len
           .
       01  MIG-Record.
      *    03 FILLER OCCURS 1 to 32752 DEPENDING ON Out-Len PIC X.
           03 FILLER OCCURS 1 to :MaxOBuff:
                     DEPENDING ON Out-Len PIC X.

v00886 FD  EEO-File
           RECORDING MODE IS V
MFMFMF     BLOCK CONTAINS 0 RECORDS
           RECORD varying 1 to :MaxOBuff:  DEPENDING ON Out-Len
           .
       01  EEO-Record.
           03 FILLER OCCURS 1 to :MaxOBuff:
                     DEPENDING ON Out-Len PIC X.

       FD  FDD-FILE
           RECORDING MODE IS F
MFMFMF     BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 156 CHARACTERS
MFMFMF     LABEL RECORDS ARE STANDARD
           .
       01  FDD-RECORD               PIC X(156)
v00894                              usage is display native.

       FD  Trace-File
           RECORDING MODE IS V
MFMFMF     BLOCK CONTAINS 0 RECORDS
           RECORD varying 1 to 32752   DEPENDING ON Out-Len-T
           .
       01  Trace-Record
v00894     usage is display native.
           03 FILLER OCCURS 1 to 32752 DEPENDING ON Out-Len-T PIC X.

v00890 FD  Alt1-File
           RECORDING MODE IS F
MFMFMF     BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS
MFMFMF     LABEL RECORDS ARE STANDARD
           .
       01  Alt1-File-Record
v00894     usage is display native.
           03 Alt1-File-Key.
             05  Alt1-File-ID          pic x(8).
             05  filler                pic x.
             05  Alt1-File-FieldName   pic x(30).
           03  filler                  pic x.
           03  Alt1-File-AlternateName pic x(60).

v00890 FD  Alt2-File
           RECORDING MODE IS F
MFMFMF     BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS
MFMFMF     LABEL RECORDS ARE STANDARD
           .
       01  Alt2-File-Record
v00894     usage is display native.
           03 Alt2-File-Key.
             05  Alt2-File-ID          pic x(8).
             05  filler                pic x.
             05  Alt2-File-FieldName   pic x(30).
           03  filler                  pic x.
           03  Alt2-File-AlternateName pic x(60).

       WORKING-STORAGE SECTION.
       01  PROGRAM-COPYRIGHT.
           03 FILLER PIC X(42) VALUE
                     'MIGRATION ENGINE, (C)IBM CORP. 2009-2014; '.
           03 FILLER PIC X(20) VALUE 'ALL RIGHTS RESERVED.'.
v00893 77  Version              pic x(8)      value '1.0.0 '.
       77  ws-here pic 9(2) value 0.

      * Working Storage fields for the Allocate-Table-Space
       01  HEAPID                  PIC S9(9) BINARY.
       01  HPSIZE                  PIC S9(9) BINARY.
       01  INCR                    PIC S9(9) BINARY.
       01  OPTS                    PIC S9(9) BINARY.
       01  ADDRSS                  USAGE IS POINTER.
       01  NBYTES                  PIC S9(9) BINARY.
       01  NEWSIZE                 PIC S9(9) BINARY.
       01  FC.
           02  Condition-Token-Value.
      *    From COPYbook  CEEIGZCT.
             88 CEE000 value low-values.
               03  Case-1-Condition-ID.
                   04  Severity    PIC S9(4) BINARY.
                   04  Msg-No      PIC S9(4) BINARY.
               03  Case-2-Condition-ID
                         REDEFINES Case-1-Condition-ID.
                   04  Class-Code  PIC S9(4) BINARY.
                   04  Cause-Code  PIC S9(4) BINARY.
               03  Case-Sev-Ctl    PIC X.
               03  Facility-ID     PIC XXX.
           02  I-S-Info            PIC S9(9) BINARY.
      * end of Allocate-Table-Space fields


       01  MISC-AREA.
           03 IO-STATUS          PIC X(2)      VALUE '00'.
           03 File-to-Process    PIC 9(4) comp-5 VALUE 0.
           03 Record-to-Process  PIC 9(4) comp-5 VALUE 0.
           03 Pret-to-Process    PIC 9(6) comp-5 VALUE 0.
           03 Pret-to-Stop       PIC 9(6) comp-5 VALUE 0.
           03 Last-Required-Pret pic 9(6) comp-5 value 0.
           03 Process-Record-sw  pic 9(4) comp-5 value 0.
               88 Process-Record-continue     value 0.
               88 Process-Record-stop         value 1.
           03 Stop-Reason        PIC 9(4) comp-5 VALUE 0.
           03 Stop-Reason-Fields PIC 9(4) comp-5 VALUE 0.
           03 Stop-Generating-sw pic 9(4) comp-5 value 0.
               88 Keep-Generating             value 0.
               88 Stop-Generating             value 1.
           03 Forcing-ADD-Field-sw pic 9(4) comp-5 value 0.
               88 Not-Forcing-Add-Field       value 0.
               88 Forcing-Add-Field           value 1.
           03 ws-average-in-len   pic 9(9)  comp-5  value 0.
           03 in-len-longest      pic 9(9)  comp-5 value 0.
           03 in-len-longest-rec  pic 9(9)  comp-5 value 0.
           03 in-len-shortest     pic 9(9)  comp-5 value 999999999.
           03 in-len-shortest-rec pic 9(9)  comp-5 value 0.
v00869     03 wsDiscardNonNumeric PIC 9(9) comp-5 VALUE 0.
           03 Mandatory-Fields   PIC 9(9) comp-5 VALUE 0.
           03 Remaining          PIC 9(9) comp-5 VALUE 0.
           03 WS-RT-CNT          PIC 9(5) COMP VALUE 0.
           03 WS-LEN             PIC S9(9) COMP VALUE 0.
           03 WS-POS             PIC 9(5) COMP VALUE 0.
           03 WS-POS7            pic 9(7)      value 0.
           03 WS-BIT             PIC 9(1)      VALUE 0.
           03 WS-Nib             PIC 9(1)      VALUE 0.
           03 WS-Type            PIC 9(4) COMP VALUE 0.
           03 End-Bit            PIC 9(4) COMP VALUE 0.
v00862     03 WS-END             PIC 9(6) COMP VALUE 0.
           03 WS-END-TBL         PIC 9(9) COMP VALUE 0.
           03 WS-END-TBL-Levels.
             05 WS-END-TBL-L1    PIC 9(9) COMP VALUE 0.
             05 WS-END-TBL-L2    PIC 9(9) COMP VALUE 0.
             05 WS-END-TBL-L3    PIC 9(9) COMP VALUE 0.
             05 WS-END-TBL-L4    PIC 9(9) COMP VALUE 0.
             05 WS-END-TBL-L5    PIC 9(9) COMP VALUE 0.
             05 WS-END-TBL-L6    PIC 9(9) COMP VALUE 0.
             05 WS-END-TBL-L7    PIC 9(9) COMP VALUE 0.
             05 WS-END-TBL-L8    PIC 9(9) COMP VALUE 0.
             05 WS-END-TBL-L9    PIC 9(9) COMP VALUE 0.
           03 filler redefines WS-END-TBL-Levels.
             05 WS-END-TBL-LEVEL OCCURS 9 TIMES PIC 9(9) COMP.
           03 WS-EntryLength     pic 9(9) comp value 0.
           03 Highwater-Field-Cnt pic 9(9) comp value 0.
           03 Lowwater-Field-Cnt  pic 9(9) comp value 999999999.
           03 save-pos           PIC 9(9) COMP VALUE 0.
           03 save-len           PIC 9(9) COMP VALUE 0.
           03 save-bit           PIC 9(4) COMP VALUE 0.
           03 save-TblPtr        PIC 9(5) COMP VALUE 0.
           03 INDXPTR            PIC 9(5) COMP VALUE 0.
           03 Generator-area.
v00862        05 Table-Field-Cnt occurs 9 times PIC 9(6) COMP VALUE 0.
              05 gTBLPTR         occurs 9 times PIC 9(5) COMP VALUE 0.
              05 Occur-Size      occurs 9 times pic 9(9) comp value 0.
           03 LastDim            pic 9         value 0.
           03 Entry-sw           pic 9(4) comp value 0.
              88 Entry-Ghost                   value 0.
              88 Entry-Root                    value 1.
           03 TFCNext            pic 9(6) comp value 0.
           03 TFCCurr            pic 9(6) comp value 0.
           03 lTblptr            pic 9(6) comp value 0.
           03 WS-OFFSET          PIC 9(6) COMP VALUE 0.
           03 Start-Entry        PIC 9(9) COMP VALUE 0.
           03 PRET               PIC 9(6) COMP VALUE 0.
           03 nPRET              PIC 9(6) COMP VALUE 0.
           03 tPRET              PIC 9(6) COMP VALUE 0.
           03 rPret              PIC 9(6) COMP VALUE 0.
           03 tSize              PIC 9(9) COMP VALUE 0.
           03 tEnd               PIC 9(9) COMP VALUE 0.
           03 tStart             PIC 9(9) COMP VALUE 0.
           03 RC                 PIC 9(4) COMP VALUE 0.
           03 X                  PIC 9(9) COMP VALUE 0.
           03 N                  PIC 9(18) COMP-5 VALUE 0.
           03 i                  PIC 9(9) COMP VALUE 0.
           03 i-Save             PIC 9(9) COMP VALUE 0.
           03 NLimit             PIC 9(18) COMP-5 VALUE 0.
v00862     03 LastN              pic 9(6) comp value 0.
           03 TT                 PIC 9(4) COMP VALUE 0.
           03 DIM                PIC 9(4) COMP VALUE 0.
           03 CURRENT-OCCURS PIC 9(5) COMP VALUE 0.
           03 Max-Occurs    PIC 9(5) COMP VALUE 0.
           03 Max-Occurs-zero    PIC 9(9) COMP VALUE 0.
           03 FCNT               PIC 9(6) COMP VALUE 0.
           03 RCNT               PIC 9(6) COMP VALUE 0.
           03 FLDCNT             PIC 9(6) COMP VALUE 0.
           03 ORIGFLDCNT         PIC 9(6) COMP VALUE 0.
           03 Resolved           PIC 9(4) COMP VALUE 0.
           03 Save-sCursor       PIC 9(9) COMP VALUE 1.
      * cursor is BIT coordinate based
           03 sCursor            PIC 9(9) COMP VALUE 1.
           03 tCursor            PIC 9(9) COMP VALUE 1.
v0.5       03 vCursor            pic 9(9) comp value 1.
           03 vCursorBits        pic 9(9) comp value 0.
v0.5       03 vLen               pic 9(9) comp value 0.
           03 sByte              pic 9(5) comp value 0.
           03 sNib               pic 9         value 0.
           03 sBit               pic 9         value 0.
           03 lByte              pic 9(5) comp value 0.
           03 lNib               pic 9         value 0.
           03 lBit               pic 9         value 0.
           03 TBLPTR             PIC 9(5) COMP VALUE 0.
           03 tTBLPTR            PIC 9(5) COMP VALUE 0.
           03 ElPtr              PIC 9(5) COMP VALUE 0.
           03 GrpPTR             PIC 9(5) COMP VALUE 0.
v00862     03 ORIG-PRET     PIC 9(6) COMP VALUE 0.
v00862     03 START-PRET    PIC 9(6) COMP VALUE 0.
v00862     03 END-PRET      PIC 9(6) COMP VALUE 0.
v00862     03 Last-Pret     PIC 9(6) COMP VALUE 0.
v00862     03 Save-START-PRET PIC 9(6) COMP VALUE 0.
v00862     03 Save-END-PRET PIC 9(6) COMP VALUE 0.
v00862     03 Save-Pret     PIC 9(6) COMP VALUE 0.
v00862     03 Save-FPtr     PIC 9(6) COMP VALUE 0.
           03 Save-Cond-sw  PIC 9(4) COMP VALUE 0.
v00862     03 Search-Pret   PIC 9(6) COMP VALUE 0.
           03 Trace-Event   pic x(5)      value spaces.
           03 Trace-Misc    pic x(20)     value spaces.
           03  FMT-BASE-FLD.
               05  FBF-Lit1         pic x    value spaces.
               05  FBF-Value        pic 9(5) value zeroes.
               05  FBF-Lit2         pic x    value spaces.
           03 WS-Message         pic x(80)     value spaces.
           03 WS-HEX             PIC X(80)     VALUE SPACES.
           03 WS-Keyword         pic x(40)     value spaces.
           03 WS-INDEX           PIC 9(5) COMP VALUE 0.
           03 WS-INDEXES         PIC X(45)     VALUE SPACES.
           03 WS-INDEXES-NO-PARENS PIC X(45)   VALUE SPACES.
           03 WS-DIM             PIC 9(4) COMP VALUE 0.
           03 WS-LEVEL           PIC 9(4) COMP VALUE 0.
v00866     03 PrevLevel          PIC 9(4) COMP VALUE 0.
           03 WS-SUB-CNT         PIC 9(4) COMP VALUE 0.
           03 CPUTime-In-Secs    PIC 9(9)      VALUE 0.
           03 Indices            pic x(44)     value spaces.
           03 ws-FDD-Okay-sw     pic 9(4) comp value 0.
               88 FDD-Not-Okay-to-Do          value 0.
               88 FDD-Okay-to-Do              value 1.
           03 ws-DAR-Okay-sw     pic 9(4) comp value 0.
               88 DAR-Not-Okay-to-Do          value 0.
               88 DAR-Okay-to-Do              value 1.
           03 ws-MIG-Okay-sw     pic 9(4) comp value 0.
               88 MIG-Not-Okay-to-Do          value 0.
               88 MIG-Okay-to-Do              value 1.
v00862     03 Required-Remaining-fields pic 9(6) comp value 0.
           03 Number-Fields-Skipped   pic 9(9) comp value 0.
           03 hash-str     pic x(19)  value spaces.
           03 hash-len     pic 9(4) comp value 0.
           03 tString      pic x(256) value spaces.
           03  tDone-sw             pic 9(4) comp value 0.
               88 tDone                           value 1.
           03 open-bracket           pic x          value x'BA'.
           03 close-bracket          pic x          value x'BB'.
           03 TimeStamp              pic 9(16) comp value zeroes.
           03 TimeStamp-prev         pic 9(16) comp value zeroes.
           03 TimeStamp-Diff         pic 9(16) comp value zeroes.
           03 date-stamp             pic 9(8) value zeroes.
           03 time-stamp             pic 9(8) value zeroes.
           03 WS-TimeStamp           pic 9(16) value zeroes.
           03 Filler redefines WS-TimeStamp.
            05 WS-TS-Year              pic 9(4).
            05 WS-TS-Month             pic 9(2).
            05 WS-TS-Day               pic 9(2).
            05 WS-TS-Hour              pic 9(2).
            05 WS-TS-Minute            pic 9(2).
            05 WS-TS-Second            pic 9(2).
            05 WS-TS-Hundredth         pic 9(2).

           03 DB2-TimeStamp-Template pic x(26)
                                     value '0000-00-00-00.00.00.000000'.
           03 ODBC-TimeStamp-Template pic x(26)
                                     value '0000-00-00 00:00:00.000000'.
           03  INDX.
             05  IND     PIC ZZZ9.
           03  DUMMY   PIC X(4).
           03  IND-X   PIC X(4).
           03 ABS-Index-area.
               05 abs-index  occurs 9 times pic 9(5) comp value 0.
v850       03  WS-VALUE             PIC X(256)    VALUE SPACES.
v850       03  Filler redefines ws-value.
v850           05  WS-VALUE-NUM18   PIC 9(18).
v850           05  FILLER           PIC X(238).
v850       03  FILLER redefines ws-value.
v850           05  FILLER           PIC X(8).
v850           05  WS-VALUE-NUM8    PIC 9(8).
v850           05  FILLER           PIC X(240).
v850       03  FILLER redefines ws-value.
v850           05  FILLER           PIC X(14).
v850           05  WS-VALUE-NUM4    PIC 9(4).
v850           05  FILLER           PIC X(238).
v850       03  Filler redefines ws-value.
v850           05  WS-VALUE-NUM18S  PIC S9(18).
v850           05  FILLER           PIC X(238).
v850       03  FILLER redefines ws-value.
v850           05  FILLER           PIC X(8).
v850           05  WS-VALUE-NUM8S   PIC S9(8).
v850           05  FILLER           PIC X(240).
v850       03  FILLER redefines ws-value.
v850           05  FILLER           PIC X(14).
v850           05  WS-VALUE-NUM4S   PIC 9(4).
v850           05  FILLER           PIC X(238).
           03  WS-EDIT-NUM18S       PIC 9(18)-.
           03  WS-NUMBER1           PIC 9(5)  COMP VALUE ZEROES.
           03  WS-NUMBER2           PIC 9(5)  COMP VALUE ZEROES.
           03  WS-LIMIT             PIC S9(5) COMP VALUE ZEROES.
           03  WS-ABS               PIC X(1)      VALUE SPACES.
           03  CONDITION-SW         PIC 9(4) COMP VALUE 0.
               88 CONDITION-FALSE                 VALUE 0.
               88 CONDITION-TRUE                  VALUE 1.
           03  CONDITION-LITERALS.
               05 FILLER     PIC X(5) VALUE 'FALSE'.
               05 FILLER     PIC X(5) VALUE 'TRUE '.
           03  CONDITION-TABLE-AREA REDEFINES CONDITION-LITERALS.
               05 CONDITION-STATE  OCCURS 2 TIMES PIC X(5).
           03  Length-Preset-SW     PIC 9(4) COMP VALUE 0.
               88  Length-not-Preset              VALUE 0.
               88  Length-Preset                  VALUE 1.
           03  FLDCNT-SAVE          PIC 9(6) COMP VALUE 0.
           03  DTPTR                PIC 9(5) COMP VALUE 0.
v00878     03  WEnd                 PIC 9(9) COMP VALUE 0.
v00878     03  WPos                 PIC 9(9) COMP VALUE 0.
v00878     03  WLen                 PIC 9(9) COMP VALUE 0.
           03  WPTR                 PIC 9(6) COMP VALUE 0.
           03  FPTR                 PIC 9(6) COMP VALUE 0.
           03  FLDT                 PIC 9(6) COMP VALUE 0.
           03  FoundAt              PIC 9(6) COMP VALUE 0.
           03  Sub                  PIC 9(6) COMP VALUE 0.
           03  Sub2                 PIC 9(6) COMP VALUE 0.
           03  TPTR                 PIC 9(6) COMP VALUE 0.
           03  wNIB                 PIC 9(1)      VALUE 0.
           03  wBit                 PIC 9(1)      VALUE 0.
           03  vPOS                 PIC 9(5) COMP VALUE 0.
           03  vNIB                 PIC 9(1)      VALUE 0.
           03  vBit                 PIC 9(1)      VALUE 0.
           03  COMPARE-FIELD        PIC X(10) VALUE SPACES.
           03  DLL-Error-Sw         PIC 9(4) COMP VALUE 0.
               88 No-DLL-Error                    VALUE 0.
               88 DLL-Long-Error                  VALUE 4.
               88 DLL-Short-Error                 VALUE 5.
               88 Table-DLL-Long-Error            VALUE 10.
               88 Data-Not-Processed-Error        VALUE 13.
           03  WS-NUM6              PIC 9(6) VALUE 0.
           03  WS-NUM5              PIC 9(5) VALUE 0.
           03  FILLER REDEFINES WS-NUM5.
               05  FILLER           PIC X(1).
               05  WS-NUM4          PIC 9(4).
           03  FILLER REDEFINES WS-NUM5.
               05  FILLER           PIC X(2).
               05  WS-NUM3          PIC 9(3).
           03  FILLER REDEFINES WS-NUM5.
               05  FILLER           PIC X(3).
               05  WS-NUM2          PIC 9(2).
           03  FILLER REDEFINES WS-NUM5.
               05  FILLER           PIC X(4).
               05  WS-NUM1          PIC 9(1).
           03  WS-NUM9              PIC 9(9) VALUE 0.
           03  WS-NUM1A             PIC 9(1) VALUE ZEROES.
           03  WS-NUM1B             PIC 9(1) VALUE ZEROES.
           03  WS-NUM1C             PIC 9(1) VALUE ZEROES.
           03  WS-NUM1D             PIC 9(1) VALUE ZEROES.
           03  WS-NUM5A             PIC 9(5) VALUE ZEROES.
           03  WS-NUM5B             PIC 9(5) VALUE ZEROES.
           03  WS-NUM5C             PIC 9(5) VALUE ZEROES.
           03  WS-NUM5D             PIC 9(5) VALUE ZEROES.
           03  ws-num2S-x.
              05  ws-num2S  pic z9.
           03  WS-STR6A             PIC X(13) VALUE SPACES.
           03  WS-STR6B             PIC X(6) VALUE SPACES.
PCPCPC*    03  LOWEST-BYTE          PIC X(1) VALUE X'20'.
PCPCPC*    03  HIGHEST-BYTE         PIC X(1) VALUE X'7F'.
MFMFMF*    03  LOWEST-BYTE          PIC X(1) VALUE X'40'.
MFMFMF*    03  HIGHEST-BYTE         PIC X(1) VALUE X'F9'.
      * THESE FIELDS ARE USED FOR THE TRACE display
           03  TR-NUM1A   PIC 9(1) VALUE 0.
           03  TR-NUM1B   PIC 9(1) VALUE 0.
           03  TR-NUM1C   PIC 9(1) VALUE 0.
           03  TR-NUM1D   PIC 9(1) VALUE 0.
           03  TR-NUM2A   PIC 9(2) VALUE 0.
           03  TR-NUM2B   PIC 9(2) VALUE 0.
           03  TR-NUM2C   PIC 9(2) VALUE 0.
           03  TR-NUM2D   PIC 9(2) VALUE 0.
           03  TR-NUM4A   PIC 9(4) VALUE 0.
           03  TR-NUM4B   PIC 9(4) VALUE 0.
           03  TR-NUM4C   PIC 9(4) VALUE 0.
           03  TR-NUM4D   PIC 9(4) VALUE 0.
           03  TR-NUM4E   PIC 9(4) VALUE 0.
           03  TR-NUM5A   PIC 9(5) VALUE 0.
           03  TR-NUM5B   PIC 9(5) VALUE 0.
           03  TR-NUM5C   PIC 9(5) VALUE 0.
           03  TR-NUM5D   PIC 9(5) VALUE 0.
           03  TR-NUM5E   PIC 9(5) VALUE 0.
           03  TR-NUM9A   PIC 9(9) VALUE 0.
           03  TR-NUM9b   PIC 9(9) VALUE 0.
           03  TR-NUM9AS  PIC 9(9)-.
           03  WS-NUM5Z-X.
              05  WS-NUM5Z   PIC ZZZZ9.
           03  TR-NUM6A   PIC 9(6) VALUE 0.
           03  stack-area.
              05  stack-cnt  pic 9(4) comp value 0.
              05  stack-max  pic 9(4) comp value 9.
              05  stack-table-area.
               07  stack-table     occurs 9 times.
v00862          09  stack-Pret                   pic 9(6)  comp value 0.
v00862          09  stack-orig-ptr               pic 9(6)  comp value 0.
                09  stack-current-occurs         pic 9(5)  comp value 0.
                09  stack-end-tbl                pic 9(9)  comp value 0.
                09  stack-Max-occurs             pic 9(5)  comp value 0.
v00862          09  stack-start-pret             pic 9(6)  comp value 0.
v00862          09  stack-end-pret               pic 9(6)  comp value 0.
                09  stack-reason                 pic 9(5)  comp value 0.
                09  stack-reason-field           pic 9(5)  comp value 0.
                09  stack-TblPtr                 pic 9(5)  comp value 0.
                09  stack-level                  pic 9(4)  comp value 0.
                09  stack-dim                    pic 9(4)  comp value 0.
                09  stack-Condition-sw           pic 9(4)  comp value 0.
                09  stack-len                    pic s9(9) comp value 0.
v00862          09  stack-last-pret              pic 9(6)  comp value 0.

           03  ws-file-Date          pic x(8)      value spaces.
           03  System-Setup          pic 9(4) comp value 0.
           03  Start-CPUTimer        pic 9(4) comp value 1.
           03  Print-CPUTimer        pic 9(4) comp value 2.
           03  Get-Dataset-Date      pic 9(4) comp value 3.
           03  Report-Sys-Info       pic 9(4) comp value 4.
           03  DCB-INFO-SPECIAL      pic 9(4) comp value 9.
           03  IN-DDName             pic x(8) value spaces.
           03  ws-DSN                pic x(44) value spaces.
           03  DATASET-NAME.
              05 DN-DCB-DDNAME         Pic  X(08) Value Spaces.
              05 DN-DCB-LRECL          pic  x(4)  value spaces.
              05 DN-DCB-DSORG          Pic  X(02) Value Spaces.
              05 DN-DCB-RECFM          Pic  X(02) Value Spaces.
              05 filler                pic x(28) value spaces.

           03  Open-DAR-File         pic 9(4) comp value 0.
           03  Start-DAR             pic 9(4) comp value 1.
           03  Write-DAR-Record      pic 9(4) comp value 2.
           03  Close-DAR-File        pic 9(4) comp value 3.

           03 transp-num4s  pic 9(4)-.
v0854      03 Init-Start-Last-Fields  pic x(8) value low-values.
v00860     03  FixLow-Cnt            pic 9(9) comp-5 value 0.
v00860     03  FixSpace-Cnt          pic 9(9) comp-5 value 0.
v00860     03  FixHigh-Cnt           pic 9(9) comp-5 value 0.
v00882     03  FixHigh-Kept-Cnt      pic 9(9) comp-5 value 0.

v00890     03  Last-Alt-Key          pic x(39)       value low-values.

v00890 copy FuncName.
       copy InitFld.

       copy LnkGap.
       copy LnkCnt.
       copy LnkRange.

       01  CONTROL-RECORD       PIC X(255) VALUE SPACES.
       01  IN-BUFFER            PIC X(32756) VALUE LOW-VALUES.
v.0.5  01  v-Buffer             PIC X(32756) VALUE LOW-VALUES.

       01  Split-Length-Area.
           03 Split-Length            occurs 10 times
                                      pic 9(4) comp.
       01  Split-Buffer-Area.
           03 Split-Buffer            occurs 10 times
v00878                                pic x(32768) value low-values.

       COPY LNKDLL.
       COPY LNKFILE.
       COPY LNKREC.
       COPY LNKGRP.
       COPY LnkEntry.
       COPY LNKDISC.
       COPY LNKRem.
v0.2   COPY LNKOPT.
       copy LnkMig.
       copy TTType.

       copy LnkDef.

v00860 copy LnkRat.

       01 WS-DISCARD-REASON-VALUES.
      *                               --123456789012345678901234567890
1          05 FILLER PIC X(32) VALUE '17NOT NUMERIC FIELD             '.
2          05 FILLER PIC X(32) VALUE '30NEG VALUE CANT BE UNSIGN TARGE'.
3          05 FILLER PIC X(32) VALUE '22CANT HOLD ENTIRE FIELD        '.
4          05 FILLER PIC X(32) VALUE '18More DLL than Data            '.
5          05 FILLER PIC X(32) VALUE '18More Data than DLL            '.
6          05 FILLER PIC X(32) VALUE '28Occurrence more than highest  '.
7          05 FILLER PIC X(32) VALUE '27Occurrence less than lowest   '.
8          05 FILLER PIC X(32) VALUE '30Computed length less than zero'.
9          05 FILLER PIC X(32) VALUE '23Computed length is zero       '.
10         05 FILLER PIC X(32) VALUE '24More Table DLL than Data      '.
11         05 FILLER PIC X(32) VALUE '26API-VGFBCNV1 RC not = 0000    '.
12         05 FILLER PIC X(32) VALUE '26Record Condition Never Met    '.
13         05 FILLER PIC X(32) VALUE '27Could not match DLL to Data   '.
14         05 FILLER PIC X(32) VALUE '25EntryLength value Invalid     '.
15         05 FILLER PIC X(32) VALUE '25API-QGGFDRC1 RC not = 0000    '.
16         05 FILLER PIC X(32) VALUE '25API-EEOR RC not = 0000        '.
17         05 FILLER PIC X(32) VALUE '00                              '.
18         05 FILLER PIC X(32) VALUE '00                              '.
19         05 FILLER PIC X(32) VALUE '00                              '.
20         05 FILLER PIC X(32) VALUE '00                              '.
21         05 FILLER PIC X(32) VALUE '13Date-Error-CC                 '.
22         05 FILLER PIC X(32) VALUE '13Date-Error-YY                 '.
23         05 FILLER PIC X(32) VALUE '13Date-Error-MM                 '.
24         05 FILLER PIC X(32) VALUE '13Date-Error-DD                 '.
25         05 FILLER PIC X(32) VALUE '15Date-Error-DDDD               '.
26         05 FILLER PIC X(32) VALUE '16Date-Error-DDDDD              '.
27         05 FILLER PIC X(32) VALUE '17Date-Error-DDDDDD             '.
28         05 FILLER PIC X(32) VALUE '14Date-Error-MMM                '.
29         05 FILLER PIC X(32) VALUE '16Date-Error-MMMMM              '.
30         05 FILLER PIC X(32) VALUE '20Date-Error-MMMMMMMMM          '.
31         05 FILLER PIC X(32) VALUE '17Date-Error-DD-EOM             '.
32         05 FILLER PIC X(32) VALUE '14Date-Error-JJJ                '.
33         05 FILLER PIC X(32) VALUE '12Date-Error-H                  '.
34         05 FILLER PIC X(32) VALUE '17Date-Error-Spaces             '.
35         05 FILLER PIC X(32) VALUE '14Date-Error-Low                '.
36         05 FILLER PIC X(32) VALUE '18Date-Error-Numeric            '.
37         05 FILLER PIC X(32) VALUE '17Date-Error-Zeroes             '.
38         05 FILLER PIC X(32) VALUE '16Date-Error-Nines              '.
39         05 FILLER PIC X(32) VALUE '15Date-Error-High               '.
40         05 FILLER PIC X(32) VALUE '12Date-Error-C                  '.
41         05 FILLER PIC X(32) VALUE '00                              '.
42         05 FILLER PIC X(32) VALUE '00                              '.
43         05 FILLER PIC X(32) VALUE '00                              '.
44         05 FILLER PIC X(32) VALUE '00                              '.
45         05 FILLER PIC X(32) VALUE '00                              '.
46         05 FILLER PIC X(32) VALUE '00                              '.
47         05 FILLER PIC X(32) VALUE '00                              '.
48         05 FILLER PIC X(32) VALUE '00                              '.
49         05 FILLER PIC X(32) VALUE '00                              '.
50         05 FILLER PIC X(32) VALUE '00                              '.
51         05 FILLER PIC X(32) VALUE '00                              '.
52         05 FILLER PIC X(32) VALUE '00                              '.
53         05 FILLER PIC X(32) VALUE '00                              '.
54         05 FILLER PIC X(32) VALUE '00                              '.
55         05 FILLER PIC X(32) VALUE '00                              '.
56         05 FILLER PIC X(32) VALUE '00                              '.
57         05 FILLER PIC X(32) VALUE '00                              '.
58         05 FILLER PIC X(32) VALUE '00                              '.
59         05 FILLER PIC X(32) VALUE '00                              '.
60         05 FILLER PIC X(32) VALUE '00                              '.
61         05 FILLER PIC X(32) VALUE '00                              '.
62         05 FILLER PIC X(32) VALUE '00                              '.
63         05 FILLER PIC X(32) VALUE '00                              '.
64         05 FILLER PIC X(32) VALUE '00                              '.

       01  Display-Memory-Allocated.
           03  filler              pic x(2) value '* '.
           03  DMA-Table           pic x(20) value spaces.
           03  filler              pic x     value space.
           03  DMA-Bytes           pic z(9)9.
           03  filler              pic x     value space.
           03  DMA-Entries         pic z(8)9.
           03  filler              pic x(15) value spaces.
           03  filler              pic x     value '*'.

       01  Log-Log.
           03  LL-Reason-Text  pic x(7)  value spaces.
           03  FILLER          PIC X(9)  VALUE ' RECORD #'.
           03  LL-Rec-Cnt      PIC 9(9)  VALUE ZEROES.
           03  FILLER          PIC X     VALUE SPACES.
           03  FILLER          PIC X(6)  VALUE 'FIELD:'.
           03  LL-FIELD        PIC 9(5)  VALUE ZEROES.
           03  FILLER          PIC X     VALUE SPACES.
v00860     03  LL-Field-Name   pic x(40) value spaces.
v00860     03  filler          pic X     value space.
           03  FILLER          PIC X(5)  VALUE 'Byte:'.
           03  LL-POS          PIC 9(5)  VALUE ZEROES.
v00860     03  filler          pic x(5)  value ' LEN:'.
v00860     03  LL-Length       pic 9(5)  value zeroes.
           03  FILLER          PIC X     VALUE SPACES.
           03  FILLER          PIC X(5)  VALUE 'CODE:'.
           03  LL-REASON-CODE  PIC 99    VALUE 01.
           03  FILLER          PIC X     VALUE SPACES.
           03  LL-REASON-MSG   PIC X(60) VALUE SPACES.

       01  LOG-DISCARD.
V00895     03  LD-Message      PIC X(7)  VALUE 'DISCARD'.
V00895     03  filler          pic x(9)  value ' RECORD #'.
           03  LD-REC-CNT      PIC 9(9)  VALUE ZEROES.
           03  FILLER          PIC X     VALUE SPACES.
           03  FILLER          PIC X(6)  VALUE 'FIELD:'.
           03  LD-FIELD        PIC 9(5)  VALUE ZEROES.
           03  FILLER          PIC X     VALUE SPACES.
v00860     03  LD-Field-Name   pic x(40) value spaces.
v00860     03  filler          pic X     value space.
           03  FILLER          PIC X(5)  VALUE 'Byte:'.
           03  LD-POS          PIC 9(5)  VALUE ZEROES.
v00860     03  filler          pic x(5)  value ' LEN:'.
v00860     03  LD-Length       pic 9(5)  value zeroes.
           03  FILLER          PIC X     VALUE SPACES.
           03  FILLER          PIC X(5)  VALUE 'CODE:'.
           03  LD-REASON-CODE  PIC 99    VALUE ZEROES.
           03  FILLER          PIC X     VALUE SPACES.
           03  LD-REASON-MSG.
             05 filler         pic x(30) value spaces.
             05 filler         pic X     value spaces.
             05 LD-Kampo-Extra-Details.
               07 LDKM-Data2x.
                09 LDKM-Data2       pic 9(9)  value zeroes.
               07 LDKM-Lit1         pic X(5)  value ' Pos:'.
               07 LDKM-Data3        pic 9(9)  value zeroes.
       01  Log-Discard-kampo-message.
v00879     03  LDKM-Lit2  pic x(18) value 'API-VGXBCNV9 RC = '.
           03  LDKM-Data1 pic x(4)  value '0000'.
           03  LDKM-Lit3  pic X(8)  value ' CharCd:'.

       01  Log-Migrate.
           03  LM-Reason-Text  pic x(7)  value spaces.
           03  FILLER          PIC X(9)  VALUE ' RECORD #'.
           03  LM-Rec-Cnt      PIC 9(9)  VALUE ZEROES.
v00860     03  filler          pic X     value space.
           03  FILLER          PIC X(8)  VALUE 'Target #'.
           03  LM-Rec-Out      PIC 9(9)  VALUE ZEROES.
           03  FILLER          PIC X     VALUE SPACES.
v00860     03  LM-Record-Name  pic x(40) value spaces.
v00860     03  filler          pic x(5)  value ' LEN:'.
v00860     03  LM-Out-Len      pic 9(5)  value zeroes.

v00886 01  Log-EEOR.
           03  LE-Reason-Text  pic x(7)  value spaces.
           03  FILLER          PIC X(9)  VALUE ' RECORD #'.
           03  LE-Rec-Cnt      PIC 9(9)  VALUE ZEROES.
           03  filler          pic X     value space.
           03  filler          pic X(9)  value 'Number = '.
           03  LE-Rec-Num      PIC 9(9)  VALUE ZEROES.

       01  LOG-Stats.
           03  filler                pic x(6)  value 'STAT1:'.
           03  filler                pic x(14) value 'Lowest Occurs:'.
           03  LS-Lowest-Occurs      pic 9(5)  value zeroes.
           03  filler                pic x     value space.
           03  filler                pic x(07) value 'Record#'.
           03  LS-Lowest-Occurs-Rec  pic 9(5)  value zeroes.
           03  filler                pic x     value space.
           03  filler                pic x(15) value 'Highest Occurs:'.
           03  LS-Highest-Occurs     pic 9(5)  value zeroes.
           03  filler                pic x     value space.
           03  filler                pic x(07) value 'Record#'.
           03  LS-Highest-Occurs-Rec pic 9(5)  value zeroes.

       01  Log-File-Column.
           03 filler              pic x     value space.
           03 LFC-Ptr             pic x(2)  value '##'.
           03 filler              pic x     value space.
           03 LFC-FileName        pic x(44) value
              'File Name-----------------------------------'.
           03 filler              pic x     value space.
           03 LFC-DDName          pic x(8)  value 'DDName--'.
           03 filler              pic x     value space.
           03 LFC-DSN             pic x(44) value
              'Dataset Name--------------------------------'.
           03 filler              pic x     value space.
           03 LFC-Date            pic x(10) value 'File Date-'.
           03 filler              pic x     value space.
           03 LFC-Length          pic x(9)  value 'Length---'.
           03 filler              pic x     value space.
           03 LFC-Format          pic x(8)  value 'Format--'.
       01  Log-File-Table.
           03 filler              pic x     value space.
           03 LFT-Ptr             pic 9(2)  value zeroes.
           03 filler              pic x     value space.
           03 LFT-FileName        pic x(44) value spaces.
           03 filler              pic x     value space.
           03 LFT-DDName          pic x(8)  value spaces.
           03 filler              pic x     value space.
           03 LFT-DSN             pic x(44) value spaces.
           03 filler              pic x     value space.
           03 LFT-Date            pic x(10) value zeroes.
           03 filler              pic x     value space.
           03 LFT-Length          pic z(8)9 value zeroes.
           03 filler              pic x     value space.
           03 LFT-Format          pic x(8)  value spaces.
v00894     03 filler              pic x     value space.
v00894     03 LFT-CSV             pic x(9)  value spaces.
       01  Log-Record-Table-Column.
           03 LRTC-sub             PIC X(5)      VALUE 'REC#-'.
           03 filler               pic x         value spaces.
           03 LRTC-FILE            PIC X(4)      VALUE 'File'.
           03 filler               pic x         value spaces.
           03 LRTC-RECORDNAME      PIC X(30)     VALUE 'Record Name'.
           03 filler               pic x         value spaces.
           03 LRTC-Field           pic X(5)      value 'Pret '.
           03 filler               pic x         value spaces.
           03 LRTC-COND            PIC x(5)      VALUE 'Cond '.
           03 filler               pic x         value spaces.
           03 LRTC-Start-PreT      pic x(5)      value 'SPret'.
           03 filler               pic x         value spaces.
           03 LRTC-End-PreT        pic x(5)      value 'EPret'.
           03 filler               pic x         value spaces.
           03 LRTC-Variability     pic x(5)      value 'Varia'.
           03 filler               pic x         value spaces.
           03 LRTC-num-vFields     pic x(5)      value 'vFiel'.
           03 filler               pic x         value spaces.
           03 LRTC-Max-Len         pic x(9)      value 'Max Len--'.
           03 filler               pic x         value spaces.
           03 LRTC-Key-Field       pic x(5)      value 'KPret'.
           03 filler               pic x         value spaces.
           03 LRTC-KN              PIC X(46)     value 'Key Name'.
       01  Log-Record-Table.
           03 LRT-sub             PIC 9(5)      VALUE ZEROES.
           03 filler              pic x         value spaces.
           03 LRT-FILE            PIC 9(4)      VALUE ZEROES.
           03 filler              pic x         value spaces.
           03 LRT-RECORDNAME      PIC X(30)     VALUE SPACES.
           03 filler              pic x         value spaces.
           03 LRT-Field           pic 9(5)      value zeroes.
           03 filler              pic x         value spaces.
           03 LRT-COND            PIC 9(5)      VALUE ZEROES.
           03 filler              pic x         value spaces.
           03 LRT-Start-PreT      pic 9(5)      value zeroes.
           03 filler              pic x         value spaces.
           03 LRT-End-PreT        pic 9(5)      value zeroes.
           03 filler              pic x         value spaces.
           03 LRT-Variability     pic 9(5)      value zeroes.
           03 filler              pic x         value spaces.
           03 LRT-num-vFields     pic 9(5)      value zeroes.
           03 filler              pic x         value spaces.
           03 LRT-Max-Len         pic 9(9)      value zeroes.
           03 filler              pic x         value spaces.
           03 LRT-Key-Field       pic 9(5)      value zeroes.
           03 filler              pic x         value spaces.
           03 LRT-KN              PIC X(46)     value spaces.
       01  Log-Table-Table.
           03 LTABLE-SUB                   PIC 9(5)      VALUE ZEROES.
           03 FILLER                       PIC X         VALUE SPACES.
           03 LTABLE-FILE                  PIC 9(4)      VALUE ZEROES.
           03 FILLER                       PIC X         VALUE SPACES.
           03 LTABLE-RECORD                PIC 9(4)      VALUE ZEROES.
           03 FILLER                       PIC X         VALUE SPACES.
           03 LTABLE-FIELD-PTR             PIC 9(5)      VALUE ZEROES.
           03 FILLER                       PIC X         VALUE SPACES.
           03 LTABLE-LEVEL-DIRECTION       PIC X         VALUE ' '.
           03 FILLER                       PIC X         VALUE SPACES.
           03 LTABLE-Level-Value           PIC 9(2)      VALUE ZEROES.
           03 FILLER                       PIC X         VALUE SPACES.
           03 LTABLE-NAME                  PIC X(30)     VALUE SPACES.
           03 FILLER                       PIC X         VALUE SPACES.
           03 LTABLE-DIMS                  PIC 9(4)      VALUE ZEROES.
           03 FILLER                       PIC X         VALUE SPACES.
           03 LTABLE-NUMBER-OF-FIELDS      PIC 9(5)      VALUE ZEROES.
           03 FILLER                       PIC X         VALUE SPACES.
           03 LTABLE-Mandatory-FIELDS      PIC 9(5)      VALUE ZEROES.
           03 FILLER                       PIC X         VALUE SPACES.
           03 LTABLE-Last-Field            PIC 9(5)      VALUE ZEROES.
           03 FILLER                       PIC X         VALUE SPACES.
           03 LTABLE-TYPE                  PIC X         VALUE SPACE.
           03 FILLER                       PIC X         VALUE SPACES.
           03 LTABLE-DEPEND-LIMIT-SW       PIC X         VALUE SPACE.
           03 FILLER                       PIC X         VALUE SPACES.
           03 LTABLE-DEPEND-FIELD          PIC 9(5)      VALUE 0.
           03 FILLER                       PIC X         VALUE SPACES.
           03 LTable-Limit-Base-Field      PIC 9(5)      VALUE 0.
           03 FILLER                       PIC X         VALUE SPACES.
           03 LTABLE-LIMIT-FIELD1          PIC 9(5)      VALUE 0.
           03 FILLER                       PIC X         VALUE SPACES.
           03 LTABLE-LIMIT-FIELD2          PIC 9(5)      VALUE 0.
           03 FILLER                       PIC X         VALUE SPACES.
           03 LTable-COND                  PIC 9(4)      VALUE 0.
           03 FILLER                       PIC X         VALUE SPACES.
           03 LTable-EL-Ptr                pic 9(4)      value 0.
       01  Log-Default-Table.
           03 LDT-Ptr                  pic 9(4)  value zeroes.
           03 Filler                   pic x     value spaces.
           03 LDT-Times                pic 9(9)  value zeroes.
           03 Filler                   pic x     value spaces.
           03 LDT-Length               pic 9(9)  value zeroes.
           03 Filler                   pic x     value spaces.
           03 LDT-Hex                  pic x     value zeroes.
           03 Filler                   pic x     value spaces.
           03 LDT-Area                 pic x(20) value spaces.
       01  Log-EntryLength-Table.
           03 LET-Ptr                  pic 9(4)  value zeroes.
           03 Filler                   pic x     value spaces.
           03 LET-Table-Field          pic 9(5)  value zeroes.
           03 Filler                   pic x     value spaces.
           03 LET-Field                pic 9(5)  value zeroes.
           03 Filler                   pic x     value spaces.
           03 LET-Value                pic 9(5)  value zeroes.
           03 Filler                   pic x     value spaces.
           03 LET-Field-sw             pic 9(4)  value zeroes.
           03 Filler                   pic x     value spaces.
           03 LET-Start                pic 9(9)  value zeroes.
           03 Filler                   pic x     value spaces.
           03 LET-FN                   pic x(43) value zeroes.
       01  Log-Function-Table.
           03 LFnT-ptr                pic 9(5)  value zeroes.
           03 filler                  pic x(2)  value spaces.
           03 LFnT-File               pic 9(4)  value zeroes.
           03 Filler                  pic x(1)  value spaces.
           03 LFnT-Record             pic 9(4)  value zeroes.
           03 Filler                  pic x(1)  value spaces.
           03 LFnT-Order              pic 9(5)  value zeroes.
           03 Filler                  pic x(1)  value spaces.
           03 LFnT-Total              pic 9(4)  value zeroes.
           03 Filler                  pic x(1)  value spaces.
           03 LFnT-Field              pic 9(5)  value zeroes.
           03 Filler                  pic x(1)  value spaces.
           03 LFnT-Fieldname          pic x(30) value spaces.
           03 Filler                  pic x(1)  value spaces.
           03 LFnT-Name               pic x(5)  value spaces.
           03 Filler                  pic x(1)  value spaces.
           03 LFnT-Data-sw            pic x     value spaces.
           03 Filler                  pic x(1)  value spaces.
           03 LFnT-Start              pic 9(5)  value zeroes.
           03 Filler                  pic x(1)  value spaces.
           03 LFnT-Length             pic 9(5)  value zeroes.
           03 Filler                  pic x(1)  value spaces.
           03 LFnT-Hash               pic 9     value zeroes.
           03 Filler                  pic x(1)  value spaces.
           03 LFnT-Disp.
              05 LFnT-Field1      pic 9(5) value 0.
              05 filler           pic x    value spaces.
              05 LFnT-Field2      pic 9(5) value 0.
              05 filler           pic x    value spaces.
              05 LFnT-Field3      pic x(8) value spaces.
              05 filler           pic x    value spaces.
              05 LFnT-Field4      pic x(8) value spaces.

       01  DISPLAY-Enhanced-COND-COL.
           03 DECC-COND-PTR           PIC X(6)     VALUE '--COND'.
           03 FILLER                  PIC X        VALUE SPACES.
           03 DECC-COND-FILE          PIC X(4)     VALUE 'FIL#'.
           03 FILLER                  PIC X        VALUE SPACES.
           03 DECC-COND-Record        PIC X(4)     VALUE 'REC#'.
           03 FILLER                  PIC X        VALUE SPACES.
           03 DECC-COND-OWNING-FIELD  PIC X(6)     VALUE '-PRET-'.
           03 FILLER                  PIC X        VALUE SPACES.
           03 DECC-CT-TABLE-SW        PIC X        VALUE 'S'.
           03 FILLER                  PIC X        VALUE SPACES.
           03 DECC-ExecuteQty         PIC X(4)     VALUE 'Qty-'.
           03 FILLER                  PIC X        VALUE SPACES.
           03 DECC-ExecuteNdx         pic x(6)     value '-Exec@'.
           03 FILLER                  PIC X        VALUE SPACES.
           03 DECC-Formula            PIC X(93)    VALUE 'Formula'.
       01  DISPLAY-Enhanced-COND-AREA.
           02 DECA-left-half.
            03 DECA-COND-PTR           PIC 9(6)      VALUE 0.
            03 FILLER                  PIC X         VALUE SPACES.
            03 DECA-COND-FILE          PIC 9(4)      VALUE 0.
            03 FILLER                  PIC X         VALUE SPACES.
            03 DECA-COND-Record        PIC 9(4)      VALUE 0.
            03 FILLER                  PIC X         VALUE SPACES.
            03 DECA-COND-OWNING-FIELD  PIC 9(6)      VALUE ZEROES.
            03 FILLER                  PIC X         VALUE SPACES.
            03 DECA-CT-TABLE-SW        PIC X         VALUE ' '.
            03 FILLER                  PIC X         VALUE SPACES.
            03 DECA-ExecuteQty         pic 9(4)      value zeroes.
            03 FILLER                  PIC X         VALUE SPACES.
            03 DECA-ExecuteNdx         pic 9(6)     value zeroes.
            03 FILLER                  PIC X        VALUE SPACES.
            03 DECA-Formula            pic x(93)    value spaces.

       01  Log-Total-Line.
           03  LTL-Description.
               05  LTL-DESC-Field  pic 9(5)   value zeroes.
               05  filler          pic x      value spaces.
               05  LTL-DESC-FieldName pic x(34) value spaces.
           03  filler              pic x      value '='.
           03  LTL-Count           pic zzz,zzz,zz9.

      * This area is for the FDD routine.
       01  fdd-MISC-AREA.
           03 fddPROGRAM-COPYRIGHT.
              05 FILLER PIC X(42) VALUE
                     'MIGRATION ENGINE, (C)IBM CORP. 2009-2014; '.
              05 FILLER PIC X(20) VALUE 'ALL RIGHTS RESERVED.'.
           03  fddVersion           pic x(8)      value '0.1.12'.
           03  FDD-OPEN-SW          PIC 9(4) COMP VALUE 0.
               88 FDD-NOT-OPEN                    VALUE 0.
               88 FDD-OPEN                        VALUE 1.
      * Fields passed to the FDD routine
           03 FDD-Discard-Code pic 9(4) comp.
           03 fdd-command pic 9(4) comp.
               88 Open-FDD-File       value 0.
               88 Start-FDD           value 1.
               88 Write-FDD-Field     value 2.
               88 Write-FDD-Message   value 3.
               88 End-FDD             value 4.
               88 Drop-FDD            value 5.
               88 Close-FDD-File      value 9.

           03  fddIO-status         PIC X(2)      VALUE '00'.
           03  SearchField          pic 9(6) comp value 0.
           03  NLEN     PIC 9(9) COMP  value zeroes.
           03  NNib     PIC 9(4) COMP  value zeroes.
           03  xNib                 pic 9          value zeroes.
           03  xBit                 pic 9          value zeroes.
           03  ws-Bytes             pic 9(4) comp value 0.
           03  sLen                 PIC 9(5) COMP value 0.
           03  fdd-dims             pic 9(4) comp value 0.
           03  Cond-Len             pic s9(9) comp value 0.
           03  ws-large-area     pic x(512) value spaces.
           03  Indice      pic x(44) value spaces.

           03  fdd-Value            PIC X(256)    VALUE SPACES.
           03  Filler  redefines fdd-Value.
               05  fdd-Value-NUM18  PIC 9(18).
               05  FILLER           PIC X(02).
v00862         05  FILLER           PIC X(236).
           03  Filler  redefines fdd-Value.
               05  FILLER           PIC X(8).
               05  fdd-Value-NUM8   PIC 9(8).
               05  FILLER           PIC X(04).
v00862         05  FILLER           PIC X(236).
           03  Filler  redefines fdd-Value.
               05  FILLER           PIC X(14).
               05  fdd-Value-NUM4   PIC 9(4).
               05  FILLER           PIC X(02).
v00862         05  FILLER           PIC X(236).
           03  Filler  redefines fdd-Value.
               05  fdd-Value-NUM18S PIC S9(18).
               05  FILLER           PIC X(02).
v00862         05  FILLER           PIC X(236).
           03  Filler  redefines fdd-Value.
               05  FILLER           PIC X(8).
               05  fdd-Value-NUM8S  PIC S9(8).
               05  FILLER           PIC X(04).
v00862         05  FILLER           PIC X(236).
           03  Filler  redefines fdd-Value.
               05  FILLER           PIC X(14).
               05  fdd-Value-NUM4S  PIC 9(4).
               05  FILLER           PIC X(02).
v00862         05  FILLER           PIC X(236).

           03  fdd-Num18Z-X.
              05  fdd-Num18Z  PIC ZZZZZZZZZZZZZZZZZ9.
           03  fdd-Num6             PIC 9(6) VALUE 0.
           03  fdd-Num5             PIC 9(5) VALUE 0.
           03  FILLER REDEFINES fdd-Num5.
               05  FILLER           PIC X(1).
               05  fdd-Num4         PIC 9(4).
           03  FILLER REDEFINES fdd-Num5.
               05  FILLER           PIC X(2).
               05  fdd-Num3         PIC 9(3).
           03  FILLER REDEFINES fdd-Num5.
               05  FILLER           PIC X(3).
               05  fdd-Num2         PIC 9(2).
           03  FILLER REDEFINES fdd-Num5.
               05  FILLER           PIC X(4).
               05  fdd-Num1         PIC 9(1).
           03 WS-ACCEPT-DATE    PIC 9(8) VALUE ZEROES.
           03 FILLER REDEFINES WS-ACCEPT-DATE.
              05 WS-ACCEPT-CCYY PIC X(4).
              05 WS-ACCEPT-MM   PIC X(2).
              05 WS-ACCEPT-DD   PIC X(2).
           03 WS-ACCEPT-TIME    PIC 9(8) VALUE ZEROES.
           03 FILLER REDEFINES WS-ACCEPT-TIME.
              05 WS-ACCEPT-HOUR   PIC X(2).
              05 WS-ACCEPT-MINUTE PIC X(2).
              05 WS-ACCEPT-SECOND PIC X(2).
              05 WS-ACCEPT-HUNDRD PIC X(2).

v00872     03 KampoAPI             pic x(8)      value 'VGXBCNV9'.
v00876     03 QGGFDRC1             pic x(8)      value 'QGGFDRC1'.
v00892     03 IKEEORE0             pic x(8)      value 'G0XKORED'.

       01  FDD-FILE-HEADING1.
           03 FILLER           PIC X      VALUE SPACES.
           03 FILLER           PIC X(05) VALUE 'DATE:'.
           03 DFH1-DATE.
              05 DFH1-DATE-CCYY  PIC X(4) VALUE SPACES.
              05 FILLER          PIC X(1) VALUE '-'.
              05 DFH1-DATE-MM    PIC X(2) VALUE SPACES.
              05 FILLER          PIC X(1) VALUE '-'.
              05 DFH1-DATE-DD    PIC X(2) VALUE SPACES.
           03 FILLER           PIC X(08) VALUE SPACES.
           03 FILLER           PIC X(54) VALUE
               '                FORMATTED DATA DISPLAY         '.
           03 FILLER           PIC X(41) VALUE SPACES.
           03 FILLER           PIC X(8)  VALUE 'Engine-v'.
           03 DFH1-Engine-Ver  pic x(8)  value spaces.
           03 FILLER           PIC X     VALUE space.
           03 FILLER           PIC X(5)  VALUE 'FDD-v'.
           03 DFH1-Version     pic x(8)  value spaces.
       01  FDD-FILE-HEADING2.
           03 FILLER           PIC X      VALUE SPACES.
           03 FILLER           PIC X(05) VALUE 'TIME:'.
           03 DFH2-TIME.
              05 DFH2-TIME-HOUR   PIC X(2) VALUE SPACES.
              05 FILLER           PIC X(1) VALUE ':'.
              05 DFH2-TIME-MINUTE PIC X(2) VALUE SPACES.
              05 FILLER           PIC X(1) VALUE ':'.
              05 DFH2-TIME-SECOND PIC X(2) VALUE SPACES.
           03 FILLER           PIC X(08) VALUE SPACES.
           03 DFH2-FILENAME    PIC X(44) VALUE SPACES.
           03 FILLER           PIC X(82) VALUE SPACES.
       01  FDD-FILE-HEADING3.
           03 FILLER           PIC X      VALUE SPACES.
           03 FILLER           PIC X(13) VALUE 'DLL: Version:'.
           03 FFH3-Version     pic x(8)   value spaces.
           03 Filler           pic x(9)   value ' GenDate:'.
           03 FFH3-GenDate     pic x(10)  value spaces.
           03 Filler           pic x(6)   value ';Time:'.
           03 FFH3-GenTime     pic x(8)   value spaces.
           03 Filler           pic x(6)   value ';User:'.
           03 FFH3-GenUser     pic x(10)  value spaces.
           03 FILLER           PIC X(06) VALUE ' Name-'.
           03 FFH3-Name        pic x(30)  value spaces.
       01  FDD-FILE-HEADING4.
           03 FILLER           PIC X      VALUE SPACES.
           03 FILLER           PIC X(11) VALUE 'OPT: Start:'.
           03 FFH4-Start       pic z(8)9  value zeroes.
           03 Filler           pic x(6)   value ' Stop:'.
           03 FFH4-Stop        pic z(8)9  value zeroes.

       01  FDD-PRINT-LINE.
           03 FILLER               PIC X      VALUE SPACES.
           03 DPLT-DATA         PIC X(155) VALUE SPACES.

       01  FDD-Total-Line.
           03  FTL-Description.
               05  FTL-DESC-Field  pic 9(6)   value zeroes.
               05  filler          pic x      value spaces.
               05  FTL-DESC-FieldName pic x(34) value spaces.
           03  filler              pic x      value '='.
           03  FTL-Count           pic zzz,zzz,zz9.

       01  DATA-COLUMN-HEADING.
           03 DCH-FIELD            PIC X(6)   VALUE ' FLD#'.
           03 FILLER               PIC X      VALUE SPACES.
           03 DCH-FIELDNAME        PIC X(44)  VALUE
              'FIELD NAME----------------------------------'.
           03 FILLER               PIC X      VALUE SPACES.
           03 DCH-VALUE            PIC X(20)  VALUE
              'VALUE---------------'.
           03 FILLER               PIC X      VALUE SPACES.
           03 DCH-HEX              PIC X(40)  VALUE
              'HEX REPRESENTATION----------------------'.
           03 FILLER               PIC X      VALUE '<'.
           03 DCH-START            PIC X(7)   VALUE 'START.B'.
           03 FILLER               PIC X      VALUE ':'.
           03 DCH-LENGTH           PIC X(5)   VALUE 'LENGT'.
           03 FILLER               PIC X      VALUE '>'.
           03 FILLER               PIC X      VALUE SPACES.
           03 DCH-TYPE             PIC X(7)   VALUE 'TYPE---'.
           03 FILLER               PIC X      VALUE SPACES.
           03 DCH-MIG-POS          PIC X(5)   VALUE 'TGPOS'.
           03 DCH-LIT5             PIC X      VALUE '.'.
           03 DCH-MIG-LEN          PIC X(5)   VALUE 'TGLEN'.
           03 filler               pic x      value space.
           03 DCH-REF              pic x(6)   value 'Ref()-'.
       01  DATA-LINE.
           03 DL-FIELD             PIC ZZZZZ9.
           03 DL-vField            PIC X      VALUE SPACES.
           03 DL-FIELDNAME         PIC X(44)  VALUE SPACES.
           03 FILLER               PIC X      VALUE SPACES.
           03 DL-VALUE             PIC X(20)  VALUE SPACES.
           03 FILLER               PIC X      VALUE SPACES.
           03 DL-HEX               PIC X(40)  VALUE SPACES.
           03 DL-LIT1              PIC X      VALUE '<'.
           03 DL-StartX.
             05 DL-START           PIC 9(5)   VALUE ZEROES.
             05 DL-LIT2            PIC X      VALUE '.'.
             05 DL-BIT             PIC 9      VALUE 0.
           03 Filler redefines DL-StartX.
             05 DL-Start-Bits      pic 9(7).
           03 DL-LIT3              PIC X      VALUE ':'.
           03 DL-LENGTHX.
              05 DL-LENGTH         PIC 9(5)   VALUE ZEROES.
           03 DL-LIT4              PIC X      VALUE '>'.
           03 FILLER               PIC X      VALUE SPACES.
           03 DL-TYPE              PIC X(7)   VALUE SPACES.
           03 FILLER               PIC X      VALUE SPACES.
           03 DL-MIGRATE-Area.
               05 DL-MIG-POS       PIC 9(5)   VALUE ZEROES.
               05 DL-LIT5          PIC X      VALUE SPACES.
               05 DL-MIG-LEN       PIC 9(5)   VALUE ZEROES.
           03 filler               pic x      value spaces.
           03 DL-REF               pic x(6)   value spaces.
       01  Data-Line-Table.
           03 DLT-FIELD            PIC ZZZZZ9.
           03 FILLER               PIC X      VALUE SPACES.
           03 FILLER               PIC X(7)   VALUE 'Table:'.
           03 DLT-NAME             PIC X(30)  VALUE SPACES.
           03 filler               pic x      value spaces.
           03 DLT-Group            pic x(5)   value spaces.
           03 filler               pic x      value spaces.
           03 DLT-ODO              pic x(3)   value spaces.
       01  Data-Line-Table-2.
           03 filler               pic x(6)   value spaces.
           03 filler               pic x(7)   value spaces.
           03 filler               pic x      value space.
           03 DLT-Depend           pic x(128) value spaces.
           03 DLT-EntryLength redefines DLT-Depend     pic x(128).
           03 DLT-Cond        redefines DLT-Depend     pic x(128).
           03 DLT-Position    redefines DLT-Depend     pic x(128).
       01  Data-Line-Table-Position.
           03 filler               pic x(14)  value spaces.
           03 filler               pic x(1)   value '<'.
           03 DLT-Start            pic 9(5)   value zeroes.
           03 filler               pic x(1)   value '.'.
           03 DLT-Start-Bit        pic 9      value zeroes.
           03 filler               pic x      value ':'.
           03 DLT-Length           pic 9(5)   value zeroes.
           03 filler               pic x      value '>'.

       01  DATA-LINE-GROUP.
           03 DLG-FIELD            PIC ZZZZZ9.
           03 FILLER               PIC X      VALUE SPACES.
           03 FILLER               PIC X(7)   VALUE 'Group:'.
           03 DLG-NAME             PIC X(30)  VALUE SPACES.

       01  FDD-RECORD-LINE.
           03 FILLER               PIC X      VALUE SPACES.
           03 FILLER               PIC X(7)   VALUE 'Record='.
           03 DRL-RECORD           PIC 9(5)   VALUE ZEROES.
           03 FILLER               PIC X(8)   VALUE ' Length='.
           03 DRL-LENGTH           PIC 9(5)   VALUE ZEROES.
           03 FILLER               PIC X(1)   VALUE SPACES.
           03 DRL-RECORDNAME       PIC X(30)  VALUE SPACES.
           03 FILLER               PIC X(1)   VALUE SPACES.
51         03 DRLC-COND            pic x(64)  value spaces.
           03 filler               pic x(26)  value spaces.


      *-------------------------------------------------------------
      * This area is for the UpdateV routine.
      *-------------------------------------------------------------
       01  uvMisc-area.
           03 uvCursor         pic 9(9) comp-5 value 0.
           03 uvFPtr           pic 9(9) comp value 0.
           03 uvPreT           pic 9(6) comp value 0.
           03 uvSave-FldInx    pic 9(9) comp value 0.
           03 uvSave-vFldInx   pic 9(9) comp value 0.
           03 uvSave-PretInx   pic 9(9) comp value 0.
           03 uvItem-just-left pic 9(6) comp-5 value 0.
           03 uvvCur         pic 9(5) comp value 0.
           03 uvvLen         pic 9(5) comp value 0.
           03 uvvPos         pic 9(5) comp value 0.
           03 uvvNib         pic 9(1)      value 0.
           03 uvvBit         pic 9(1)      value 0.
           03 uvsPos         pic 9(5) comp value 0.
           03 uvlPos         pic 9(5) comp value 0.
           03 uvePos         pic 9(9) comp value 0.
           03 uvsNib         pic 9(1)      value 0.
           03 uvsBit         pic 9(1)      value 0.
           03 uvsLen         pic 9(5) comp value 0.
           03 uv-Num5        pic 9(5)        value 0.
           03 uvFoundAt      pic 9(6) comp-5 value 0.
               88 uvField-Not-Found          value 0.
               88 uvField-Found              values 1 thru 99999.
           03 uvOld-FoundAt  pic 9(6) comp-5 value 0.
           03 uvClosestFound pic 9(6) comp-5 value 0.
               88 uvClosest-Not-Found        value 0.
               88 uvClosest-Found            values 1 thru 99999.
           03 uvStart-Search pic 9(6) comp-5 value 0.
           03 uvStop-Search  pic 9(6) comp-5 value 0.
           03 uvSearch-Sw    pic 9(2) comp-5 value 0.
              88 uvStop-Searching            value 0.
              88 uvStart-Searching           value 1.
           03 uvResolve-Sw   pic 9(2) comp-5 value 0.
              88 uvisNotResolved             value 0.
              88 uvisResolved                value 1.
           03 uvFindFld-sw   pic 9(2) comp-5 value 0.
              88 uvFindFld-Normal            value 0.
              88 uvFindFld-Base              value 1.
           03 uvMinTmp       pic 9(6) comp-5 value 0.
           03 uvFoundPret    pic 9(6) comp-5 value 0.
           03 uvUsedPret     pic 9(6) comp-5 value 0.
           03 uvUsedHash     pic 9(6) comp-5 value 0.
           03 uvUserPret     pic 9(6) comp-5 value 0.
           03 uvSearch-Field pic 9(6) comp-5 value 0.
           03 uv-Count       pic 9(9) comp-5 value 0.
           03 uvwDidCount    pic 9(2) comp-5 value 0.
           03 uvTblPtr       pic 9(5)      value 0.
           03 uv-num18s-X.
             05 uv-num18s    pic s9(18)    value 0.
           03 uv-num18-X redefines uv-num18s-X.
             05 uv-num18     pic 9(18).
           03 uvwPos         pic 9(5) comp value 0.
           03 uvwLen         pic 9(5) comp value 0.
           03 uvSub          pic 9(6) comp-5 value 0.
           03 uvPtr-Stop     pic 9(6) comp-5 value 0.
           03 uv-Level       pic 9(4) comp-5 value 0.
           03 uv-Parent      pic 9(5) comp value 0.
           03 uv-Dim         pic 9(4) comp value 0.
           03 uvBase-Addr    pic 9(5) comp value 0.
           03 uv-Indx        pic x(44)     value spaces.
           03 uv-Bytes             pic 9(4) comp value 0.
           03 uv-Pos               PIC 9(5) COMP VALUE 0.
v0850      03 uvDL-Value           PIC X(256)    VALUE SPACES.
v0850      03 uv-Value             PIC X(256)    VALUE SPACES.
v0850      03 Filler  redefines uv-Value.
v0850         05 uv-Value-NUM18   PIC 9(18).
v0850         05 FILLER           PIC X(238).
v0850      03 Filler  redefines uv-Value.
v0850         05 FILLER           PIC X(8).
v0850         05 uv-Value-NUM8    PIC 9(8).
v0850         05 FILLER           PIC X(238).
v0850      03 Filler  redefines uv-Value.
v0850         05 FILLER           PIC X(14).
v0850         05 uv-Value-NUM4    PIC 9(4).
v0850         05 FILLER           PIC X(238).
v0850      03 Filler  redefines uv-Value.
v0850         05 uv-Value-NUM18S  PIC S9(18).
v0850         05 FILLER           PIC X(238).
v0850      03 Filler  redefines uv-Value.
v0850         05 FILLER           PIC X(8).
v0850         05 uv-Value-NUM8S   PIC S9(8).
v0850         05 FILLER           PIC X(238).
v0850      03 Filler  redefines uv-Value.
v0850         05 FILLER           PIC X(14).
v0850         05 uv-Value-NUM4S   PIC 9(4).
v0850         05 FILLER           PIC X(238).
v0850      03 Uv-Value-PD18X  redefines uv-Value.
v0850         05 uv-Value-PD18    PIC 9(18) comp-3.
v0850         05 FILLER           PIC X(246).
v0850      03 Uv-Value-BIN18X  redefines uv-Value.
v0850         05 uv-Value-BIN18   PIC 9(18) comp-3.
v0850         05 FILLER           PIC X(246).
           03 uv-EDIT-NUM18S       PIC 9(18)-.
           03 uvConstantValue      pic s9(18) comp value 0.
           03 uvSearch-Tbl.
             05 uvSearch-Index occurs 9 times pic 9(5) comp value 0.
           03 uvOnes-Tbl.
             05 uvOnes-Index   occurs 9 times pic 9(5) comp value 1.
           03 uvResolved        pic 9(4) comp value 0.
v00872     03 uvcLen         pic 9(9) comp value 0.
v00878     03 uvFieldsResolved-cnt    pic 9(9) comp value 0.
v00878     03 uvAttemptResolve-cnt    pic 9(9) comp value 0.
v00878     03 uvFieldValueInvalid-cnt pic 9(9) comp value 0.

v00872* Working-storage area for the MIGRATE routine
v00872 01  mgBuffer     pic x(327520) value low-values.

       01  mgMISC-WORK-AREA.
           03 mgVersion            pic x(8)      value '0.3.25'.
           03 Migrate-Command      pic 9(2) comp-5 value 0.
              88 Open-MIG-File                value 0.
              88 Move-Source-to-Target        value 1.
              88 Write-MIG-Record       value 2.
              88 Close-MIG-File         value 3.
              88 Report-Stats-Back      value 4.
              88 Start-Target-Record    value 6.
              88 Resolve-vFields        value 7.
           03 mgMax-Out-Len        pic 9(5) comp
                                            value :MaxOBuff:.
v00878     03 mgMax-Buffer-Len     pic 9(9) value 327670.
           03 MIG-OPEN-SW          PIC 9(4) COMP VALUE 0.
              88 MIG-NOT-OPEN                    VALUE 0.
              88 MIG-OPEN                        VALUE 1.
           03 mgIO-STATUS          PIC X(2)      VALUE '00'.
v00886     03 EEOR-IO-STATUS       PIC X(2)      VALUE '00'.
           03 mgBit-Basis          pic 9(4) comp value 0.
           03 mgSave-Cursor        pic 9(5) comp value 0.
           03 mgKAMPOYR-value      pic 9(4) comp value 0.

v0850      03 mg-Value             PIC X(256)    VALUE SPACES.
v0850      03 Filler  redefines mg-Value.
v0850         05  mg-Value-NUM18   PIC 9(18).
v0850         05  FILLER           PIC X(238).
v0850      03 Filler  redefines mg-Value.
v0850         05  FILLER           PIC X(8).
v0850         05  mg-Value-NUM8    PIC 9(8).
v0850         05  FILLER           PIC X(240).
v0850      03 Filler  redefines mg-Value.
v0850         05  FILLER           PIC X(14).
v0850         05  mg-Value-NUM4    PIC 9(4).
v0850         05  FILLER           PIC X(238).
v0850      03 Filler  redefines mg-Value.
v0850         05  mg-Value-NUM18S  PIC S9(18).
v0850         05  FILLER           PIC X(238).
v0850      03 Filler  redefines mg-Value.
v0850         05  FILLER           PIC X(8).
v0850         05  mg-Value-NUM8S   PIC S9(8).
v0850         05  FILLER           PIC X(240).
v0850      03 Filler  redefines mg-Value.
v0850         05  FILLER           PIC X(14).
v0850         05  mg-Value-NUM4S   PIC 9(4).
v0850         05  FILLER           PIC X(238).
           03 mg-Bytes             pic 9(5) comp value 0.
           03 mg-PD-valuex.
              05  mg-PD-value      pic 9(18)  comp-3 value 0.
              05  mg-PDs-value     redefines mg-PD-value
                                    pic s9(18) comp-3.
           03 mg-BN-valuex.
              05  mg-BN-value      pic s9(18) comp   value 0.
           03 mg-ZD-valuex.
              05  mg-ZD-value      pic 9(18)         value 0.
              05  mg-ZDs-value     redefines mg-ZD-value
                                    pic s9(18).
CW0315     03 mg-LS-valuex.
CW0315        05  mg-LS-value      pic S9(18)
CW0315                                 SIGN LEADING SEPARATE
CW0315                                 VALUE 0.
CW0315     03 mg-TS-valuex.
CW0315        05  mg-TS-value      pic S9(18)
CW0315                                 SIGN TRAILING SEPARATE
CW0315                                 VALUE 0.
           03 mg-BIT-valuex.
              05  mg-BIT-value     pic X(8)          value spaces.
           03 mgTblPtr             pic 9(4) comp value 0.
           03 mg-POS            pic 9(9) comp value 0.
           03 mg-Len            pic 9(9) comp value 0.
           03 mg-Len2           pic 9(9) comp value 0.
           03 mg-Len3           pic 9(9) comp value 0.
           03 mgsLen            pic 9(9) comp value 0.
           03 mgFPtr            pic 9(9) comp value 0.
           03 mgFLDT            pic 9(6) comp value 0.
           03 mgPreT            pic 9(5) comp value 0.
           03 tmgPreT           pic 9(9) comp value 0.
           03 mgCode            pic 9         value 0.
              88 mgCode-LowVal                value 0.
              88 mgCode-Space                 value 1.
              88 mgCode-HighVal-Kept          value 8.
              88 mgCode-HighVal               value 9.
           03 mgFunc            pic 9(9) comp value 0.
           03 mgResolved        pic 9(4) comp value 0.
           03 mgResolve-sw        pic 9(4) comp value 0.
              88 mgNothing-was-resolved         value 0.
              88 mgSomething-was-resolved       value 1.
           03 mgResolve-Attempts pic 9(4) comp value 0.
           03 mgSearch-sw         pic 9(2) comp value 0.
              88 mgStop-Searching               value 0.
              88 mgStart-Searching              value 1.
           03 mgStart           pic 9(9) comp value 0.
           03 mgProgram-mode-sw   pic 9(4) comp value 0.
               88 mgNormal-mode                 value 0.
               88 mgResolve-mode               value 1.
v0854      03 mgLastPos         pic 9(9) comp value 0.
v0854      03 mgN               pic 9(9) comp value 0.
           03 mgLen             pic 9(9) comp value 0.
v00878     03 mgPos             pic 9(9) comp value 0.
v00860     03 mgBadZD           pic 9         value 0.
v00860     03 mgFirst-Discard-Code pic 9(4) comp value 0.
v00882     03 mgFirst-Overflow     pic 9(4) comp value 0.
           03 mgFmt             pic x(40)        value spaces.
           03 mgSubS            pic 9(4) comp value 0.
           03 mgSubT            pic 9(4) comp value 0.

      * fields for API calls within Migrate for Client: Kampo
       01  Code-Conv-Request-Packet.
           03 Code-Conv-Process-Class              PIC  X(8).
           03 Code-Conv-Request-Option             PIC  X(1).
           03 Specified-ConmgVersion-Code.
              05 one-byte-code                     PIC  X(1).
              05 two-byte-code                     PIC  X(2).
           03 Code-Conv-Target-Data-Info.
              05 Code-Conv-Target-Data-Length      PIC S9(9) COMP.
              05 Target-Data-Storage-Address       POINTER.
           03 Post-Code-Conv-Data-Info.
              05 Post-Code-Conv-Data-LengthX.
                07 Post-Code-Conv-Data-Length        PIC S9(9) COMP.
              05 Post-Conv-Data-Storage-Address    POINTER.
           03 Code-Inconvert-Detection-Pos         PIC S9(9) COMP.
           03 Code-Return-Code                     PIC  X(4).
      * fields for API calls within Migrate for Client: Kampo
       01  Target-Data-Storage.
           03  FILLER                         PIC  X(10000).
      * fields for API calls within Migrate for Client: Kampo
       01  Post-conv-Data-Storage.
           03  FILLER                         PIC  X(32000).
       01  CodeConvErrorInfo.
           03  Maximum-Number-of-error-entry  pic 9(4) comp value 0.
           03  Number-of-error-conversion     pic 9(4) comp value 0.
           03  Error-Character-info-Area occurs 0 to 9999 times
                                depending on Number-of-error-conversion.
v00877         05  CCEI-Character-code        pic X(2)      value space.
               05  CCEI-Offset                pic 9(4) comp value 0.

       copy APISplit.
v00881 01  API-Buffer         pic x(327680) value low-values.

      *end of working-storage for Migrate

      * storage area for the GetLen routine
       01  glMISC-AREA.
           03 glFPtr          PIC 9(6) COMP VALUE 0.
           03 glTblPtr        PIC 9(5) COMP VALUE 0.
           03 glValueOf-Len   Pic 9(5) comp value 0.
           03 glAddrOf-Len    pic 9(5) comp value 0.
           03 glAddrOf-Data   pic 9(5) comp value 0.
           03 glSearchField   pic 9(6) comp value 0.
v0850      03  gl-Value             PIC X(256)    VALUE SPACES.
v0850      03  gl-Value-NUM redefines gl-Value.
v0850          05  gl-Value-NUM18   PIC 9(18).
v0850          05  FILLER           PIC X(238).
v0850      03  FILLER redefines gl-Value.
v0850          05  FILLER           PIC X(8).
v0850          05  gl-Value-NUM8    PIC 9(8).
v0850          05  FILLER           PIC X(240).
v0850      03  FILLER redefines gl-Value.
v0850          05  FILLER           PIC X(14).
v0850          05  gl-Value-NUM4    PIC 9(4).
v0850          05  FILLER           PIC X(238).
v0850      03  FILLER redefines gl-Value.
v0850          05  gl-Value-NUM18S  PIC S9(18).
v0850          05  FILLER           PIC X(238).
v0850      03  FILLER redefines gl-Value.
v0850          05  FILLER           PIC X(8).
v0850          05  gl-Value-NUM8S   PIC S9(8).
v0850          05  FILLER           PIC X(240).
v0850      03  FILLER redefines gl-Value.
v0850          05  FILLER           PIC X(14).
v0850          05  gl-Value-NUM4S   PIC S9(4).
v0850          05  FILLER           PIC X(238).
           03  gl-Number1           PIC 9(5)  COMP VALUE ZEROES.
           03  gl-Number2           PIC 9(5)  COMP VALUE ZEROES.
           03  glEditNumber   pic 9(5)-.
           03  gl-Base              pic 9(9)  comp value 0.

      * Storage area for the GetValue routine
       01  gv-misc-area.
           03 gvPos     PIC 9(9) COMP VALUE 0.
           03 gvLen     PIC 9(9) COMP VALUE 0.
           03 gvBit     PIC 9(4) COMP VALUE 0.
           03 gvPtr     PIC 9(6) COMP VALUE 0.
           03 gvPret    PIC 9(6) COMP VALUE 0.
           03 gvType    PIC 9(4) COMP VALUE 0.

      * Storage area for the LogShowDLL routine
       01 LogShowDLL-Misc-Area.
           03 bit-nib           pic x           value space.
           03 tLen              pic s9(9) comp  value 0.
           03 ws-num18S-x.
              05 ws-num18S      pic z(17)9.
           03  wField.
             05 wLevel          pic z9.
             05 filler          pic x(2)       value spaces.
             05 wFieldName      pic x(44)      value spaces.
           03 Record-cond       pic x(128)     value spaces.
           03 NoShowFields.
             05 FirstNoShow     pic 9(5) comp  value 0.
             05 LastNoShow      pic 9(5) comp  value 0.
             05 NumberOfNoShows pic 9(5) comp  value 0.

       01  LOG-CONTROL-INDENT.
           03  FILLER               PIC X(151) VALUE SPACES.

       01  LOG-CONTROL-DLL.
           03  filler           pic x(8)  value 'DLL Ver:'.
           03  LCD-Version      pic x(8)  value spaces.
           03  filler           pic x(9)  value ' GenDate:'.
           03  LCD-GenDate      pic x(10) value spaces.
           03  filler           pic x(9)  value ' GenTime:'.
           03  LCD-GenTime      pic x(8)  value spaces.
           03  filler           pic x(9)  value ' GenUser:'.
           03  LCD-GenUser      pic x(10) value spaces.
           03  filler           pic x(9)  value ' GenTool:'.
           03  LCD-GenTool      pic x(10) value spaces.

       01  LOG-CONTROL-DLL2.
           03  filler               pic x(4)  value 'Opt:'.
           03  filler               pic x(08) value 'Defined:'.
           03  filler               pic x(8)  value ' Fields:'.
           03  LCD-DefinedFields    pic Z(8)9.
           03  filler               pic x(8)  value ' Tables:'.
           03  LCD-DefinedTables    pic Z(8)9.
           03  filler               pic x(8)  value ' MaxRec:'.
           03  LCD-MaxRecordFields  pic Z(8)9.
           03  filler               pic x(5)  value ' Ops:'.
           03  LCD-DefinedOps       pic Z(8)9.
           03  filler               pic x(7)  value ' Conds:'.
           03  LCD-DefinedConds     pic Z(8)9.
           03  filler               pic x(7)  value ' Funcs:'.
           03  LCD-DefinedFuncs     pic Z(8)9.
           03  filler               pic x(4)  value ' OE:'.
v00887     03  LCD-OutputEditX.
v00887       05  LCD-OutputEdit     pic Z(8)9.

       01  LOG-Column.
           03 LC-Cnt            pic X(6)  value 'PreT'.
           03 filler            pic x(2)  value spaces.
           03 LC-Command        pic x(7)  value 'Command'.
           03 filler            pic x(1)  value spaces.
           03 LC-Field          pic x(48) value 'Field'.
           03 filler            pic x     value spaces.
           03 LC-Attrib         pic x(27) value 'Attribute'.
           03 filler            pic x     value spaces.
           03 LC-Owner          pic x(5)  value 'Owner'.
           03 filler            pic x     value spaces.
           03 LC-Event          pic x(30) value
                                       'Event-------------------------'.

       01  LOG-Detail.
           02 LD-Cnt-Command-Field.
             03 LD-Cnt          pic z(6)  value zeroes.
             03 filler          pic x(2)  value spaces.
             03 LD-Command      pic x(7)  value spaces.
             03 filler          pic x(1)  value spaces.
             03 LD-FieldName    pic x(48) value spaces.
             03 filler          pic x     value spaces.
           02 LD-Attrib         pic x(27) value spaces.
           02 filler            pic x     value spaces.
           02 LD-Owner-Event.
             03 LD-Owner        pic z(4)9 value zeroes.
             03 filler          pic x(1)  value spaces.
             03 LD-Event        pic x(30) value spaces.
       01  LOG-Remarks.
           02 filler            pic x(16) value spaces.
           02 LR-Text           pic x(64) value spaces.

       01  LOG-Detail-NoShow.
           03 LDN-skipped       pic z(6)  value zeroes.
           03 filler            pic x(13) value '    Not Shown'.


      * Storage area for the GetBuff routine
       copy bitarray.
       01  gbMISC-AREA.
           03  gb-Index             PIC 9(4) COMP VALUE 0.
           03  Filler  REDEFINES gb-Index.
               05  gb-index-1       PIC X.
               05  gb-Index-2       PIC X.
           03 gbwLen   PIC 9(9) COMP VALUE 0.
           03  gbLast-Byte-Area.
               05 filler            pic x(1)  value low-values.
               05 gbLast-Byte         pic x(1)  value space.
MFMFMF            88 Byte-Unsigned        value x'F0'.
MFMFMF            88 Byte-Positive        value x'C0'.
MFMFMF            88 Byte-Negative        value x'D0'.
PCPCPC*           88 Byte-Unsigned        value x'F0'.
PCPCPC*           88 Byte-Positive        value x'C0'.
PCPCPC*           88 Byte-Negative        value x'D0'.
           03  gbLast-Byte-Num redefines gbLast-Byte-Area
                                    pic 9(2) comp-5.
           03  gb-Hex               PIC X(80) VALUE SPACES.
v0850      03  gb-Value-area        pic x(256)    value spaces.
v0850      03  gb-Value-NUM      redefines gb-Value-area.
v0850          05  gb-Value-NUM18   PIC 9(18).
v0850          05  FILLER           PIC X(238).
v0850      03  FILLER            redefines gb-Value-area.
v0850          05  FILLER           PIC X(8).
v0850          05  gb-Value-NUM8    PIC 9(8).
v0850          05  FILLER           PIC X(240).
v0850      03  FILLER            redefines gb-Value-area.
v0850          05  FILLER           PIC X(14).
v0850          05  gb-Value-NUM4    PIC 9(4).
v0850          05  FILLER           PIC X(238).
v0850      03  FILLER             redefines gb-Value-area.
v0850          05  FILLER           PIC X(17).
v0850          05  gb-Value-NUM1    PIC 9(1).
v0850          05  FILLER           PIC X(238).
v0850      03  FILLER               redefines gb-Value-area.
v0850          05  gb-Value-NUM18S  PIC S9(18).
v0850          05  FILLER           PIC X(238).
v0850      03  FILLER            redefines gb-Value-area.
v0850          05  FILLER           PIC X(8).
v0850          05  gb-Value-NUM8S   PIC S9(8).
v0850          05  FILLER           PIC X(240).
v0850      03  FILLER            redefines gb-Value-area.
v0850          05  FILLER           PIC X(14).
v0850          05  gb-Value-NUM4S   PIC 9(4).
v0850          05  FILLER           PIC X(238).
           03  gb-BitS.
               05  gb-Bit OCCURS 8 TIMES PIC 9(1) VALUE 0.
           03  gbUSAGE-PD           PIC 9(4) COMP VALUE 3.
           03  gbUSAGE-BIN          PIC 9(4) COMP VALUE 2.
           03  gbUSAGE-ALPHANUM     PIC 9(4) COMP VALUE 1.
           03  gbUSAGE-ZD           PIC 9(4) COMP VALUE 0.
CW0315     03  gbUSAGE-LS           PIC 9(4) COMP VALUE 0.
CW0315     03  gbUSAGE-TS           PIC 9(4) COMP VALUE 0.
v0854      03  gbUnknownSign        pic 9(4) comp value 2.
           03  gbNot-Signed         PIC 9(4) COMP VALUE 0.
           03  gbSigned             PIC 9(4) COMP VALUE 1.
           03  gbSignSignal         pic 9(4) comp value 0.
           03  gbBuffer             pic 9(2) comp-5 value 0.
      *Fields passed to/from this routine
      *        Position fields
           03  gbPos     PIC 9(9) COMP VALUE 0.
           03  gbPosNib  Pic 9         value 0.
           03  gbPosBit  Pic 9         value 0.
v00894     03  gbEnd     PIC 9(9) COMP VALUE 0.
      *        Length   fields
           03  gbLen     PIC 9(9) COMP VALUE 0.
           03  gbLenNib  PIC 9         VALUE 0.
           03  gbLenBit  PIC 9         VALUE 0.
      *        Type of field to convert from/to
           03  gbType    PIC 9(4) COMP VALUE 0.
                88  gbTYPE-UNKNOWN                 VALUE 0.
                88  gbTYPE-CH                      VALUE 1.
                88  gbTYPE-PD                      VALUE 2.
                88  gbTYPE-ZD                      VALUE 3.
                88  gbTYPE-BIN                     VALUE 4.
                88  gbTYPE-BIT                     VALUE 5.
                88  gbTYPE-NIB                     VALUE 6.
                88  gbTYPE-PD-NEC                  VALUE 7.
                88  gbTYPE-PD-NEC4                 VALUE 8.
                88  gbTYPE-BIS                     VALUE 9.
v0854      03  gbField   PIC 9(6) COMP VALUE 0.

      * Storage area for the write-trace routine
       01  Write-Trace-Misc-Area.
           03  Trace-Write-Command  pic 9(4) comp value 0.
             88 Open-Trace-File              value 0.
             88 Write-Trace-FldT             value 1.
             88 Close-Trace-File             value 2.
           03  Trace-OPEN-SW        PIC 9(4) COMP VALUE 0.
             88 Trace-NOT-OPEN                    VALUE 0.
             88 Trace-OPEN                        VALUE 1.
           03  root                 pic 9         value zeroes.
           03  ws-dims              pic 9(4) comp value zeroes.
           03  OUT-LEN-T            pic 9(5) comp value zeroes.
       01  Trace-Field-Start.
           05 Filler                pic x(8)      value 'Record #'.
           05 TFS-rec-cnt           pic 9(9)      value zeroes.
       01  TRACE-FIELD-col.
           05 TFLDC-PTR             PIC X(5)      VALUE 'FPTR '.
           05 FILLER                PIC X         VALUE ' '.
           05 TFLDC-FIELD           PIC X(5)      VALUE 'Pret-'.
           05 FILLER                PIC X         VALUE SPACE.
           05 TFLDC-FieldName       PIC X(44)     VALUE 'FieldName'.
           05 FILLER                PIC X         VALUE spaces.
           05 FILLER                PIC X         VALUE '<'.
           05 TFLDC-START-BYTE      PIC X(5)      VALUE 'START'.
           05 FILLER                PIC X         VALUE '.'.
           05 TFLDC-START-BIT       PIC X         VALUE 'B'.
           05 FILLER                PIC X         VALUE '>'.
           05 FILLER                PIC X         VALUE SPACE.
           05 FILLER                PIC X         VALUE '<'.
           05 TFLDC-LENGTH-BYTE     PIC X(5)      VALUE 'LENGT'.
           05 FILLER                PIC X         VALUE '.'.
           05 TFLDC-LENGTH-BIT      PIC X         VALUE 'B'.
           05 FILLER                PIC X         VALUE '>'.
           05 FILLER                PIC X         VALUE SPACE.
           05 TFLDC-Target-Start    PIC X(5)      VALUE 'T-STR'.
           05 FILLER                PIC X         VALUE SPACE.
           05 TFLDC-Target-Length   PIC X(5)      VALUE 'T-LEN'.
           05 FILLER                PIC X         VALUE SPACE.
           05 TFLDC-vField-Sw       PIC X         value 'V'.
           05 TFLDC-Occurred-Resolved-Sw         PIC X   value 'O'.
           05 FILLER                PIC X         VALUE SPACE.
           05 TFLDC-Hash            pic x         value 'H'.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDC-Level           pic x(2)      value 'Lv'.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDC-EntryLength     pic X(9)      value 'EntryLeng'.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDC-First           pic x(5)      value 'Start'.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDC-Last            pic x(5)      value 'Last '.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDC-Root            PIC X(6)      VALUE 'Root  '.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDC-Type            PIC X         VALUE 'T'.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDC-Next            pic x(5)      value 'Next '.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDC-Prior           pic x(5)      value 'Prior'.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDC-vNext           pic x(5)      value 'vNext'.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDC-vPrior          pic x(5)      value 'vPrio'.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDC-Parent          pic x(6)      value 'HasPar'.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDC-NextSib         pic x(6)      value 'NextSb'.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDC-PrevSib         pic x(6)      value 'PrevSb'.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDC-FirstChild      pic x(6)      value '1stChd'.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDC-LastChild       pic x(6)      value 'LstChd'.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDC-LastGhost       pic x(6)      value 'Ghost '.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDC-UsedNIC         pic x(6)      value 'UsdNIC'.

       01  TRACE-FIELD-AREA.
           05 TFLDT-PTR             PIC 9(5)      VALUE ZEROES.
           05 FILLER                PIC X         VALUE ' '.
           05 TFLDT-FIELD           PIC 9(5)      VALUE ZEROES.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDT-FieldName       pic x(44)     value spaces.
           05 FILLER                PIC X         VALUE SPACE.
           05 FILLER                PIC X         VALUE '<'.
           05 TFLDT-START-BYTE      PIC 9(5)      VALUE ZEROES.
           05 FILLER                PIC X         VALUE '.'.
           05 TFLDT-START-BIT       PIC 9         VALUE ZEROES.
           05 FILLER                PIC X         VALUE '>'.
           05 FILLER                PIC X         VALUE SPACE.
           05 FILLER                PIC X         VALUE '<'.
           05 TFLDT-LENGTH-BYTE     PIC 9(5)      VALUE ZEROES.
           05 FILLER                PIC X         VALUE '.'.
           05 TFLDT-LENGTH-BIT      PIC 9         VALUE ZEROES.
           05 FILLER                PIC X         VALUE '>'.
           05 FILLER                PIC X         VALUE SPACE.
           05 TFLDT-Target-Start    PIC 9(5)      VALUE ZEROES.
           05 FILLER                PIC X         VALUE SPACE.
           05 TFLDT-Target-Length   PIC 9(5)      VALUE ZEROES.
           05 FILLER                PIC X         VALUE SPACE.
           05 TFLDT-vField-Sw       PIC X         value spaces.
           05 TFLDT-Field-Resolved-Sw            PIC X   value '-'.
           05 FILLER                PIC X         VALUE SPACE.
           05 TFLDT-Hash-x.
              07 TFLDT-Hash         pic 9         value zeroes.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDT-Level           pic 9(2)      value zeroes.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDT-EntryLength     pic 9(9)      value zeroes.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDT-Start           pic 9(5)      value zeroes.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDT-Last            pic 9(5)      value zeroes.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDT-Root            PIC 9(6)      VALUE zeroes.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDT-Type            PIC x         VALUE spaces.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDT-Next            pic 9(5)      value zeroes.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDT-Prior           pic 9(5)      value zeroes.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDT-vNext           pic 9(5)      value zeroes.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDT-vPrior          pic 9(5)      value zeroes.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDT-Parent          pic 9(6)      value zeroes.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDT-NextSib         pic 9(6)      value zeroes.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDT-PrevSib         pic 9(6)      value zeroes.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDT-FirstChild      pic 9(6)      value zeroes.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDT-LastChild       pic 9(6)      value zeroes.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDT-LastGhost       pic 9(6)      value zeroes.
           05 FILLER                PIC X         VALUE spaces.
           05 TFLDT-UsedNIC         pic 9(6)      value zeroes.
      * Trace the Condition Table area
       01 Trace-Cond-Heading.
          03 filler                     pic x     value '"'.
          03 tch-text                   pic x(30) value spaces.
          03 filler                     pic x     value space.
          03 tch-value                  pic 9(9)  value zeroes.
          03 filler                     pic x     value '"'.
       01 Trace-Cond-Cond-Col.
          03 tccc-Cond-Ptr             pic x(9) value 'Cond-Ptr-'.
          03 filler                    pic x    value space.
          03 tccc-ExecutNdx            pic x(9) value 'ExecutNdx'.
          03 filler                    pic x    value space.
          03 tccc-ExecutQty            pic x(9) value 'ExecutQty'.
          03 filler                    pic x    value space.
          03 tccc-ResultNdx            pic x(9) value 'ResultNdx'.
          03 filler                    pic x    value space.
          03 tccc-Formula              pic x(15)  value
                                       'Formula--------'.
       01 Trace-Cond-Cond-Detail.
          03 tccd-details.
            05 tccd-Cond-Ptr         pic 9(9) value 0.
            05 filler                pic x    value space.
            05 tccd-ExecutNdx        pic 9(9) value 0.
            05 filler                pic x    value space.
            05 tccd-ExecutQty        pic 9(9) value 0.
            05 filler                pic x    value space.
            05 tccd-ResultNdx        pic 9(9) value 0.
            05 filler                pic x    value space.
          03 tccd-Formula              pic x(768) value spaces.
       01 Trace-Cond-Exec-Col.
          03 tcec-ndx                  pic x(6) value 'Ex-Ndx'.
          03 filler                    pic x    value space.
          03 tcec-ExecutionList        pic x(9) value 'Exec-List'.
       01 Trace-Cond-Exec-Detail.
          03 tced-ndx                  pic 9(6) value 0.
          03 filler                    pic x    value space.
          03 tced-ExecutionList        pic 9(9) value 0.
       01 Trace-Cond-Formula-Col.
          03 tcfc-ndx                     pic x(6)  value 'fNdx'.
          03 filler                       pic x     value spaces.
          03 tcfc-FAId                    pic x(4)  value 'FAId'.
          03 filler                       pic x     value spaces.
          03 tcfc-FAType                  pic x(2)  value 'Ty'.
          03 filler                       pic xx    value spaces.
          03 tcfc-misc                    pic x(20) value 'Misc'.
          03 filler                       pic xx    value spaces.
          03 tcfc-FAPriority              pic x(6)  value 'Priort'.
          03 filler                       pic x     value spaces.
          03 tcfc-FAOperandAvail-SW       pic x     value 'D'.
          03 filler                       pic x     value spaces.
          03 tcfc-FAOperatorAvail-SW      pic x     value 'R'.
          03 filler                       pic x     value spaces.
          03 tcfc-FAParmQty               pic x(6)  value 'PrmQty'.
          03 filler                       pic x     value spaces.
          03 tcfc-FAParmNdx1              pic x(6)  value 'Parmx1'.
          03 filler                       pic x     value spaces.
          03 tcfc-FAParmNdx2              pic x(6)  value 'Parmx2'.
          03 filler                       pic x     value spaces.
          03 tcfc-FALength                pic x(5)  value 'Lengt'.
       01 Trace-Cond-Formula-Detail.
          03 tcfd-ndx                     pic 9(6)  value 0.
          03 filler                       pic x     value spaces.
          03 tcfd-FAId                    pic 9(4)  value 0.
          03 filler                       pic x     value spaces.
          03 tcfd-FAType                  pic 9(2)  value 0.
          03 filler                       pic xx    value ' "'.
          03 tcfd-misc                    pic x(20) value spaces.
          03 filler                       pic xx    value '" '.
          03 tcfd-FAPriority              pic 9(6)  value 0.
          03 filler                       pic x     value spaces.
          03 tcfd-FAOperandAvail-SW       pic 9     value 0.
          03 filler                       pic x     value spaces.
          03 tcfd-FAOperatorAvail-SW      pic 9     value 0.
          03 filler                       pic x     value spaces.
          03 tcfd-FAParmQty               pic 9(6)  value 0.
          03 filler                       pic x     value spaces.
          03 tcfd-FAParmNdx1              pic 9(6)  value 0.
          03 filler                       pic x     value spaces.
          03 tcfd-FAParmNdx2              pic 9(6)  value 0.
          03 filler                       pic x     value spaces.
          03 tcfd-FALength                pic 9(5)  value 0.

      * Storage area for the ChkCond routine
       77  ExecutionStopAt  pic 9(9) comp-5 value 0.
       01  Chkcond-var-name.
           03 cvn-field          pic z(6).
           03 filler             pic x    value '-'.
           03 cvn-fieldname      pic x(30).

       01  Chkcond-trace-header.
           03 cth-ELNdx          pic x(6)  value 'ELNdx'.
           03 filler             pic x     value space.
           03 cth-FAID           pic x(6)  value 'FAId'.
           03 filler             pic x     value space.
           03 cth-FAType         pic x(10) value 'FAType----'.
           03 filler             pic x     value space.
           03 cth-FAPriority     pic x(4)  value 'Prio'.
           03 filler             pic x     value space.
           03 cth-misc           pic x(30) value 'Misc----------------'.
           03 filler             pic x     value space.
           03 cth-FAParmNdx1     pic x(6)  value 'Parm1-'.
           03 filler             pic x     value space.
           03 cth-FAParmNdx2     pic x(6)  value 'Parm2-'.
       01  Chkcond-trace-line.
           03 ctl-ELNdx                     pic 9(6).
           03 filler                        pic x     value space.
           03 ctl-FAID                      pic 9(6).
           03 filler                        pic x     value space.
           03 ctl-FAType                    pic x(10).
           03 filler                        pic x     value space.
           03 ctl-FAPriority                pic 9(4).
           03 filler                        pic x     value space.
           03 ctl-misc                      pic x(30).
           03 filler                        pic x     value space.
           03 ctl-FAParmNdx1                pic 9(6).
           03 filler                        pic x     value space.
           03 ctl-FAParmNdx2                pic 9(6).

v00886*fields to suport Kampo API EEOR
v00886 01  EEOR-Work-area.
v00886     03  EEOR-RECORD-LEN OCCURS 10 TIMES  PIC S9(4) COMP.
v00886     03  EEOR-RECORD     OCCURS 10 TIMES  PIC  X(32768).
v00886 copy LnkEEOR.

v00890 copy LnkMap.

      * The DateFMT-Table-Area is populated within LoadDLL routine
       copy LnkDateF.

      *--------------------------------------------------------------
      *Allocate storage for PC only.
      * Until I can figure out a GetMain for the PC.
      *--------------------------------------------------------------
PCPCPC*01  Storage-Pret               pic x(24800012).
PCPCPC*01  Storage-Pre-Chain          pic x(1600000).
PCPCPC*01  Storage-Table              pic x(649947).
PCPCPC*01  Storage-Fld                pic x(12999878).
PCPCPC*01  Storage-Cond               pic x(78527).
PCPCPC*01  Storage-Func               pic x(40602).
PCPCPC*01  Storage-Formula            pic x(4939506).
PCPCPC*01  Storage-FormExec           pic x(39996).
PCPCPC*01  Storage-Log                pic x(1800012).
PCPCPC*01  Storage-Alt1               pic x(97914).
PCPCPC*01  Storage-Alt2               pic x(97914).
PCPCPC*01  Storage-DAR                pic x(99000000).
PCPCPC*01  Storage-Sample             pic x(160118244).
PCPCPC*01  Storage-Others             pic x(26418004).
PCPCPC*01  Storage-KeyList            pic x(4800004).

       copy gwrp001a.
       LINKAGE SECTION.
       01  JCL-PARM.
           03 JP-LENGTH   PIC 9(4) COMP.
           03 JP-TRACE    PIC X(5).

       COPY LNKPRET.
v0854  COPY LnkChain.
       COPY LNKTBL.
       COPY LNKFLD.
       COPY LNKCOND.
       COPY LnkFunc.
       copy lnkform.
       copy LNKDAR.
       copy LNKLOG.
v00890 copy LNKALT.

       PROCEDURE DIVISION using JCL-PARM.
           call 'HKSys' using Start-CPUTimer
           perform SHOW-COPYRIGHT
           perform Initialization
           perform Check-JCL-Parm
           perform Get-SysInfo
           perform START-LOG-FILE
           call 'HKSys' using Print-CPUTimer
V0.3       call 'LoadDLL' using DLL-Area
              FILE-TABLE-AREA, Record-Table-Area,
              Table-Table-Area,
              Pre-Field-Table-Area,
v0854         Pre-Chain-Table-Area,
              Cond-Table-Area,
v0.5          Func-TABLE-AREA, EntryLength-TABLE-AREA
              Gaps-in-Bits-Area,
              Counters-and-Totals, Options-in-Effect, Log-Record
              Formula-Area
              Formula-Execution-Area
              Default-Table-Area
              Remarks-Table-Area
              Range-Table-Area
v00890        Alt1-Table-Area, Alt2-Table-Area
              DateFMT-Table-Area

           if Return-Code > 0 then
             display 'Engine: Error Loading DLLs. RC=' Return-code
               '. (Error message is above)'
             move return-code to RC
             subtract 1 from prefield-cnt
             perform Show-DLL-CONTROL-RECORDS
             move RC to return-code
             close LOG-FILE
             stop run
           end-if
           call 'HKSys' using Print-CPUTimer

           perform Show-DLL-CONTROL-RECORDS
           if Trace-On
             perform Trace-FormulaArray
             if Opt-TraceStart not = 1
               set Trace-off to true
             end-if
           end-if

v00890     if not OPT-ShowMap-No
v00890       set Map-DLL-addr     to address of DLL-Area
v00890       set Map-File-addr    to address of File-Table-Area
v00890       set Map-Rec-addr     to address of Record-Table-Area
v00890       set Map-Pret-addr    to address of Pre-Field-Table-Area
v00890       set Map-Tbl-addr     to address of Table-Table-Area
v00890       set Map-Entry-addr   to address of EntryLength-Table-Area
v00890       set Map-Cond-addr    to address of Cond-Table-Area
v00890       set Map-Func-addr    to address of Func-Table-Area
v00890       set Map-Options-addr to address of Options-in-Effect
v00890       call 'map' using Map-linkage
v00890     end-if

           perform PROCESS-INPUT

           go to END-PROGRAM
           .

      *--------------------------------------------------------------
      * read through the input source file
      *--------------------------------------------------------------
       PROCESS-INPUT.
           display ' '
           display 'Engine:Processing of Source data started-----------'
                   '--------------------------------------------------*'
           call 'HKSys' using Print-CPUTimer
           set Program-Parsing-Record to true
           if trace-on
             display 'TRACE:Program Parsing Record'
           end-if
           move 'Processing Messages:' to Log-Record
           write LOG-RECORD
           call 'DAR' using Open-DAR-File, In-Buffer, v-Buffer,
                 Pre-Field-Table-Area,
                 Field-Table-Area
                 Table-Table-Area
                 File-Table-Area, Record-Table-Area, DLL-Area,
                 COND-TABLE-AREA
                 EntryLength-TABLE-AREA
                 Counters-and-Totals
                 Discard-Table-Area
                 Options-in-Effect
v00856           DAR-Table, Sample-Table, Others-Table, KeyList-Table
                 Remarks-Table-Area
                 Range-Table-Area
v00890           Alt1-Table-Area, Alt2-Table-Area
                 DateFMT-Table-Area
           set Open-Mig-File to true
           perform Migrate
           if Trace-On
             set Open-Trace-File to true
             perform Write-Trace
           end-if
           move DB2-TimeStamp-Template to DB2-TimeStamp-Value
           if Opt-DB2-TimeStamp-set
             move OPT-DB2-TimeStamp-Value to DB2-TimeStamp-Value
           end-if
           move ODBC-TimeStamp-Template to ODBC-TimeStamp-Value
           if Opt-ODBC-TimeStamp-set
             move OPT-ODBC-TimeStamp-Value to ODBC-TimeStamp-Value
           end-if

           if Sample-Requested
             call 'Sample' using
              Options-in-Effect, Record-Table-Area, Pre-Field-Table-Area
           end-if

      *    'set Event request off if not requested
           if FDD-Not-Requested
              set FDD-Not-Okay-to-Do to true
           end-if
           if DAR-Not-Requested
              set DAR-Not-Okay-to-Do to true
           end-if
           if MIG-Not-Requested
              set MIG-Not-Okay-to-Do to true
           end-if

           perform PI-OPEN-FILES
v0854      if IO-STATUS NOT = '00'
v0854      or CAT-Discards > Opt-ErrorLimit
v0854      or WS-REC-CNT >= Opt-StopAft
v0854        set Process-Record-Stop to true
v0854      else
v0854        set Process-Record-Continue to true
v0854      end-if

      *----------------------------------------------------------------
      * Map out the fields to the record just read.
      * If there is only one record type and all the fields are fixed
      * length, this process is very straight forward.
      * If there are multiple record types, then we have to look
      * at the actual data to determine which record type was read.
      * For variable length fields or records and especially ODO
      * tables, the process is complex.
      * Validation of the Data fields (i.e., Numerics) are done
      * in the Ratify routine. This mapping ensures there is
      * alignment of fields to the record read (ie. More-DLL-than-data)
      *----------------------------------------------------------------
v0854      perform until Process-Record-Stop
             perform PI-READ-RECORD
             perform Ready-or-not-Trace
v0854        if Process-Record-Continue
               Perform Check-Record-Condition
               if Condition-True
Mapit            perform PI-PROCESS-RECORD
                 if Discard-Nothing
                   perform Validate-Mapping
                   if Discard-Nothing
                     perform Finish-Up-vFields
                   end-if
                 end-if
                 perform Ratify-Data-Fields
                 perform FDD-DAR-MIG
               else
                 perform PI-DISCARD-INPUT
               end-if
             else
               if IO-Status = '10'
                display 'Engine:Finished reading Source (end of file)'
               else
                display 'Engine:Infile IO-Status=' IO-Status
               end-if
               set Process-Record-Stop to true
               call 'HKSys' using Print-CPUTimer
             end-if
           end-perform


           if CAT-Discards > Opt-ErrorLimit
             move 10 to IO-Status
             display 'Engine:Discards exceeds ErrorLimit. Program Stops'
           end-if
           if WS-REC-CNT >= Opt-StopAft
             move 10 to IO-Status
             display 'Engine:Rec-Cnt exceeds StopAft. Program Stops'
           end-if

           set Program-Ending to true
           if trace-on
             display 'TRACE:Program Ending'
           end-if

      *    ' shut down the updateV routine
           perform UpdateV

           if FDD-Requested
             if CAT-Discards > Opt-ErrorLimit
                move '    ***Error Limit reached. Processing Stops.***'
                  to ws-message
                set Write-FDD-Message to true
                perform FDD
             end-if
             if WS-REC-CNT >= Opt-StopAft
                move '    ***Record Limit reached. Processing Stops.***'
                  to ws-message
                set Write-FDD-Message to true
                perform FDD
             end-if
           end-if

           perform PI-CLOSE-FILES
           call 'DAR' using Close-DAR-File, in-Buffer, v-Buffer,
                 Pre-Field-Table-Area
                 Field-Table-Area
                 Table-Table-Area
                 File-Table-Area, Record-Table-Area, DLL-Area,
                 COND-TABLE-AREA
                 EntryLength-TABLE-AREA
                 Counters-and-Totals
                 Discard-Table-Area
                 Options-in-Effect
v00856           DAR-Table, Sample-Table, Others-Table, KeyList-Table
                 Remarks-Table-Area
                 Range-Table-Area
v00890           Alt1-Table-Area, Alt2-Table-Area
                 DateFMT-Table-Area
           set Close-Mig-File to true
           perform Migrate
           if Trace-on
             set Close-Trace-File to true
             perform Write-Trace
           end-if
           display 'Engine:Processing of Source data end---------------'
                   '--------------------------------------------------*'
           display ' '
           call 'HKSys' using Print-CPUTimer
           .
      * Check to see which record is to be processed.
      * If a record does not have a COND option then it will be
      *  processed.
       Check-Record-Condition.
v0854      move ZEROES to DISCARD-CNT
v00860     move zeroes to Log-Cnt
           set Condition-False to true
           perform varying Rec-Ptr from 1 by 1
                       until Rec-Ptr > Record-Cnt
                          or Condition-True
             if RT-COND(Rec-Ptr) > 0
               if RT-Variability(Rec-Ptr) <= in-len
v00860           move RT-Cond(Rec-Ptr) to Cond-ptr
v00860           move zeroes to ws-offset
v00860           perform ChkCond
               end-if
             else
               set Condition-True to true
             end-if
           end-perform
           if Condition-True
             subtract 1 from Rec-Ptr
             move Pret-File(1)           to File-to-Process
             move Rec-Ptr                to Record-to-Process
             move RT-Start-Pret(Rec-Ptr) to Pret-to-Process
             move RT-End-Pret(Rec-Ptr)   to Pret-to-Stop
             move RT-Last-Base-Pret(Rec-Ptr) to Last-Required-Pret
             add 1 to RT-Count(Rec-Ptr)
             if RT-First-Record(Rec-Ptr) = zeroes
               move ws-rec-cnt to RT-First-Record(Rec-Ptr)
             end-if
             move ws-rec-cnt   to RT-Last-Record(Rec-Ptr)
           else
             set Discard-Record-Cond-Never-Met to true
             move zeroes to PreT, sCursor
v00860       move zeroes to Fldt
             call 'NOTEDISC' using PreT, sCursor, Discard-Table-Area
v00860                             Fldt
             add 1 to discard-count(Discard-code)
             add 1 to Record-not-Matched-cnt
             if Record-not-Matched-First = 0
               move ws-rec-cnt to Record-not-Matched-First
             end-if
           end-if
           .

       Ready-or-not-Trace.
           if ws-rec-cnt = Opt-TraceStart
             display 'TRACE:TraceStart() record found. '
               'Trace now on.'
               ':record #' ws-rec-cnt
               ':In-Len=' In-Len
               ':In-Bits=' In-Bits
             set trace-on to true
           end-if
           if Trace-on
             add 1 to trace-count
             if trace-count > Opt-TraceStopAft
                 display 'TRACE:TraceStopAft() limit reached. '
                   'Trace now off.'
                   ':record #' ws-rec-cnt
                 set trace-off to true
             end-if
           end-if
           .
       Trace-FormulaArray.
           if Trace-NOT-OPEN
              open output Trace-FILE
              if IO-STATUS not = '00'
                  display 'Write-Trace:Cond:TRACE open error('
                  IO-STATUS ')'
                  move 12 to return-code
                  stop run
              end-if
              set Trace-OPEN to true
              move 32752 to Out-Len-T
           end-if
           move '==Condition Table== entries=' to tch-text
           move Cond-Cnt to tch-value
           compute out-len-t = length of Trace-Cond-Heading
           write trace-record from Trace-Cond-Heading
           compute out-len-t = length of Trace-Cond-Cond-Col
           write trace-record from Trace-Cond-Cond-Col
           perform varying COND-PTR from 1 by 1
             until COND-PTR > COND-CNT
              move Cond-Ptr to tccd-Cond-ptr
              move Cond-ExecuteNdx(cond-ptr) to tccd-ExecutNdx
              move Cond-ExecuteQty(cond-ptr) to tccd-ExecutQty
              move Cond-ResultNdx(cond-ptr)  to tccd-ResultNdx
              move '"'                       to tccd-Formula(1:1)
              move Cond-statement(cond-ptr)
                   (1:Cond-Statement-Len(cond-ptr))
                                             to tccd-Formula(2:)
              move '"'
                to tccd-Formula(Cond-Statement-Len(cond-ptr) + 2:1)
              compute out-len-t = length of tccd-details
                                + Cond-Statement-Len(cond-ptr) + 2
              write trace-record from Trace-Cond-Cond-Detail
           end-perform
           compute out-len-t = 19
           move '"===end of list==="' to trace-record
           write Trace-Record

           move '==Execution List==:Qty=' to tch-text
           move TotalExecutionQty to tch-value
           compute out-len-t = length of Trace-Cond-Heading
           write trace-record from Trace-Cond-Heading
           compute out-len-t = length of Trace-cond-Exec-Col
           write trace-record from Trace-Cond-Exec-Col
           perform varying tced-ndx from 1 by 1
             until tced-ndx > TotalExecutionQty
               move FAExecutionList(tced-ndx) to tced-ExecutionList
               compute out-len-t = length of Trace-Cond-Exec-Detail
               write trace-record from Trace-Cond-exec-Detail
           end-perform
           compute out-len-t = 19
           move '"===end of list==="' to trace-record
           write Trace-Record


           move '== Formula Array. Entries=' to tch-text
           move TotalEntryQty                to tch-value
           compute out-len-t = length of Trace-Cond-Heading
           write trace-record from Trace-Cond-Heading
           compute out-len-t = length of Trace-Cond-Formula-Col
           write trace-record from Trace-Cond-formula-col
           perform varying tcfd-ndx from 1 by 1
             until tcfd-ndx > TotalEntryQty
             evaluate true
              when FAType-Var (tcfd-ndx)
                      move FAVarName(tcfd-ndx) to tcfd-misc
              when FAType-Func(tcfd-ndx)
                      move FAFuncName(tcfd-ndx) to TR-NUM2D
                      move TR-NUM2D             to tcfd-misc
              when FAType-Paren(tcfd-ndx)
                      move FAFuncName(tcfd-ndx) to TR-NUM2D
                      move TR-NUM2D             to tcfd-misc
                      move FAParmNdx(tcfd-ndx, 1) to transp-num4s
                      move transp-num4s    to tcfd-misc(16:5)
              when FAType-Set (tcfd-ndx)
                      move spaces to tcfd-misc
              when FAType-Oper(tcfd-ndx)
                      move spaces to tcfd-misc
                      if FAUsesLeftOperand(tcfd-ndx) = 1
                          move FAParmNdx(tcfd-ndx, 1) to transp-num4s
                          move transp-num4s    to tcfd-misc(1:5)
                      end-if
                      move FAOperName(tcfd-ndx) to tcfd-misc(7:9)
                      if FAUsesRightOperand(tcfd-ndx) = 1
                          move FAParmNdx(tcfd-ndx, 2) to transp-num4s
                          move transp-num4s    to tcfd-misc(16:5)
                      end-if
              when FAType-Text(tcfd-ndx)
                      move FAValText(tcfd-ndx) to tcfd-misc
              when FAType-Hex (tcfd-ndx)
                      move "x'" to tcfd-misc
                      move FAValHex(tcfd-ndx) to tcfd-misc(3:10)
                      move "'"  to tcfd-misc(13:1)
              when FAType-Bin (tcfd-ndx)
                      move "b'" to tcfd-misc
                      move FAValBin(tcfd-ndx) to tcfd-misc(3:10)
                      move "'"  to tcfd-misc(13:1)
              when FAType-Int (tcfd-ndx)
                      move FAValInt(tcfd-ndx) to transp-num4s
                      move transp-num4s to tcfd-misc
             end-evaluate
             move FAType(tcfd-ndx)         to tcfd-FAType
             move FAPriority(tcfd-ndx)     to tcfd-FAPriority
             move FAOperandAvail-SW(tcfd-ndx) to tcfd-FAOperandAvail-SW
             move FAOperatorAvail-SW(tcfd-ndx)
               to tcfd-FAOperatorAvail-SW
             move FAParmQty(tcfd-ndx)      to tcfd-FAParmQty
             move FAParmNdx(tcfd-ndx, 1)   to tcfd-FAParmNdx1
             move FAParmNdx(tcfd-ndx, 2)   to tcfd-FAParmNdx2
             move FALength(tcfd-ndx)       to tcfd-FALength
             compute out-len-t = length of Trace-Cond-Formula-Detail
             write trace-record from Trace-Cond-Formula-Detail
           end-perform
           compute out-len-t = 19
           move '"===end of list==="' to trace-record
           write Trace-Record
           move 32752 to Out-Len-T
           .

      *-------------------------------------------------------------
      * Map the DLL definition with the record read.
      * This will load the FIELD-TABLE with positions and lengths values
      * so that later processing can occur such as FDD, DAR, MIG.
      * Basically this MAPS the DLL to the DATA.
      * Input: In-Buffer -> a source data record
      *        PreField-Table-Area -> DLL's interpreted
      *        Table-Table-Area -> Additional DLL table details
      *        Cond-Table-Area -> Additional DLL Cond() details
      *        Func-Table-Area -> Additional DLL vField details
      * Output: Field-Table-Area
      *         v-Buffer
      *-------------------------------------------------------------
       PI-PROCESS-RECORD.

           if Trace-on
             display 'TRACE:PI-Process-Record:rec#' ws-rec-cnt
                ':In-LEN=' In-Len
                ':Rec Ptr='     Record-to-Process
             display 'TRACE:PI-Process-Record'
                ':Start Pret='  Pret-to-Process
                ':End Pret='    Pret-to-Stop
                ':Last Pret='   Last-Required-Pret
                ':#vFields ='   RT-num-vFields(Record-to-Process)
             display 'Event'
                     ' Field'
                     ' <Start'
                     '.Lengt>'
                     ' -Pret-'
                     ' Field name--------------------'
                     ' E'
                     ' Type'
                     ' Value---------------'
                     ' Key-Value-----------'
           end-if
           move zeroes to return-code
           set Program-Pulling-Data-Field to true

      * reset/clear the field-Table
           move 0 to FIELD-CNT
           set No-DLL-Error to true
           move ZEROES to DISCARD-CNT
           move zeroes to First-Field-in-Error
                          First-Fields-Error
           move zeroes to Offset-Field-is-zero
           move zeroes to Number-Fields-Skipped
           move :MaxOBuff: to Out-Len
           set OUT-LEN-Field-is-UnResolved to true
           move zeroes to RT-First-vField(Record-to-Process)
                          RT-Last-vField(Record-to-Process)
                          vField-Start
                          vField-Last
                          vField-Resolved
                          vField-Unresolved
           move ZEROES to FIELDS-IN-ERROR

v0854      if Opt-DB2-TimeStamp-now or Opt-ODBC-TimeStamp-now
             perform Ready-the-TimeStamp-Values
v0854      end-if

           set ws-key-value-not-set to true

      * reset/clear the Buffer areas
           move 1 to vCursor
           move 1 to sCursor
           move 1 to tCursor
           set Keep-Generating to true


           move zeroes to return-code
           set Not-Forcing-Add-Field to true

      * reset the chain all fields within this record
      *    'determine the number of bytes
      *    ' (there 8 bytes per Chain entry)
           compute wPos = ((Pret-to-Process - 1) * 8) + 1
v00862     compute wEnd = Pret-to-Stop * 8
v00862     compute wLen = (wEnd - wPos) + 1

      *    'clear the chain of just those bytes
v0854      move low-values to Pre-Chain-Table-Area(wPos:wLen)

      *----------------------------------------------------------------
      * Work through every entry on PreField and create an entry to the
      *  Field-Table which will hold the records ACTUAL Start Position
      *  and Length which will point to the in-buffer (source record).
      *  This process is for fields, tables, and groups.
      * The Start Position always starts at 1. The start position is
      *  known in the program as sCursor. As field lengths are derived
      *  the next sCursor will be set for the next field.
      *  Sometimes the sCursor will not be adjusted because length of 0
      *  or there was an error which could not determine length. The
      *  program will stop processing a record if encounters any such
      *  errors.
      * The Length is determined by any variablity such as
      *  occurs depending on tables, varchar type fields, or OFFSET.
      * If the entry is a table then we add all the sub fields
      *  to the FIELD-TABLE and to the Index table which holds the
      *  indices for each FIELD-TABLE entry.
      * vFields are treated as much as possible as a Field, however
      *  the data is created and placed into the v-Buffer. There is a
      *  separate Start Position called vCursor where these vFields
      *  are stored. These are recomputed and repopulated into the
      *  v-Buffer for each record read.
      *---------------------------------------------------------------
           perform varying PRET from Pret-to-Process by 1
                     until (sCursor > In-Bits)
                        or PRET > Pret-to-Stop
                        or Stop-Generating
             move ZEROES to RETURN-CODE
             set Discard-Nothing to true
             move spaces to ws-value
             evaluate true
               when NORMAL-FIELD(PRET)
                 perform Add-Normal-Field thru ANF-exit
               when NORMAL-vFIELD(PRET)
                 perform Add-Normal-vField thru ANvF-exit
               when TABLE-FIELD(PRET)
                 perform Add-Main-Table thru AMT-exit
v0854 *          'Point to field ater table
v0854            move PRET-TABLE(PRET) to TBLPTR
v0854            compute PRET = (PRET + TABLE-NUMBER-OF-FIELDS(TBLPTR))
               when Record-FIELD(PRET)
                 perform Add-Record-Field thru ARF-exit
               when GROUP-FIELD(PRET)
                 display 'Engine:GROUP command deprecated:pret=' Pret
                   ':rec#=' ws-rec-cnt
                 move 12 to return-code
                 stop run
             end-evaluate
           end-perform

           if TRACE-ON
             display 'TRACE:*---------- END OF RECORD --------*'
             display 'TRACE:* Pret=' PRET '>' Pret-to-Stop
                     '          *'
             display 'TRACE:* sCursor=' sCursor '>' In-Bits
                     '    *'
v0851        if Pret <= Pret-To-Stop
v0851          display 'TRACE:* Errors=' Fields-in-Error '>0'
v0851                  '             *'
v0851          display 'TRACE:* File=' Pret-file(Pret)
v0851                  ' not= ' File-to-Process '            *'
v0851          display 'TRACE:* Record=' Pret-Record(Pret)
v0851                  ' not=' Record-to-Process '           *'
v0851        end-if
             display 'TRACE:* FIELDS EXPANDED TO:' FIELD-CNT
                     '        *'
             display 'TRACE:* Last Required Pret:'
                         RT-Last-BASE-Pret(record-to-process)
                     '       *'
             display 'TRACE:* vfield start=' vField-Start
                     '             *'
             display 'TRACE:* vfield Last =' vField-Last
                     '             *'
             display 'TRACE:* Discard code=' discard-code
                     '             *'
             display 'TRACE:* Stop-Generator-sw=' Stop-Generating-sw
                     '             *'
             display 'TRACE:*---------------------------------*'
           end-if

      *Empty Last Field-
      * if the last field not processed is a offset/varchar field
      * and we are at end of record and everything else is okay
      *  then we'll create this 'empty' field
           if Pret <= Pret-to-Stop
             if (Keep-Generating)
             and (sCursor > In-Bits)
             and (Pret = Pret-to-Stop)
             and (NORMAL-FIELD(PRET))
             and (Type-CH (Pret)
v00894         or Type-ZD (Pret))
             and (Length-Field-Offset(Pret)
               or Length-Field-VarChar(Pret)
v00894         or File-CSV-Yes(1))
                 set Forcing-Add-Field to true
                 perform Add-Normal-Field thru ANF-exit
                 add 1 to Pret
             end-if
           end-if


           if Field-Cnt > Highwater-Field-Cnt
             move Field-Cnt to Highwater-Field-cnt
           end-if
           if Field-Cnt < Lowwater-Field-Cnt
             move Field-Cnt to Lowwater-Field-Cnt
           end-if


           move zeroes to return-code
           .
      * finish up any remaining vFields
       Finish-Up-vFields.
           perform varying PRET from Pret by 1
                     until PRET > Pret-to-Stop
             move ZEROES to RETURN-CODE
             set Discard-Nothing to true
             if NORMAL-vFIELD(PRET)
                 perform Add-Normal-vField thru ANvF-exit
             end-if
           end-perform
           .

      *Set the Time Stamp values for both DB2 and ODBC formats
      *note. both formats could be used in the same run
       Ready-the-TimeStamp-Values.
           accept date-stamp from date YYYYMMDD
           accept time-stamp from time
           compute TimeStamp = (date-stamp * 100000000) + time-stamp
           move TimeStamp to WS-TimeStamp
           if Opt-DB2-TimeStamp-now
               move ws-ts-year   to DB2-TS-Year
               move ws-ts-month  to DB2-TS-Month
               move ws-ts-day    to DB2-TS-Day
               move ws-ts-hour   to DB2-TS-Hour
               move ws-ts-minute to DB2-TS-Minute
               move ws-ts-second to DB2-TS-Second
               if (TimeStamp - TimeStamp-Prev) <= 99
                 add 1 to DB2-TS-Sequence
               else
                 move 1 to DB2-TS-Sequence
               end-if
           end-if
           if Opt-ODBC-TimeStamp-now
               move ws-ts-year   to ODBC-TS-Year
               move ws-ts-month  to ODBC-TS-Month
               move ws-ts-day    to ODBC-TS-Day
               move ws-ts-hour   to ODBC-TS-Hour
               move ws-ts-minute to ODBC-TS-Minute
               move ws-ts-second to ODBC-TS-Second
               if (TimeStamp - TimeStamp-Prev) <= 99
                 add 1 to ODBC-TS-Sequence
               else
                 move 1 to ODBC-TS-Sequence
               end-if
           end-if
           move TimeStamp to TimeStamp-Prev
           .

      *----------------------------------------------------------------
      * Validate we've mapped every DATA byte and every DLL field
      *----------------------------------------------------------------
       Validate-Mapping.
           evaluate true
             when PRET <= Pret-to-Stop
               if PRET <= RT-Last-Base-Pret(Record-to-Process)
               or Number-Fields-Skipped > 0
                 if Table-Field(Pret)
                   move Pret-Table(Pret) to tblptr
                 end-if
                 if (Table-Field(Pret) and sCursor > in-Bits
                     and Table-is-Group(TblPtr))
                   add 1 to CAT-GROUP-EOR
                   evaluate true
                     when Number-Fields-Skipped > 0
                          perform More-DLL-than-Data-Error
                     when (sCursor - 1) > in-Bits
                          perform More-Data-than-DLL-Error
                     when (sCursor - 1) < in-Bits
      *                 'believe this can never be true***
                          perform Data-unProcessed-Error
                   end-evaluate
                 else
                   perform More-DLL-than-Data-Error
                 end-if
v00878         else
v00879           if Table-Occur(Pret)
v00879             move Pret-Table(Pret) to TblPtr
v00879           end-if
v00878           if sCursor > in-Bits
v00878           and (Normal-FIELD(Pret)
v00878            or (Table-Occur(Pret)
v00879                and Table-is-not-Group(TblPtr)))
v00878*            ' At end of record *and*
v00878*            ' there is a required field(s) yet to process
v00878             perform More-DLL-than-Data-Error
v00878           end-if
               end-if

             when Number-Fields-Skipped > 0
                  perform More-DLL-than-Data-Error
             when (sCursor - 1) > in-Bits
                  perform More-Data-than-DLL-Error
             when (sCursor - 1) < in-Bits
                  perform Data-unProcessed-Error
           end-evaluate
           .
       More-DLL-than-Data-Error.
           if trace-on display 'Trace: discard 4' end-if
           set Discard-More-DLL-than-Data to true
v00860     move zeroes to Fldt
           call 'NOTEDISC' using PreT, sCursor, Discard-Table-Area
v00860                    Fldt
           add 1 to fields-in-error
           if First-Field-in-Error = 0
             move Pret to First-Field-in-Error
             move Discard-Code to First-Fields-Error
           end-if
           add 1 to discard-count(Discard-code)
           set DLL-Long-Error to true
           .
       Data-unProcessed-Error.
           if trace-on display 'Trace: discard 13' end-if
           set Discard-No-match-DLL-to-Data to true
v00860     move zeroes to Fldt
           call 'NOTEDISC' using PreT, sCursor, Discard-Table-Area
v00860                    Fldt
           add 1 to fields-in-error
           if First-Field-in-Error = 0
             move Pret to First-Field-in-Error
             move Discard-Code to First-Fields-Error
           end-if
           add 1 to discard-count(Discard-code)
           set Data-Not-Processed-Error to true
           .
       More-Data-than-DLL-Error.
           move zeroes to Pret
           set Discard-More-Data-than-DLL to true
           if trace-on
             display 'Trace:' discard-reason(discard-code)
                ': Pos=' sCursor
           end-if
v00860     move zeroes to Fldt
           call 'NOTEDISC' using PreT, sCursor, Discard-Table-Area
v00860                    Fldt
           add 1 to fields-in-error
           if First-Field-in-Error = 0
               move 1        to First-Field-in-Error
               move Discard-Code to First-Fields-Error
           end-if
           add 1 to discard-count(discard-code)
           set DLL-Short-Error to true
           .
      *----------------------------------------------------------------
      * Approve and give formal sanction of the record.
      *----------------------------------------------------------------
       Ratify-Data-Fields.
           set ratInBuffer-ptr to address of in-buffer
           set ratPret-ptr     to address of Pre-Field-Table-Area
           set ratFld-ptr      to address of Field-Table-Area
           set ratDisc-ptr     to address of Discard-Table-Area
           set ratCnt-ptr      to address of Counters-and-Totals
           set ratDateFMT-ptr  to address of DateFMT-Table-Area
           call 'Ratify' using Ratify-Parameters
           .
      *----------------------------------------------------------------
      * Process through the loaded FIELD-TABLE. This is where the
      * the possible FDD, DAR, MIG files will be created.
      *----------------------------------------------------------------
       FDD-DAR-MIG.
           move discard-code to FDD-Discard-Code

           perform Check-FDD-DAR-MIG-Okay

      *    ' Create Migrate record  (will also do final resolve)

v00862     move zeroes to Log-Cnt
v0851      if MIG-Okay-to-Do and First-Field-in-Error = 0
             if trace-on
               display 'TRACE:Program Migrating'
             end-if
             set Discard-Nothing to true
             set Program-Migrating to true
             set Move-Source-to-Target to true
             perform Migrate
v00891*      'if error turn off all processing and terminate program
v00891       if Discard-API-QGGFDRC1-RC-not-00
v00891         set Process-Record-Stop to true
v00891         set MIG-Not-Okay-to-Do to true
v00891         set FDD-Not-Okay-to-Do to true
v00891         set DAR-Not-Okay-to-Do to true
v00891       end-if
           end-if

      *    'Check if we need to do a resolve of vfields
      *    ' because might not have requested Migrate or Field errors
           if MIG-Not-Okay-to-Do
v0851      or First-Field-in-Error > 0
             if DAR-Okay-to-Do
             or FDD-Okay-to-Do
v0852           set Program-Migrating to true
                move zeroes to FPtr
                move Record-to-Process to Rec-Ptr
                set Resolve-vFields to true
                perform Migrate
             end-if
           end-if

           if trace-on
      *      'trace after migrate and updatev has resolved
             display 'TRACE:FDD-DAR-MIG'
             perform varying FPTR from 1 by 1 until FPTR > Field-CNT
               set Write-Trace-Fldt to true
               perform Write-Trace
             end-perform
           end-if

      *    'Set Key value of record
           perform Set-Key-Value

      *    ' create the FDD report for this record

           if FDD-Okay-to-Do
             set Program-Building-FDD to true
             if trace-on
               display 'TRACE:Program Building FDD'
             end-if
             move FDD-Discard-Code to Discard-Code
             move Record-to-Process to Rec-Ptr
             set Start-FDD to true
             perform FDD
             move zeroes to return-code
             set Discard-Nothing to true
             perform varying Fldt from 1 by 1 until Fldt > FIELD-CNT
               or Return-code > 0 or Discard-Something
               set Write-FDD-Field to true
               perform FDD
             end-perform
             set End-FDD to true
             perform FDD
             move zeroes to return-code
           end-if

           perform Write-out-the-Log-Entries

v0851      if First-Field-in-Error > 0
             perform PI-DISCARD-INPUT
           else
             add 1 to CAT-GOOD-RECORDS
           end-if

      *    ' create the DAR report for this record

           if DAR-Okay-to-Do
             set Program-Building-DAR to true
             if trace-on
               display 'TRACE:Program Building DAR'
             end-if
             call 'DAR' using Write-DAR-Record, In-Buffer, v-Buffer,
                 Pre-Field-Table-Area
                 Field-Table-Area
                 Table-Table-Area
                 File-Table-Area, Record-Table-Area, DLL-Area,
                 COND-TABLE-AREA
                 EntryLength-TABLE-AREA
                 Counters-and-Totals
                 Discard-Table-Area
                 Options-in-Effect
v00856           DAR-Table, Sample-Table, Others-Table, KeyList-Table
                 Remarks-Table-Area
                 Range-Table-Area
v00890           Alt1-Table-Area, Alt2-Table-Area
                 DateFMT-Table-Area
           end-if
           if trace-on
             display 'TRACE:vfield Resolved  =' vField-Resolved
             display 'TRACE:vfield Unresolved=' vField-Unresolved
           end-if

           .

v00860*Write out the LOG entries for this record, if any
v00860 Write-out-the-Log-Entries.
           perform varying Log-Ptr from 1 by 1 until Log-Ptr > Log-cnt
v00882       or Opt-LogShowLowHigh-No
             evaluate true
               when LogReason-LowVal (log-Ptr)
                               move 'LowVal '          to LL-Reason-Text
                               move 'Convert to 0'     to LL-Reason-Msg
                               add 1 to FixLow-Cnt
               when LogReason-Space  (log-Ptr)
                               move 'Space  '          to LL-Reason-Text
                               move 'Convert to Space' to LL-Reason-Msg
                               add 1 to FixSpace-Cnt
v00882         when LogReason-HighVal-Kept(log-Ptr)
v00882                         move 'HighVal'          to LL-Reason-Text
v00882                         move 'Kept as is  '     to LL-Reason-Msg
v00882                         add 1 to FixHigh-Kept-Cnt
               when LogReason-HighVal(log-Ptr)
                               move 'HighVal'          to LL-Reason-Text
                               move 'Convert to 9'     to LL-Reason-Msg
                               add 1 to FixHigh-Cnt
             end-evaluate
             move ws-rec-cnt            to LL-Rec-Cnt
             move logPret(log-Ptr)      to LL-FIELD
             move logField(log-Ptr)     to FldT
             if Fldt > 0
             and Pret-Dims(LD-Field) > 0
               move spaces                to LD-Field-Name
               call 'FmtInd' using
                     FLDT-INDEX-TABLE(Fldt), Pret-Dims(LL-Field), Indice
               string PRET-FIELDNAME(LL-Field) delimited by SPACES
                      Indice                   delimited by SPACES
                 into LL-Field-Name
               end-string
             else
                  move PRET-FIELDNAME(LL-Field) to LL-Field-Name
             end-if
             move Pret-Length-Byte(LL-FIELD) to LL-Length
             move FLDT-Start-Byte(FldT)      to LL-POS
             if LogReason-Space(log-Ptr)
             and Fldt > 0
             and Fldt-Start-Byte(Fldt) > 0
             and Fldt-Length-Byte(Fldt) > 0
      *         'Format the value into hex notation i.e. x'FF'
                call 'HEXSHOW' USING
                 in-buffer(Fldt-Start-Byte(Fldt):Fldt-Length-Byte(Fldt))
                 Fldt-Length-Byte(Fldt), WS-HEX
                move "x'" to LL-Reason-Msg(18:2)
                compute N = Fldt-Length-Byte(Fldt) * 2
                move ws-hex(1:N) to LL-Reason-Msg(20:N)
                move "'"         to LL-Reason-Msg(20 + N:1)
             end-if
             write Log-Record from Log-Log
           end-perform
           .
      *This checks the processing options to ensure the right records
      * are processed.
       Check-FDD-DAR-MIG-Okay.
           if FDD-Requested
              if ws-rec-cnt >= Opt-FDD-Start and
                 ws-rec-cnt <= Opt-FDD-Stop
                 set FDD-Okay-to-Do to true
              else
                 set FDD-Not-Okay-to-Do to true
              end-if
           end-if
           if DAR-Requested
              if ws-rec-cnt >= Opt-DAR-Start and
                 ws-rec-cnt <= Opt-DAR-Stop
                 set DAR-Okay-to-Do to true
              else
                 set DAR-Not-Okay-to-Do to true
              end-if
           end-if
           if MIG-Requested
              if ws-rec-cnt >= Opt-MIG-Start and
                 ws-rec-cnt <= Opt-MIG-Stop
                 set MIG-Okay-to-Do to true
              else
                 set MIG-Not-Okay-to-Do to true
              end-if
           end-if
           if TRACE-ON
             if FDD-Okay-to-Do
               display 'TRACE:FDD-Okay-to-do'
             else
               display 'TRACE:FDD-NOT-Okay-to-do'
             end-if
             if DAR-Okay-to-Do
               display 'TRACE:DAR-Okay-to-do'
             else
               display 'TRACE:DAR-NOT-Okay-to-do'
             end-if
             if MIG-Okay-to-Do
               display 'TRACE:MIG-Okay-to-do'
             else
               display 'TRACE:MIG-NOT-Okay-to-do'
             end-if
           end-if
           .


      *----------------------------------------------------------------
      *This will take the PRET Record entry and add to the Field-Table
      *Length of record could be zero if variable, it will be resolved
      * after all actual fields load for the record
      *----------------------------------------------------------------
       Add-Record-Field.
           if sCursor > In-Bits
              go to ARF-Exit
           end-if
           move zeroes to ws-LEN
           perform AddField
           set Not-a-vField(Field-Cnt) to true
           move zeroes to FLDT-Target-Length(Field-Cnt)
           move PRET-LEVEL(Pret) to FLDT-LEVEL(Field-Cnt)
           perform Update-Chain

           if TRACE-ON
              move 'Recrd' to Trace-Event
              perform TRACE-ENTRY
           end-if

      * Done with this entry, point to next possible field position.
           compute sCursor = sCursor + ws-len
           .
       ARF-Exit.
           exit.

      * set the chaining between PRET & FLDT, based on Field-cnt & Pret
      * Also set the parent of the new field entry pointing to FLDT
       Update-Chain.
v0854 *    'sequence of check is for most expected record types
           evaluate true
             when Normal-Field(Pret) or Normal-vField(Pret)
                 set Entry-Root to true
             when Occur-Field(Pret) or ODO-Field(Pret)
               or Occur-vField(Pret) or ODO-vField(Pret)
                 if pret-Hash(Pret) <= 1
                   set Entry-Root to true
                 else
                   move Pret-Hash(Pret) to LastDim
                   if FLDT-INDEX(Field-Cnt, LastDim) = 1
                     set Entry-Root to true
                   else
                     set Entry-Ghost to true
                   end-if
                 end-if
             when Table-Field(Pret)
                 compute LastDim = Pret-Hash(Pret) + 1
                 if FLDT-INDEX(Field-Cnt, LastDim) <= 1
                   set Entry-Root to true
                 else
                   set Entry-Ghost to true
                 end-if
             when Record-Field(Pret)
                 set Entry-Root to true
             when other
                 set Entry-Root to true
           end-evaluate

           if Pret-StartField(Pret) = 0
             move Field-Cnt to Pret-StartField(Pret)
           end-if

           if Entry-Root
             move Field-Cnt to Fldt-Root(Field-Cnt)
           else
             set FldRoot to Pret-StartField(Pret)
             move Fldt-Root(FldRoot) to Fldt-Root(Field-Cnt)
           end-if

      *    'Do not place the ghost entries into the "field chain"
           if Entry-Root
             if Pret-LastField(Pret) > 0
               set PriorFld to Pret-LastField(Pret)
               move Field-Cnt            to FLDT-Next(PriorFld)
               move Pret-LastField(Pret) to FLDT-Prior(Field-Cnt)
             end-if
             move Field-Cnt to Pret-LastField(Pret)
           else
             if Regular-Field(Pret)
               move Field-Cnt to Pret-LastField(Pret)
             end-if
           end-if

           set FldRoot to Pret-LastField(Pret)
           move Field-Cnt to Fldt-LastGhost(FldRoot)

           if Pret-HashPar(Pret) > 0
v0854        move Pret-HashPar(Pret) to rPret
             set FldInx to Pret-LastField(rPret)
             move Fldt-LastGhost(FldInx) to Fldt-Parent(Field-Cnt)
           end-if
      *    'Update Sibling and Child chains
           if Fldt-Parent(Field-Cnt) > 0
      *      'update the parent
             set FldInxPar to Fldt-Parent(Field-Cnt)
             if Fldt-FirstChild(FldInxPar) = 0
               move Field-Cnt to Fldt-FirstChild(FldInxPar)
             end-if
      *      'now update this child
             if Fldt-FirstChild(FldInxPar) not = Field-Cnt
               move Fldt-LastChild(FldInxPar) to Fldt-PrevSib(Field-Cnt)
               set PriorFld to Fldt-LastChild(FldInxPar)
               move Field-Cnt to Fldt-NextSib(PriorFld)
             end-if
             move Field-Cnt to Fldt-LastChild(FldInxPar)
           end-if
           .

      *----------------------------------------------------------------
      *This will take the PRET entry and add it to the Field-Table
      *It is possible not to add to the table if the length is zero,
      * this usually means a varchar field with no length.
      *----------------------------------------------------------------
       Add-Normal-Field.
           if sCursor > In-Bits
           and Not-Forcing-Add-Field
              add 1 to Number-Fields-Skipped
              go to ANF-Exit
           end-if

HK1219*    'check condition on field, if any
v0854      if Pret-Cond(Pret) > 0 then
             move Condition-sw to Save-Cond-Sw
             perform Check-any-Field-Condition
             if Condition-False
                move Save-Cond-Sw to Condition-Sw
                go to ANF-Exit
             end-if
             move Save-Cond-Sw to Condition-Sw
v0854      end-if

           perform GetLen
           if ws-len < 0
              set Discard-Computed-Len-LT-zero to true
v00860        move zeroes to Fldt
              call 'NOTEDISC' using PreT, sCursor, Discard-Table-Area
v00860                    Fldt
              add 1 to fields-in-error
              if First-Field-in-Error = 0
                  move Pret to First-Field-in-Error
                  move Discard-Code to First-Fields-Error
              end-if
              add 1 to discard-count(Discard-Code)
              go to ANF-Exit
           end-if
           if ws-len = 0
             if TRACE-ON
               display 'TRACE:' PRET
                      ':' PRET-FIELDNAME(PRET)(1:23)
                      ':Length zero field added to Field-Tbl'
             end-if
             if return-code = 1 and LENGTH-FIELD-OFFSET(PreT)
               move PRET-LENGTH-FIELD(PreT) to Offset-Field-is-zero
               add 1 to CAT-OFFSET-LEN-ZERO
             end-if
v00855     end-if

v00878*    ' if adding this field would put it past the record length
v00878*    '   then we need to discard this field 'more dll than data'
v00878     if ((sCursor - 1) + ws-len) > In-Bits
v00878     and Not-Forcing-Add-Field
v00878        set Discard-More-DLL-than-Data to true
v00878        move zeroes to Fldt
v00878        call 'NOTEDISC' using PreT, sCursor, Discard-Table-Area
v00878                    Fldt
v00878        add 1 to fields-in-error
v00878        if First-Field-in-Error = 0
v00878            move Pret to First-Field-in-Error
v00878            move Discard-Code to First-Fields-Error
v00878        end-if
v00878        add 1 to discard-count(Discard-Code)
v00878        go to ANF-Exit
v00878     end-if

           move zeroes to return-code


           perform AddField
           if Discard-Something
             set Field-Not-Valid(Field-Cnt) to true
             set Discard-Nothing to true
           end-if
           if Pret <= RT-Last-Base-Pret(Record-to-process)
             move sCursor to Pret-Start(Pret)
           end-if
           set Not-a-vField(Field-Cnt)                 to true
           move PRET-LEVEL(Pret) to FLDT-LEVEL(Field-Cnt)
           if Mig-Requested and MIG-Event-Requested(Pret)
             move PRET-MIG-LEN(Pret) to FLDT-Target-Length(Field-Cnt)
           end-if
           perform Update-Chain
           if TRACE-ON
              move 'Field' to Trace-Event
              perform TRACE-ENTRY
           end-if
      * Done with this entry, point to next possible field position.
           compute sCursor = sCursor + ws-len

v00894* skip over the delimiter, unless at end of record
v00894     if File-CSV-Yes(1)
v00894     and sCursor < in-bits
v00894       compute sCursor = sCursor + (FT-Delim-Len(1) * 8)
v00894     end-if

           .
       ANF-Exit.
           exit.

       Set-Key-Value.
           set ws-key-value-Not-set to true
           move 'C'    to ws-key-type
           move 0      to ws-key-length
           if RT-Key-Field(record-to-process) > 0
            move RT-Key-Field(record-to-process) to Pret
            if Pret-StartField(Pret) > 0
              set ws-key-value-set to true
              move Pret-StartField(Pret) to gvPtr
              perform GetValue
              evaluate true
v0853           when Types-Numeric(Pret) perform Set-Key-Value-Numeric
v0853           when other               perform Set-Key-Value-Character
              end-evaluate
            else
              move '*Key never found*' to ws-key-value
v0853         set ws-key-type-Character to true
v0853         move 17 to ws-key-length
            end-if
           end-if
           if trace-on
             display 'Trace:ws-Key-value-sw=' ws-key-value-sw
              ':Value=' ws-key-value '<<'
              ':Type=' ws-key-type
              ':Len=' ws-key-Length
              ':rec#=' ws-rec-cnt
           end-if
           .
       Set-Key-Value-Numeric.
v0853      evaluate true
v0853        when Discard-Nothing
      *        'set the Key-value and sign
               if ws-value(20:1) = 'S'
                 move ws-value-num18s to WS-EDIT-NUM18S
                 move WS-EDIT-NUM18S to ws-key-value
v0853            set ws-key-type-NumericSigned to true
               else
                 move ws-value-num18 to ws-key-value
v0853            set ws-key-type-NumericUnsigned to true
               end-if

v0853 *        'set the length of the key to be displayed
v0853          evaluate true
v0853            when Type-ZD(Pret)
v0853                  move Fldt-Length-Byte(gvPtr) to ws-key-length
v0853            when Type-PD(Pret)
v0853              compute
v0853                ws-key-length = (Fldt-Length-Byte(gvPtr) * 2) - 1
v0853            when Type-BIN(Pret)
v0853            when Type-BIS(Pret)
v0853              evaluate Fldt-Length-Byte(gvPtr)
v0853                when 1 THRU 2 move 5  to ws-key-length
v0853                when 3 THRU 4 move 10 to ws-key-length
v0853                when 5 THRU 8 move 18 to ws-key-length
v0853              end-evaluate
v0853            when Type-PD-NEC(Pret)
v0853              compute ws-key-length = Fldt-Length-Byte(gvPtr) * 2
v0853            when Type-PD-NEC4(Pret)
v0853              move Fldt-Length-Byte(gvPtr) to ws-key-length
v0853          end-evaluate

             when other
      *        'Invalid numeric key value
               move zeroes               to ws-value-num18
               move 'U'                  to ws-value(20:1)
               move '*Key Not Numeric*'  to ws-Key-value
v0853          set ws-key-type-character to true
v0853          move 17                   to ws-key-length
           end-evaluate
           .
v0853  Set-Key-Value-Character.
v0853      move ws-value                to ws-key-value
v0853      set ws-key-type-Character    to true
v0853      move Fldt-Length-Byte(gvPtr) to ws-key-length
           .

      *This will take the PRET vField and add it to the Field-Table
      *This is for Virtual Field. Most of the time we'll have to
      * determine the value and place also INTO the v-Buffer. The
      * v-Buffer is similar to the in-buffer except instead of getting
      * the data from a source file, it will get it from within the
      * program (thus why it is a virtual field).
      *vCursor is BYTE based
       Add-Normal-vField.
           if vCursor = 1
             move 1 to vCursorBits
           else
             compute vCursorBits = ((vCursor - 1) * 8) + 1
           end-if
save       move sCursor to Save-sCursor
           move vCursorBits to sCursor
           move Pret-length(Pret) to ws-len
           perform AddField
restor     move Save-sCursor to sCursor

           set Is-a-vField(Field-Cnt) to true
           move PRET-LEVEL(Pret) to FLDT-LEVEL(Field-Cnt)
           if Mig-Requested and MIG-Event-Requested(Pret)
             move PRET-MIG-LEN(Pret) to FLDT-Target-Length(Field-Cnt)
           end-if
           if Regular-vField(Pret)
              if RT-First-vField(Record-to-Process) = 0
                move Field-Cnt to RT-First-vField(Record-to-Process)
              end-if
              move Field-Cnt to RT-Last-vField(Record-to-Process)
           end-if
           if Occur-vField(Pret)
           or ODO-vField(Pret)
               perform Add-Field-Index
               perform Update-Chain
           else
               perform Update-Chain
           end-if
           if TRACE-ON
              move 'vFld ' to Trace-Event
              perform TRACE-ENTRY
           end-if
      * update the vField Chain
           add 1 to vField-Unresolved
           if vField-Start = 0
             move Field-Cnt to vField-Start
           end-if
           if vField-Last > 0
             set PriorFld     to vField-Last
             move Field-Cnt   to FLDT-vNext(PriorFld)
             move vField-Last to FLDT-vPrior(Field-Cnt)
           end-if
           move Field-Cnt to vField-Last

      *    'initialize the vfield's content
           evaluate true
             when Type-CH(Pret)
               move spaces to v-Buffer(vCursor:Pret-length-Byte(Pret))
             when Type-ZD(Pret)
               move zeroes to v-Buffer(vCursor:Pret-Length-Byte(Pret))
HK1229       when Type-PD(Pret)
HK1229         move low-values
HK1229           to v-Buffer(vCursor:Pret-Length-Byte(Pret))
HK1229         move x'0F'
HK1229           to v-Buffer(vCursor + Pret-Length-Byte(Pret) - 1:1)
HK1229       when Type-BIS(Pret)
HK1229       when Type-BIN(Pret)
HK1229       when TYPE-NIB(Pret)
HK1229       when TYPE-PD-NEC(Pret)
HK1229       when TYPE-PD-NEC4(Pret)
HK1229         move low-values
HK1229           to v-Buffer(vCursor:Pret-Length-Byte(Pret))
      *      when Type-BIT(Pret) **need to deal with this**
             when other
               display 'engine:internal error:vfield:' Pret
                 ':unknown type'
               move 12 to return-code
               stop run
           end-evaluate

      * update the vField if at all possible
           move field-Cnt to FPtr
           move vCursor to uvCursor
           move Pret    to uvPret
           move FPtr    to uvFPtr
           perform UpdateV
           move zeroes to return-code


           add Pret-Length-Byte(Pret) to vCursor
           .
       ANvF-Exit.
           exit.

       TRACE-ENTRY.
           if Fields-in-Error > 0
             display 'TRACE:Field in error'
               '##:Pret=' Pret
               ':Field-cnt=' Field-Cnt
           end-if
           move Fldt-Start-Byte(Field-Cnt) to ws-num5a
           move Fldt-Length-Byte(Field-Cnt) to ws-num5b
           if ws-key-value = low-values
             move spaces to trace-misc
           else
             move ws-key-value to trace-misc
           end-if
           display
               Trace-Event
               ' ' Field-Cnt
               ' <' ws-num5a
               '.' ws-num5b '>'
               ' ' PRET
               ' ' PRET-FIELDNAME(Pret)
               ' ' PRET-ENTRY-TYPE(PRET)
               ' ' PRET-TYPE(PRET)
               ' ' ws-value(1:20)
               ' ' trace-misc
           .

       PI-DISCARD-INPUT.
           add 1 to CAT-DISCARDS
           move zeroes to wsDiscardNonNumeric
           perform varying DISCARD-PTR from 1 by 1
                      until DISCARD-PTR > DISCARD-CNT
v00869       evaluate true
v00869         when DT-Reason(Discard-Ptr) = 1
v00869           add 1 to wsDiscardNonNumeric
v00869           if wsDiscardNonNumeric <= OPT-LogShowNonNumerics
v00869             perform Log-the-discard-entry
v00869           end-if
v00869         when DT-Reason(Discard-Ptr) = 12 and Opt-DiscardUnMatch
v00869           perform Log-the-discard-entry
v00869         when DT-Reason(Discard-Ptr) = 12
v00869           continue
v00869         when other
v00869          perform Log-the-discard-entry
v00869       end-evaluate
             if FDD-Requested
               set Drop-FDD to true
               perform FDD
             end-if
           end-perform
           if Discard-Cnt = 1
           and DT-Reason(1) = 12
           and Opt-DiscardUnMatch-No
             continue
           else
             evaluate true
                when FILE-IS-FIX-LENGTH(1)
                   write DISCARD-FIX-REC from IN-BUFFER(1:IN-LEN)
                when FILE-IS-VARIABLE-LENGTH(1)
                   write DISCARD-VAR-REC from IN-BUFFER(1:IN-LEN)
             end-evaluate
             if IO-STATUS NOT = '00'
               display 'ENGINE:ERROR WRITING DISCARD:' IO-STATUS
                ':INLEN=' IN-LEN
               move 12 to return-code
               stop run
             end-if
           end-if
           .
v00869 Log-the-discard-entry.
v00895     move 'DISCARD' to LD-Message
v00878     move spaces to LD-Kampo-Extra-Details
           move WS-REC-CNT                     to LD-REC-CNT
           move DT-FIELD(Discard-Ptr)          to LD-FIELD
           move DT-Fldt(Discard-Ptr)           to Fldt
           if Fldt > 0
v00886       if Pret-Dims(LD-Field) = 0
v00886         move PRET-FIELDNAME(LD-Field)   to LD-Field-Name
v00886       else
               move spaces                     to LD-Field-Name
               call 'FmtInd' using
                  FLDT-INDEX-TABLE(Fldt), Pret-Dims(LD-Field), Indice
               string PRET-FIELDNAME(LD-Field) delimited by SPACES
                      Indice                   delimited by SPACES
                 into LD-Field-Name
               end-string
v00886       end-if
           else
v00886       move spaces                       to LD-Field-Name
           end-if
           move DT-START (DISCARD-PTR)         to LD-POS
v00886     if FLDT > 0
v00885       if DT-Reason(Discard-Ptr) = 3
v00885         move Fldt-Length-Byte(FLDT) to LD-Length
v00885       else
               move Pret-Length-Byte(LD-FIELD)   to LD-Length
v00885       end-if
v00886     else
v00886       move zeroes to LD-Length
v00886     end-if
           move DT-REASON (DISCARD-PTR)        to LD-REASON-CODE
v00878     evaluate DT-Reason(Discard-Ptr)
v00878       when 4
v00878       when 5
v00878       when 12
v00878       when 13
v00878         move spaces to LD-Reason-Msg
v00878         move In-Len to ws-num5
v00878         string Discard-Reason(LD-Reason-Code)
v00878             (1:Discard-Length(LD-Reason-Code)) delimited by size
v00878              ' LENGTH='                        delimited by size
v00878              ws-num5                           delimited by size
v00878           into LD-Reason-Msg
v00878         end-string
             when 11
      *        'Kampo API-override reason msg and show more details
v00895         move 'DISCARD' to LD-Message
               move 'API-VGXBCNV9 RC = '  to LDKM-Lit2
               move DT-Data1(Discard-Ptr) to LDKM-Data1
               move ' CharCd:'            to LDKM-Lit3
               move Log-Discard-kampo-message to LD-Reason-Msg
v00878*        'Format the value into hex notation i.e. x'FF'
v00878         move "x'0000'" to LDKM-Data2x
v00878         move spaces to WS-Hex
v00878         move 2 to tSize
v00878         call 'HEXSHOW' USING
v00878              DT-Data2x(Discard-Ptr)(1:2), tSize, WS-Hex
v00878         move ws-hex(1:4) to LDKM-Data2(3:4)
               move DT-Data3(Discard-Ptr) to LDKM-Data3
               move ' POS:' to LDKM-Lit1
v00886       when 16
v00886*        'Kampo API-EEOR-override reason msg and show RC value
               move 'API-EEOR     RC = '  to LDKM-Lit2
               move DT-Data1(Discard-Ptr) to LDKM-Data1
               move spaces                to LDKM-Lit3
               move Log-Discard-kampo-message to LD-Reason-Msg
v00878       when other
v00878         move DISCARD-REASON(LD-REASON-CODE) to LD-REASON-MSG
v00878     end-evaluate
      *    'show hex value when discard code is 1 (Not Numeric field)
           if DT-Reason(Discard-Ptr) = 1
           and Fldt > 0
           and Fldt-Start-Byte(Fldt) > 0
           and Fldt-Length-Byte(Fldt) > 0
      *      'Format the value into hex notation i.e. x'FF'
             call 'HEXSHOW' USING
              in-buffer
              (Fldt-Start-Byte(Fldt):Fldt-Length-Byte(Fldt))
              Fldt-Length-Byte(Fldt), WS-HEX
             move "x'" to LD-Reason-Msg(19:2)
             compute N = Fldt-Length-Byte(Fldt) * 2
             move ws-hex(1:N) to LD-Reason-Msg(21:N)
             move "'"              to LD-Reason-Msg(21 + N:1)
           end-if
           write LOG-RECORD from LOG-DISCARD
           .
       PI-OPEN-FILES.
           if TRACE-ON
             display 'TRACE:ENGINE:PI-OPEN-FILES'
           end-if
           set Open-FDD-File to true
           perform FDD
           evaluate true
             when FILE-IS-FIX-LENGTH(1)      OPEN INPUT IN-FIX
             when FILE-IS-VARIABLE-LENGTH(1) OPEN INPUT IN-VAR
           end-evaluate
           if IO-STATUS NOT = '00'
               move FT-Format(1) to ws-num1
               move SPACES to LOG-RECORD
               string 'INPUT FILE ERROR:' delimited by size
                     IO-STATUS            delimited by size
                     '. DLL Format is '   delimited by size
                     ws-num1
                     ' (0=Fixed, 1=Variable)' delimited by size
                 into LOG-RECORD
               go to TERMINATE-WITH-ERROR
           end-if
MFMFMF*    perform DCB-INFO-SPECIAL
           call 'HKSys' using DCB-Info-Special, IN-DDName, Dataset-Name
           move
MFMFMF          DN-DCB-LRECL
PCPCPC*         ZEROES
                             to FT-FILE-LENGTH(1)
                                IN-LEN
           compute in-bits = in-len * 8
           display 'Engine:Max INFILE LRecl=' FT-FILE-LENGTH(1)
                ':Bits=' in-bits ':FMT=' FT-FORMAT(1)
           if OPT-ShowProgress > 0
               accept date-stamp from date YYYYMMDD
               accept time-stamp from time
               compute TimeStamp = (date-stamp * 100000000) + time-stamp
               move TimeStamp to WS-TimeStamp
               move ws-ts-year   to ODBC-TS-Year
               move ws-ts-month  to ODBC-TS-Month
               move ws-ts-day    to ODBC-TS-Day
               move ws-ts-hour   to ODBC-TS-Hour
               move ws-ts-minute to ODBC-TS-Minute
               move ws-ts-second to ODBC-TS-Second
               if (TimeStamp - TimeStamp-Prev) <= 99
                 add 1 to ODBC-TS-Sequence
               else
                 move 1 to ODBC-TS-Sequence
               end-if
               display 'INFILE:Open :' ws-rec-cnt
                  ' ' ODBC-TimeStamp-Value
                  upon syspunch
           end-if
           evaluate true
             when FILE-IS-FIX-LENGTH(1)
               OPEN OUTPUT DISCARD-FIX
             when FILE-IS-VARIABLE-LENGTH(1)
               OPEN OUTPUT DISCARD-VAR
           end-evaluate
           if IO-STATUS NOT = '00'
             move SPACES to LOG-RECORD
             string 'DISCARD OPEN FILE ERROR:' delimited by size
                   IO-STATUS            delimited by size
               into LOG-RECORD
             go to TERMINATE-WITH-ERROR
           end-if
           .
       PI-READ-RECORD.
           evaluate true
v0854        when CAT-Discards > Opt-ErrorLimit
v0854          or WS-REC-CNT >= Opt-StopAft
v0854          set Process-Record-Stop to true
             when FILE-IS-FIX-LENGTH(1)
               read IN-FIX
v0854            at end
v0854               set Process-Record-Stop to true
                 not at end
                        add 1 to WS-REC-CNT
v0854                   compute in-bits = IN-LEN * 8
v0854                   move IN-FIX-REC(1:IN-LEN) to IN-BUFFER(1:IN-LEN)
                        if OPT-ShowProgress > 0
                           perform Update-Progress-Bar
                        end-if
               end-read
             when FILE-IS-VARIABLE-LENGTH(1)
               read IN-VAR
v0854            at end
v0854               set Process-Record-Stop to true
                 not at end
                        add 1 to WS-REC-CNT
v0854                   compute in-bits = IN-LEN * 8
v0854                   move IN-VAR-REC(1:IN-LEN) to IN-BUFFER(1:IN-LEN)
                        if OPT-ShowProgress > 0
                           perform Update-Progress-Bar
                        end-if
               end-read
           end-evaluate
v00887     if not Process-Record-Stop
v00887       add in-len to In-Byte-Count
v00887       if in-len > in-len-longest
v00887         move in-len to in-len-longest
v00887         move ws-rec-cnt to in-len-longest-rec
v00887       end-if
v00887       if in-len < in-len-shortest
v00887         move in-len to in-len-shortest
v00887         move ws-rec-cnt to in-len-shortest-rec
v00887       end-if
v00887     end-if
           .
       Update-Progress-Bar.
           divide ws-rec-cnt by Opt-ShowProgress giving Tally
             remainder Remaining
           if Remaining = 0
           or ws-rec-cnt = 1
           or ws-rec-cnt = 2
             accept date-stamp from date YYYYMMDD
             accept time-stamp from time
             compute TimeStamp = (date-stamp * 100000000) + time-stamp
             move TimeStamp to WS-TimeStamp
             move ws-ts-year     to ODBC-TS-Year
             move ws-ts-month    to ODBC-TS-Month
             move ws-ts-day      to ODBC-TS-Day
             move ws-ts-hour     to ODBC-TS-Hour
             move ws-ts-minute to ODBC-TS-Minute
             move ws-ts-second to ODBC-TS-Second
             if (TimeStamp - TimeStamp-Prev) <= 99
               add 1 to ODBC-TS-Sequence
             else
               move 1 to ODBC-TS-Sequence
             end-if
             display 'INFILE:#Read=' ws-rec-cnt
                ' ' ODBC-TimeStamp-Value
                upon syspunch
           end-if
           .
       PI-CLOSE-FILES.
           if TRACE-ON
             display 'ENGINE:PI-CLOSE-FILES'
           end-if

           set Close-FDD-File to true
           perform FDD

           evaluate true
             when FILE-IS-FIX-LENGTH(1)
               CLOSE IN-FIX
                     DISCARD-FIX
             when FILE-IS-VARIABLE-LENGTH(1)
               CLOSE IN-VAR
                     DISCARD-VAR
           end-evaluate
           if OPT-ShowProgress > 0
               accept date-stamp from date YYYYMMDD
               accept time-stamp from time
               compute TimeStamp = (date-stamp * 100000000) + time-stamp
               move TimeStamp to WS-TimeStamp
               move ws-ts-year   to ODBC-TS-Year
               move ws-ts-month  to ODBC-TS-Month
               move ws-ts-day    to ODBC-TS-Day
               move ws-ts-hour   to ODBC-TS-Hour
               move ws-ts-minute to ODBC-TS-Minute
               move ws-ts-second to ODBC-TS-Second
               if (TimeStamp - TimeStamp-Prev) <= 99
                 add 1 to ODBC-TS-Sequence
               else
                 move 1 to ODBC-TS-Sequence
               end-if
               display 'INFILE:Close:' ws-rec-cnt
                  ' ' ODBC-TimeStamp-Value
                  upon syspunch
           end-if
           .
      *--------------------------------------------------------------
       SHOW-COPYRIGHT.
           display PROGRAM-COPYRIGHT
                   ' Version:' Version
           move Version to Engine-Version
           .
      *--------------------------------------------------------------

       START-LOG-FILE.
           OPEN OUTPUT LOG-FILE

           move spaces to log-record
           move PROGRAM-COPYRIGHT to Log-record
           move ' Version:' to Log-record(63:9)
           move Version to Log-record(72:8)
           write Log-record
           move '_' to Log-record
           write log-record
           move spaces to log-record

           if OPT-LogShowSysInfo
             call 'HKSys' using Report-Sys-Info, Version
           end-if
           .

       TERMINATE-WITH-ERROR.
           display 'ENGINE:' LOG-RECORD
           write LOG-RECORD
           CLOSE LOG-FILE
           move 12 to RETURN-CODE
           STOP RUN
           .
       END-PROGRAM.
           if Opt-LogShowTotals
             perform LogShowTotals
           end-if

           CLOSE LOG-FILE

           display 'ENGINE:-------------Engine Stats------------------*'
           display 'Counters and totals:'
           display '# of DLL records.............'  CAT-CONTROL-IN
           display "# of DLL 'FILE' commands....."  CAT-CONTROL-FILE
           display "# of DLL 'RECORD' commands..."  CAT-CONTROL-RECORD
           display "# of DLL 'TABLE' commands...."  CAT-CONTROL-TABLE
           display "# of DLL 'FIELD' commands...."  CAT-CONTROL-FIELD
           display "# of DLL 'vFIELD' commands..."  CAT-CONTROL-vFIELD
           display '# of Input record............'  WS-REC-CNT
           if ws-rec-cnt > 0
             compute ws-average-in-len = in-byte-count / ws-rec-cnt
           else
             move zeroes to ws-average-in-len
           end-if
           display ' Average input record length.'  ws-average-in-len
           display ' Shortest Record Length......'  in-len-shortest
                   ' at record# ' in-len-shortest-rec
           display ' Longest Record Length.......'  in-len-longest
                   ' at record# ' in-len-longest-rec
           display '# of Discard records.........'  CAT-DISCARDS
           display '# of Good records............'  CAT-GOOD-RECORDS
           display '# of No Group at EOR.........'  CAT-GROUP-EOR
           display '# of define GAP in bits......'  GIB-Count
             ' First field at ' GIB-First-Field
           display 'Highest DefinedFields........'  Prefield-cnt
           display 'Highest DefinedTables........'  Table-Cnt
           display 'Highest MaxRecordFields......'  Highwater-Field-Cnt
           display 'Highest DefinedConds.........'  Cond-Cnt
           display 'Highest Field-Cnt............'  Highwater-Field-Cnt
           display 'Lowest Field-Cnt.............'  Lowwater-Field-Cnt
           display 'Max-Occurs-Zero..............'  Max-Occurs-Zero
           if Opt-FixZDWhenLowHigh-Yes
            display 'FixZDwhenLowHigh-cnt.........' FixZDwhenLowHigh-cnt
             ' First Record at ' FixZDwhenLowHigh-rec
             ' Field at ' FixZDwhenLowHigh-Field
           end-if
           if Opt-FixPDWhenLowHigh-Yes
            display 'FixPDwhenLowHigh-cnt.........' FixPDwhenLowHigh-cnt
              ' First Record at ' FixPDwhenLowHigh-rec
              ' Field at ' FixPDwhenLowHigh-Field
           end-if
           display '# of LOWVAL  fields fixed....'  FixLow-Cnt
           display '# of HIGHVAL fields fixed....'  FixHigh-Cnt
           display '# of HIGHVAL fields kept.....'  FixHigh-Kept-Cnt
           display '# of SPACE   fields fixed....'  FixSpace-Cnt

v00878     display 'Highest used Log count.......'  Log-Highest-Used
v00878             ' out of ' log-max

           display 'Engine:Discard Summary. '
           display '## Discard Reason-----------------------------'
                   ' ----Count'
           perform varying N from 1 by 1 until N > Discard-Reason-Max
             if Discard-Length(N) > 0
              move N to ws-num2
              move all '.' to FTL-Description
              move Discard-Reason(N)(1:Discard-Length(N))
                to FTL-Description(1:Discard-Length(N))
              move Discard-Count(N) to FTL-Count
              display ws-num2 ' ' FDD-Total-Line
             end-if
           end-perform
           display '-----------------end of list-----------------'

           display 'ENGINE:----------End of Engine Stats--------------*'
           move return-code to RC

      *    'free this storage from user heap
MFMFMF     call "CEEFRST" using ADDRSS, FC
MFMFMF     if CEE000 of FC then
      *        'discard user heap
MFMFMF         call "CEEDSHP" using HEAPID, FC
MFMFMF         if CEE000 THEN
MFMFMF             continue
MFMFMF         else
MFMFMF             display "Engine:Error discarding user heap"
MFMFMF         end-if
MFMFMF     else
MFMFMF         display "Engine:Error freeing storage from heap"
MFMFMF     end-if

           call 'HKSys' using Print-CPUTimer
           move RC to return-code
v00890     if RC = 0 and CAT-DISCARDS > 0
v00890       move 4 to return-code
v00891                 RC
v00890     end-if

v00895     if OPT-CnvDiscard-No
v00895     and CAT-num-api-VGFBCNV1-Err > 0
v00895       move 1 to return-code, RC
v00895     end-if

           Display 'Engine v' Version ' ended (rc=' RC ')'
           stop run
           .
      *-------------------------------------------------------------
      * This will write the Main table entry to the Field-Table.
      * It will then run the generator for each sub-field
      *-------------------------------------------------------------
       Add-Main-Table.
           if sCursor > In-Bits
              go to AMT-Exit
           end-if
           if Trace-On perform Trace-Start-of-Main-Table end-if

      * Intialize start of Main (level 1) table fields
           move low-values to Generator-Area
           move 1 to WS-DIM
           move Pret to Orig-Pret
           move Pret-Level(Pret) to ws-level
           move Pret-Table(Pret) to gTblPtr(ws-level)
                                    TblPtr
           if Table-is-Group(tblPtr)
             move 0 to ws-dim
           end-if
           move 0 to ws-sub-cnt
           move 0 to CURRENT-OCCURS
           move 0 to Stack-Cnt
           move low-values to stack-table-area
           move low-values to ABS-Index-area

      * Determine table length if possible
           perform Determine-Table-length
           if Stop-Generating
             if trace-on display 'TRACE:L1:Stop-Generating'       end-if
             go to AMT-Exit
           end-if
           if Table-Limit-or-Length-Set(TblPtr) and ws-len = 0
             if trace-on display 'TRACE:L1:Limit/Length & len=0'  end-if
             go to AMT-Exit
           end-if
           move zeroes to return-code

      * check if this table matches any Condition statement
      * note. an non-existant condition also means Condition-True
           set Condition-true to true
v0854      if Table-Cond(TblPtr) > 0 then
             perform Check-any-Table-Condition
             if Condition-False
               go to AMT-Exit
             end-if
v0854      end-if

      * Determine number of entries (Max-Occurs)
      * Should the max-occurs be zero then exit out.
      * Still write out the table but with zero length, etc.
           perform Determine-Number-Entries
           if Max-Occurs = 0
             perform Add-an-Empty-Table-Field
             go to AMT-exit
           end-if
      * Determine the maximum EntryLength size, if stated.
           perform Determine-EntryLength-Size thru DES-Exit
           if Stop-Generating
             go to AMT-Exit
           end-if

      * Determine size of table (end-tbl)
      * if fix occurs or NO LIMIT nor DEPEND then use the record length
      * Still write out the table but with zero length, etc.
           perform Determine-end-of-Table
           if sCursor >= ws-end-tbl
             perform Add-an-Empty-Table-Field
             go to AMT-exit
           end-if
           move ws-end-tbl to ws-end-tbl-L1 ws-end-tbl-L2 ws-end-tbl-L3
                              ws-end-tbl-L4 ws-end-tbl-L5 ws-end-tbl-L6
                              ws-end-tbl-L7 ws-end-tbl-L8 ws-end-tbl-L9

      * Determine Start and Ending entries of the PreField-Table
           compute START-PRET = PRET + 1
           compute END-PRET = START-PRET +
                              TABLE-NUMBER-OF-FIELDS(TBLPTR) - 1

           if TRACE-ON perform Trace-Table-Stats end-if

           perform Push-stack
           perform Generator

           if Trace-On perform Trace-End-of-Main-Table end-if
           .
       AMT-Exit.
           exit.

      *----------------------------------------------------------------
      *This will create all the sub-Fields for the table
      *STOP when reached END OF TABLE LENGTH or
      *  passed the number of fields needed
      *  or passed the END OF RECORD
      *  or we have a condition that did not pass.
      *There is a main loop and a nested loop:
      * Main Loop - loop the number of OCCURRENCES in the data
      *  Sub-Loop - loop the number of FIELDS for this occurrence
      *NOTE. It is expected that the last record in the Field-Table
      *  was the Table when this routine begins
      *NOTE. a non-existant Condition means Condition is true
      *----------------------------------------------------------------
       Generator.
           if Trace-on perform Trace-Loop-Start end-if

           move zeroes to Stop-Reason, Stop-Reason-Fields
           perform until Stop-Reason > 0
             perform Add-an-Occurrence
           end-perform

           if Trace-on perform Trace-Loop-Stops end-if

           move orig-pret to pret
           if current-occurs < Table-Lowest(TBLPTR)
             perform Discard-Table-in-Error
           end-if
           .

       Add-an-Occurrence.
           add 1 to CURRENT-OCCURS
           move zeroes to Occur-Size(1)
           Occur-Size(2), Occur-Size(3), Occur-Size(4)
           if Current-Occurs > 1
             move ORIG-PRET to PRET
v0854        if Table-Cond(TblPtr) > 0 then
               perform Check-any-Table-Condition
v0854        end-if
             perform Determine-EntryLength-Size thru DES-Exit
           end-if
           evaluate true
             when sCursor >= WS-END-TBL         move 1 to Stop-Reason
             when CONDITION-FALSE               move 2 to Stop-Reason
             when sCursor > IN-Bits             move 3 to Stop-Reason
             when CURRENT-OCCURS > Max-Occurs move 4 to Stop-Reason
             when Fields-in-Error > 0           move 5 to Stop-Reason
             when Stop-Generating               move 6 to Stop-Reason
             when other
               perform Add-the-Table-Field thru ATTF-Exit
               move field-cnt to Table-Field-Cnt(ws-level)
               perform Add-all-the-fields
           end-evaluate
           if Stop-Reason = 4
             subtract 1 from Current-Occurs
           end-if
           .
       Add-all-the-fields.
           move zeroes to Stop-Reason-Fields
           move zeroes to Last-Pret
           perform varying PRET from START-PRET by 1
              until Stop-Reason-Fields > zeroes
             perform Check-Stop-Reason-for-Fields
             if Stop-Reason-Fields = zeroes
               perform Add-a-Field thru AAF-Exit
               move pret to last-pret
             end-if
           end-perform
           if trace-on
             move 'Stop1' to Trace-Event
             move Stop-Reason-Fields to tr-num2a
             move Stop-Generating-sw to tr-num1a
             display Trace-Event
               ' Reason=' tr-num2a
               ':gen=' tr-num1a
               ':cur-occur=' current-occurs
               ':level=' ws-level
               ':dim=' ws-dim
               ':pret=' Pret
               ':end-pret=' End-Pret
               ':Last=' Last-Pret
           end-if
           if Keep-Generating
             evaluate Stop-Reason-Fields
               when 1
               when 3
               when 7
                 perform Check-for-Mandatory-fields
                 if Fields-in-Error = 0
                 and End-Pret not = Last-Pret
                   perform Add-empty-fields
                 else
                   if Stop-Reason-Fields not = 7
                     move Stop-Reason-Fields to Stop-Reason
                   end-if
                 end-if
               when 2
               when 4
                 continue
               when 5
               when 6
                 move Stop-Reason-Fields to Stop-Reason
             end-evaluate
           else
             move Stop-Reason-Fields to Stop-Reason
           end-if
           .
       Add-a-Field.
           evaluate true
             when TABLE-FIELD(PRET)
                  perform AAF-ADD-L-2-Table thru AAL2T-exit
                  perform Update-Table-from-Previous
             when Regular-vField(PreT)
                  perform ATF-ADD-vFIELD-FIELD thru AAvF-exit
             when Group-Field(Pret)
                  display 'Engine:add-a-field:Group command depracted'
                   ':pret=' Pret ':rec#=' ws-rec-cnt
                  move 12 to return-code
                  stop run
             when other
                  perform ATF-ADD-FIELD-FIELD thru AAFF-exit
           end-evaluate
           .
       AAF-Exit.
           exit.

      *-------------------------------------------------------------*
      * Level 2 Area                                                *
      *-------------------------------------------------------------*
      * Embedded TABLE-OCCUR or TABLE-ODO table.
      * This is for Level 2. Level 1 was the MAIN table.
      *-------------------------------------------------------------*
       AAF-ADD-L-2-Table.
           add 1 to ws-sub-cnt
           if TRACE-ON perform Trace-Start-Level end-if
           perform Push-stack

           move Pret to Orig-Pret
           move Pret-Level(Pret) to ws-level
           move Pret-Table(Pret) to gTblPtr(ws-level)
                                    Tblptr
           move Table-Dims(TblPtr) to ws-dim
           move zeroes to occur-size(2)
                          occur-size(3), Occur-size(4)
           move 0 to Current-Occurs
           if ws-Dim > Table-Dims(Tblptr) go to Dim-Overflow end-if

      * Determine table length if possible (ws-len)
           perform Determine-Table-length
           if Stop-Generating
             if trace-on display 'TRACE:L2:Stop-Generating'       end-if
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL2T-exit
           end-if
           if Table-Limit-or-Length-Set(TblPtr) and ws-len = 0
             if trace-on display 'TRACE:L2:Limit/Length & len=0'  end-if
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL2T-exit
           end-if
           move zeroes to return-code

      * check if this table matches any Condition statement
      * note. an non-existant condition also means Condition-True
           set Condition-true to true
v0854      if Table-Cond(TblPtr) > 0 then
             perform Check-any-Table-Condition
             if Condition-False
               if trace-on display 'TRACE:L2:Condition False'     end-if
               perform Pull-Stack
               subtract 1 from ws-sub-cnt
               go to AAL2T-exit
             end-if
v0854      end-if


      * Max-Occurs
           perform Determine-Number-Entries
           if Max-Occurs = 0
             if trace-on display 'TRACE:L2:Max-Occurs=0'          end-if
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL2T-exit
           end-if

      * Determine the maximum EntryLength size, if stated.
           perform Determine-EntryLength-Size thru DES-Exit
           if Stop-Generating
             if trace-on display 'TRACE:L2:Stop-Generating(EL)'   end-if
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL2T-exit
           end-if

      * Set ws-end-tbl
           perform Determine-end-of-Table
      * cannot go past outer table's length (previous table)
           if ws-end-tbl > ws-end-tbl-L1
             move ws-end-tbl-L1 to ws-end-tbl
           end-if
           move ws-end-tbl to ws-end-tbl-L2
           if sCursor >= ws-end-tbl
             if trace-on display 'TRACE:L2:Cursor>End-Tbl'        end-if
             move zeroes to Current-Occurs
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL2T-exit
           end-if


      * Determine Start and Ending entries of the PreField-Table
           compute START-PRET = PRET + 1
           compute END-PRET = START-PRET +
                              TABLE-NUMBER-OF-FIELDS(TBLPTR) - 1

           if TRACE-ON perform Trace-Table-Stats end-if
           perform Generator-L2


           if trace-on display 'TRACE:L2:Table Complete'        end-if
           perform Pull-Stack
           if trace-on perform Trace-end-Level end-if
           subtract 1 from ws-sub-cnt
           .
       AAL2T-exit.
           exit.

       Generator-L2.
           if Trace-on perform Trace-Loop-Start end-if

           move zeroes to Stop-Reason, Stop-Reason-Fields
           perform until Stop-Reason > 0
             add 1 to CURRENT-OCCURS
             perform Add-an-Occurrence-L2
           end-perform

           if Trace-on perform Trace-Loop-Stops end-if

           if current-occurs < Table-Lowest(TBLPTR)
             perform Discard-Table-in-Error
           end-if
           .
       Add-an-Occurrence-L2.
           move zeroes to Occur-Size(2)
                          Occur-Size(3), Occur-Size(4)
           if Current-Occurs > 1
             move ORIG-PRET to PRET
v0854        if Table-Cond(TblPtr) > 0 then
               perform Check-any-Table-Condition
v0854        end-if
             perform Determine-EntryLength-Size thru DES-Exit
           end-if
           evaluate true
             when sCursor >= WS-END-TBL         move 1 to Stop-Reason
             when CONDITION-FALSE               move 2 to Stop-Reason
             when sCursor > IN-Bits             move 3 to Stop-Reason
             when CURRENT-OCCURS > Max-Occurs   move 4 to Stop-Reason
             when Fields-in-Error > 0           move 5 to Stop-Reason
             when Stop-Generating               move 6 to Stop-Reason
             when other
               perform Add-the-Table-Field thru ATTF-Exit
               move field-cnt to Table-Field-Cnt(ws-level)
               perform Add-all-the-fields-L2
           end-evaluate
           if Stop-Reason = 4
             subtract 1 from Current-Occurs
           end-if
           .
       Add-all-the-fields-L2.
           move zeroes to Stop-Reason-Fields
           perform varying PRET from START-PRET by 1
              until Stop-Reason-Fields > zeroes
             perform Check-Stop-Reason-for-Fields
             if Stop-Reason-Fields = zeroes
               perform Add-a-Field-L2 thru AAFL2-Exit
               move pret to last-pret
             end-if
           end-perform
           if trace-on
             display 'TRACE:' ws-sub-cnt '(2)field Loop stops. Reason='
               Stop-Reason-Fields
               ':current-occurs=' current-occurs
               ':level=' ws-level
               ':dim=' ws-dim
           end-if
           if Keep-Generating
             evaluate Stop-Reason-Fields
               when 1
               when 3
               when 7
                 perform Check-for-Mandatory-fields
                 if Fields-in-Error = 0
                 and End-Pret not = Last-Pret
                   perform Add-empty-fields
                 else
                   move Stop-Reason-Fields to Stop-Reason
                 end-if
               when 2
               when 4
                 continue
               when 5
               when 6
                 move Stop-Reason-Fields to Stop-Reason
             end-evaluate
             move ORIG-PRET to PRET
           else
             move Stop-Reason-Fields to Stop-Reason
           end-if
           .
       Add-a-Field-L2.
           evaluate true
             when TABLE-FIELD(PRET)
                  perform AAF-ADD-L-3-Table thru AAL3T-exit
                  perform Update-Table-from-Previous
             when Regular-vField(PreT)
                  perform ATF-ADD-vFIELD-FIELD thru AAvF-exit
             when Group-Field(Pret)
                  display 'Engine:add-a-field2:Group command depracted'
                   ':pret=' Pret ':rec#=' ws-rec-cnt
                  move 12 to return-code
                  stop run
             when other
                  perform ATF-ADD-FIELD-FIELD thru AAFF-exit
           end-evaluate
           .
       AAFL2-Exit.
           exit.

      *-------------------------------------------------------------*
      * Level 3 Area                                                *
      *-------------------------------------------------------------*
      * Embedded TABLE-OCCUR or TABLE-ODO table.
      * This is for Level 3. Level 1 was the MAIN table.
      *-------------------------------------------------------------*
       AAF-ADD-L-3-Table.
           add 1 to ws-sub-cnt
           if TRACE-ON perform Trace-Start-Level end-if
           perform Push-stack

           move Pret to Orig-Pret
           move Pret-Level(Pret) to ws-level
           move Pret-Table(Pret) to gTblPtr(ws-level)
                                    TblPtr
           move Table-Dims(TblPtr) to ws-dim
           move zeroes to occur-size(3)
                          occur-size(4)
           move 0 to Current-Occurs
           if ws-Dim > Table-Dims(Tblptr) go to Dim-Overflow end-if

      * Determine table length if possible (ws-len)
           perform Determine-Table-length
           if Stop-Generating
             if trace-on display 'TRACE:L3:Stop Generating'       end-if
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL3T-exit
           end-if
           if Table-Limit-or-Length-Set(TblPtr) and ws-len = 0
             if trace-on display 'TRACE:L3:Limit/Length & len=0'  end-if
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL3T-exit
           end-if
           move zeroes to return-code

      * check if this table matches any Condition statement
      * note. an non-existant condition also means Condition-True
           set Condition-true to true
v0854      if Table-Cond(TblPtr) > 0 then
             perform Check-any-Table-Condition
             if Condition-False
               if trace-on display 'TRACE:L3:Condition False'     end-if
               perform Pull-Stack
               subtract 1 from ws-sub-cnt
               go to AAL3T-exit
             end-if
v0854      end-if


      * Max-Occurs
           perform Determine-Number-Entries
           if Max-Occurs = 0
             if trace-on display 'TRACE:L3:Max-Occurs = 0'        end-if
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL3T-exit
           end-if

      * Determine the maximum EntryLength size, if stated.
           perform Determine-EntryLength-Size thru DES-Exit
           if Stop-Generating
             if trace-on display 'TRACE:L3:Stop-Generating(EL)'   end-if
             go to AAL3T-exit
           end-if

      * Set ws-end-tbl
           perform Determine-end-of-Table
      * cannot go past outer table's length (previous table)
           if ws-end-tbl > ws-end-tbl-L1
             move ws-end-tbl-L1 to ws-end-tbl
           end-if
           move ws-end-tbl to ws-end-tbl-L3
           if sCursor >= ws-end-tbl
             if trace-on
               display 'TRACE:L3:Cursor>=End-Tbl'
                ':End-tbl-L1=' ws-end-tbl-L1
                ':L2=' ws-end-tbl-L2
             end-if
             move zeroes to Current-Occurs
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL3T-exit
           end-if

      * Determine Start and Ending entries of the PreField-Table
           compute START-PRET = PRET + 1
           compute END-PRET = START-PRET +
                              TABLE-NUMBER-OF-FIELDS(TBLPTR) - 1

           if TRACE-ON perform Trace-Table-Stats end-if
           perform Generator-L3


           if trace-on display 'TRACE:L3:Table Complete'        end-if
           perform Pull-Stack
           if trace-on perform Trace-end-Level end-if
           subtract 1 from ws-sub-cnt
           .
       AAL3T-exit.
           exit.

       Generator-L3.
           if Trace-on perform Trace-Loop-Start end-if

           move zeroes to Stop-Reason, Stop-Reason-Fields
           perform until Stop-Reason > 0
             add 1 to CURRENT-OCCURS
             perform Add-an-Occurrence-L3
           end-perform

           if Trace-on perform Trace-Loop-Stops end-if

           if current-occurs < Table-Lowest(TBLPTR)
             perform Discard-Table-in-Error
           end-if
           .
       Add-an-Occurrence-L3.
           move zeroes to Occur-Size(3)
                          Occur-Size(4)
           if Current-Occurs > 1
             move ORIG-PRET to PRET
v0854        if Table-Cond(TblPtr) > 0 then
               perform Check-any-Table-Condition
v0854        end-if
             perform Determine-EntryLength-Size thru DES-Exit
           end-if
           evaluate true
             when sCursor >= WS-END-TBL         move 1 to Stop-Reason
             when CONDITION-FALSE               move 2 to Stop-Reason
             when sCursor > IN-Bits             move 3 to Stop-Reason
             when CURRENT-OCCURS > Max-Occurs   move 4 to Stop-Reason
             when Fields-in-Error > 0           move 5 to Stop-Reason
             when Stop-Generating               move 6 to Stop-Reason
             when other
               perform Add-the-Table-Field thru ATTF-Exit
               move field-cnt to Table-Field-Cnt(ws-level)
               perform Add-all-the-fields-L3
           end-evaluate
           if Stop-Reason = 4
             subtract 1 from Current-Occurs
           end-if
           .
       Add-all-the-fields-L3.
           move zeroes to Stop-Reason-Fields
           perform varying PRET from START-PRET by 1
              until Stop-Reason-Fields > zeroes
             perform Check-Stop-Reason-for-Fields
             if Stop-Reason-Fields = zeroes
               perform Add-a-Field-L3 thru AAFL3-Exit
               move pret to last-pret
             end-if
           end-perform
           if trace-on
             display 'TRACE:' ws-sub-cnt '(3)field Loop stops. Reason='
               Stop-Reason-Fields
               ':current-occurs=' current-occurs
               ':level=' ws-level
               ':dim=' ws-dim
           end-if
           if Keep-Generating
             evaluate Stop-Reason-Fields
               when 1
               when 3
               when 7
                 perform Check-for-Mandatory-fields
                 if Fields-in-Error = 0
                 and End-Pret not = Last-Pret
                   perform Add-empty-fields
                 else
                   if Stop-Reason-Fields not = 7
                     move Stop-Reason-Fields to Stop-Reason
                   end-if
                 end-if
               when 2
               when 4
                 continue
               when 5
               when 6
                 move Stop-Reason-Fields to Stop-Reason
             end-evaluate
             move ORIG-PRET to PRET
           else
             move Stop-Reason-Fields to Stop-Reason
           end-if
           .
       Add-a-Field-L3.
           evaluate true
             when TABLE-FIELD(PRET)
                  perform AAF-ADD-L-4-Table thru AAL4T-exit
                  perform Update-Table-from-Previous
             when Regular-vField(PreT)
                  perform ATF-ADD-vFIELD-FIELD thru AAvF-exit
             when Group-Field(Pret)
                  display 'Engine:add-a-field3:Group command depracted'
                   ':pret=' Pret ':rec#=' ws-rec-cnt
                  move 12 to return-code
                  stop run
             when other
                  perform ATF-ADD-FIELD-FIELD thru AAFF-exit
           end-evaluate
           .
       AAFL3-Exit.
           exit.

      *-------------------------------------------------------------*
      * Level 4 Area                                                *
      *-------------------------------------------------------------*
      * Embedded TABLE-OCCUR or TABLE-ODO table.
      * This is for Level 4. Level 1 was the MAIN table.
      *-------------------------------------------------------------*
       AAF-ADD-L-4-Table.
           add 1 to ws-sub-cnt
           if TRACE-ON perform Trace-Start-Level end-if
           perform Push-stack

           move Pret to Orig-Pret
           move Pret-Level(Pret) to ws-level
           move Pret-Table(Pret) to gTblPtr(ws-level)
                                    TblPtr
           move Table-Dims(TblPtr) to ws-dim
           move zeroes to occur-size(4)
                          occur-size(5)
           move 0 to Current-Occurs
           if ws-Dim > Table-Dims(Tblptr) go to Dim-Overflow end-if

      * Determine table length if possible (ws-len)
           perform Determine-Table-length
           if Stop-Generating
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL4T-exit
           end-if
           if Table-Limit-or-Length-Set(TblPtr) and ws-len = 0
             if trace-on display 'TRACE:L4:Limit/Length & len=0'  end-if
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL4T-exit
           end-if
           move zeroes to return-code

      * check if this table matches any Condition statement
      * note. an non-existant condition also means Condition-True
           set Condition-true to true
v0854      if Table-Cond(TblPtr) > 0 then
             perform Check-any-Table-Condition
             if Condition-False
               perform Pull-Stack
               subtract 1 from ws-sub-cnt
               go to AAL4T-exit
             end-if
v0854      end-if


      * Max-Occurs
           perform Determine-Number-Entries
           if Max-Occurs = 0
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL4T-exit
           end-if

      * Determine the maximum EntryLength size, if stated.
           perform Determine-EntryLength-Size thru DES-Exit
           if Stop-Generating
             go to AAL4T-exit
           end-if

      * Set ws-end-tbl
           perform Determine-end-of-Table
      * cannot go past outer table's length (previous table)
           if ws-end-tbl > ws-end-tbl-L1
             move ws-end-tbl-L1 to ws-end-tbl
           end-if
           move ws-end-tbl to ws-end-tbl-L4
           if sCursor >= ws-end-tbl
             move zeroes to Current-Occurs
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL4T-exit
           end-if

      * Determine Start and Ending entries of the PreField-Table
           compute START-PRET = PRET + 1
           compute END-PRET = START-PRET +
                              TABLE-NUMBER-OF-FIELDS(TBLPTR) - 1

           if TRACE-ON perform Trace-Table-Stats end-if
           perform Generator-L4


           perform Pull-Stack
           if trace-on perform Trace-end-Level end-if
           subtract 1 from ws-sub-cnt
           .
       AAL4T-exit.
           exit.

       Generator-L4.
           if Trace-on perform Trace-Loop-Start end-if

           move zeroes to Stop-Reason, Stop-Reason-Fields
           perform until Stop-Reason > 0
             add 1 to CURRENT-OCCURS
             perform Add-an-Occurrence-L4
           end-perform

           if Trace-on perform Trace-Loop-Stops end-if

           if current-occurs < Table-Lowest(TBLPTR)
             perform Discard-Table-in-Error
           end-if
           .
       Add-an-Occurrence-L4.
           move zeroes to Occur-Size(4)
                          Occur-Size(5)
           if Current-Occurs > 1
             move ORIG-PRET to PRET
v0854        if Table-Cond(TblPtr) > 0 then
               perform Check-any-Table-Condition
v0854        end-if
             perform Determine-EntryLength-Size thru DES-Exit
           end-if
           evaluate true
             when sCursor >= WS-END-TBL         move 1 to Stop-Reason
             when CONDITION-FALSE               move 2 to Stop-Reason
             when sCursor > IN-Bits             move 3 to Stop-Reason
             when CURRENT-OCCURS > Max-Occurs   move 4 to Stop-Reason
             when Fields-in-Error > 0           move 5 to Stop-Reason
             when Stop-Generating               move 6 to Stop-Reason
             when other
               perform Add-the-Table-Field thru ATTF-Exit
               move field-cnt to Table-Field-Cnt(ws-level)
      *        perform Add-Field-Index
               perform Add-all-the-fields-L4
           end-evaluate
           if Stop-Reason = 4
             subtract 1 from Current-Occurs
           end-if
           .
       Add-all-the-fields-L4.
           move zeroes to Stop-Reason-Fields
           perform varying PRET from START-PRET by 1
              until Stop-Reason-Fields > zeroes
             perform Check-Stop-Reason-for-Fields
             if Stop-Reason-Fields = zeroes
               perform Add-a-Field-L4 thru AAFL4-Exit
               move pret to last-pret
             end-if
           end-perform
           if trace-on
             display 'TRACE:' ws-sub-cnt '(4)field Loop stops. Reason='
               Stop-Reason-Fields
               ':current-occurs=' current-occurs
               ':level=' ws-level
               ':dim=' ws-dim
           end-if
           if Keep-Generating
             evaluate Stop-Reason-Fields
               when 1
               when 3
               when 7
                 perform Check-for-Mandatory-fields
                 if Fields-in-Error = 0
                 and End-Pret not = Last-Pret
                   perform Add-empty-fields
                 else
                   if Stop-Reason-Fields not = 7
                     move Stop-Reason-Fields to Stop-Reason
                   end-if
                 end-if
               when 2
               when 4
                 continue
               when 5
               when 6
                 move Stop-Reason-Fields to Stop-Reason
             end-evaluate
             move ORIG-PRET to PRET
           else
             move Stop-Reason-Fields to Stop-Reason
           end-if
           .
       Add-a-Field-L4.
           evaluate true
             when TABLE-FIELD(PRET)
                  perform AAF-ADD-L-5-Table thru AAL5T-exit
                  perform Update-Table-from-Previous
             when Regular-vField(PreT)
                  perform ATF-ADD-vFIELD-FIELD thru AAvF-exit
             when Group-Field(Pret)
                  display 'Engine:add-a-field4:Group command depracted'
                   ':pret=' Pret ':rec#=' ws-rec-cnt
                  move 12 to return-code
                  stop run
             when other
                  perform ATF-ADD-FIELD-FIELD thru AAFF-exit
           end-evaluate
           .
       AAFL4-Exit.
           exit.

      *-------------------------------------------------------------*
      * Level 5 Area                                                *
      *-------------------------------------------------------------*
      * Embedded TABLE-OCCUR or TABLE-ODO table.
      * This is for Level 5. Level 1 was the MAIN table.
      *-------------------------------------------------------------*
       AAF-ADD-L-5-Table.
           add 1 to ws-sub-cnt
           if TRACE-ON perform Trace-Start-Level end-if
           perform Push-stack

           move Pret to Orig-Pret
           move Pret-Level(Pret) to ws-level
           move Pret-Table(Pret) to gTblPtr(ws-level)
                                    TblPtr
           move Table-Dims(TblPtr) to ws-dim
           move zeroes to occur-size(5)
                          occur-size(6)
           move 0 to Current-Occurs
           if ws-Dim > Table-Dims(Tblptr) go to Dim-Overflow end-if

      * Determine table length if possible (ws-len)
           perform Determine-Table-length
           if Stop-Generating
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL5T-exit
           end-if
           if Table-Limit-or-Length-Set(TblPtr) and ws-len = 0
             if trace-on display 'TRACE:L5:Limit/Length & len=0'  end-if
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL5T-exit
           end-if
           move zeroes to return-code

      * check if this table matches any Condition statement
      * note. an non-existant condition also means Condition-True
           set Condition-true to true
v0854      if Table-Cond(TblPtr) > 0 then
             perform Check-any-Table-Condition
             if Condition-False
               perform Pull-Stack
               subtract 1 from ws-sub-cnt
               go to AAL5T-exit
             end-if
v0854      end-if


      * Max-Occurs
           perform Determine-Number-Entries
           if Max-Occurs = 0
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL5T-exit
           end-if

      * Determine the maximum EntryLength size, if stated.
           perform Determine-EntryLength-Size thru DES-Exit
           if Stop-Generating
             go to AAL5T-exit
           end-if

      * Set ws-end-tbl
           perform Determine-end-of-Table
      * cannot go past outer table's length (previous table)
           if ws-end-tbl > ws-end-tbl-L1
             move ws-end-tbl-L1 to ws-end-tbl
           end-if
           move ws-end-tbl to ws-end-tbl-L5
           if sCursor >= ws-end-tbl
             move zeroes to Current-Occurs
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL5T-exit
           end-if

      * Determine Start and Ending entries of the PreField-Table
           compute START-PRET = PRET + 1
           compute END-PRET = START-PRET +
                              TABLE-NUMBER-OF-FIELDS(TBLPTR) - 1

           if TRACE-ON perform Trace-Table-Stats end-if
           perform Generator-L5


           perform Pull-Stack
           if trace-on perform Trace-end-Level end-if
           subtract 1 from ws-sub-cnt
           .
       AAL5T-exit.
           exit.

       Generator-L5.
           if Trace-on perform Trace-Loop-Start end-if

           move zeroes to Stop-Reason, Stop-Reason-Fields
           perform until Stop-Reason > 0
             add 1 to CURRENT-OCCURS
             perform Add-an-Occurrence-L5
           end-perform

           if Trace-on perform Trace-Loop-Stops end-if

           if current-occurs < Table-Lowest(TBLPTR)
             perform Discard-Table-in-Error
           end-if
           .
       Add-an-Occurrence-L5.
           move zeroes to Occur-Size(5)
                          Occur-Size(6)
           if Current-Occurs > 1
             move ORIG-PRET to PRET
v0854        if Table-Cond(TblPtr) > 0 then
               perform Check-any-Table-Condition
v0854        end-if
             perform Determine-EntryLength-Size thru DES-Exit
           end-if
           evaluate true
             when sCursor >= WS-END-TBL         move 1 to Stop-Reason
             when CONDITION-FALSE               move 2 to Stop-Reason
             when sCursor > IN-Bits             move 3 to Stop-Reason
             when CURRENT-OCCURS > Max-Occurs   move 4 to Stop-Reason
             when Fields-in-Error > 0           move 5 to Stop-Reason
             when Stop-Generating               move 6 to Stop-Reason
             when other
               perform Add-the-Table-Field thru ATTF-Exit
               move field-cnt to Table-Field-Cnt(ws-level)
               perform Add-all-the-fields-L5
           end-evaluate
           if Stop-Reason = 4
             subtract 1 from Current-Occurs
           end-if
           .
       Add-all-the-fields-L5.
           move zeroes to Stop-Reason-Fields
           perform varying PRET from START-PRET by 1
              until Stop-Reason-Fields > zeroes
             perform Check-Stop-Reason-for-Fields
             if Stop-Reason-Fields = zeroes
               perform Add-a-Field-L5 thru AAFL5-Exit
               move pret to last-pret
             end-if
           end-perform
           if trace-on
             display 'TRACE:' ws-sub-cnt '(5)field Loop stops. Reason='
               Stop-Reason-Fields
               ':current-occurs=' current-occurs
               ':level=' ws-level
               ':dim=' ws-dim
           end-if
           if Keep-Generating
             evaluate Stop-Reason-Fields
               when 1
               when 3
               when 7
                 perform Check-for-Mandatory-fields
                 if Fields-in-Error = 0
                 and End-Pret not = Last-Pret
                   perform Add-empty-fields
                 else
                   if Stop-Reason-Fields not = 7
                     move Stop-Reason-Fields to Stop-Reason
                   end-if
                 end-if
               when 2
               when 4
                 continue
               when 5
               when 6
                 move Stop-Reason-Fields to Stop-Reason
             end-evaluate
             move ORIG-PRET to PRET
           else
             move Stop-Reason-Fields to Stop-Reason
           end-if
           .
       Add-a-Field-L5.
           evaluate true
             when TABLE-FIELD(PRET)
                  perform AAF-ADD-L-6-Table thru AAL6T-exit
                  perform Update-Table-from-Previous
             when Regular-vField(PreT)
                  perform ATF-ADD-vFIELD-FIELD thru AAvF-exit
             when Group-Field(Pret)
                  display 'Engine:add-a-field5:Group command depracted'
                   ':pret=' Pret ':rec#=' ws-rec-cnt
                  move 12 to return-code
                  stop run
             when other
                  perform ATF-ADD-FIELD-FIELD thru AAFF-exit
           end-evaluate
           .
       AAFL5-Exit.
           exit.

      *-------------------------------------------------------------*
      * Level 6 Area                                                *
      *-------------------------------------------------------------*
      * Embedded TABLE-OCCUR or TABLE-ODO table.
      * This is for Level 6. Level 1 was the MAIN table.
      *-------------------------------------------------------------*
       AAF-ADD-L-6-Table.
           add 1 to ws-sub-cnt
           if TRACE-ON perform Trace-Start-Level end-if
           perform Push-stack

           move Pret to Orig-Pret
           move Pret-Level(Pret) to ws-level
           move Pret-Table(Pret) to gTblPtr(ws-level)
                                    TblPtr
           move Table-Dims(TblPtr) to ws-dim
           move zeroes to occur-size(6)
                          occur-size(7)
           move 0 to Current-Occurs
           if ws-Dim > Table-Dims(Tblptr) go to Dim-Overflow end-if

      * Determine table length if possible (ws-len)
           perform Determine-Table-length
           if Stop-Generating
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL6T-exit
           end-if
           if Table-Limit-or-Length-Set(TblPtr) and ws-len = 0
             if trace-on display 'TRACE:L6:Limit/Length & len=0'  end-if
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL6T-exit
           end-if
           move zeroes to return-code

      * check if this table matches any Condition statement
      * note. an non-existant condition also means Condition-True
           set Condition-true to true
v0854      if Table-Cond(TblPtr) > 0 then
             perform Check-any-Table-Condition
             if Condition-False
               perform Pull-Stack
               subtract 1 from ws-sub-cnt
               go to AAL6T-exit
             end-if
v0854      end-if


      * Max-Occurs
           perform Determine-Number-Entries
           if Max-Occurs = 0
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL6T-exit
           end-if

      * Determine the maximum EntryLength size, if stated.
           perform Determine-EntryLength-Size thru DES-Exit
           if Stop-Generating
             go to AAL6T-exit
           end-if

      * Set ws-end-tbl
           perform Determine-end-of-Table
      * cannot go past outer table's length (previous table)
           if ws-end-tbl > ws-end-tbl-L1
             move ws-end-tbl-L1 to ws-end-tbl
           end-if
           move ws-end-tbl to ws-end-tbl-L6
           if sCursor >= ws-end-tbl
             move zeroes to Current-Occurs
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL6T-exit
           end-if

      * Determine Start and Ending entries of the PreField-Table
           compute START-PRET = PRET + 1
           compute END-PRET = START-PRET +
                              TABLE-NUMBER-OF-FIELDS(TBLPTR) - 1

           if TRACE-ON perform Trace-Table-Stats end-if
           perform Generator-L6


           perform Pull-Stack
           if trace-on perform Trace-end-Level end-if
           subtract 1 from ws-sub-cnt
           .
       AAL6T-exit.
           exit.

       Generator-L6.
           if Trace-on perform Trace-Loop-Start end-if

           move zeroes to Stop-Reason, Stop-Reason-Fields
           perform until Stop-Reason > 0
             add 1 to CURRENT-OCCURS
             perform Add-an-Occurrence-L6
           end-perform

           if Trace-on perform Trace-Loop-Stops end-if

           if current-occurs < Table-Lowest(TBLPTR)
             perform Discard-Table-in-Error
           end-if
           .
       Add-an-Occurrence-L6.
           move zeroes to Occur-Size(6)
                          Occur-Size(7)
           if Current-Occurs > 1
             move ORIG-PRET to PRET
v0854        if Table-Cond(TblPtr) > 0 then
               perform Check-any-Table-Condition
v0854        end-if
             perform Determine-EntryLength-Size thru DES-Exit
           end-if
           evaluate true
             when sCursor >= WS-END-TBL         move 1 to Stop-Reason
             when CONDITION-FALSE               move 2 to Stop-Reason
             when sCursor > IN-Bits             move 3 to Stop-Reason
             when CURRENT-OCCURS > Max-Occurs   move 4 to Stop-Reason
             when Fields-in-Error > 0           move 5 to Stop-Reason
             when Stop-Generating               move 6 to Stop-Reason
             when other
               perform Add-the-Table-Field thru ATTF-Exit
               move field-cnt to Table-Field-Cnt(ws-level)
               perform Add-all-the-fields-L6
           end-evaluate
           if Stop-Reason = 4
             subtract 1 from Current-Occurs
           end-if
           .
       Add-all-the-fields-L6.
           move zeroes to Stop-Reason-Fields
           perform varying PRET from START-PRET by 1
              until Stop-Reason-Fields > zeroes
             perform Check-Stop-Reason-for-Fields
             if Stop-Reason-Fields = zeroes
               perform Add-a-Field-L6 thru AAFL6-Exit
               move pret to last-pret
             end-if
           end-perform
           if trace-on
             display 'TRACE:' ws-sub-cnt '(6)field Loop stops. Reason='
               Stop-Reason-Fields
               ':current-occurs=' current-occurs
               ':level=' ws-level
               ':dim=' ws-dim
           end-if
           if Keep-Generating
             evaluate Stop-Reason-Fields
               when 1
               when 3
               when 7
                 perform Check-for-Mandatory-fields
                 if Fields-in-Error = 0
                 and End-Pret not = Last-Pret
                   perform Add-empty-fields
                 else
                   if Stop-Reason-Fields not = 7
                     move Stop-Reason-Fields to Stop-Reason
                   end-if
                 end-if
               when 2
               when 4
                 continue
               when 5
               when 6
                 move Stop-Reason-Fields to Stop-Reason
             end-evaluate
             move ORIG-PRET to PRET
           else
             move Stop-Reason-Fields to Stop-Reason
           end-if
           .
       Add-a-Field-L6.
           evaluate true
             when TABLE-FIELD(PRET)
                  perform AAF-ADD-L-7-Table thru AAL7T-exit
                  perform Update-Table-from-Previous
             when Regular-vField(PreT)
                  perform ATF-ADD-vFIELD-FIELD thru AAvF-exit
             when Group-Field(Pret)
                  display 'Engine:add-a-field6:Group command depracted'
                   ':pret=' Pret ':rec#=' ws-rec-cnt
                  move 12 to return-code
                  stop run
             when other
                  perform ATF-ADD-FIELD-FIELD thru AAFF-exit
           end-evaluate
           .
       AAFL6-Exit.
           exit.

      *-------------------------------------------------------------*
      * Level 7 Area                                                *
      *-------------------------------------------------------------*
      * Embedded TABLE-OCCUR or TABLE-ODO table.
      * This is for Level 7. Level 1 was the MAIN table.
      *-------------------------------------------------------------*
       AAF-ADD-L-7-Table.
           add 1 to ws-sub-cnt
           if TRACE-ON perform Trace-Start-Level end-if
           perform Push-stack

           move Pret to Orig-Pret
           move Pret-Level(Pret) to ws-level
           move Pret-Table(Pret) to gTblPtr(ws-level)
                                    TblPtr
           move Table-Dims(TblPtr) to ws-dim
           move zeroes to occur-size(7)
                          occur-size(8)
           move 0 to Current-Occurs
           if ws-Dim > Table-Dims(Tblptr) go to Dim-Overflow end-if

      * Determine table length if possible (ws-len)
           perform Determine-Table-length
           if Stop-Generating
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL7T-exit
           end-if
           if Table-Limit-or-Length-Set(TblPtr) and ws-len = 0
             if trace-on display 'TRACE:L7:Limit/Length & len=0'  end-if
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL7T-exit
           end-if
           move zeroes to return-code

      * check if this table matches any Condition statement
      * note. an non-existant condition also means Condition-True
           set Condition-true to true
v0854      if Table-Cond(TblPtr) > 0 then
             perform Check-any-Table-Condition
             if Condition-False
               perform Pull-Stack
               subtract 1 from ws-sub-cnt
               go to AAL7T-exit
             end-if
v0854      end-if


      * Max-Occurs
           perform Determine-Number-Entries
           if Max-Occurs = 0
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL7T-exit
           end-if

      * Determine the maximum EntryLength size, if stated.
           perform Determine-EntryLength-Size thru DES-Exit
           if Stop-Generating
             go to AAL7T-exit
           end-if

      * Set ws-end-tbl
           perform Determine-end-of-Table
      * cannot go past outer table's length (previous table)
           if ws-end-tbl > ws-end-tbl-L1
             move ws-end-tbl-L1 to ws-end-tbl
           end-if
           move ws-end-tbl to ws-end-tbl-L7
           if sCursor >= ws-end-tbl
             move zeroes to Current-Occurs
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL7T-exit
           end-if

      * Determine Start and Ending entries of the PreField-Table
           compute START-PRET = PRET + 1
           compute END-PRET = START-PRET +
                              TABLE-NUMBER-OF-FIELDS(TBLPTR) - 1

           if TRACE-ON perform Trace-Table-Stats end-if
           perform Generator-L7


           perform Pull-Stack
           if trace-on perform Trace-end-Level end-if
           subtract 1 from ws-sub-cnt
           .
       AAL7T-exit.
           exit.

       Generator-L7.
           if Trace-on perform Trace-Loop-Start end-if

           move zeroes to Stop-Reason, Stop-Reason-Fields
           perform until Stop-Reason > 0
             add 1 to CURRENT-OCCURS
             perform Add-an-Occurrence-L7
           end-perform

           if Trace-on perform Trace-Loop-Stops end-if

           if current-occurs < Table-Lowest(TBLPTR)
             perform Discard-Table-in-Error
           end-if
           .
       Add-an-Occurrence-L7.
           move zeroes to Occur-Size(7)
                          Occur-Size(8)
           if Current-Occurs > 1
             move ORIG-PRET to PRET
v0854        if Table-Cond(TblPtr) > 0 then
               perform Check-any-Table-Condition
v0854        end-if
             perform Determine-EntryLength-Size thru DES-Exit
           end-if
           evaluate true
             when sCursor >= WS-END-TBL         move 1 to Stop-Reason
             when CONDITION-FALSE               move 2 to Stop-Reason
             when sCursor > IN-Bits             move 3 to Stop-Reason
             when CURRENT-OCCURS > Max-Occurs   move 4 to Stop-Reason
             when Fields-in-Error > 0           move 5 to Stop-Reason
             when Stop-Generating               move 6 to Stop-Reason
             when other
               perform Add-the-Table-Field thru ATTF-Exit
               move field-cnt to Table-Field-Cnt(ws-level)
               perform Add-all-the-fields-L7
           end-evaluate
           if Stop-Reason = 4
             subtract 1 from Current-Occurs
           end-if
           .
       Add-all-the-fields-L7.
           move zeroes to Stop-Reason-Fields
           perform varying PRET from START-PRET by 1
              until Stop-Reason-Fields > zeroes
             perform Check-Stop-Reason-for-Fields
             if Stop-Reason-Fields = zeroes
               perform Add-a-Field-L7 thru AAFL7-Exit
               move pret to last-pret
             end-if
           end-perform
           if trace-on
             display 'TRACE:' ws-sub-cnt '(7)field Loop stops. Reason='
               Stop-Reason-Fields
               ':current-occurs=' current-occurs
               ':level=' ws-level
               ':dim=' ws-dim
           end-if
           if Keep-Generating
             evaluate Stop-Reason-Fields
               when 1
               when 3
               when 7
                 perform Check-for-Mandatory-fields
                 if Fields-in-Error = 0
                 and End-Pret not = Last-Pret
                   perform Add-empty-fields
                 else
                   if Stop-Reason-Fields not = 7
                     move Stop-Reason-Fields to Stop-Reason
                   end-if
                 end-if
               when 2
               when 4
                 continue
               when 5
               when 6
                 move Stop-Reason-Fields to Stop-Reason
             end-evaluate
             move ORIG-PRET to PRET
           else
             move Stop-Reason-Fields to Stop-Reason
           end-if
           .
       Add-a-Field-L7.
           evaluate true
             when TABLE-FIELD(PRET)
                  perform AAF-ADD-L-8-Table thru AAL8T-exit
                  perform Update-Table-from-Previous
             when Regular-vField(PreT)
                  perform ATF-ADD-vFIELD-FIELD thru AAvF-exit
             when Group-Field(Pret)
                  display 'Engine:add-a-field7:Group command depracted'
                   ':pret=' Pret ':rec#=' ws-rec-cnt
                  move 12 to return-code
                  stop run
             when other
                  perform ATF-ADD-FIELD-FIELD thru AAFF-exit
           end-evaluate
           .
       AAFL7-Exit.
           exit.

      *-------------------------------------------------------------*
      * Level 8 Area                                                *
      *-------------------------------------------------------------*
      * Embedded TABLE-OCCUR or TABLE-ODO table.
      * This is for Level 8. Level 1 was the MAIN table.
      *-------------------------------------------------------------*
       AAF-ADD-L-8-Table.
           add 1 to ws-sub-cnt
           if TRACE-ON perform Trace-Start-Level end-if
           perform Push-stack

           move Pret to Orig-Pret
           move Pret-Level(Pret) to ws-level
           move Pret-Table(Pret) to gTblPtr(ws-level)
                                    TblPtr
           move Table-Dims(TblPtr) to ws-dim
           move zeroes to occur-size(8)
                          occur-size(9)
           move 0 to Current-Occurs
           if ws-Dim > Table-Dims(Tblptr) go to Dim-Overflow end-if

      * Determine table length if possible (ws-len)
           perform Determine-Table-length
           if Stop-Generating
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL8T-exit
           end-if
           if Table-Limit-or-Length-Set(TblPtr) and ws-len = 0
             if trace-on display 'TRACE:L8:Limit/Length & len=0'  end-if
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL8T-exit
           end-if
           move zeroes to return-code

      * check if this table matches any Condition statement
      * note. an non-existant condition also means Condition-True
           set Condition-true to true
v0854      if Table-Cond(TblPtr) > 0 then
             perform Check-any-Table-Condition
             if Condition-False
               perform Pull-Stack
               subtract 1 from ws-sub-cnt
               go to AAL8T-exit
             end-if
v0854      end-if


      * Max-Occurs
           perform Determine-Number-Entries
           if Max-Occurs = 0
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL8T-exit
           end-if

      * Determine the maximum EntryLength size, if stated.
           perform Determine-EntryLength-Size thru DES-Exit
           if Stop-Generating
             go to AAL8T-exit
           end-if

      * Set ws-end-tbl
           perform Determine-end-of-Table
      * cannot go past outer table's length (previous table)
           if ws-end-tbl > ws-end-tbl-L1
             move ws-end-tbl-L1 to ws-end-tbl
           end-if
           move ws-end-tbl to ws-end-tbl-L8
           if sCursor >= ws-end-tbl
             move zeroes to Current-Occurs
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL8T-exit
           end-if

      * Determine Start and Ending entries of the PreField-Table
           compute START-PRET = PRET + 1
           compute END-PRET = START-PRET +
                              TABLE-NUMBER-OF-FIELDS(TBLPTR) - 1

           if TRACE-ON perform Trace-Table-Stats end-if
           perform Generator-L8


           perform Pull-Stack
           if trace-on perform Trace-end-Level end-if
           subtract 1 from ws-sub-cnt
           .
       AAL8T-exit.
           exit.

       Generator-L8.
           if Trace-on perform Trace-Loop-Start end-if

           move zeroes to Stop-Reason, Stop-Reason-Fields
           perform until Stop-Reason > 0
             add 1 to CURRENT-OCCURS
             perform Add-an-Occurrence-L8
           end-perform

           if Trace-on perform Trace-Loop-Stops end-if

           if current-occurs < Table-Lowest(TBLPTR)
             perform Discard-Table-in-Error
           end-if
           .
       Add-an-Occurrence-L8.
           move zeroes to Occur-Size(8)
                          Occur-Size(9)
           if Current-Occurs > 1
             move ORIG-PRET to PRET
v0854        if Table-Cond(TblPtr) > 0 then
               perform Check-any-Table-Condition
v0854        end-if
             perform Determine-EntryLength-Size thru DES-Exit
           end-if
           evaluate true
             when sCursor >= WS-END-TBL         move 1 to Stop-Reason
             when CONDITION-FALSE               move 2 to Stop-Reason
             when sCursor > IN-Bits             move 3 to Stop-Reason
             when CURRENT-OCCURS > Max-Occurs   move 4 to Stop-Reason
             when Fields-in-Error > 0           move 5 to Stop-Reason
             when Stop-Generating               move 6 to Stop-Reason
             when other
               perform Add-the-Table-Field thru ATTF-Exit
               move field-cnt to Table-Field-Cnt(ws-level)
               perform Add-all-the-fields-L8
           end-evaluate
           if Stop-Reason = 4
             subtract 1 from Current-Occurs
           end-if
           .
       Add-all-the-fields-L8.
           move zeroes to Stop-Reason-Fields
           perform varying PRET from START-PRET by 1
              until Stop-Reason-Fields > zeroes
             perform Check-Stop-Reason-for-Fields
             if Stop-Reason-Fields = zeroes
               perform Add-a-Field-L8 thru AAFL8-Exit
               move pret to last-pret
             end-if
           end-perform
           if trace-on
             display 'TRACE:' ws-sub-cnt '(8)field Loop stops. Reason='
               Stop-Reason-Fields
               ':current-occurs=' current-occurs
               ':level=' ws-level
               ':dim=' ws-dim
           end-if
           if Keep-Generating
             evaluate Stop-Reason-Fields
               when 1
               when 3
               when 7
                 perform Check-for-Mandatory-fields
                 if Fields-in-Error = 0
                 and End-Pret not = Last-Pret
                   perform Add-empty-fields
                 else
                   if Stop-Reason-Fields not = 7
                     move Stop-Reason-Fields to Stop-Reason
                   end-if
                 end-if
               when 2
               when 4
                 continue
               when 5
               when 6
                 move Stop-Reason-Fields to Stop-Reason
             end-evaluate
             move ORIG-PRET to PRET
           else
             move Stop-Reason-Fields to Stop-Reason
           end-if
           .
       Add-a-Field-L8.
           evaluate true
             when TABLE-FIELD(PRET)
                  perform AAF-ADD-L-9-Table thru AAL9T-exit
                  perform Update-Table-from-Previous
             when Regular-vField(PreT)
                  perform ATF-ADD-vFIELD-FIELD thru AAvF-exit
             when Group-Field(Pret)
                  display 'Engine:add-a-field8:Group command depracted'
                   ':pret=' Pret ':rec#=' ws-rec-cnt
                  move 12 to return-code
                  stop run
             when other
                  perform ATF-ADD-FIELD-FIELD thru AAFF-exit
           end-evaluate
           .
       AAFL8-Exit.
           exit.



      *-------------------------------------------------------------*
      * Level 9 Area                                                *
      *-------------------------------------------------------------*
       AAF-ADD-L-9-Table.
           add 1 to ws-sub-cnt
           if TRACE-ON perform Trace-Start-Level end-if
           perform Push-stack

           move Pret to Orig-Pret
           move Pret-Level(Pret) to ws-level
           move Pret-Table(Pret) to gTblPtr(ws-level)
                                    TblPtr
           move Table-Dims(TblPtr) to ws-dim
           move zeroes to occur-size(9)
           move 0 to Current-Occurs
           if ws-Dim > Table-Dims(Tblptr) go to Dim-Overflow end-if

      * Determine table length if possible (ws-len)
           perform Determine-Table-length
           if Stop-Generating
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL9T-exit
           end-if
           if Table-Limit-or-Length-Set(TblPtr) and ws-len = 0
             if trace-on display 'TRACE:L9:Limit/Length & len=0'  end-if
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL9T-exit
           end-if
           move zeroes to return-code

      * check if this table matches any Condition statement
      * note. an non-existant condition also means Condition-True
           set Condition-true to true
v0854      if Table-Cond(TblPtr) > 0 then
             perform Check-any-Table-Condition
             if Condition-False
               perform Pull-Stack
               subtract 1 from ws-sub-cnt
               go to AAL9T-exit
             end-if
v0854      end-if


      * Max-Occurs
           perform Determine-Number-Entries
           if Max-Occurs = 0
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL9T-exit
           end-if

      * Determine the maximum EntryLength size, if stated.
           perform Determine-EntryLength-Size thru DES-Exit
           if Stop-Generating
             go to AAL9T-exit
           end-if

      * Set ws-end-tbl
           perform Determine-end-of-Table
      * cannot go past outer table's length (previous table)
           if ws-end-tbl > ws-end-tbl-L8
             move ws-end-tbl-L8 to ws-end-tbl
           end-if
           move ws-end-tbl to ws-end-tbl-L9
           if sCursor >= ws-end-tbl
             move zeroes to Current-Occurs
             perform Pull-Stack
             subtract 1 from ws-sub-cnt
             go to AAL9T-exit
           end-if


      * Determine Start and Ending entries of the PreField-Table
           compute START-PRET = PRET + 1
           compute END-PRET = START-PRET +
                              TABLE-NUMBER-OF-FIELDS(TBLPTR) - 1

           if TRACE-ON perform Trace-Table-Stats end-if
           perform Generator-L9

           perform Pull-Stack
           subtract 1 from ws-sub-cnt
           if trace-on perform Trace-end-Level end-if
           .
       AAL9T-exit.
           exit.

       Generator-L9.
           if Trace-on perform Trace-Loop-Start end-if

           move zeroes to Stop-Reason, Stop-Reason-Fields
           perform until Stop-Reason > 0
             add 1 to CURRENT-OCCURS
             perform Add-an-Occurrence-L9
           end-perform

           if Trace-on perform Trace-Loop-Stops end-if

           if current-occurs < Table-Lowest(TBLPTR)
             perform Discard-Table-in-Error
           end-if
           .
       Add-an-Occurrence-L9.
           move zeroes to Occur-Size(9)
           if Current-Occurs > 1
             move ORIG-PRET to PRET
v0854        if Table-Cond(TblPtr) > 0 then
               perform Check-any-Table-Condition
v0854        end-if
             perform Determine-EntryLength-Size thru DES-Exit
           end-if
           evaluate true
             when sCursor >= WS-END-TBL         move 1 to Stop-Reason
             when CONDITION-FALSE               move 2 to Stop-Reason
             when sCursor > IN-Bits             move 3 to Stop-Reason
             when CURRENT-OCCURS > Max-Occurs   move 4 to Stop-Reason
             when Fields-in-Error > 0           move 5 to Stop-Reason
             when Stop-Generating               move 6 to Stop-Reason
             when other
               perform Add-the-Table-Field thru ATTF-Exit
               move field-cnt to Table-Field-Cnt(ws-level)
               perform Add-all-the-fields-L9
           end-evaluate
           if Stop-Reason = 4
             subtract 1 from Current-Occurs
           end-if
           .
       Add-all-the-fields-L9.
           move zeroes to Stop-Reason-Fields
           perform varying PRET from START-PRET by 1
              until Stop-Reason-Fields > zeroes
             perform Check-Stop-Reason-for-Fields
             if Stop-Reason-Fields = zeroes
               perform Add-a-Field-L9 thru AAFL9-exit
               move pret to last-pret
             end-if
           end-perform
           if trace-on
             display 'TRACE:' ws-sub-cnt '(9)field Loop stops. Reason='
               Stop-Reason-Fields
               ':current-occurs=' current-occurs
               ':level=' ws-level
               ':dim=' ws-dim
           end-if
           if Keep-Generating
             evaluate Stop-Reason-Fields
               when 1
               when 3
               when 7
                 perform Check-for-Mandatory-fields
                 if Fields-in-Error = 0
                 and End-Pret not = Last-Pret
                   perform Add-empty-fields
                 else
                   if Stop-Reason-Fields not = 7
                     move Stop-Reason-Fields to Stop-Reason
                   end-if
                 end-if
               when 2
               when 4
                 continue
               when 5
               when 6
                 move Stop-Reason-Fields to Stop-Reason
             end-evaluate
             move ORIG-PRET to PRET
           else
             move Stop-Reason-Fields to Stop-Reason
           end-if
           .
       Add-a-Field-L9.
           evaluate true
             when TABLE-FIELD(PRET)
                  display '*******************************************'
                  display '*Too many sub-tables. Max is 9.           *'
                  display '*******************************************'
                  move 12 to return-code
                  stop run
             when Regular-vField(PreT)
                  perform ATF-ADD-vFIELD-FIELD thru AAvF-exit
             when Group-Field(Pret)
                  display 'Engine:add-a-field9:Group command depracted'
                   ':pret=' Pret ':rec#=' ws-rec-cnt
                  move 12 to return-code
                  stop run
             when other
                  perform ATF-ADD-FIELD-FIELD thru AAFF-exit
           end-evaluate
           .
       AAFL9-Exit.
           exit.

      *-------------------------------------------------------------*
      * Common routines                                             *
      *-------------------------------------------------------------*

      *This will point to the new PRET and add the previous
      * table length's to current table length and occurrence size.
       Update-Table-from-Previous.
           move gTblPtr(ws-level + 1)          to tTblPtr
           compute pret = Pret + TABLE-NUMBER-OF-FIELDS(tTblPtr)
           move Table-Field-Cnt(ws-level + 1) to TFCNext
           move Table-Field-Cnt(ws-level)     to TFCCurr
      * Only sum *if* the table was added (TFCNext > 0)
           if TFCNext > 0
             add FLDT-Length(TFCNext) to FLDT-Length(TFCCurr)
             add Occur-Size(ws-level + 1) to Occur-Size(ws-level)
           end-if
           move zeroes to Occur-Size(ws-level + 1)
           move zeroes to Table-Field-Cnt(ws-level + 1)
           move zeroes to gTblPtr(ws-level + 1)
           .
      *When occurrence is 'short' this will create EMPTY fields
      *-Table fields are skipped and all of its sub-fields
      *-Group fields are skipped.
      *-vFields are processed and added to field table
      *-Fields get zero length and added to field table
       Add-empty-fields.
           if trace-on
             display 'TRACE:Add EMPTY fields:Last-Pret=' Last-Pret
               ':last Start-Pret=' Start-Pret
               ':End-Pret=' End-Pret
               ':Level=' ws-level
               ':dim=' ws-dim
           end-if
           move start-pret to save-pret
           compute Start-Pret = Last-Pret + 1
           perform varying PRET from START-PRET by 1
                     until Pret > End-Pret
             evaluate true
               when TABLE-FIELD(PRET)
                    move Pret-Table(Pret) to N
                    compute pret = Pret + TABLE-NUMBER-OF-FIELDS(N)
               when Regular-vField(PreT)
                    perform ATF-ADD-vFIELD-FIELD thru AAvF-exit
               when Group-Field(Pret)
                    continue
               when other
                    move zeroes to ws-len
                    move zeroes to return-code
                    perform AddField
                    set Not-a-vField(Field-Cnt) to true
                    move PRET-LEVEL(Pret) to FLDT-LEVEL(Field-Cnt)
                    perform Add-Field-Index
                    perform Update-Chain
                    if TRACE-ON
                      perform Trace-Add-Field-Field
                      display 'TRACE:******----empty added--------*****'
                    end-if
             end-evaluate
           end-perform
           move end-pret to pret
           move save-pret to start-pret
           .
       Add-an-Empty-Table-Field.
           perform Add-the-Table-Field thru ATTF-Exit
           move zeroes                      to FLDT-Length(Field-Cnt)
           move zeroes to Current-Occurs
           move zeroes to FLDT-EntryLength(Field-Cnt)
           .
       Add-The-Table-Field.
           if sCursor > In-Bits
              go to ATTF-Exit
           end-if
           perform AddField
           set Not-a-vField(Field-Cnt) to true
           move PRET-LEVEL(Pret) to FLDT-LEVEL(Field-Cnt)
           compute FLDT-EntryLength(Field-Cnt) = ws-EntryLength
           if ws-level > 1
             move Table-Field-Cnt(ws-level - 1)
               to FLDT-Table-Field(Field-Cnt)
           else
             move Table-Field-cnt(ws-level)
               to FLDT-Table-Field(Field-Cnt)
           end-if

           move zeroes to FLDT-Length(Field-Cnt)
           perform Add-Field-Index
           perform Update-Chain


           if TRACE-ON
              perform Trace-Table-Entry
           end-if
           .
       ATTF-Exit.
           exit.

       Determine-Table-length.
           perform GetLen
           if trace-on
             move 'GetLn' to Trace-Event
             divide sCursor by 8 giving ws-num5a remainder gbPosBit
             if gbPosBit not = 0 then add 1 to ws-num5a end-if
             move ws-len to ws-num5b
             display
               Trace-Event
               '      '
               ' <' ws-num5a
               '.' ws-num5b '>'
               ' ' Pret
               ' ' Pret-FIELDNAME(Pret)
               ' T#' Pret-Table(Pret)
           end-if
           if ws-len < 0
              set Discard-Computed-Len-LT-zero to true
v00860        move zeroes to Fldt
              call 'NOTEDISC' using PreT, sCursor, Discard-Table-Area
v00860                    Fldt
              add 1 to fields-in-error
              add 1 to discard-count(Discard-Code)
              if First-Field-in-Error = 0
                move Pret to First-Field-in-Error
                move Discard-Code to First-Fields-Error
              end-if
              set Stop-Generating to true
              if trace-on
                perform Trace-Stop-Generating
              end-if
           end-if
           .
       Determine-Number-Entries.
           move Table-Highest(TblPtr) to Max-Occurs
           evaluate True
             when TABLE-DEPEND-SET(TblPtr)
               perform Determine-Max-Occurs
               if Max-Occurs = 0 then
v0847            add 1 to Max-Occurs-Zero
v0847            if Max-Occurs-Zero < 11
                  display 'Engine:Occurs depending on value is '
                     '0/invalid'
                   ':Rec#=' ws-rec-cnt ':Tblptr=' TblPtr
v0847             if Max-Occurs-Zero = 10
v0847              display 'Engine:Occurs depending on value is '
v0847                '0/invalid messages now suppressed after 10'
v0847             end-if
v0847            end-if
               end-if
               set Length-not-Preset to true
             when TABLE-LIMIT-SET(TblPtr)
             when Table-Length-Set(TblPtr)
               set Length-Preset     to true
           end-evaluate
           .

       Determine-Max-Occurs.
           move zeroes to Max-Occurs
           move TABLE-DEPEND-FIELD(TblPtr) to FoundAt
           if Pret-LastField(FoundAt) > 0
             move Pret-LastField(FoundAt) to FoundAt
             move Fldt-LastGhost(FoundAt) to FoundAt
           else
             display 'Engine:Table Depend field never generated'
               ':Field=' TABLE-DEPEND-FIELD(TblPtr)
               ':Pret=' Pret
               ':rec#=' ws-rec-cnt
               ':tbl#=' TblPtr
               ':Last=' Pret-LastField(FoundAt)
             perform varying FPTR from 1 by 1 until FPTR > Field-CNT
               set Write-Trace-Fldt to true
               perform Write-Trace
             end-perform
             move 12 to return-code
             stop run
           end-if
           move FoundAt to gvPtr
           perform GetValue
           if Discard-Something
v0847        move zeroes to Max-Occurs
v0847        set Discard-Nothing to true
v0847      else
             evaluate ws-value(20:1)
               when 'S' move ws-value-num18s     to Max-Occurs
               when 'U' move ws-value-num18      to Max-Occurs
             end-evaluate
v0847      end-if
           .
       Determine-EntryLength-Size.
           move 999999999 to ws-EntryLength
           if Table-EL-Ptr(TblPtr) = 0
             go to DES-Exit
           end-if
           move Table-EL-Ptr(TblPtr) to EL-Ptr
           evaluate true
            when EL-Use-Value(EL-Ptr)
              compute ws-EntryLength = EL-Value(EL-Ptr) * 8
            when EL-Use-Field(EL-Ptr)
              move EL-Field(EL-Ptr) to N
              if EL-Field(EL-Ptr) < Pret
      *        'use from FLDT which is already done
               move EL-Field(EL-Ptr) to Prefield-ptr
               set FldInx to Pret-LastField(Prefield-ptr)
               move Fldt-Start(FldInx) to Start-Entry
               move Fldt-length-Byte(FldInx) to gbLen
               move Fldt-Length-Nib(FldInx)  to gbLenNib
               move Fldt-Length(FldInx)      to gbLenBit
v0854          move Prefield-ptr to gbField
              else
      *        'use from PRET which needs to be prefetched
               compute Start-Entry = sCursor + (EL-Start(EL-Ptr) - 1)
               move Pret-length-Byte(N) to gbLen
               move Pret-Length-Nib(N)  to gbLenNib
               move Pret-Length(N)      to gbLenBit
v0854          move N to gbField
              end-if
              move Pret-Type(N)    to gbType
               move 0 to gbBuffer
               divide Start-Entry by 8 giving gbPos remainder gbPosBit
               if gbPosBit = 0 then
                 move 8 to gbPosBit
               else
                 add 1 to gbPos
               end-if
               if gbPosBit > 4
                 move 2 to gbPosNib
               else
                 move 1 to gbPosNib
               end-if
               perform GetBuff
              if Discard-Something
                set Discard-EntryLength-Invalid to true
                move 0 to ws-EntryLength
v00860          move zeroes to Fldt
                call 'NOTEDISC' using PreT, sCursor, Discard-Table-Area
v00860                    Fldt
                add 1 to fields-in-error
                add 1 to discard-count(Discard-Code)
                if First-Field-in-Error = 0
                  move Pret to First-Field-in-Error
                  move Discard-Code to First-Fields-Error
                end-if
                set Stop-Generating to true
                if Trace-on
                  compute i =  sCursor / 8
                  compute X =  Pret-Length(N) / 8
                  display 'TRACE:EntryLength value invalid'
                    ':rec#=' ws-rec-cnt
                    ':type=' Record-to-Process
                    ':Buff=' in-Buffer(i:X) '<<'
                    ':TblPtr=' TblPtr
                    ':ELPtr=' EL-Ptr
                  perform Trace-Stop-Generating
                end-if
                go to DES-Exit
              end-if
              evaluate ws-value(20:1)
                when 'S' compute ws-EntryLength = ws-Value-Num18s * 8
                when 'U' compute ws-EntryLength = ws-Value-Num18  * 8
              end-evaluate
           end-evaluate
           if Trace-on
             display 'TRACE:EntryLength=' ws-EntryLength
              ':pret=' Pret ':EL-Field=' EL-Field(EL-Ptr)
           end-if
           .
       DES-Exit.
           exit.

       Determine-end-of-Table.
           if ws-len > 0
             compute WS-END-TBL = (sCursor + WS-LEN) - 1
           else
             if (ws-Entrylength not = 999999999)
             and (Table-EL-Ptr(TblPtr) > 0)
               compute ws-End-Tbl = sCursor +
                  (ws-EntryLength - 1)
             else
               move IN-bits to WS-END-TBL
             end-if
           end-if
v00866     if ws-Level > 1
v00866       if ws-end-tbl > ws-end-tbl-level(ws-level - 1)
v00866         move ws-end-tbl-level(ws-level - 1) to ws-end-tbl
v00866       end-if
v00866     end-if
           .
      * looks for a reason to stop processing fields within a table
      * We are no longer going to check sCursor at field level
      *  but if sCursor is beyond processing we move zero to the length.
       Check-Stop-Reason-for-Fields.
           move Table-Field-Cnt(ws-level) to TFCCurr
           evaluate true
               when sCursor >  WS-END-TBL  move 1 to Stop-Reason-Fields
               when CONDITION-FALSE        move 2 to Stop-Reason-Fields
               when sCursor >  IN-Bits     move 3 to Stop-Reason-Fields
               when Pret > End-Pret        move 4 to Stop-Reason-Fields
               when Fields-in-Error > 0    move 5 to Stop-Reason-Fields
               when Stop-Generating        move 6 to Stop-Reason-Fields
               when FLDT-EntryLength(TFCCurr) > 0
                and Occur-Size(ws-level) = FLDT-EntryLength(TFCCurr)
                                           move 7 to Stop-Reason-Fields
               when other
                 move zeroes to Stop-Reason-Fields
           end-evaluate
           .
      *This is see how many Mandatory fields are left to process
      * and report if there are any errors
       Check-for-Mandatory-fields.
           if Pret-Table(Pret) > 0
             set TblInx to Pret-Table(Pret)
             if Pret < (Start-Pret + Table-Mandatory-Fields(TblInx))
               set Discard-More-TBL-DLL-than-Data to true
               set Table-DLL-Long-Error to true
v00860         move zeroes to Fldt
               call 'NOTEDISC' using PreT, sCursor
                                     Discard-Table-Area
v00860                               Fldt
               add 1 to fields-in-error
               if First-Field-in-Error = 0
                 move Pret to First-Field-in-Error
                 move Discard-Code to First-Fields-Error
               end-if
               add 1 to discard-count(discard-code)
               if trace-on
                display 'trace:More Table DLL than Data'
                 ':Pret=' Pret
                 ':Start-pret=' Start-pret
                 ':#Mand=' Table-Mandatory-Fields(TblInx)
                 ':scursor=' sCursor
                 ':stop=' Stop-Reason-Fields
                 ':rec=' ws-rec-cnt
               end-if
             end-if
           end-if
           .

       Discard-Table-in-Error.
           set Stop-Generating to true
           set Discard-Occur-LT-Lowest to true
           if trace-on
             display 'Engine:' discard-reason(Discard-code)
               ':Pret=' orig-Pret ':Cursor=' sCursor
               ':curr=' current-occurs
               ':low=' TABLE-LOWEST(TBLPTR)
             perform Trace-Stop-Generating
           end-if
v00860     move zeroes to Fldt
           call 'NOTEDISC' using Orig-PreT, sCursor
                                 Discard-Table-Area
v00860                           Fldt
           add 1 to fields-in-error
           if First-Field-in-Error = 0
              move Orig-Pret to First-Field-in-Error
              move Discard-Code to First-Fields-Error
           end-if
           add 1 to discard-count(discard-code)
           .

      * if we have a condition on the table, go seek if condtion met
       Check-any-Table-Condition.
           move Pret     to Save-Pret
           move End-Pret to Save-End-Pret
             move Table-Cond(TblPtr) to Cond-ptr
             compute ws-offset = sCursor - 1
             perform ChkCond
           move Save-End-Pret to End-Pret
           move Save-Pret     to Pret
           if TRACE-ON
             move 'CkCnd' to Trace-Event
             if Condition-True
               move 'True'  to WS-STR6B
             else
               move 'False' to WS-STR6B
             end-if
             display
               Trace-Event
               ' ' WS-STR6B
               '             '
               ' ' Pret
               ' ' Pret-FIELDNAME(Pret)
               ' T#' TblPtr
           end-if
           .
      * if we have a condition on the field, go seek if condtion met
       Check-any-Field-Condition.
           move Pret     to Save-Pret
           move End-Pret to Save-End-Pret
             move Pret-Cond(Pret) to Cond-ptr
             compute ws-offset = sCursor - 1
             perform ChkCond
           move Save-End-Pret to End-Pret
           move Save-Pret     to Pret

           if TRACE-ON
           and Pret-Cond(Pret) > 0 then
             move 'CkCnd' to Trace-Event
             if Condition-True
               move 'True'  to WS-STR6B
             else
               move 'False' to WS-STR6B
             end-if
             display
               Trace-Event
               ' ' WS-STR6B
               '             '
               ' ' Pret
               ' ' Pret-FIELDNAME(Pret)
               ' C#' Pret-Cond(Pret)
           end-if
           .



       Dim-Overflow.
           display '************************************************'
           display '*Engine:Expand-Table-Fields:Too Many Dimensions*'
           display '* trying to be generated.                      *'
           display '*Details'
                   ':Pret=' Pret
                   ':Fld=' field-cnt
                   ':TblPtr=' TblPtr '     *'
           display '* dim=' ws-Dim
                   ' vs '    Table-Dims(TblPtr)
                   ':subt=' ws-sub-cnt
                   ':level=' ws-level
                   '        *'
           display '************************************************'
           move 12 to return-code
           stop run
           .

       Push-Stack.
           add 1 to stack-cnt
           if stack-cnt > stack-max
             display 'ENGINE: stack  ptr overflow'
             move 12 to return-code
             stop run
           end-if
           move PRET               to stack-Pret(stack-cnt)
           move Orig-PRET          to stack-Orig-ptr(stack-cnt)
           move ws-level           to stack-level(stack-cnt)
           move ws-dim             to stack-dim(stack-cnt)
           move Current-Occurs     to stack-Current-Occurs(stack-cnt)
           move ws-end-tbl         to stack-end-tbl(stack-cnt)
           move Max-occurs         to stack-Max-occurs(stack-cnt)
           move start-pret         to stack-start-pret(stack-cnt)
           move end-pret           to stack-end-pret(stack-cnt)
           move stop-reason        to stack-reason(stack-cnt)
           move stop-reason-fields to stack-reason-field(stack-cnt)
           move TblPtr             to stack-TblPtr(stack-cnt)
           move Condition-Sw       to stack-Condition-sw(stack-cnt)
           move ws-len             to stack-len(stack-cnt)
           move last-pret          to stack-last-pret(stack-cnt)
           .
       Pull-Stack.
           if stack-cnt = 0 then
             display 'ENGINE:stack ptr underflow'
              ':level=' ws-level
              ':dim=' ws-dim
              ':Table-Field=' Table-field-cnt(ws-level)
              ':Orig-Pret=' Orig-Pret
              ':Pret=' Pret
              perform Trace-Stop-Generating
              move 16 to return-code
              stop run
           else
             move stack-Pret(stack-cnt) to PRET
             move stack-orig-ptr(stack-cnt) to Orig-PRET
             move stack-Current-Occurs(Stack-cnt) to Current-Occurs
             move stack-dim(Stack-Cnt)      to ws-dim
             move stack-level(Stack-Cnt)    to ws-level
             move stack-end-tbl(stack-cnt)  to ws-end-tbl
             move stack-Max-occurs(stack-cnt) to Max-occurs
             move stack-start-pret(stack-cnt) to start-pret
             move stack-end-pret(stack-cnt) to end-pret
             move stack-reason(stack-cnt)   to stop-reason
             move stack-reason-field(stack-cnt) to stop-reason-fields
             move stack-TblPtr(stack-cnt)     to TblPtr
             move stack-Condition-sw(stack-cnt) to Condition-Sw
             move stack-len(stack-cnt)          to ws-len
             move stack-last-pret(stack-cnt)    to last-pret
             if Trace-On
               display 'TRACE:Stack pulled from:' Stack-Cnt
                ':level=' ws-level
                ':dim=' ws-dim
                ':Table-Field=' Table-field-cnt(ws-level)
                ':Orig-Pret=' Orig-Pret
                ':Pret=' Pret
             end-if
             subtract 1 from stack-cnt
           end-if
           .

       ATF-ADD-FIELD-FIELD.
           if sCursor > In-Bits
           or sCursor > WS-END-TBL
              move zeroes to ws-len
              perform Add-just-this-field
              move Table-field-Cnt(ws-level) to TFCCurr
              add FLDT-Length(field-cnt) to FLDT-Length(TFCCurr)
                                            Occur-Size(ws-level)
              go to AAFF-Exit
           end-if

HK1219*    'check condition on field, if any
v0854      if Pret-Cond(Pret) > 0 then
             move Condition-sw to Save-Cond-Sw
             perform Check-any-Field-Condition
             if Condition-False
                move zeroes to ws-len
                move Save-Cond-Sw to Condition-Sw
                move Table-field-Cnt(ws-level) to TFCCurr
                add FLDT-Length(field-cnt) to FLDT-Length(TFCCurr)
                                              Occur-Size(ws-level)
                go to AAFF-Exit
             end-if
             move Save-Cond-Sw to Condition-Sw
v0854      end-if

           perform GetLen
           if ws-len < 0
              set Discard-Computed-Len-LT-zero to true
v00860        move zeroes to Fldt
              call 'NOTEDISC' using PreT, sCursor
                                    Discard-Table-Area
v00860                              Fldt
              add 1 to fields-in-error
              if First-Field-in-Error = 0
                 move Pret to First-Field-in-Error
                 move Discard-Code to First-Fields-Error
              end-if
              add 1 to discard-count(Discard-code)
              move Table-field-Cnt(ws-level) to TFCCurr
              add FLDT-Length(field-cnt) to FLDT-Length(TFCCurr)
                                            Occur-Size(ws-level)
              go to AAFF-exit
           end-if
           if ws-len = 0
             if TRACE-ON
               move PRET               to TR-NUM5A
               display 'TRACE:ADD-FIELD:' TR-NUM5A
                  ':' PRET-FIELDNAME(PRET)(1:23)
                  ':Length is zero field, stored in field-tbl'
             end-if
             if return-code = 1 and LENGTH-FIELD-OFFSET(PreT)
               move PRET to Offset-Field-is-zero
               add 1 to CAT-OFFSET-LEN-ZERO
             end-if
v00855     end-if
           perform Add-just-this-field

           move Table-field-Cnt(ws-level) to TFCCurr
           add FLDT-Length(field-cnt) to FLDT-Length(TFCCurr)
                                         Occur-Size(ws-level)
           .
       AAFF-Exit.
           exit.

       Add-just-this-field.
           move zeroes to return-code

           perform AddField
           if Discard-Something
             set Field-not-Valid(Field-Cnt) to true
             set Discard-Nothing to true
           end-if
           set Not-a-vField(Field-Cnt) to true
           move PRET-LEVEL(Pret) to FLDT-LEVEL(Field-Cnt)
           move Table-Field-cnt(ws-level) to FLDT-Table-Field(Field-cnt)
           perform Add-Field-Index
           perform Update-Chain

           if TRACE-ON perform Trace-Add-Field-Field end-if

      * Point to next field in source
      * if we are trying to store a field that is beyond
      *  the table or the record, we are going to just move zeroes
      *  to the length thus not bumping sCursor any more.
           move Table-Field-Cnt(ws-level) to TFCCurr
           if sCursor > WS-END-TBL
           or sCursor > IN-Bits or
             (Occur-Size(ws-level) + ws-len) > FLDT-EntryLength(TFCCurr)
             move zeroes to ws-len
             move zeroes to FLDT-LENGTH(Field-Cnt)
                            FLDT-Length-Byte(Field-Cnt)
             move 1 to Stop-Reason-Fields
             if Trace-On
               display 'TRACE:Trying to go beyond table'
                ':sCursor=' sCursor
                ':end-tbl=' ws-End-Tbl
                ':in-bits=' in-bits
                ':level=' ws-level
                ':dim=' ws-dim
               display '  '
                ':EL=' FLDT-EntryLength(n)
                ':TFC=' Table-Field-Cnt(ws-level)
                ':sub=' ws-sub-cnt
               perform varying n from 1 by 1 until n > 4
                 if Table-Field-Cnt(n) > 0
                   move Table-Field-Cnt(n) to i
                   display '  TFC1=' Table-Field-Cnt(n)
                           ':EL=' FLDT-EntryLength(i)
                 end-if
               end-perform
             end-if
           else
             compute sCursor = sCursor + ws-Len
           end-if

      * For tracing Purposes, check to see where we are at.
           if TRACE-ON
             if Fields-in-Error > 0
               display 'TRACE:Field in error'
                 ':Pret=' Pret
                 ':Field-Cnt=' Field-Cnt
             end-if
             if sCursor > WS-END-TBL
               move PRET              to TR-NUM5A
               move sCursor           to TR-NUM5B
               display 'TRACE:REACHED END OF TABLE SIZE:Pret=' TR-NUM5A
                ':sCursor=' TR-NUM5B
                ':tblptr=' TblPtr
             end-if
             if CURRENT-OCCURS > TABLE-HIGHEST(TBLPTR)
               move TBLPTR            to TR-NUM5A
               move TABLE-HIGHEST(TBLPTR) to TR-NUM5B
               display 'TRACE:OCCURS REACHED HIGHEST(FF)'
                ':TBLPTR=' TR-NUM5A ':HIGHEST=' TR-NUM5B
             end-if
           end-if
           .
      * This will work with a vField (OCCUR-vFIELD or ODO-vFIELD)
       ATF-ADD-vFIELD-FIELD.
           perform Add-Normal-vField thru ANvF-exit
           move Table-Field-cnt(ws-level) to FLDT-Table-Field(Field-cnt)

           if TRACE-ON perform Trace-Add-Field-Field end-if
           .
       AAvF-Exit.
           exit.


      * This will add a single entry into the FIELD table
      * Input: Pret - PreTable subscript
      *        sCursor - the start bit location
      *        ws-Len - the number of bits
      *        Pre-Field-Table -
      * Output: Field-cnt; subscript to newest entry
      *         Field-Table; updated table
       AddField.
           if Field-Max = zeroes
             display 'Engine:No Entries defined for Field Table'
              ':Max=' Field-Max
              ':RecType=' Record-to-process
              ':Rec#=' ws-rec-cnt
             move 12 to return-code
             stop run
           end-if
           add 1 to FIELD-CNT
           if FIELD-CNT > FIELD-MAX
             display 'Engine:Too many fields being generated'
              ':Max=' Field-Max
              ':RecType=' Record-to-process
              ':Rec#=' ws-rec-cnt
             move 12 to return-code
             stop run
           end-if

           set FldInx to Field-Cnt
v0854      move Field-Table-Init to Field-Table(FldInx)

           set PretInx to Pret
           set FLDT-FIELD(FldInx) to PretInx

      *    'set the starting bit/byte position
      *    ' Note. the sCursor was set by the previous field processed
      *    '  aka (Next).
HK1212     evaluate ws-dim
             when 0
               if Start-Address-is-Absolute(PretInx)
                 move Pret-Start(PretInx) to sCursor
               end-if
             when 1
               if current-occurs = 1
               and Start-Address-is-Absolute(PretInx)
                  move Pret-Start(PretInx) to sCursor
               end-if
           end-evaluate
           move sCursor to Fldt-Start(FldInx)
           divide Fldt-Start(FldInx) by 8
               giving Fldt-Start-Byte(FldInx)
               remainder Fldt-Start-Bit(FldInx)
           if Fldt-Start-Bit(FldInx) = 0 then
             move 8 to Fldt-Start-Bit(FldInx)
           else
             add 1 to Fldt-Start-Byte(FldInx)
           end-if
           if Fldt-Start-Bit(FldInx) > 4 then
             move 2 to Fldt-Start-Nib(FldInx)
           else
             move 1 to Fldt-Start-Nib(FldInx)
           end-if

      *    'set the field's length in bit/byte
           if WS-LEN = Pret-Length(PretInx)
             move Pret-Length     (PretInx) to Fldt-Length(FldInx)
             move Pret-length-Byte(PretInx) to Fldt-Length-Byte(Fldinx)
             move Pret-length-Nib (PretInx) to Fldt-Length-Nib (Fldinx)
           else
             move WS-LEN     to FLDT-LENGTH(FldInx)
             if ws-len > 0
               divide ws-Len by 8
                      giving FLDT-Length-Byte(FldInx)
                      remainder ws-bit
               if ws-bit = 0 then
                  move 8 to ws-bit
               else
                  add 1 to FLDT-Length-Byte(FldInx)
               end-if
               divide Fldt-Length(FldInx) by 4
                   giving Fldt-Length-Nib(FldInx)
             else
               move zeroes to FLDT-Length-Byte(FldInx)
                              Fldt-Length-Nib(FldInx)
             end-if
           end-if

           .

      *this will bump the index.
      *this needs to maintain the absolute address of the index
       Add-Field-Index.
           if ws-dim > 9
             display 'engine:logic error:dim# error in table processing'
               ':dim=' ws-dim ':field=' field-cnt ':pret=' pret
               move 12 to return-code
               stop run
           end-if
           if ws-dim > 0
             move Current-Occurs to ABS-Index(ws-dim)
           end-if
           compute n = ws-dim + 1
           perform varying n from n by 1 until n > 9
             move zeroes to ABS-Index(n)
           end-perform
           move ABS-Index-Area to FLDT-Index-Table(field-cnt)
           .

      *---------------------------------------------------------------
      *                        Trace areas
      *---------------------------------------------------------------
       Trace-Add-Field-Field.
           move FIELD-CNT                    to TR-NUM5A
           move FLDT-START-Byte(FIELD-CNT) to TR-NUM5B
           move FLDT-LENGTH-byte(FIELD-CNT) to TR-NUM5C
           move PRET-ENTRY-TYPE(PRET)         to TR-NUM1A
           move PRET-TYPE(PRET)               to TR-NUM1B
           call 'fmtind' using
                          FLDT-Index-Table(field-cnt), ws-dim, indices
           move Indices to Trace-Misc
           move 'Store' to Trace-Event
           display Trace-Event
              ' ' tr-num5a
              ' <' tr-num5b
               '.' tr-num5c '>'
              ' ' Pret
              ' ' Pret-FieldName(Pret)
              ' ' Pret-Entry-type(Pret)
              ' ' Pret-Type(Pret)
              ' ' Trace-Misc
           .
       Trace-Start-of-Main-Table.
           move 'Start' to Trace-Event
           display
               Trace-Event
               ' *Main Table*       '
               ' ' Pret
               ' ' Pret-FIELDNAME(Pret)
               ' T#' Pret-Table(Pret)
           .
       Trace-End-of-Main-Table.
           move 'End' to Trace-Event
           display Trace-Event
               ' * Main Table *'
               '     '
               ' ' Pret
               ' ' Pret-fieldName(Pret)
               ' T#' Pret-Table(Pret)
           .
       Trace-Table-Stats.
           compute ws-num5a = ws-end-tbl / 8
           divide ws-end-tbl by 8 giving ws-num5a remainder gbPosBit
           if gbPosBit not = 0 then add 1 to ws-num5a end-if
           divide sCursor by 8 giving ws-num5b remainder gbPosBit
           if gbPosBit not = 0 then add 1 to ws-num5b end-if
           move 'Stat1' to Trace-Event
           display Trace-Event
               '      '
               ' End-tbl=' ws-num5a
               ' Cursor=' ws-num5b
               ' Len=' ws-Len

           move 'Stat2' to Trace-Event
           display Trace-Event
               '      '
               ' Pret-Start=' Start-Pret
               ':Pret-End=' End-Pret
               ':#Fields=' TABLE-NUMBER-OF-FIELDS(TBLPTR)
               ':Level=' ws-sub-cnt
           .
       Trace-Start-Level.
           move PRET         to TR-NUM5A
           move END-PRET     to TR-NUM5B
           move IN-LEN       to TR-NUM5C
           move WS-DIM       to TR-NUM5D
           display 'TRACE:***START Level ' ws-sub-cnt ' TABLE****'
                    ':STACK=' Stack-cnt
                    ':PRET=' TR-NUM5A
                    ':END=' TR-NUM5B
                    ':' Pret-FieldName(Pret)
           move WS-Level     to TR-NUM5A
           display  '  In-LEN=' TR-NUM5C '(' in-bits ')'
                    ':Level=' TR-NUM5A
                    ':DIM=' TR-NUM5D
                    ':Oc#=' CURRENT-OCCURS
           call 'ShowLocation' using
                  by content z'  sCursor=' by reference sCursor
           .

       TRACE-TABLE-ENTRY.
           move FIELD-CNT   to TR-NUM5B
           divide sCursor by 8 giving tr-num5c remainder gbPosBit
           if gbPosBit not = 0 then add 1 to tr-num5c end-if
           move 'AddTb' to Trace-Event
           call 'fmtind' using
                          FLDT-Index-Table(field-cnt), ws-dim, indices
           move Indices to Trace-Misc
           display Trace-Event
               ' ' tr-num5b
               ' <' tr-num5c
               '.     >'
               ' ' Pret
               ' ' Pret-FieldName(Pret)
               ' ' Trace-Misc
               ' T#' TblPtr
               ':Dim=' ws-dim
               ':Oc#=' Current-Occurs
           .
       Trace-end-Level.
           move N           to TR-NUM5A
           move sCursor     to TR-NUM5B
           display 'TRACE:****END OF LEVEL ' ws-sub-cnt ' TABLE******'
               ':PRET=' PRET
               ':' PRET-FIELDNAME(PRET)(1:15)
               ':Level=' ws-Level
               ':dim#=' ws-dim
           .
       Trace-Stop-Generating.
           display 'TRACE:*****************************'
           display 'TRACE:*     STOP GENERATING       *'
           display 'TRACE:* Field-Cnt=' Field-Cnt '           *'
           display 'TRACE:* Pret     =' Pret      '           *'
           display 'TRACE:* Cursor   =' sCursor   '       *'
           display 'TRACE:* Level    =' ws-Level  '           *'
           display 'TRACE:*****************************'
           perform varying FPTR from 1 by 1 until FPTR > Field-CNT
               set Write-Trace-Fldt to true
               perform Write-Trace
           end-perform
           .
       Trace-Loop-Start.
           move 'LoopA' to trace-event
           divide sCursor by 8 giving ws-num5a remainder gbPosBit
           if gbPosBit not = 0 then add 1 to ws-num5a end-if
           divide ws-end-tbl by 8 giving ws-num5b remainder gbPosBit
           if gbPosBit not = 0 then add 1 to ws-num5b end-if
           display Trace-Event
               '      '
               ' <' ws-num5a
               '.' ws-num5b '>'
               ' ' Start-Pret
               ' thru ' End-Pret
               ':Max=' Max-Occurs
               ':#T' TblPtr
           .
       Trace-Loop-Stops.
           move 'StopA' to Trace-Event
           move Stop-Reason to tr-num2a
           move Stop-Reason-Fields to tr-num2b
           move ws-level to tr-num1a
           display Trace-Event
            ' Reason=' tr-num2a
            ' ReasonF=' tr-num2b
            ' Cur-Occurs=' Current-Occurs
            ' Orig-Pret=' Orig-Pret
            ' level=' tr-num1a
            ' dim=' ws-dim
            ' Lowest=' Table-Lowest(TBLPtr)
            ' T#=' TblPtr
           .


       Show-DLL-CONTROL-RECORDS.
           if RC > 0 perform LogShowError end-if

           if OPT-LOGShowDLLReport    perform LogShowDLL          end-if

      * Show the details of the other tables created from the DLL such
      *  as the Record, Table, Condition, Literals, etc.
           if OPT-LOGShowFiles        perform LogShowFiles        end-if
           if OPT-LOGShowRecords      perform LogShowRecords      end-if
           if OPT-LOGShowTables       perform LogShowTables       end-if
           if OPT-LOGShowDefaults     perform LogShowDefaults     end-if
           if OPT-LOGShowFunctions    perform LogShowFunctions    end-if
           if OPT-LOGShowConditions   perform LogShowConditions   end-if
           if OPT-LOGShowEntryLengths perform LogShowEntryLengths end-if

           if RC > 0 perform LogShowError end-if
           .
       LogShowError.
           move '*********************************************'
             to log-record
           write log-record
           move '* Caution:Error Loading DLL records. RC=0000*'
               to log-record
           move RC to WS-NUM4
           move ws-Num4 to log-record(41:4)
           write log-record
           move '* See SYSOUT for further details.           *'
             to log-record
           write log-record
           move '*********************************************'
             to log-record
           write log-record
           .
       LogShowDll.
      *---------------------------------------------------------
      *Remarks. This routine will create the LOG records of the DLL
      *Inputs:
      *    DLL-Area
      *    File-table-Area
      *    Record-Table-Area
      *    Table-Table-Area
      *    Cond-Table-Area
      *    Pre-field-Table-Area
      *    Counters-and-Totals
      *    Options-in-Effect
      *Outputs:
      *  Log-Record
      *Change-History.
      * Date------ Init Description of Change-------------------
      * 2014/08/25 HK   Change to be PERFORM instead of sub-routine
      * 2014/07/16 HK   Corrected start/end pret loop
      * 2014/05/21 HK   Corrected start/end pret loop
      * 2014/05/07 HK   Corrected sPos computes
      * 2014/05/06 HK   Base line
      *---------------------------------------------------------

           perform DLL-DLL
           perform Event-Check
           perform varying FileInx from 1 by 1 until FileInx > File-Cnt
             perform DLL-File
             perform varying RtInx from FT-Record(FileInx) by 1
                       until RtInx > Record-Cnt
               perform DLL-Record
               perform varying PretInx
                       from RT-Start-Pret(RtInx) by 1
                      until PretInx > RT-End-Pret(RtInx)
                 perform DLL-PreField
               end-perform
             end-perform
           end-perform
           if NumberOfNoShows > 0
             perform Write-NoShows
           end-if
           move all '_' to Log-record
           write log-record
           .
       DLL-DLL.
           move spaces to Log-record
           move 'DLL Compilation Report' to log-record(35:)
           write log-record

           move SPACES to LOG-RECORD
           write log-record

           move SPACES to LOG-RECORD
           move DLL-Version to LCD-Version
           move DLL-GenDate to LCD-GenDate
           move DLL-GenTime to LCD-GenTime
           move DLL-GenUser to LCD-GenUser
           move DLL-GenTool to LCD-GenTool
           write Log-Record from LOG-CONTROL-DLL
           move SPACES to LOG-RECORD
           move OPT-DefinedFields   to LCD-DefinedFields
           move OPT-DefinedTables   to LCD-DefinedTables
           move OPT-MaxRecordFields to LCD-MaxRecordFields
           move OPT-DefinedOps      to LCD-DefinedOps
           move OPT-DefinedConds    to LCD-DefinedConds
           move OPT-DefinedFuncs    to LCD-DefinedFuncs
v00887     if DLL-OutputEdit-Yes
v00886       move DLL-OutputEditOffset to LCD-OutputEdit
v00887     else
v00887       move 'n/a'                to LCD-OutputEditX
v00887     end-if
           write Log-Record from Log-Control-DLL2
           .
       Event-Check.
           move SPACES to LOG-RECORD
           if FDD-Requested and CAT-FDD-FIELDS = 0
             move 'Caution:FDD requested but no Event(FDD) fields'
               to log-record
             write log-record
           end-if
           if DAR-Requested and CAT-DAR-FIELDS = 0
             move 'Caution:DAR requested but no Event(DAR) fields'
               to log-record
             write log-record
           end-if
           if Mig-Requested and CAT-MIG-FIELDS = 0
             move 'Caution:Migrate requested but no Event(MIG) fields'
               to log-record
             write log-record
           end-if
           move SPACES to LOG-RECORD
           .
       DLL-File.
           move SPACES to LOG-RECORD
           write Log-Record
           write Log-record from LOG-Column
           move spaces to LOG-Detail
           move 'File'   to LD-Command
      * Field
           move all '. ' to LD-FieldName
           call 'Len' using FT-FileName(FileInx), tLen
                            value length FT-FileName
           move FT-FileName(FileInx)(1:tLen) to LD-FieldName(1:tLen)
      * Attribute
           move 1 to wPos
           move 'RecFM(' to LD-Attrib(wPos:6), add 6 to wPos
           if File-is-Fix-LENGTH(FileInx)
             move 'F' to LD-Attrib(wPos:1)
           else
             move 'V' to LD-Attrib(wPos:1)
           end-if
           add 1 to wPos
           move ')' to LD-Attrib(wPos:1), add 1 to wPos
           write LOG-RECORD from Log-detail
      * show any remarks
           perform varying Remarks-Ptr from 1 by 1
                     until Remarks-Ptr > Remarks-Cnt
             move Remarks-Text(Remarks-Ptr) to LR-Text
             write Log-Record from Log-Remarks
           end-perform
           .
       DLL-Record.
           if NumberOfNoShows > 0
             perform Write-NoShows
           end-if
           move spaces to LOG-Detail
           set Prefield-Ptr to PretInx
           move Prefield-Ptr to LD-Cnt
           move 'Record' to LD-Command
      * Field
           move all '. ' to LD-FieldName
           set pretInx to rt-field(RtInx)
           call 'Len' using Pret-FieldName(PretInx), tLen
                            value length Pret-FieldName(PretInx)
           move Pret-FieldName(PretInx)(1:tLen) to LD-FieldName(1:tLen)
      * Attribute
           move 1 to wPos
           move spaces to LD-Attrib
           move 'CH(1:' to LD-Attrib(wPos:5), add 5 to wPos
           move RT-Variability(RtInx) to ws-num18s
           perform Put-Number-to-Attrib
           move ')' to LD-Attrib(wPos:1), add 1 to wPos
      * Event
           move spaces to LD-Owner-Event
           move 1 to wPos
           if RT-Key-Field(RtInx) > 0
             move 'Key(' to LD-Owner-Event(wPos:4), add 4 to wPos
             move RT-Key-Field(RtInx) to ws-num5
             move ws-num5 to LD-Owner-Event(wPos:5), add 5 to wPos
             move ')'     to LD-Owner-Event(wPos:1), add 1 to wPos
           end-if
v00867     if RT-PTable(RtInx) not = spaces
v00867       if RT-Key-Field(RtInx) > 0
v00867         move ',' to LD-Owner-Event(wPos:1), add 1 to wPos
v00867       end-if
v00867       move 'PTable(' to LD-Owner-Event(wPos:7), add 7 to wPos
v00867       call 'Len' using RT-PTable(RtInx), tLen
v00867                      value length RT-PTable(RtInx)
v00867       move RT-PTable(RtInx)(1:tLen) to LD-Owner-Event(wPos:tLen)
v00867       add tLen to wPos
v00867       move ')' to LD-Owner-Event(wPos:1), add 1 to wPos
v00867     end-if
           write Log-Record from Log-Detail

           if RT-COND(RtInx) > 0
             move spaces to Log-record
             move 10 to wPos
             move RT-COND(RtInx)     to COND-PTR
             move Cond-Statement(Cond-Ptr) to Record-Cond
             move Cond-Statement-Len(Cond-Ptr) to tLen
             if tLen > 128
               move record-cond(1:112) to Log-record(1:128)
               add 128 to wPos
               move '...' to Log-record(wPos:4), add 3 to wPos
             else
               move record-cond(1:tLen) to Log-record(wPos:tLen)
               add tLen to wPos
             end-if
             move ')' to Log-record(wPos:1), add 1 to wPos
             write log-record
           end-if
           move spaces to Log-Record
           .
       DLL-PreField.
           evaluate true
             when REGULAR-FIELD(PretInx)
                  if OPT-LOGShowDLLFields
                    perform DLL-Field
                  else
                    perform DLL-Field-NoShow
                  end-if
             when GROUP-FIELD(PretInx)   perform DLL-Group
             when TABLE-FIELD(PretInx)   perform DLL-TABLE
             when Record-FIELD(PretInx)  continue
           end-evaluate
           .

       DLL-Field.
           move spaces to LOG-Detail
           set Prefield-Ptr to PretInx
           move Prefield-ptr to LD-Cnt
           evaluate true
             when NORMAL-FIELD(PretInx)  move 'Field'   to LD-Command
             when OCCUR-FIELD(PretInx)   move 'Field'   to LD-Command
             when ODO-FIELD(PretInx)     move 'Field'   to LD-Command
             when NORMAL-vFIELD(PretInx) move 'vField'  to LD-Command
             when OCCUR-vFIELD(PretInx)  move 'vField'  to LD-Command
             when ODO-vFIELD(PretInx)    move 'vField'  to LD-Command
           end-evaluate
      * Field
           move spaces   to LD-FieldName
           move all '. ' to LD-FieldName(5:)
           compute wPos = (Pret-Level(PretInx) * 2) - 1
           move Pret-Level(PretInx) to wLevel
           move wlevel to LD-FieldName(wPos:2)
           add 2 to wPos
           add 2 to wPos
           call 'Len' using Pret-FieldName(PretInx) tLen
                            value length Pret-FieldName
           move Pret-FieldName(PretInx)(1:tLen)
             to LD-FieldName(wPos:tLen)
           add tLen to wPos
           if Pret-Hash(PretInx) > 0
             call 'fmtHash' using Pret-Hash(PretInx), hash-str, hash-len
             move hash-str(1:hash-len) to LD-FieldName(wPos:hash-len)
             add hash-len to wPos
           end-if
      * Attribute
           move spaces to LD-Attrib
           move 1 to wPos
           if not Regular-vField(PretInx)
             move Pret-Type(PretInx)   to ws-num5
             move TT-Type-Short(ws-num5) to LD-Attrib
             add 2 to wPos
             move '(' to LD-Attrib(wPos:1)
             add 1 to wPos
             move Pret-Start-Byte(PretInx) to ws-pos
             move Pret-Start-Nib (PretInx) to ws-nib
             move Pret-Start-Bit (PretInx) to ws-bit
             move ws-pos to ws-num18s
             perform Put-Number-to-Attrib
             move '.'  to LD-Attrib(wPos:1), add 1 to wPos
             move ws-bit to LD-Attrib(wPos:1), add 1 to wPos
             move ':'  to LD-Attrib(wPos:1), add 1 to wPos
             evaluate true
               when Length-Field-Offset(PretInx)
                   move 'Offset('           to LD-Attrib(wPos:7)
                   add 7 to wPos
                   move PRET-LENGTH-FIELD(PretInx) to ws-num5
                   move ws-num5             to LD-Attrib(wPos:5)
                   add 5 to wPos
                   move ')'                 to LD-Attrib(wPos:1)
                   add 1 to wPos
               when Length-Field-VarChar(PretInx)
                   move 'VarChr('           to LD-Attrib(wPos:7)
                   add 7 to wPos
                   move PRET-LENGTH-FIELD(PretInx) to ws-num5
                   move ws-num5             to LD-Attrib(wPos:5)
                   add 5 to wPos
                   move ')'                 to LD-Attrib(wPos:1)
                   add 1 to wPos
               when other
                 evaluate true
                     when Type-Bit(PretInx)
                       compute ws-num18s = Pret-Length(PretInx)
                       move 'b' to bit-nib
                     when Type-Nib(PretInx)
                     when Type-PD-NEC4(PretInx)
                       compute ws-num18s = Pret-Length(PretInx) / 4
                       move 'n' to bit-nib
                     when other
                       compute ws-num18s = Pret-Length(PretInx) / 8
                       move ' ' to bit-nib
                 end-evaluate
                 perform Put-Number-to-Attrib
                 if bit-nib not = ' '
                   move bit-nib to LD-Attrib(wPos:1)
                   add 1 to wPos
                 end-if
             end-evaluate
             move ')' to LD-Attrib(wPos:1)
             add 1 to wPos
             if Pret-Date-FMT(PretInx) > 0
               move ',' to LD-Attrib(wPos:1)
               add 1 to wPos
               move Pret-Date-FMT(PretInx) to DateFMT-Ptr
               move DateFMT-Text(DateFMT-Ptr)
                 to LD-Attrib(wPos:DateFMT-Length(DateFMT-Ptr))
               add DateFMT-Length(DateFMT-Ptr) to wPos
             end-if
           end-if
      * vField Attribute func(#####),5,C
           if Regular-vField(PretInx)
             move 'Func(' to LD-Attrib(wPos:5)
             add 5 to wPos
             move Pret-Func(PretInx) to ws-num5
             move ws-num5 to LD-Attrib(wPos:5)
             add 5 to wPos
             move '),' to LD-Attrib(wPos:2), add 2 to wPos
             move Pret-Length-Byte(PretInx) to ws-num18s
             perform Put-Number-to-Attrib
             move ',' to LD-Attrib(wPos:1), add 1 to wPos
             move Pret-Type(PretInx)   to ws-num5
             move TT-Type-Short(ws-num5) to LD-Attrib(wPos:2)
             add 2 to wPos
           end-if
      * Owner
           move Pret-HashPar(PretInx) to LD-Owner
      * Event  F,D(#)R(#),M(#,ch,def)
           move 1 to wPos
           move PRET-FDD-SW(PretInx) to LD-Event(wPos:1)
           add 1 to wPos
           move ',' to LD-Event(wPos:1)
           add 1 to wPos
           move PRET-DAR-SW(PretInx) to LD-Event(wPos:1)
           add 1 to wPos
           if Pret-DAR-Limit(PretInx) > 0
               move '(' to LD-Event(wPos:1), add 1 to wPos
               move 0 to tLen
               move PRET-DAR-Limit(PretInx) to ws-Num18S
               inspect ws-num18S-X tallying tLen for leading space
               move ws-num18s-x (tLen + 1:) to LD-Event(wPos:18 - tLen)
               compute wPos = wPos + (18 - tLen)
               move ')' to LD-Event(wPos:1), add 1 to wPos
           end-if
           if Pret-Range(PretInx) > 0
               move ',' to LD-Event(wPos:1), add 1 to wPos
               move 'R(' to LD-Event(wPos:2), add 2 to wPos
               move 0 to tLen
               move PRET-Range(PretInx) to ws-Num18S
               inspect ws-num18S-X tallying tLen for leading space
               move ws-num18s-x (tLen + 1:) to LD-Event(wPos:18 - tLen)
               compute wPos = wPos + (18 - tLen)
               move ')' to LD-Event(wPos:1), add 1 to wPos
           end-if
           if MIG-Event-Requested(PretInx)
             move ',' to LD-Event(wPos:1), add 1 to wPos
             move PRET-MIG-SW(PretInx) to LD-Event(wPos:1)
             add 1 to wPos
             move '(' to LD-Event(wPos:1), add 1 to wPos
             move PRET-MIG-LEN(PretInx) to ws-num18s
             move 0 to tLen
             inspect ws-num18S-X tallying tLen for leading space
             move ws-num18s-x (tLen + 1:) to LD-Event(wPos:18 - tLen)
             compute wPos = wPos + (18 - tLen)
             move ',' to LD-Event(wPos:1), add 1 to wPos
             move PRET-MIG-TYPE(PretInx) to ws-num5
             move TT-TYPE-Short(ws-num5) to LD-Event(wPos:2)
             add 2 to wPos
             if Pret-MIG-Date-FMT(PretInx) > 0
               move ',' to LD-Event(wPos:1)
               add 1 to wPos
               move Pret-MIG-Date-FMT(PretInx) to DateFMT-Ptr
               move DateFMT-Text(DateFMT-Ptr)
                 to LD-Event(wPos:DateFMT-Length(DateFMT-Ptr))
               add DateFMT-Length(DateFMT-Ptr) to wPos
             end-if
             if Pret-Mig-API(PretInx) not = spaces
               move ',' to LD-Event(wPos:1), add 1 to wPos
               call 'len' using PRET-MIG-API(PretInx) tLen
                              value length Pret-Mig-API
               move Pret-Mig-API(PretInx)(1:tLen) to LD-Event(wPos:tLen)
               add tLen to wPos
             end-if
             move ')' to LD-Event(wPos:1), add 1 to wPos
           end-if

           write LOG-RECORD from LOG-Detail
           .
       DLL-Field-NoShow.
           add 1 to NumberofNoShows
           if FirstNoShow = 0
             set FirstNoShow to PretInx
           end-if
           set LastnoShow to PretInx
           .
       DLL-Group.
           if NumberOfNoShows > 0
             perform Write-NoShows
           end-if
           move spaces to LOG-Detail
           set Prefield-Ptr to PretInx
           move Prefield-ptr to LD-Cnt
           move 'Group'  to LD-Command
      * Field
           move spaces   to LD-FieldName
           move all '. ' to LD-FieldName(5:)
           compute wPos = (Pret-Level(PretInx) * 2) - 1
           move Pret-Level(PretInx) to wLevel
           move wlevel to LD-FieldName(wPos:2)
           add 2 to wPos
           add 2 to wPos
           call 'fmtHash' using Pret-Hash(PretInx), hash-str, hash-len
           call 'Len' using Pret-FieldName(PretInx) tLen
                            value length Pret-FieldName
           move Pret-FieldName(PretInx)(1:tLen)
             to LD-FieldName(wPos:tLen)
           add tLen to wPos
           move hash-str(1:hash-len) to LD-FieldName(wPos:hash-len)
           add hash-len to wPos
      * Owner
           move Pret-HashPar(PretInx) to LD-Owner

           write LOG-RECORD from LOG-Detail
           .
       DLL-TABLE.
           if NumberOfNoShows > 0
             perform Write-NoShows
           end-if
           move spaces to LOG-Detail
           set Prefield-Ptr to PretInx
           set TblInx to Pret-Table(PretInx)
           move Prefield-ptr to LD-Cnt
           move 'Table'  to LD-Command
           move Table-Group-sw(TblInx) to LD-Command(7:1)
      * Field
           move spaces   to LD-FieldName
           move all '. ' to LD-FieldName(5:)
           compute wPos = (Pret-Level(PretInx) * 2) - 1
           move Pret-Level(PretInx) to wLevel
           move wlevel to LD-FieldName(wPos:2)
           add 2 to wPos
           add 2 to wPos
           call 'Len' using Pret-FieldName(PretInx) tLen
                            value length Pret-FieldName
           move Pret-FieldName(PretInx)(1:tLen)
             to LD-FieldName(wPos:tLen)
           add tLen to wPos
           if Pret-Hash(PretInx) > 0
             call 'fmtHash' using Pret-Hash(PretInx), hash-str, hash-len
             move hash-str(1:hash-len) to LD-FieldName(wPos:hash-len)
             add hash-len to wPos
           end-if
      * Attribute
           move 1 to wPos
           move spaces to LD-Attrib
           move 'CH(' to LD-Attrib(wPos:3), add 3 to wPos
           move Pret-Start-Byte(PretInx) to ws-num18s
           perform Put-Number-to-Attrib
           move ':' to LD-Attrib(wPos:1), add 1 to wPos
           move Pret-Length-Byte(PretInx) to ws-num18s
           perform Put-Number-to-Attrib
           move ')' to LD-Attrib(wPos:1), add 1 to wPos

      * Owner
           move Pret-HashPar(PretInx) to LD-Owner
      * Event
           move 1 to wPos
           evaluate true
            when Table-Occur(PretInx)
                 move 'Occur(' to LD-Event(wPos:6)
                 add 6 to wPos
            when Table-ODO(PretInx)
                 move 'ODO(' to LD-Event(wPos:4)
                 add 4 to wPos
           end-evaluate
           move Table-Lowest(TblInx) to ws-num18s
           perform Put-Number-to-Event
           if Table-ODO(PretInx)
             move ',' to LD-Event(wPos:1), add 1 to wPos
             move Table-Highest(TblInx) to ws-num18s
             perform Put-Number-to-Event
           end-if
           move '),Fields=' to LD-Event(wPos:9); add 9 to wPOs
           move Table-Number-of-Fields(TblInx) to ws-num18s
           perform Put-Number-to-Event

           write Log-record from Log-Detail

      * Attributes continued, if needed

           if TABLE-DEPEND-SET(TblInx)
             move spaces to Log-Detail
             move 1 to wPos
             move 'Depend(' to LD-Attrib(wPos:7), add 7 to wPos
             move TABLE-DEPEND-FIELD(TblInx) to ws-num5
             move ws-num5   to LD-attrib(wPos:5), add 5 to wPos
             move ')'       to LD-Attrib(wPos:1), add 1 to wPos
             write Log-Record from Log-Detail
           end-if

           if TABLE-LIMIT-SET(TblInx)
             move spaces to Log-Detail
             move 1 to wPos
             move 'Limit(' to LD-Attrib(wPos:6), add 6 to wPos
             if Table-Limit-Base-Field(TblInx) > zeroes
               move open-bracket to LD-Attrib(wPos:1), add 1 to wPos
               move Table-Limit-Base-Field(TblInx) to ws-num18s
               perform Put-Number-to-Attrib
               move close-bracket to LD-Attrib(wPos:1), add 1 to wPos
             end-if
             if Table-Limit-Field1-is-Value(TblInx)
               move '=' to LD-attrib(wPos:1), add 1 to wPos
             end-if
             move Table-Limit-Field1(TblInx) to ws-num18s
             perform Put-Number-to-Attrib
             if Table-Limit-Compute(TblInx) not = spaces
               move Table-Limit-Compute(TblInx) to LD-Attrib(wPos:1)
               add 1 to wPos
               if Table-Limit-Field2-is-Value(TblInx)
                 move '=' to LD-attrib(wPos:1), add 1 to wPos
               end-if
               move Table-Limit-Field2(TblInx) to ws-num18s
               perform Put-Number-to-Attrib
             end-if
             move ')'       to LD-Attrib(wPos:1), add 1 to wPos
             write Log-Record from Log-Detail
           end-if

           if Table-EL-Ptr(TblInx) > 0
             move spaces to Log-Detail
             move 1 to wPos
             move 'EntryLength(' to LD-Attrib(wPos:12)
             add 12 to wPos
             move Table-EL-Ptr(TblInx) to ws-Num5
             move ws-num5              to LD-attrib(wPos:5)
             add 5 to wPos
             move ')' to LD-attrib(wPos:1), add 1 to wPos
             write Log-record from Log-Detail
           end-if

           if TABLE-COND(TblInx) > 0
             move spaces to Log-Detail
             move 1 to wPos
             move 'Cond(' to LD-Attrib(wPos:5)
             add 5 to wPos
             move TABLE-COND(TblInx)   to ws-Num5
             move ws-num5              to LD-attrib(wPos:5)
             add 5 to wPos
             move ')' to LD-attrib(wPos:1), add 1 to wPos
             write Log-record from Log-Detail
           end-if
           .
       Write-NoShows.
           move FirstNoShow     to LDN-skipped
           write log-record from Log-Detail-Noshow
           if FirstNoShow not = LastNoShow
             move LastNoShow      to LDN-skipped
             write log-record from Log-Detail-Noshow
           end-if
           move low-values to NoShowFields
           .
       Put-Number-to-Attrib.
           MOVE 0 TO tLen
           INSPECT ws-num18S-X TALLYING tLen FOR LEADING SPACE
           MOVE ws-num18s-x (tLen + 1 :) to LD-Attrib(wPos:18 - tLen)
           compute wPos = wPos + (18 - tLen)
           .
       Put-Number-to-Event.
           MOVE 0 TO tLen
           INSPECT ws-num18S-X TALLYING tLen FOR LEADING SPACE
           MOVE ws-num18s-x (tLen + 1 :) to LD-Event(wPos:18 - tLen)
           compute wPos = wPos + (18 - tLen)
           .
      *END PROGRAM ShowDLL.

      * This will show the files details.
      * At present the program only supports one input file. However,
      *  there is some structure for multiple files. When implemented
      *  the other input files will be INFILEx. Where x is 1 to 99.
      *  The INFILEx will correspond to the FILE statements encountered.
       LogShowFiles.
           move spaces to log-record
           move ' Files Table' to log-record(32:)
           write log-record
           move spaces to log-record
           write log-record from Log-File-Column
           move spaces to log-record
           perform varying FileInx from 1 by 1 until FileInx > File-CNT
              set LFT-Ptr to FileInx
              move FT-FILENAME(FileInx) to LFT-FileName
              move FT-DSN(FileInx)         to LFT-DSN
              move FT-DDName(FileInx)      to LFT-DDName
              move FT-Date(FileInx)(1:4)   to LFT-Date(1:4)
              move '-'                     to LFT-Date(5:1)
              move FT-Date(FileInx)(5:2)   to LFT-Date(6:2)
              move '-'                     to LFT-Date(8:1)
              move FT-Date(FileInx)(7:2)   to LFT-Date(9:2)
              move FT-FILE-LENGTH(FileInx) to LFT-Length
              if FILE-IS-FIX-LENGTH(FileInx)
                move 'Fixed' to LFT-Format
              else
                move 'Variable' to LFT-Format
              end-if
v00894        if File-CSV-No(FileInx)
v00894          move spaces to LFT-CSV
v00894        else
v00894          move 'CSV(' to LFT-CSV
v00894          move FT-Delim(FileInx) to LFT-CSV(5:)
v00894          move ')' to LFT-CSV(FT-Delim-Len(FileInx) + 5:1)
v00894        end-if
              write log-record from Log-File-Table
           end-perform
           move all '_' to Log-record
           write log-record
           .
       LogShowRecords.
           move spaces to log-record
           move record-cnt to ws-num5
           move spaces to log-record
           string 'Record table. Number of Records ='
                                                     delimited by size
                  ws-num5                            delimited by size
             into log-record
           end-string
           write log-record
           move spaces to log-record
           write log-record from Log-Record-Table-Column
           move spaces to log-record
           perform varying rtInx from 1 by 1 until rtInx > Record-CNT
              set LRT-SUB to rtInx
              move RT-FILE(rtInx)           to LRT-FILE
              move RT-Field(rtInx)          to Pret
              move PRET-FIELDNAME(Pret)     to LRT-RECORDNAME
              move RT-COND(rtInx)           to LRT-COND
              move RT-Start-Pret(rtInx)     to LRT-Start-Pret
              move RT-End-Pret(rtInx)       to LRT-End-Pret
              move RT-Variability(rtInx)    to LRT-Variability
              move RT-num-vFields(rtInx)    to LRT-num-vFields
              move RT-Max-Len(rtInx)        to LRT-Max-Len
              move RT-Key-Field(rtInx)      to LRT-Key-Field
              move RT-KN(rtInx)(1:30)       to LRT-KN
              write LOG-RECORD from LOG-Record-Table
           end-perform
           move all '_' to Log-record
           write log-record
           .
       LogShowTables.
           move spaces to log-record
           move table-cnt to ws-num5
           string 'Table table. Number of tables ='    delimited by size
                  ws-num5                              delimited by size
             into log-record
           end-string
           write log-record
           move spaces to log-record
           string
            'TBL#  File Rec# -Pret '
                                                       delimited by size
            'LVL TABLE-NAME-------------------- DIMS #FLDS #MAND Last#'
                                                       delimited by size
            ' T S DpFld LBASE LFLD1 LFLD2 COND EnPtr'
                                                       delimited by size
            into log-record
           end-string
           write log-record
           perform varying tblInx from 1 by 1 until tblInx > TABLE-CNT
              set LTable-sub to tblInx
              move TABLE-FILE(tblInx)       to LTABLE-FILE
              move TABLE-RECORD(tblInx)     to LTABLE-RECORD
              move TABLE-FIELD-PTR (tblInx) to LTABLE-FIELD-PTR
              move Table-level-Direction (tblInx)
                to LTable-level-Direction
              move Table-level-Value (tblInx) TO LTable-level-Value
              move TABLE-FIELD-PTR (tblInx) to Pret
              move Pret-FieldName(Pret)       TO LTABLE-NAME
              move TABLE-DIMS (tblInx)        TO LTABLE-DIMS
              move TABLE-NUMBER-OF-FIELDS(tblInx)
                to LTable-Number-of-fields
              move TABLE-Mandatory-FIELDS(tblInx)
                to LTable-Mandatory-fields
              move TABLE-Last-Field     (tblInx) TO LTABLE-LAST-FIELD
              move TABLE-TYPE           (tblInx) TO LTABLE-TYPE
              move TABLE-DEPEND-LIMIT-SW(tblInx)
                to LTABLE-DEPEND-LIMIT-SW
              move TABLE-DEPEND-FIELD (tblInx)   TO LTABLE-DEPEND-FIELD
              move Table-Limit-Base-Field(tblInx)
                to LTable-Limit-Base-Field
              move TABLE-LIMIT-FIELD1 (tblInx) to LTable-Limit-Field1
              move TABLE-LIMIT-FIELD2 (tblInx) to LTable-Limit-Field2
              move TABLE-COND (tblInx)         to LTable-Cond
              move Table-EL-Ptr (tblInx)       to LTable-EL-Ptr
              write log-record from Log-Table-Table
           end-perform
           move all '_' to Log-record
           write log-record
           .
       LogShowDefaults.
           move spaces to log-record
           move default-cnt to ws-num5
           string 'Default table. Number of entries =' delimited by size
                  ws-num5 delimited by size
             into log-record
           write log-record
           move spaces to log-record
           move
            'DEF# --Times-- --Length- H Value---------------'
             to log-record
           write log-record
           move spaces to log-record
           perform varying Default-Ptr from 1 by 1
              until Default-Ptr > Default-Cnt
              move Default-Ptr                 to LDT-Ptr
              move Default-Times(Default-Ptr)  to LDT-Times
              move Default-Length(Default-Ptr) to LDT-Length
              move Default-Hex-sw(Default-Ptr) to LDT-Hex
              if Default-Hex(default-ptr)
                move Default-Length(Default-ptr) to wLen
                call 'HexShow' using Default-Value(Default-ptr),
                  wLen, ws-Hex
                move "x'" to LDT-Area
                move ws-Hex to LDT-Area(3:)
                move "'"  to LDT-Area((wLen * 2) + 3:1)
              else
                move Default-Value(Default-ptr) to LDT-Area
              end-if
              write log-record from Log-Default-Table
           end-perform
           move all '_' to Log-record
           write log-record
           .
       LogShowFunctions.
           move spaces to log-record
           move func-cnt to ws-num5
           string 'Function Table. Number of Funcs=' delimited by size
                  ws-num5 delimited by size
             into log-record
           write log-record
           move spaces to log-record
           string
            'FUN#   Fil# Rec# Ordr  -#of Pret- ' delimited by size
            'vField-Name------------------- Func- D'
                                                     delimited by size
            ' Start Lengt # Options---------------------'
                                                     delimited by size
            into log-record
           write log-record
           move spaces to log-record
           perform varying FunInx from 1 by 1 until FunInx > FUNC-CNT
                                                 or FunInx > Func-Max
              set PretInx to Func-Owning-Field(FunInx)
              move spaces to LFnT-Disp
              evaluate true
                when Func-String(FunInx)
                     move FT-String-Value(FunInx) to LFnT-Disp
                when Func-Hex(FunInx)
                     move FT-Hex-Value(FunInx)(1:FT-Length(FunInx))
                       to ws-keyword
                     call 'HEXSHOW' using
                          by content   ws-keyword, FT-Length(FunInx)
                          by reference WS-HEX
                     move "x'"                    to LFnT-Disp
                     move ws-hex                  to LFnT-Disp(3:)
                     compute wLen = (FT-length(FunInx) * 2) + 3
                     move "'" to LFnT-Disp(wLen:1)
                when Func-RDW(FunInx)
                     move FT-RDW(FunInx)          to LFnT-field1
                when Func-Space(FunInx)
                     move FT-Spaces(FunInx)       to LFnT-field1
                when Func-Count(FunInx)
                     move FT-Count-Field(FunInx)  to LFnT-field1
                     move FT-Count-Constant(FunInx) to LFnT-field2
                     move FT-Count-Oper(FunInx)     to LFnT-field3
                when Func-Size(FunInx)
                     if Size-Field-is-RDW(FunInx)
                        move '*RDW*'                to LFnT-Disp(1:5)
                     else
                        move FT-Size-Field(FunInx)  to LFnT-field1
                     end-if
                     move FT-Size-Constant(FunInx)  to LFnT-field2
                     move FT-Size-Oper(FunInx)      to LFnT-field3
                     move FT-Size-Field-SW(FunInx) to LFnT-field3(3:)
                when Func-Start(FunInx)
                     move FT-Start-Field(FunInx)     to LFnT-field1
                     move FT-Start-Constant(FunInx)  to LFnT-field2
                     move FT-Start-Oper(FunInx)      to LFnT-field3
                     move FT-Start-Field-SW(FunInx) to LFnT-field3(3:)
                     move open-bracket                to FBF-lit1
                     move FT-Start-Base-Field(FunInx) to FBF-Value
                     move close-bracket               to FBF-lit2
                     move Fmt-Base-Fld                to LFnT-field4
                when Func-Entry(FunInx)
                     move FT-Entry-Field(FunInx)     to LFnT-field1
                     move FT-Entry-Constant(FunInx)  to LFnT-field2
                     move FT-Entry-Oper(FunInx)      to LFnT-field3
                when Func-End(FunInx)
                     move FT-End-Field(FunInx)     to LFnT-field1
                     move FT-End-Constant(FunInx)  to LFnT-field2
                     move FT-End-Oper(FunInx)      to LFnT-field3
                     move FT-End-Field-SW(FunInx)  to LFnT-field3(3:)
                     move open-bracket              to FBF-lit1
                     move FT-End-Base-Field(FunInx) to FBF-Value
                     move close-bracket             to FBF-lit2
                     move Fmt-Base-Fld              to LFnT-field4
                when Func-Field(FunInx)
                     move FT-FIELD(FunInx)         to LFnT-field1
                     move FT-Field-Start(FunInx)   to LFnT-field2
                when Func-TimeStamp(FunInx)
                     move FT-TimeStamp-Option(FunInx) to LFnT-Disp
                when Func-Build(FunInx)
                     move FT-Build-Cond-Ptr(FunInx) to LFnT-field1
                when other
                     move '* Unknown *' to LFnT-Disp
              end-evaluate
              compute N = Func-SW(FunInx) + 1
              set LFnT-Ptr to FunInx
              move Func-File(FunInx)         to LFnT-File
              move Func-Record(FunInx)       to LFnT-Record
              move Func-Order(FunInx)        to LFnT-Order
              move Func-Total(FunInx)        to LFnT-Total
              move Func-OWNING-FIELD(FunInx) to LFnT-Field
              move PRET-FIELDNAME(PretInx)   to LFnT-Fieldname
              move ws-func-name-table(N)     to LFnT-Name
              move FT-Data-SW(FunInx)        to LFnT-Data-sw
              move FT-Start(FunInx)          to LFnT-Start
              move FT-Length(FunInx)         to LFnT-Length
              move FT-Hash  (FunInx)         to LFnT-Hash
              write log-record from Log-Function-Table
           end-perform
           move all '_' to Log-record
           write log-record
           .
       LogShowConditions.
           move spaces to log-record
           move Cond-cnt to ws-num6
           string 'Condition Table. Number of Conditions ='
                     delimited by size
                   ws-num6 delimited by size
             into log-record
           write log-record
           move spaces to log-record
           write log-record from DISPLAY-Enhanced-COND-COL
           move spaces to log-record
           perform varying COND-PTR from 1 by 1
            until COND-PTR > COND-CNT
             move cond-ptr                     to DECA-COND-PTR
             move cond-file(cond-ptr)          to DECA-COND-file
             move cond-Record(cond-ptr)        to DECA-COND-Record
             move cond-owning-field(cond-ptr)  to DECA-COND-OWNING-FIELD
             move cond-record-table-sw(cond-ptr) to DECA-CT-TABLE-SW
             move Cond-ExecuteNdx(cond-ptr)      to DECA-ExecuteNdx
             move Cond-ExecuteQty(cond-ptr)      to DECA-ExecuteQty
             move Cond-Statement(Cond-ptr)       to DECA-Formula
             write log-record from Display-Enhanced-cond-area
           end-perform
           move all '_' to Log-record
           write log-record
           .
       LogShowEntryLengths.
           move spaces to log-record
           move el-cnt to ws-num5
           string 'EntryLength Table. Number=' delimited by size
                   ws-num5 delimited by size
             into log-record
           write log-record
           move spaces to log-record
           string
            'EPtr -Tbl# Pret# -Val- -SW- --Start--' delimited by size
            ' Field Name---------------------------------'
                                                    delimited by size
             into log-record
           write log-record
           move spaces to log-record
           perform varying EL-Ptr from 1 by 1
             until EL-Ptr > EL-CNT
               move EL-Ptr                 to LET-Ptr
               move EL-Table-field(EL-Ptr) to LET-Table-Field
               move EL-Field     (EL-Ptr)  to LET-Field
               move EL-Value     (EL-Ptr)  to LET-Value
               move EL-Field-Sw  (EL-Ptr)  to LET-Field-sw
               move EL-Start     (EL-Ptr)  to LET-Start
               move EL-FN(EL-Ptr)          to LET-FN
               write log-record from Log-EntryLength-Table
           end-perform
           move all '_' to Log-record
           write log-record
           .
      *------------------------------------------------------------
      * Move File totals to LOG report
      *------------------------------------------------------------
       LogShowTotals.
           move spaces to Log-Record
           write Log-Record
           move all '_' to Log-Record
           write Log-Record
           move 'File Totals:' to Log-record
           write Log-Record

           move 'OPTIONS-IN'       to LTL-Description
           move OPT-Count          to LTL-Count
           move Log-Total-Line     to Log-record
           write Log-Record

           move 'CONTROL-IN'       to LTL-Description
           move CAT-CONTROL-IN     to LTL-Count
           move Log-Total-Line     to Log-record
           write Log-Record

           move 'CONTROL-FILE'     to LTL-Description
           move CAT-CONTROL-FILE   to LTL-Count
           move Log-Total-Line     to Log-record
           write Log-Record

           move 'CONTROL-RECORD'   to LTL-Description
           move CAT-CONTROL-RECORD to LTL-Count
           move Log-Total-Line     to Log-record
           write Log-Record

           move 'CONTROL-TABLE'    to LTL-Description
           move CAT-CONTROL-TABLE  to LTL-Count
           move Log-Total-Line     to Log-record
           write Log-Record

           move 'CONTROL-FIELD'    to LTL-Description
           move CAT-CONTROL-FIELD  to LTL-Count
           move Log-Total-Line     to Log-record
           write Log-Record

           move 'INPUT RECORDS'    to LTL-Description
           move WS-REC-CNT         to LTL-Count
           move Log-Total-Line     to Log-record
           write Log-Record


           move 'Input Record Types:' to Log-record
           write Log-Record
           move spaces to log-record

           perform varying rtInx from 1 by 1 until rtInx > record-cnt
             move spaces to LTL-Description
             set ws-num3 to rtInx
             move ws-num3 to LTL-Description(3:3)
             move RT-Field(rtInx) to Pret
             move PRET-FIELDNAME(Pret) to LTL-Description(7:)
             move rt-Count(rtInx)    to LTL-Count
             move Log-Total-Line     to Log-record
             write Log-Record
           end-perform

           move 'DISCARDS'         to LTL-Description
           move CAT-DISCARDS       to LTL-Count
           move Log-Total-Line     to Log-record
           write Log-Record

           move 'GOOD RECORDS'     to LTL-Description
           move CAT-GOOD-RECORDS   to LTL-Count
           move Log-Total-Line     to Log-record
           write Log-Record

v00886     move 'OUTPUT RECORDS'   to LTL-Description
v00886     move ms-rec-out         to LTL-Count
v00886     move Log-Total-Line     to Log-record
v00886     write Log-Record

v00886     move 'OUTPUT RECORDS EEOR'   to LTL-Description
v00886     move CAT-num-API-EEOR   to LTL-Count
v00886     move Log-Total-Line     to Log-record
v00886     write Log-Record

v00879     move 'DIVIDED RECORDS'   to LTL-Description
v00879     move CAT-num-API-divided to LTL-Count
v00879     move Log-Total-Line      to Log-record
v00879     write Log-Record


           move 'Output Record Types:' to Log-record
           write Log-Record
           move spaces to log-record

           perform varying rtInx from 1 by 1 until rtInx > record-cnt
             move spaces to LTL-Description
             set ws-num3 to rtInx
             move ws-num3 to LTL-Description(3:3)
             move rt-Field(rtInx) to Pret
             move PRET-FIELDNAME(Pret) to LTL-Description(7:)
             move rt-Migrate(rtInx)  to LTL-Count
             move Log-Total-Line     to Log-record
             write Log-Record
           end-perform

           move 'Gap in Bits in DLL'      to LTL-Description
           move GIB-Count          to LTL-Count
           move Log-Total-Line     to Log-record
           write Log-Record
           if GIB-Count > zeroes
             move '  First Field in Gap'    to LTL-Description
             move GIB-First-Field           to LTL-Count
             move Log-Total-Line     to Log-record
             write Log-Record
           end-if

           move 'Offset lengths of zero' to LTL-Description
           move CAT-OFFSET-LEN-ZERO to LTL-Count
           move Log-Total-Line      to Log-record
           write Log-Record

           move 'No Group @ EOR'   to LTL-Description
           move CAT-GROUP-EOR      to LTL-Count
           move Log-Total-Line     to Log-record
           write Log-Record

           move 'FDD FIELDS'       to LTL-Description
           move CAT-FDD-Fields     to LTL-Count
           move Log-Total-Line     to Log-record
           write Log-Record

           move 'DAR FIELDS'       to LTL-Description
           move CAT-DAR-Fields     to LTL-Count
           move Log-Total-Line     to Log-record
           write Log-Record

           move 'MIG FIELDS'       to LTL-Description
           move CAT-MIG-Fields     to LTL-Count
           move Log-Total-Line     to Log-record
           write Log-Record

           perform varying N from 1 by 1 until N > Discard-Reason-Max
             move Discard-Reason(N) to LTL-Description
             move Discard-Count(N)  to LTL-Count
             move Log-Total-Line    to Log-record
             write Log-Record
           end-perform

v00895     move 'API VGGBCNV called' to LTL-Description
v00895     move CAT-Num-API-VGFBCNV1 to LTL-Count
v00895     move Log-Total-Line       to Log-record
v00895     write Log-Record
v00895
v00895     move 'API VGGBCNV RC9610' to LTL-Description
v00895     move CAT-Num-API-VGFBCNV1-err to LTL-Count
v00895     move Log-Total-Line       to Log-record
v00895     write Log-Record

           move 'Fields in Error:' to Log-record
           write Log-Record
           move spaces to Log-Total-Line
           perform varying N from 1 by 1 until N > PreField-Cnt
             if Pret-Errors(N) > 0 then
               move N                  to LTL-DESC-Field
               move Pret-FieldName(n)  to LTL-DESC-FieldName
               move Pret-Errors(n)     to LTL-Count
               move Log-Total-Line     to Log-record
               write Log-Record
             end-if
           end-perform

           move all '_' to Log-record
           write log-record
           .
       Check-JCL-Parm.
           set TRACE-NOT-SET to true
           if JP-LENGTH = 5
             if JP-TRACE = 'TRACE'
               set TRACE-ON to true
             end-if
           end-if
           evaluate true
             when Trace-On      Display 'Trace is ON via JCL'
             when Trace-Not-Set Display 'Trace not set in JCL'
           end-evaluate
           .
       Initialization.
           set Program-Initializing to true
           if trace-on
             display 'TRACE:Program Initializing'
           end-if
      *    'initialize Range Table area
           move 16 to RangeTbl-Max
           move 64 to RangeVal-Max
           move 00 to RangeTbl-Ptr
           move 00 to RangeTbl-Cnt
           move 00 to Range-Ptr
           perform varying RngTblInx from 1 by 1
             until RngTblInx > RangeTbl-Max
             move zeroes to Range-Zero-Ptr(RngTblInx)
                            Range-CNT(RngTblInx)
             perform varying RngInx from 1 by 1
               until RngInx > RangeVal-Max
               move zeroes to Range-Low(RngTblInx, RngInx)
                              Range-High(RngTblInx, RngInx)
               move spaces to Range-Text(RngTblInx, RngInx)
             end-perform
           end-perform

v00864*    initialize the Discard area
v00864     move zeroes to Discard-Cnt
v00864     move 99999  to Discard-Max
v00864     perform varying Discard-Ptr from 1 by 1
v00864       until Discard-Ptr > Discard-max
v00864         move zeroes to DT-FIELD (Discard-Ptr)
v00864                        DT-Fldt  (Discard-Ptr)
v00864                        DT-START (Discard-Ptr)
v00864                        DT-REASON(Discard-Ptr)
v00864                        DT-Data2 (Discard-Ptr)
v00864                        DT-Data3 (Discard-Ptr)
v00864         move '0000' to DT-Data1 (Discard-Ptr)
v00864     end-perform
v00864     move zeroes to Discard-Ptr
v00864                    Discard-HexLen
v00864     move spaces to Discard-Hex
v00864     set Discard-Nothing to true

v0.2       call 'Load-Options' using
              Options-in-Effect, Counters-and-Totals, Range-Table-Area

           if RangeTbl-Cnt > 0
      *      'validate every range table has a zero-value entry
             perform varying RngTblInx from 1 by 1
                      until RngTblInx > RangeTbl-Cnt
               if Range-Cnt(RngTblInx) > 0
               and Range-Zero-Ptr(RngTblInx) = 0
                 set Range-Ptr to RngTblInx
                 display 'Engine:No zero-value for DAR Range of Values'
                  ':DARRange(Table(' Range-Ptr ')'
                 move 12 to return-code
                 stop run
               end-if
             end-perform
           end-if

      *    ' initalize the DateFMT-Table-Area
           move zeroes to DateFMT-Ptr
           move zeroes to DateFMT-Cnt
           move 99     to DateFMT-Max

           call 'SumDLL' using Options-in-Effect, File-Table-Area

           display
           '*----------- Memory Allocations --------------------------*'
           display
           '* Table Name              #Bytes  #Entries                *'
           perform Allocate-Table-Space
           perform Allocate-DAR-Space
           display
           '*---------------------------------------------------------*'
v00890     if OPT-UseAlt-Yes
v00890       perform Load-the-Alternate-Names
v00890     end-if


           move WS-DISCARD-REASON-VALUES TO WS-DISCARD-REASON-AREA

      *    'Intialize the Field-Table  entry
           move zeroes     to FldI-Next
                              FldI-Prior
           move low-values to FldI-Index-Table
           move 999999999  to FldI-EntryLength
           move zeroes     to FldI-Target-Start
           move zeroes     to FldI-Target-Length
           move 'U' to FldI-sign-sw
           move spaces to FldI-vField-Sw
           move zeroes to FldI-vNext
                          FldI-vPrior
           move zeroes to FldI-Table-Field
           move space to FldI-Valid-sw
           move zeroes to FldI-Functions-Resolved
           move zeroes to FldI-Parent
                          FldI-NextSib
                          FldI-PrevSib
                          FldI-FirstChild
                          FldI-LastChild
                          FldI-Root
                          FldI-LastGhost
                          FldI-Level
                          FldI-UsedNextInChain
           .
       Get-SysInfo.
MFMFMF     call 'HKSys' using System-Setup
      * get the file details of select files
           move 'INFILE' to In-DDName
           call 'HKSys' using Get-Dataset-Date, In-DDName,
                Infile-DSN, Infile-File-Date
           move 'DLL'    to In-DDName
           call 'HKSys' using Get-Dataset-Date, In-DDName,
                Cntrl-DSN, Cntrl-File-Date
           move 'SYSOUT' to In-DDName
           call 'HKSys' using Get-Dataset-Date, In-DDName,
                Sysout-DSN, Sysout-File-Date
           move 'DISCARD' to In-DDName
           call 'HKSys' using Get-Dataset-Date, In-DDName,
                Discard-DSN, Discard-File-Date
           .
      *--------------------------FDD-----------------------------------
      * Remarks. This routine creates the Formatted Data Display report
      * INPUTS:
      *  Command - 9(4) COMP, command to this routine.
      *            0 - Open FDD file
      *            1 - Start the FDD for a field
      *            2 - Write field to the FDD
      *            3 - Write Message to the FDD
      *            3 - End FDD provides totals etc
      *            9 - Close FDD file
      *  LnkData - DATA-FIELD structure
      *  LnkPret - PRE-FIELD-TABLE-AREA structure
      *  LnkFld  - FIELD-TABLE-AREA structure
      * OUTPUTS:
      *  FDD     - The Formatted Data Display Report.
      *These are the fields passed to the FDD routine
      *77  FldT         PIC 9(5) COMP.
      *copy LnkFile.
      *copy LnkRec.
      *copy LnkPret.
      *copy LnkFld.
      *copy LnkTbl.
      *copy LnkCond.
      *copy LnkFunc.
      *copy LnkEntry.
      *copy LnkCnt.
      *copy LnkGap.
      *copy LnkDisc.
      *copy LnkOpt.
      *copy LnkDLL.
      *    IN-BUFFER            PIC X(32756) VALUE LOW-VALUES.
      *    v-Buffer             PIC X(32756) VALUE LOW-VALUES.
      *
      * This routine is called
      *---------------------------------------------------------
      *Change-History.  (be sure to change VERSION)
      * Date------ Init Ver---  Description of Change-------------------
      * 2014/07/17 HK   v0.1.12 remove getbyte routine
      * 2014/04/14 HK   v0.1.11 show no group at eor counter
      * 2014/04/10 HK   v0.1.10 genDate, genTime, and genUser
      * 2014/03/26 HK   v0.1.9  added EntryLength Function
      *                         correct varname move
      * 2014/03/20 HK   v0.1.8  correct DataValidationNumeric option
      * 2014/03/19 HK   v0.1.7  correct BIS/BIN output format
      *                         support zero length offset/varchar
      * 2014/03/12 HK   v0.1.6  remove lnkData; get/format as needed
      * 2014/02/17 HK   v0.1.5  show vfield under hex column
      * 2013/12/09 HK   v0.1.4  Allow for fields with zero length
      * 2013/11/26 HK   v0.1.3  Improve Field in error message
      * 2013/11/07 HK   v0.1.2  move 'No migrate' if nomig specified
      * 2013/10/22 HK   v0.1.1  Support of new Data-Field-Area
      *                         Additional code for Func options
      *                         Resolve unresolved fields
      * 2013/10/18 HK   v0.1.0  vField Support
      * 2013/10/16 HK   v0.0.0  New FDD member
      *---------------------------------------------------------
      *PROCEDURE DIVISION using
      *    LS-command,
      *    FldT,
      *    File-Table-Area,
      *    Record-Table-Area,
      *    Pre-Field-Table-Area,
      *    Field-Table-Area,
      *    Table-Table-Area,
      *    Cond-Table-Area,
      *    Func-Table-Area,
      *    EntryLength-Table-Area,
      *    Counters-and-Totals,
      *    Gaps-in-Bits-Area,
      *    Discard-Table-Area,
      *    Options-in-Effect,
      *    DLL-Area,
      *    In-Buffer
      *    v-Buffer
       FDD.
           evaluate true
             when Open-FDD-File          Perform Open-the-FDD-File
             when Start-FDD              Perform Start-the-FDD
             when Write-FDD-Field        Perform Write-the-FDD-Field
             when Write-FDD-Message      Perform Write-the-FDD-message
             when End-FDD                Perform End-the-FDD
             when Drop-FDD               Perform Drop-the-Record
             when Close-FDD-File         Perform Close-the-FDD-File
           end-evaluate
           .
       Open-the-FDD-File.
           if FDD-not-Open
             set FDD-Open to true
             move fddVersion to FDD-Version
             Display 'FDD v' Version
             OPEN OUTPUT FDD-FILE
             if fddIO-status not = '00'
               display 'FDD:Open Error ' fddIO-status
               move 12 to return-code
               stop run
             end-if
             if FDD-Requested
               perform SETUP-FDD-FILE
               perform WRITE-FDD-HEADINGS
             end-if
           end-if
           .
       Close-the-FDD-File.
           if FDD-Open
             if FDD-Requested
               perform Write-FDD-Report-Final
             end-if
             close FDD-File
             set FDD-NOT-OPEN to true
           end-if
           .
       Setup-FDD-File.
           move spaces to FDD-Record
           move Engine-Version to DFH1-Engine-Ver
           move fddVersion to DFH1-Version
           move Program-Copyright to FDD-Record(2:)
           write FDD-RECORD
           perform Check-IO-Status-FDD
           ACCEPT WS-ACCEPT-DATE from DATE YYYYMMDD
             move WS-ACCEPT-CCYY to DFH1-DATE-CCYY
             move WS-ACCEPT-MM   to DFH1-DATE-MM
             move WS-ACCEPT-DD   to DFH1-DATE-DD
           ACCEPT WS-ACCEPT-TIME from TIME
             move WS-ACCEPT-HOUR   to DFH2-TIME-HOUR
             move WS-ACCEPT-MINUTE to DFH2-TIME-MINUTE
             move WS-ACCEPT-SECOND to DFH2-TIME-SECOND
           .
v00878 Check-IO-Status-FDD.
           evaluate FDDIO-STATUS
               when 00 continue
               when 34
                    display 'FDD:Out of space. File Status:'
                     FDDIO-STATUS
                     ':rec#=' ws-rec-cnt
                    move 12 to return-code
                    stop run
               when other
                  display 'FDD:Write Error. File Status:'
                    FDDIO-STATUS
                     ':rec#=' ws-rec-cnt
                    move 12 to return-code
                    stop run
           end-evaluate
           .
       WRITE-FDD-HEADINGS.
           write FDD-RECORD from FDD-FILE-HEADING1
           perform Check-IO-Status-FDD
           move FT-FILENAME(1)   to DFH2-FILENAME
           write FDD-RECORD from FDD-FILE-HEADING2
           perform Check-IO-Status-FDD
           move SPACES to FDD-RECORD
           move DLL-Version to FFH3-Version
           move DLL-GenDate to FFH3-GenDate
           move DLL-GenTime to FFH3-GenTime
           move DLL-GenUser to FFH3-GenUser
           write FDD-RECORD from FDD-File-Heading3
           perform Check-IO-Status-FDD
           move spaces to FDD-Record
           move Opt-FDD-Start to FFH4-Start
           move Opt-FDD-Stop  to FFH4-Stop
           write FDD-Record from FDD-File-Heading4
           perform Check-IO-Status-FDD
           move spaces to FDD-Record
           write FDD-RECORD
           perform Check-IO-Status-FDD
           .
       WRITE-FDD-COLUMNS.
           move spaces to FDD-PRINT-LINE
           if OPT-FDD-Show-Bits-Yes
             move 'BITADDR' to DCH-Start
           else
             move 'START.B' to DCH-Start
           end-if
           move DATA-COLUMN-HEADING to DPLT-DATA
           write FDD-RECORD from FDD-PRINT-LINE
           perform Check-IO-Status-FDD
           .
      *------------------------------------------------------------
      * Move File totals to FDD report
      *------------------------------------------------------------
       Write-FDD-Report-Final.
           move spaces to FDD-Record
           write FDD-Record
           perform Check-IO-Status-FDD
           move all '-' to FDD-Record
           write FDD-Record
           perform Check-IO-Status-FDD
           move 'File Totals:' to DPLT-Data
           write FDD-Record from FDD-Print-Line
           perform Check-IO-Status-FDD

           move 'OPTIONS-IN'       to FTL-Description
           move OPT-Count          to FTL-Count
           move FDD-Total-Line     to DPLT-DATA
           write FDD-Record from FDD-Print-Line
           perform Check-IO-Status-FDD

           move 'CONTROL-IN'       to FTL-Description
           move CAT-CONTROL-IN     to FTL-Count
           move FDD-Total-Line     to DPLT-DATA
           write FDD-Record from FDD-Print-Line
           perform Check-IO-Status-FDD

           move 'CONTROL-FILE'     to FTL-Description
           move CAT-CONTROL-FILE   to FTL-Count
           move FDD-Total-Line     to DPLT-DATA
           write FDD-Record from FDD-Print-Line
           perform Check-IO-Status-FDD

           move 'CONTROL-RECORD'   to FTL-Description
           move CAT-CONTROL-RECORD to FTL-Count
           move FDD-Total-Line     to DPLT-DATA
           write FDD-Record from FDD-Print-Line
           perform Check-IO-Status-FDD

           move 'CONTROL-TABLE'    to FTL-Description
           move CAT-CONTROL-TABLE  to FTL-Count
           move FDD-Total-Line     to DPLT-DATA
           write FDD-Record from FDD-Print-Line
           perform Check-IO-Status-FDD

           move 'CONTROL-FIELD'    to FTL-Description
           move CAT-CONTROL-FIELD  to FTL-Count
           move FDD-Total-Line     to DPLT-DATA
           write FDD-Record from FDD-Print-Line
           perform Check-IO-Status-FDD

           move 'INPUT RECORDS'    to FTL-Description
           move WS-REC-CNT         to FTL-Count
           move FDD-Total-Line     to DPLT-DATA
           write FDD-Record from FDD-Print-Line
           perform Check-IO-Status-FDD

           move 'Gap in Bits in DLL'      to FTL-Description
           move GIB-Count          to FTL-Count
           move FDD-Total-Line     to DPLT-DATA
           write FDD-Record from FDD-Print-Line
           perform Check-IO-Status-FDD
           if GIB-Count > zeroes
             move '  First Field in Gap'    to FTL-Description
             move GIB-First-Field           to FTL-Count
             move FDD-Total-Line     to DPLT-DATA
             write FDD-Record from FDD-Print-Line
             perform Check-IO-Status-FDD
           end-if

           move 'Offset lengths of zero' to FTL-Description
           move CAT-OFFSET-LEN-ZERO to FTL-Count
           move FDD-Total-Line      to DPLT-DATA
           write FDD-Record from FDD-Print-Line
           perform Check-IO-Status-FDD

           move 'DISCARDS'         to FTL-Description
           move CAT-DISCARDS       to FTL-Count
           move FDD-Total-Line     to DPLT-DATA
           write FDD-Record from FDD-Print-Line
           perform Check-IO-Status-FDD

           move 'GOOD RECORDS'     to FTL-Description
           move CAT-GOOD-RECORDS   to FTL-Count
           move FDD-Total-Line     to DPLT-DATA
           write FDD-Record from FDD-Print-Line
           perform Check-IO-Status-FDD

V00879     move 'DIVIDED RECORDS'   to FTL-Description
V00879     move CAT-num-API-divided to FTL-Count
V00879     move FDD-Total-Line      to DPLT-DATA
V00879     write FDD-Record from FDD-Print-Line
V00879     perform Check-IO-Status-FDD

           move 'No Group @ EOR'   to FTL-Description
           move CAT-GROUP-EOR      to FTL-Count
           move FDD-Total-Line     to DPLT-DATA
           write FDD-Record from FDD-Print-Line
           perform Check-IO-Status-FDD

           move 'FDD FIELDS'       to FTL-Description
           move CAT-FDD-Fields     to FTL-Count
           move FDD-Total-Line     to DPLT-DATA
           write FDD-Record from FDD-Print-Line
           perform Check-IO-Status-FDD

           move 'DAR FIELDS'       to FTL-Description
           move CAT-DAR-Fields     to FTL-Count
           move FDD-Total-Line     to DPLT-DATA
           write FDD-Record from FDD-Print-Line
           perform Check-IO-Status-FDD

           move 'MIG FIELDS'       to FTL-Description
           move CAT-MIG-Fields     to FTL-Count
           move FDD-Total-Line     to DPLT-DATA
           write FDD-Record from FDD-Print-Line
           perform Check-IO-Status-FDD

           perform varying N from 1 by 1 until N > Discard-Reason-Max
             move Discard-Reason(N) to FTL-Description
             move Discard-Count(N)  to FTL-Count
             move FDD-Total-Line    to DPLT-DATA
             write FDD-Record from FDD-Print-Line
             perform Check-IO-Status-FDD
           end-perform
           move 'Fields in Error:' to DPLT-Data
           write FDD-Record from FDD-Print-Line
           perform Check-IO-Status-FDD
           move spaces to FDD-Total-Line
           perform varying N from 1 by 1 until N > PreField-Cnt
             if Pret-Errors(N) > 0 then
               move N                  to FTL-DESC-Field
               move Pret-FieldName(n)  to FTL-DESC-FieldName
               move Pret-Errors(n)     to FTL-Count
               move FDD-Total-Line     to DPLT-DATA
               write FDD-Record from FDD-Print-Line
               perform Check-IO-Status-FDD
             end-if
           end-perform
           .
       Start-the-FDD.
           move WS-REC-CNT to DRL-RECORD
           move IN-LEN     to DRL-LENGTH
           move rt-Field(Rec-Ptr) to Pret
           move PRET-FIELDNAME(Pret)   to DRL-RECORDNAME
           move RT-COND(Rec-Ptr) to COND-PTR
           move SPACES     to DRLC-COND
           if RT-COND(Rec-Ptr) = 0
             move 'cond()' to DRLC-COND
           else
      *    note. we will only print out max of 64 bytes.
             move spaces to ws-large-area
             string 'cond('                    delimited by size
                Cond-Statement(Cond-ptr)
                (1:Cond-Statement-Len(Cond-Ptr))  delimited by size
                ')'                            delimited by size
                into ws-large-area
             end-string
             move ws-large-area to DRLC-COND
           end-if
           write FDD-RECORD from FDD-RECORD-LINE
           perform Check-IO-Status-FDD

           perform WRITE-FDD-COLUMNS
           move spaces to Data-line
           .
      *----------------------------------------------------------------
      * Write the Record that is being dropped. For now this is
      * because the COND statement didn't match anything.
      *----------------------------------------------------------------
       Drop-the-Record.
           if DT-Reason(Discard-Ptr) = 12
             move WS-REC-CNT  to DRL-RECORD
             move IN-LEN      to DRL-LENGTH
             move DT-REASON (DISCARD-PTR) to N
             move discard-reason(N) to DRL-RECORDNAME
             move '*Dropped*' to DRLC-Cond
             write FDD-RECORD from FDD-RECORD-LINE
             perform Check-IO-Status-FDD
           end-if

           move spaces to Data-line
           .

      *----------------------------------------------------------------
      * Write the FDD end of report details for a record.
      *----------------------------------------------------------------
       End-the-FDD.
           if FIELDS-IN-ERROR > 0
              if DLL-Short-Error or DLL-Long-Error
                or Data-not-Processed-Error
                move spaces to FDD-PRINT-LINE
                move discard-reason(DLL-Error-SW) to Data-Line
                move DATA-LINE to DPLT-DATA
                write FDD-RECORD from FDD-PRINT-LINE
                perform Check-IO-Status-FDD
              else
                if Table-DLL-Long-Error
                  move discard-reason(10) to Data-Line
                  move DATA-LINE to DPLT-DATA
                  write FDD-RECORD from FDD-PRINT-LINE
                  perform Check-IO-Status-FDD
                end-if
                move spaces to FDD-PRINT-LINE
                move spaces to Data-Line
v00886          if First-Fields-Error = 0 or > Discard-Reason-Max
                  display 'FDD:first-fields-error error'
                    ':' first-fields-error
                  move 12 to return-code
                  stop run
                end-if
                if First-Field-In-Error = 0
                  display 'FDD:first-field-In-error error'
                    ':' first-fields-error
                  move 12 to return-code
                  stop run
                end-if
                if First-Fields-Error = 11 or 12 or 15 or 16
                  string 'Record Error:'      delimited by size
                       DISCARD-REASON(First-Fields-Error)
                                              delimited by size
                    into Data-Line
                  end-string
                else
                  string 'Field Error:'       delimited by size
                       DISCARD-REASON(First-Fields-Error)
                      (1:discard-Length(First-Fields-Error))
                                            delimited by size
                       '. Near field:'      delimited by size
                       First-Field-in-Error delimited by size
                       ':'                  delimited by size
                       Pret-FieldName(First-Field-in-error)
                                            delimited by size
                    into Data-Line
                  end-string
                end-if
                move DATA-LINE to DPLT-DATA
                write FDD-RECORD from FDD-PRINT-LINE
                perform Check-IO-Status-FDD
                move spaces to FDD-PRINT-LINE
              end-if
           end-if
           if Offset-Field-is-zero > 0 then
              move spaces to FDD-PRINT-LINE
              move spaces to Data-Line
              string ' Warning. OFFSET value is zero for field '
                                          delimited by size
                    Offset-Field-is-zero delimited by size
              into Data-Line
              move DATA-LINE to DPLT-DATA
              write FDD-RECORD from FDD-PRINT-LINE
              perform Check-IO-Status-FDD
           end-if
           .
       Write-the-FDD-message.
           write FDD-Record from ws-message(1:59)
           perform Check-IO-Status-FDD
           .
       Write-the-FDD-Field.
           if Fldt = zeroes
             display 'FDD:Error Fldt zeroes!'
             move 12 to return-code
             stop run
           end-if
           move Fldt-Field(Fldt)         to PreT
           if FDD-Event-Requested (PreT)
             evaluate true
               when Normal-Field(Pret)
               when Occur-Field(Pret)
               when ODO-Field(Pret)
               when NORMAL-vField(Pret)
               when Occur-vField(Pret)
               when ODO-vField(Pret)
                                        perform FDD-Write-Regular
               when TABLE-FIELD(PreT)   perform FDD-Write-Table
             end-evaluate
           end-if
           move ZEROES to RETURN-CODE
           .

       FDD-Write-Regular.
           move Fldt-Field(Fldt)         to DL-FIELD

      * Field Name with subcript if any
           if NORMAL-vField(Pret)
           or Occur-vField(Pret)
           or ODO-vField(Pret)
             move 'v' to DL-vField
           else
             move space to DL-vField
           end-if
           if PRET-DIMS (PreT) = 0 THEN
             move PRET-FIELDNAME(PreT) to DL-FieldName
           else
             call 'FmtInd' using
                 FLDT-INDEX-TABLE(Fldt), Pret-Dims(Pret), Indice
             move SPACES to DL-FieldName
             string PRET-FIELDNAME(Pret)   delimited by SPACES
                    Indice                 delimited by SPACES
               into DL-FieldName
             end-string
           end-if

      * Start Position
           if OPT-FDD-Show-Bits-Yes
             move Fldt-Start(Fldt)     to ws-Pos7
             move ws-pos7              to DL-Start-Bits
           else
             move Fldt-Start-Byte(Fldt) to ws-pos
             move ws-pos              to DL-START
             move Fldt-Start-Bit(Fldt) to ws-bit
             move ws-Bit               to DL-BIT
           end-if

      * Length in bits
           move Fldt-Length(Fldt) to ws-Len

      * Length for display
           if FldT-Length(FldT) = zeroes
             move 'EMPTY' to DL-LENGTHX
             move zeroes  to ws-Bytes
           else
             evaluate true
               when Type-BIT (Pret)
                 compute ws-Bytes = ws-Len
               when Type-Nib (Pret)
               when Type-PD-NEC4(Pret)
                 compute ws-Bytes = ws-Len / 4
               when other
                 compute ws-Bytes = ws-Len / 8
             end-evaluate
             move ws-Bytes to DL-Length
           end-if

      * Value from the buffer/vBuffer and format it
           if FldT-Length(FldT) > 0
             evaluate true
               when NORMAL-vField(Pret)
               when Occur-vField(Pret)
               when ODO-vField(Pret)
                 move 1 to gbBuffer
                 move Fldt-Start-Byte(Fldt) to gbPos
                 move Fldt-Start-Nib(Fldt)  to gbPosNib
                 move Fldt-Start-Bit(Fldt)  to gbPosBit
                 move Fldt-Length-Byte(Fldt) to gbLen
                 move Fldt-Length-Nib(Fldt)  to gbLenNib
                 move Fldt-Length(Fldt)      to gbLenBit
                 move Pret-Type(Pret)        to gbType
v0854            move Pret to gbField
                 perform GetBuff
                 move ws-value to fdd-value
               when other
                 if Field-Valid(FldT)
                   move 0 to gbBuffer
                   move Fldt-Start-Byte(Fldt) to gbPos
                   move Fldt-Start-Nib(Fldt)  to gbPosNib
                   move Fldt-Start-Bit(Fldt)  to gbPosBit
                   move Fldt-Length-Byte(Fldt) to gbLen
                   move Fldt-Length-Nib(Fldt)  to gbLenNib
                   move Fldt-Length(Fldt)      to gbLenBit
                   move Pret-Type(Pret)        to gbType
v0854              move Pret to gbField
                   perform GetBuff
                   move ws-value to fdd-value
                 else
                   set Discard-NOT-NUMERIC-FIELD to true
                 end-if
             end-evaluate
             IF Discard-Something
               move '*Invalid*' to DL-Value
               set Discard-Nothing to true
             else
               perform Format-DL-Value
             end-if
           else
             move spaces to DL-Value
           end-if

           if Opt-Not-ShowNonPrintables
           and Type-Ch(Pret)
           and ws-Bytes > zeroes
             perform Replace-Non-Printable
           end-if

      * Hex representation
           if OPT-ShowVFieldFormula
           and Regular-vField(Pret)
             perform Format-vField
           else
             if DL-LENGTHX = 'EMPTY'
             or ws-Len = zeroes
               move spaces to DL-Hex
             else
               perform Format-DL-Hex
             end-if
           end-if

      * Type of field
           move Pret-Type(Pret) to N
           move TT-TYPE(N)             to DL-TYPE
           if Type-ZD(Pret) or Type-PD(Pret)
             evaluate fdd-Value(20:1)
               when 'S' move 's' to DL-Type(3:1)
               when 'U' move 'u' to DL-Type(3:1)
             end-evaluate
           end-if

           move '<'                    to DL-LIT1
           if OPT-FDD-Show-Bits-No
             move '.'                    to DL-LIT2
           end-if
           move ':'                    to DL-LIT3
           move '>'                    to DL-LIT4

      * Target Start Position and Length
           if MIG-Event-Requested(Pret)
             move FLDT-TARGET-Start(Fldt)    TO DL-MIG-POS
             move '.'                        to DL-LIT5
             move FLDT-TARGET-Length(Fldt) TO DL-MIG-LEN
           else
             move 'No Migrate'           to DL-Migrate-area
           end-if
      * Ref() if specified
           move Pret-Ref(Pret) to DL-Ref

      * write the detail line
           move spaces to FDD-PRINT-LINE
           move DATA-LINE to DPLT-DATA
           write FDD-RECORD from FDD-PRINT-LINE
           perform Check-IO-Status-FDD
           .

      *Take the fdd-Value and present it in a pretty format, if needed
       Format-DL-Value.
           evaluate true
             when Type-Ch (Pret)
               move ws-Bytes to wLen
               if wLen > length of fdd-Value
                 move Length of fdd-Value to wLen
               end-if
               evaluate fdd-Value(1:wLen)
                 when spaces      move '*spaces*'       to DL-Value
                 when low-values  move '*low-values*'   to DL-Value
                 when high-values move '*high-values*'  to DL-Value
                 when other       move fdd-Value(1:wLen) to DL-Value
               end-evaluate
             when Type-Bit (Pret)
               move fdd-Value to DL-Value
             when TYPE-NIB    (PreT)
             when TYPE-PD-NEC4(PreT)
               compute ws-pos = (18 - ws-Bytes) + 1
               move fdd-Value(ws-pos:ws-bytes) to DL-Value
             when Type-ZD(Pret)
               compute ws-pos = (18 - ws-Bytes) + 1
               evaluate fdd-Value(20:1)
                 when 'S'
                   move fdd-Value-NUM18S to WS-EDIT-NUM18S
                   move WS-EDIT-NUM18S(ws-pos:ws-Bytes + 1) to DL-Value
                 when 'U'
                   move fdd-Value-NUM18(ws-pos:ws-Bytes) to DL-Value
               end-evaluate
             when Type-PD(Pret)
               compute wLen = (ws-Bytes * 2) - 1
               compute ws-pos = (18 - wLen) + 1
               evaluate fdd-Value(20:1)
                 when 'S'
                   move fdd-Value-NUM18S to WS-EDIT-NUM18S
                   move WS-EDIT-NUM18S(ws-pos:wLen + 1) to DL-Value
                 when 'U'
                   move fdd-Value-NUM18(ws-pos:wLen)    to DL-Value
               end-evaluate
             when Type-PD-NEC(Pret)
               compute wLen = ws-Bytes * 2
               compute ws-pos = (18 - wLen) + 1
               evaluate fdd-Value(20:1)
                 when 'S'
                   move fdd-Value-NUM18S to WS-EDIT-NUM18S
                   move WS-EDIT-NUM18S(ws-pos:wLen + 1) to DL-Value
                 when 'U'
                   move fdd-Value-NUM18(ws-pos:wLen)    to DL-Value
               end-evaluate
             when Type-BIS(Pret)
             when Type-BIN(Pret)
               evaluate ws-Bytes
                 when 1 THRU 2 move 5  to WLEN
                 when 3 THRU 4 move 10 to WLEN
                 when 5 THRU 8 move 18 to WLEN
               end-evaluate
               compute ws-pos = (18 - WLEN) + 1
               evaluate fdd-Value(20:1)
                 when 'S'
                   move fdd-Value-NUM18S to WS-EDIT-NUM18S
                   move WS-EDIT-NUM18S(ws-pos:wLen + 1) to DL-Value
                 when 'U'
                   move fdd-Value-Num18(ws-pos:wLen)    to DL-Value
               end-evaluate
           end-evaluate
           .
       Format-DL-Hex.
           if TYPE-BIT (PreT)
             move spaces to DL-Hex
             string "B'"                 delimited by size
                    DL-Value             delimited by SPACE
                    "'"                  delimited by size
               into DL-HEX
           else
             if ws-Len > 0
                perform Get-Hex
             else
               move spaces to DL-HEX
             end-if
           end-if
           .
      *This will create a hex representation of the data.
      *This will also present if it is nibble format
      *Input:IN-BUFFER(WS-POS:WS-LEN)
      *Output: WS-HEX; The hex representation of the data
       Get-Hex.
           move SPACES to WS-HEX
           move FLDT-Start-Byte(Fldt) to ws-pos
           move FLDT-Start-Nib(Fldt)  to wnib
           move FLDT-Start-Bit(Fldt)  to wbit
           move FLDT-Length-Byte(Fldt) to ws-len
           move ws-len to nLen
           if TYPE-PD-NEC4 (PreT) or TYPE-NIB(PreT)
             compute nLen = ws-len + 1
             compute nNib = fldt-length(FLDT) / 4
             evaluate true
              when Normal-vField(Pret)
              when Occur-vField(Pret)
              when ODO-vField(Pret)
               call 'HEXSHOW' using  v-BUFFER(ws-Pos:nLen), nLen, WS-HEX
              when other
               call 'HEXSHOW' using IN-BUFFER(ws-Pos:nLen), nLen, WS-HEX
             end-evaluate
             move WS-HEX(wNib:nNib) TO DL-Hex
           else
             if NLEN > 20 THEN
               move 20 to WS-LEN, NLEN
             end-if
             evaluate true
              when Normal-vField(Pret)
              when Occur-vField(Pret)
              when ODO-vField(Pret)
               call 'HEXSHOW' using  v-BUFFER(WS-POS:nLen), nLen, WS-HEX
              when other
               call 'HEXSHOW' using IN-BUFFER(WS-POS:nLen), nLen, WS-HEX
             end-evaluate
             move WS-HEX to DL-HEX
           end-if
           .

v00890 copy vFieldp.

      * Replace NON-printables with a period symbol
       Replace-Non-Printable.
           move ws-Bytes to wLen
           if ws-Bytes > length of DL-Value
             move length of DL-Value to wLen
           end-if
           perform varying ws-pos from 1 by 1 until ws-pos > wLen
             if DL-VALUE(ws-pos:1) < Opt-Lowest-Byte
             or DL-VALUE(ws-pos:1) > Opt-Highest-Byte
               move '.' to DL-VALUE(ws-pos:1)
             end-if
           end-perform
           .
       FDD-Write-Table.
           move PreT   to FLDCNT-SAVE
           move PreT                   to DLT-FIELD
           move PRET-TABLE(PreT)   to TblPtr
      *  format table name with index
           if Pret-Hash(Pret) > 0
             compute fdd-dims = Pret-Dims(Pret)
             call 'FmtInd' using
               FLDT-INDEX-TABLE(Fldt), fdd-dims, Indice
             move spaces to DLT-Name
             string PRET-FIELDNAME(Pret)        delimited by SPACES
                    Indice                      delimited by SPACES
               into DLT-Name
             end-string
           else
             move PRET-FIELDNAME(Pret) to DLT-Name
           end-if
           if Table-is-Group(TblPtr)
             move 'Group' to DLT-Group
           else
             move spaces  to DLT-Group
           end-if
           if Table-Type-ODO(TblPtr)
             move 'odo'   to DLT-ODO
           else
             move spaces  to DLT-ODO
           end-if
           move spaces to FDD-PRINT-LINE
           move DATA-LINE-TABLE        to DPLT-DATA
           write FDD-RECORD from FDD-PRINT-LINE
           perform Check-IO-Status-FDD

           move FLDT-Start-Byte(Fldt) to ws-pos
           move FLDT-Start-Nib(Fldt)  to ws-nib
           move FLDT-Start-Bit(Fldt)  to ws-bit
           move ws-pos              to DLT-Start
           move ws-bit              to DLT-Start-Bit
           compute DLT-Length = FLDT-Length(Fldt) / 8
           move spaces to FDD-PRINT-LINE
           move Data-Line-Table-Position to DPLT-DATA
           write FDD-RECORD from FDD-PRINT-LINE
           perform Check-IO-Status-FDD


           move spaces        to DLT-Depend
           if Table-Cond(TblPtr) > 0
             move spaces          to DLT-Depend
             move Table-Cond(TblPtr) to Cond-Ptr
             move 'cond(' to DLT-Cond
             move Cond-Statement(Cond-Ptr) to DLT-Cond(6:)
             move Cond-Statement-Len(Cond-ptr) to Cond-Len
             if Cond-Len + 6 <= length of DLT-Cond
               move ')' to DLT-Cond(Cond-Len + 6:1)
             end-if
             move spaces to FDD-PRINT-LINE
             move DATA-LINE-TABLE-2        to DPLT-DATA
             write FDD-RECORD from FDD-PRINT-LINE
             perform Check-IO-Status-FDD
           end-if
           if Table-EL-Ptr(TblPtr) > 0
             move Table-EL-Ptr(TblPtr) to EL-Ptr
             evaluate true
               when EL-Use-Field(EL-Ptr)
                 move spaces      to DLT-Depend
                 move 1 to ws-pos
                 move 'EntryLength(' to DLT-Depend(ws-pos:)
                 add 12 to ws-pos
                 move '#' to DLT-Depend(ws-pos:1)
                 add 1 to ws-pos
                 move EL-Field(EL-Ptr) to fdd-Num5
                 move fdd-Num5 to DLT-Depend(ws-pos:5)
                 add 5 to ws-pos
                 move ')=' to DLT-Depend(ws-pos:2)
                 add 2 to ws-pos
                 compute fdd-Num5 = FLDT-EntryLength(Fldt) / 8
                 move fdd-Num5 to DLT-Depend(ws-pos:5)
                 add 5 to ws-pos
               when EL-Use-Value(EL-Ptr)
                 move spaces      to DLT-Depend
                 move 1 to ws-pos
                 move 'EntryLength(=' to DLT-Depend(ws-pos:)
                 add 13 to ws-pos
                 move EL-Value(EL-Ptr) to fdd-Num5
                 move fdd-Num5 to DLT-Depend(ws-pos:5)
                 add 5 to ws-pos
                 move ')' to DLT-Depend(ws-pos:2)
                 add 1 to ws-pos
             end-evaluate
             move spaces to FDD-PRINT-LINE
             move DATA-LINE-TABLE-2        to DPLT-DATA
             write FDD-RECORD from FDD-PRINT-LINE
             perform Check-IO-Status-FDD
           end-if
           if TABLE-NO-DEPEND-LIMIT(TblPtr)
             move 'limited by RDW' to DLT-Depend
           end-if
           if TABLE-DEPEND-SET(TblPtr)
             move spaces          to DLT-Depend
             move TABLE-DEPEND-FIELD(TblPtr) to fdd-Num5
             string 'Depend(#'             delimited by size
                    fdd-Num5               delimited by size
                    ')'                    delimited by size
               into DLT-Depend
             move spaces to FDD-PRINT-LINE
             move DATA-LINE-TABLE-2             to DPLT-DATA
             write FDD-RECORD from FDD-PRINT-LINE
             perform Check-IO-Status-FDD
           end-if
           if TABLE-LIMIT-SET(TblPtr)
             move spaces          to DLT-Depend
             move 'limit('        to DLT-Depend
             move 7               to ws-pos
             if Table-Limit-Base-Field(TblPtr) > 0
               move Open-Bracket to DLT-Depend(ws-pos:1)
               add 1 to ws-pos
               move '#'       to DLT-Depend(ws-pos:1)
               add 1 to ws-pos
               move Table-Limit-Base-Field(TblPtr) to fdd-Num5
               move fdd-Num5   to DLT-Depend(ws-pos:5)
               add 5 to ws-pos
               move Close-Bracket to DLT-Depend(ws-pos:1)
               add 1 to ws-pos
             end-if
             if TABLE-LIMIT-FIELD1-IS-POINTER(TblPtr)
               move '@'        to DLT-Depend(ws-pos:1)
             else
               move '#'        to DLT-Depend(ws-pos:1)
             end-if
             add 1 to ws-pos
             move TABLE-LIMIT-FIELD1(TblPtr) to fdd-Num5
             move fdd-Num5                     to DLT-Depend(ws-pos:5)
             add 5 to ws-pos
             if not TABLE-LIMIT-NO-COMPUTE(TblPtr)
              move TABLE-LIMIT-COMPUTE(TblPtr) to DLT-Depend(ws-pos:1)
              add 1 to ws-pos
              if TABLE-LIMIT-FIELD2-IS-POINTER(TblPtr)
               move '@' to DLT-Depend(ws-pos:1)
              else
               move '#' to DLT-Depend(ws-pos:1)
              end-if
              add 1 to ws-pos
              move TABLE-LIMIT-FIELD2(TblPtr) to fdd-Num5
              move fdd-Num5                     to DLT-Depend(ws-pos:5)
              add 5 to ws-pos
             end-if
             move ')' to DLT-Depend(ws-pos:1)
             add 1 to ws-pos
             move spaces to FDD-PRINT-LINE
             move DATA-LINE-TABLE-2           to DPLT-DATA
             write FDD-RECORD from FDD-PRINT-LINE
             perform Check-IO-Status-FDD
           end-if
           if TABLE-LENGTH-SET(TblPtr)
             move spaces          to DLT-Depend
             move 1 to ws-pos
             move 'length(#' to DLT-Depend(ws-pos:8)
             add 8 to ws-pos
             move Table-Limit-Field1(TblPtr) to fdd-num6
             move fdd-Num6     to DLT-Depend(ws-pos:6)
             add 6 to ws-pos
             move ')'                to DLT-Depend(ws-pos:1)
             add 1 to ws-pos
             move spaces to FDD-PRINT-LINE
             move DATA-LINE-TABLE-2           to DPLT-DATA
             write FDD-RECORD from FDD-PRINT-LINE
             perform Check-IO-Status-FDD
           end-if
           move FLDCNT-SAVE to PreT
           .
      *END PROGRAM FDD.

      *------------------------------------------------------------
      * Populate the v-buffer
      * Even if there is no data ready (aka Unresolved) this routine
      *  will still initialize the field.
      * Return-code:
      *  00 - no vfield resolved
      *  01 - vfield resolved
      * Variables Passed:
      *    vCursor         pic 9(9) comp.
      *    Pret           pic 9(5) comp.
      *    FPtr           pic 9(5) comp.
      *    Resolved       pic 9(4) comp.
      *    v-Buffer       PIC X(32756).
      *    in-Buffer      PIC X(32756).
      *     LnkPret.
      *     LnkFld.
      *     LnkFunc.
      *     LnkTbl.
      *     LnkRec.
      *     LnkCnt.
      *     LnkDisc.
      *------------------------------------------------------------
       UpdateV.
           evaluate true
             when Program-Ending
               display 'UpdateV:------------- UpdateV stats ----------*'
v00878         display 'UpdateV:# of Fields attempt to resolve.:'
v00878                                           uvFieldsResolved-cnt
v00878         display 'UpdateV:# of Fields Resolved...........:'
v00878                                           uvAttemptResolve-cnt
v00878         display 'UpdateV:Field Function value is invalid:'
v00878                                           uvFieldValueInvalid-cnt
               display 'UpdateV:--------------------------------------*'
             when Other
               perform UpdateV-Pull-or-Migrate
           end-evaluate
           .
       UpdateV-Pull-or-Migrate.
           set uvSave-FldInx to FldInx
           set uvSave-vFldInx to vFldInx
           set uvSave-PretInx to PretInx
           move uvPret to uvUserPret
           move uvCursor to uvvCur
           set PretInx to uvPret
           set FunStart to Pret-func(PretInx)
           compute uvPtr-Stop =
              Pret-func(uvPret) + Func-Total(FunStart) - 1
           perform varying FunInx from Pret-Func(PretInx) by 1
             until FunInx > uvPtr-Stop
               set uvisNotResolved to true
               perform Determine-UsedPret
               if Program-Pulling-Data-Field
                 evaluate true
                  when Func-is-Internal(FunInx)
                   perform Attempt-to-Resolve-Function
                  when uvUsedPret < uvUserPret
                   perform Attempt-to-Resolve-Function
                  when uvUsedPret > uvUserPret
                   move Pret-LastField(uvUsedPret)
                     to FLDT-UsedNextInChain(uvFPtr)
                 end-evaluate
               end-if
               if Program-Migrating
                 perform Attempt-to-Resolve-Function
               end-if
               if uvisResolved
                 add 1 to uvFieldsResolved-cnt
                 add 1 to Fldt-Functions-Resolved(uvFptr)
               end-if
           end-perform
      *    ' if all functions resolved for this field, adjust the chain
           if Fldt-Functions-Resolved(uvFptr) = Func-Total(FunStart)
             move 01 to uvResolved
             set vFldInx to uvFPtr
             Subtract 1 from vField-Unresolved
             add 1 to vField-Resolved
      *      ' remove the Unresolved from the chain
             set NextFld  to FLDT-vNext(vFldInx)
             set PriorFld to FLDT-vPrior(vFldInx)
             evaluate true
               when FLDT-vPrior(vFldInx) = 0
                and FLDT-vNext(vFldInx)  = 0
      *          'only 1 left in chain so remove it
                 move zeroes to vField-Start, vField-Last
               when FLDT-vPrior(vFldInx) = 0
      *          'adjust first in chain
                 move FLDT-vNext(vFldInx) to vField-Start
                 move zeroes              to FLDT-vPrior(NextFld)
               when FLDT-vNext(vFldInx) = 0
      *          'adjust last in chain
                 set vField-Last to PriorFld
                 move zeroes to FLDT-vNext(PriorFld)
                                FLDT-vPrior(vFldInx)
               when other
                 move FLDT-vNext(vFldInx)  to FLDT-vNext(PriorFld)
                 move FLDT-vPrior(vFldInx) to FLDT-vPrior(NextFld)
             end-evaluate
             move zeroes to FLDT-vNext(vFldInx)
                            FLDT-vPrior(vFldInx)
           else
             move 00 to uvResolved
           end-if

           set FldInx to uvSave-FldInx
           set vFldInx to uvSave-vFldInx
           set PretInx to uvSave-PretInx
           .
       Attempt-to-Resolve-Function.
           add 1 to uvAttemptResolve-cnt
           move FT-Start(FunInx) to uvvPos
           move FT-Length(FunInx) to uvvLen
           evaluate true
             when Func-String(FunInx)    perform UpdateV-String
             when Func-Hex(FunInx)       perform UpdateV-Hex
             when Func-RDW(FunInx)       perform UpdateV-RDW
             when Func-Space(FunInx)     perform UpdateV-Space
             when Func-Count(FunInx)     perform UpdateV-Count
             when Func-Size(FunInx)      perform UpdateV-Size
             when Func-Start(FunInx)     perform UpdateV-Start
             when Func-End(FunInx)       perform UpdateV-End
             when Func-Field(FunInx)     perform UpdateV-Field
             when Func-TimeStamp(FunInx) perform UpdateV-TimeStamp
             when Func-Entry(FunInx)     perform UpdateV-Entry
HK1229       when Func-Build(FunInx)     perform UpdateV-Build
v00872       when Func-Copy(FunInx)      perform UpdateV-Copy
           end-evaluate
           compute uvvCur = uvvCur + uvvLen
           .
       Determine-UsedPret.
           evaluate true
             when Func-is-Internal(FunInx)
                  move zeroes   to uvUsedPret
             when Func-Count(FunInx)
                  move FT-Count-Field(FunInx) to uvUsedPret
             when Func-Size(FunInx)
                  move FT-Size-Field(FunInx)  to uvUsedPret
             when Func-Start(FunInx)
                  move FT-Start-Field(FunInx) to uvUsedPret
             when Func-End(FunInx)
                  move FT-End-Field(FunInx)   to uvUsedPret
             when Func-Field(FunInx)
                  move FT-Field(FunInx)       to uvUsedPret
             when Func-Entry(FunInx)
                  move FT-Entry-Field(FunInx) to uvUsedPret
             when Func-RDW(FunInx)
                  move zeroes   to uvUsedPret
HK1229       when Func-Build(FunInx)
HK1229            move zeroes   to uvUsedPret
             when Other
               set sub to FunInx
               display 'Engine:updatev:internal error:Unknown Function'
                ':funinx=' sub
                ':Pret=' uvUserPret
                ':rec#=' ws-rec-cnt
               move 12 to return-code
               stop run
           end-evaluate
           .
      *----------------------------------------------------------------
      * STRING
      *----------------------------------------------------------------
       UpdateV-String.
           move FT-String-Value(FunInx)(1:uvvLen)
             to v-Buffer(uvvCur:uvvLen)
           set uvisResolved to true
           .
      *----------------------------------------------------------------
      * HEX
      *----------------------------------------------------------------
       UpdateV-Hex.
           move FT-Hex-Value(FunInx)(1:uvvLen)
             to v-Buffer(uvvCur:uvvLen)
           set uvisResolved to true
           .
      *----------------------------------------------------------------
      * RDW
      *----------------------------------------------------------------
       UpdateV-RDW.
           if Func-is-Source(FunInx)
             move in-len to uv-Num5
             move uv-Num5 to v-Buffer(uvvCur:uvvLen)
             set uvisResolved to true
           end-if
           if Func-is-Target(FunInx)
             if OUT-LEN-Field-is-Resolved
               set uvisResolved to true
               move out-len to uv-Num5
             else
               move zeroes to uv-Num5
             end-if
             move uv-Num5 to v-Buffer(uvvCur:uvvLen)
           end-if
           .
      *----------------------------------------------------------------
      * SPACE
      *----------------------------------------------------------------
       UpdateV-Space.
           move spaces to v-Buffer(uvvCur:FT-Spaces(FunInx))
           set uvisResolved to true
           .
      *----------------------------------------------------------------
      * COUNT
      *----------------------------------------------------------------
       UpdateV-Count.
           if Program-Migrating
             move zeroes to uv-Count
             move FT-Count-Field(FunInx) to uvSearch-Field
             move uvSearch-Field         to uvUsedPret
             perform FindFldt
             if uvField-Found
      *        'get LastGhost and use the right-most index as count
               set FldInx to Fldt-LastGhost(uvFoundAt)
               move FT-Hash(FunInx) to uvUsedHash
               move Fldt-Index(FldInx, uvUsedHash + 1) to uv-Count
             end-if
v0851        move uv-Count to uv-num18
             perform UpdateV-Count-Constant
             perform UpdateV-Count-Move
             set uvisResolved to true
           else
v0851          move zeroes to uv-num18
               perform UpdateV-Count-Move
           end-if
           .
       UpdateV-Count-Constant.
           evaluate true
            when Count-no-Constant(FunInx)
              continue
            when Count-Add-Constant(FunInx)
v0851         compute uv-num18  = uv-num18
                                + FT-Count-Constant(FunInx)
            when Count-Subtract-Constant(FunInx)
v0851         compute uv-num18  = uv-num18
                                - FT-Count-Constant(FunInx)
           end-evaluate
           .
       UpdateV-Count-Move.
           compute uvwPos = (18 - FT-Length(FunInx)) + 1
v0851      move uv-num18 (uvwPos:FT-Length(FunInx))
             to v-Buffer(uvvCur:uvvLen)
           .
      *----------------------------------------------------------------
      * SIZE of a Table (normal) or a size of a Field
      *----------------------------------------------------------------
       UpdateV-Size.
           if Program-Migrating
             if Size-Field-is-RDW(FunInx)
                perform UpdateV-Size-RDW
             else
                perform UpdateV-Size-Normal
             end-if
           else
v0851        move zeroes to uv-num18
             perform UpdateV-Size-Move
           end-if
           .
       UpdateV-Size-RDW.
           if Func-is-Source(FunInx)
             move in-len  to uvsPos
           else
             if OUT-LEN-Field-is-Resolved
               move out-len to uvsPos
             else
               move zeroes to uvsPos
             end-if
           end-if
           if uvsPos > 0
v0851        move uvsPos to uv-num18
             perform UpdateV-Size-Constant
             perform UpdateV-Size-Move
             set uvisResolved to true
           else
v0851        move zeroes to uv-num18
             perform UpdateV-Size-Constant
             perform UpdateV-Size-Move
           end-if
           .
      * size = end - start + 1
       UpdateV-Size-Normal.
           move zeroes to uvBase-Addr
           move FT-Size-Field(FunInx) to uvSearch-Field
                                         uvUsedPret
           perform FindFldt
           if uvField-Found
             perform Get-Start-Position
             set PretInx to Fldt-Field(uvFoundAt)
             if Pret-Table(PretInx) = 0
               set FldInx to Fldt-LastGhost(uvFoundAt)
             else
               set TblInx to Pret-Table(PretInx)
               if Table-is-Group(TblInx)
                 perform Find-End-of-Group
               else
                 set FldInx to Fldt-LastGhost(uvFoundAt)
               end-if
             end-if
             perform Get-End-Position
v0851        compute uv-num18  = (uvePos - uvsPos) + 1
             perform UpdateV-Size-Constant
             perform UpdateV-Size-Move
             set uvisResolved to true
           else
v0851        move zeroes to uv-num18
HK1217       perform UpdateV-Size-Constant
             perform UpdateV-Size-Move
           end-if
           .
       UpdateV-Size-Constant.
           evaluate true
             when Size-Add-Constant(FunInx)
v0851              compute uv-num18  = uv-num18
                                + FT-Size-Constant(FunInx)
             when Size-Subtract-Constant(FunInx)
v0851              compute uv-num18  = uv-num18
                                - FT-Size-Constant(FunInx)
           end-evaluate
           .
       UpdateV-Size-Move.
           compute uvwPos = (18 - FT-Length(FunInx)) + 1
v0851      move uv-num18 (uvwPos:FT-Length(FunInx))
             to v-Buffer(uvvCur:uvvLen)
           .
      *----------------------------------------------------------------
      * START
      *----------------------------------------------------------------
       UpdateV-Start.
           if Program-Migrating
             if Start-Field-is-RDW(FunInx)
                perform UpdateV-Start-RDW
             else
                perform UpdateV-Start-Normal
             end-if
           else
v0851        move zeroes to uv-num18
             perform UpdateV-Start-Move
           end-if
           .
       UpdateV-Start-RDW.
           if Func-is-Source(FunInx)
             move in-len  to uvsPos
           else
             if OUT-LEN-Field-is-Resolved
               move out-len to uvsPos
             else
               move zeroes  to uvsPos
             end-if
           end-if
           if uvsPos > 0
v0851        move uvsPos to uv-num18
             perform UpdateV-Start-Constant
             perform UpdateV-Start-Move
             set uvisResolved to true
           else
v0851        move zeroes to uv-num18
             perform UpdateV-Start-Move
           end-if
           .
       UpdateV-Start-Normal.
           move zeroes to uvsPos,
v0851                     uv-num18
      * resolve base address
           if FT-Start-Base-Field(FunInx) > 0
             move FT-Start-Base-Field(FunInx) to uvSearch-Field
             move FT-Start-Field(FunInx)      to uvUsedPret
             perform Resolve-Base-Addr
           else
             move zeroes to uvBase-Addr
           end-if
      * get the start-field
           move FT-Start-Field(FunInx) to uvSearch-Field
                                            uvUsedPret
           perform FindFldt
           evaluate true
             when uvField-Found
               perform UpdateV-Start-uvField-Found
             when uvField-Not-Found
               perform FindFldt-Closest
               if uvClosest-Found
                 move uvClosestFound to uvFoundAt
                 perform UpdateV-Start-uvField-Found
               else
v0851            move zeroes to uv-num18
                 perform UpdateV-Start-Move
               end-if
           end-evaluate
           .
       UpdateV-Start-uvField-Found.
           perform Get-Start-Position
v0851      compute uv-num18  = uvsPos - uvBase-Addr
           perform UpdateV-Start-Constant
           perform UpdateV-Start-Move
           set uvisResolved to true
           .
       UpdateV-Start-Constant.
           evaluate true
             when Start-Add-Constant(FunInx)
v0851              compute uv-num18  = uv-num18
                                + FT-Start-Constant(FunInx)
             when Start-Subtract-Constant(FunInx)
v0851              compute uv-num18  = uv-num18
                                - FT-Start-Constant(FunInx)
           end-evaluate
           .
       UpdateV-Start-Move.
           compute uvwPos = (18 - FT-Length(FunInx)) + 1
v0851      move uv-num18 (uvwPos:FT-Length(FunInx))
             to v-Buffer(uvvCur:uvvLen)
           .
      *----------------------------------------------------------------
      * EntryLength
      *----------------------------------------------------------------
       UpdateV-Entry.
           if Program-Migrating
             move zeroes to uvsPos,
v0851                       uv-num18
             move zeroes to uvBase-Addr
      * get the entry-field
      *go up the user field's instance parentage chain (Hash Parent)
      * to find the instance parent with the expected #hashes.
             set uvStart-Searching to true
             set fldinx to Fldt-Parent(uvFPtr)
             perform until uvStop-Searching
               evaluate true
                 when Fldt-Field(FldInx) = FT-Entry-Field(FunInx)
                   set uvFoundAt to FldInx
                   set uvStop-Searching to true
                 when other
                   set FldInx to Fldt-Parent(FldInx)
               end-evaluate
             end-perform
             perform UpdateV-Entry-uvField-Found
           else
v0851        move zeroes to uv-num18
             perform UpdateV-Entry-Move
           end-if
           .
       UpdateV-Entry-uvField-Found.
      *    'now get the byte location of the starting location
           set FldInx to uvFoundAt
           evaluate true
             when Func-is-Source(FunInx)
               move FLDT-Start-Byte(FldInx) to uvsPos
               move FLDT-Start-Nib(FldInx) to uvsNib
               move FLDT-Start-Bit(FldInx) to uvsBit
             when Func-is-Target(FunInx)
               move FLDT-Target-Start(FldInx) to uvsPos
             when other
               move zeroes to uvsPos
           end-evaluate

           perform Get-End-Position
v0851      compute uv-num18  = uvePos - uvsPos + 1

           perform UpdateV-Entry-Constant
           perform UpdateV-Entry-Move
           set uvisResolved to true
           .
       UpdateV-Entry-Constant.
           evaluate true
             when Entry-Add-Constant(FunInx)
v0851              compute uv-num18  = uv-num18
                                + FT-Entry-Constant(FunInx)
             when Entry-Subtract-Constant(FunInx)
v0851              compute uv-num18  = uv-num18
                                - FT-Entry-Constant(FunInx)
           end-evaluate
           .
       UpdateV-Entry-Move.
           compute uvwPos = (18 - FT-Length(FunInx)) + 1
v0851      move uv-num18 (uvwPos:FT-Length(FunInx))
             to v-Buffer(uvvCur:uvvLen)
           .
      *----------------------------------------------------------------
      * END
      *----------------------------------------------------------------
       UpdateV-End.
           if Program-Migrating
             if End-Field-is-RDW(FunInx)
                perform UpdateV-End-RDW
             else
                perform UpdateV-End-Normal
             end-if
           else
v0851        move zeroes to uv-num18
             perform UpdateV-End-Move
           end-if
           .
       UpdateV-End-RDW.
           if Func-is-Source(FunInx)
             move in-len  to uvsPos
           else
             if OUT-LEN-Field-is-Resolved
               move out-len to uvsPos
             else
               move zeroes  to uvsPos
             end-if
           end-if
           if uvsPos > 0
v0851        move uvsPos to uv-num18
             perform UpdateV-End-Constant
             perform UpdateV-End-Move
             set uvisResolved to true
           else
v0851        move zeroes to uv-num18
             perform UpdateV-End-Move
           end-if
           .
       UpdateV-End-Normal.
           move zeroes to uvsPos,
v0851                     uv-num18
      * resolve base address
           if FT-End-Base-Field(FunInx) > 0
             move FT-End-Base-Field(FunInx) to uvSearch-Field
             move FT-End-Field(FunInx)      to uvUsedPret
             perform Resolve-Base-Addr
           else
             move zeroes to uvBase-Addr
           end-if
      * get the End-field
           move FT-End-Field(FunInx) to uvSearch-Field
           move uvSearch-Field         to uvUsedPret
           perform FindFldt
           evaluate true
             when uvField-Found
               perform UpdateV-End-uvField-Found
             when uvField-Not-Found
               if Program-Migrating
                 perform UpdateV-End-Closest
               else
v0851            move zeroes to uv-num18
                 perform UpdateV-End-Move
               end-if
           end-evaluate
           .
       UpdateV-End-Closest.
           perform FindFldt-Closest
           if uvClosest-Found
             move uvClosestFound to uvFoundAt
             perform UpdateV-End-uvField-Found
           else
v0851        move zeroes to uv-num18
             perform UpdateV-End-Move
           end-if
           .

       UpdateV-End-uvField-Found.
           set PretInx to Fldt-Field(uvFoundAt)
           if Pret-Table(PretInx) = 0
             set FldInx to Fldt-LastGhost(uvFoundAt)
           else
             set TblInx to Pret-Table(PretInx)
             if Table-is-Group(TblInx)
               perform Find-End-of-Group
             else
               set FldInx to Fldt-LastGhost(uvFoundAt)
             end-if
           end-if
           perform Get-End-Position
v0851      compute uv-num18  = uvePos - uvBase-Addr
           perform UpdateV-End-Constant
           perform UpdateV-End-Move
           set uvisResolved to true
           .
       UpdateV-End-Constant.
           evaluate true
             when End-Add-Constant(FunInx)
v0851              compute uv-num18  = uv-num18
                                + FT-End-Constant(FunInx)
             when End-Subtract-Constant(FunInx)
v0851              compute uv-num18  = uv-num18
                                - FT-End-Constant(FunInx)
           end-evaluate
           .
       UpdateV-End-Move.
           compute uvwPos = (18 - FT-Length(FunInx)) + 1
v0851      move uv-num18s(uvwPos:FT-Length(FunInx))
             to v-Buffer(uvvCur:uvvLen)
           .

HK1229*'the build function will pass the parameters to the formula
HK1229*' routine.
HK1229 UpdateV-Build.
           move FT-Build-Cond-Ptr(FunInx) to cond-ptr
           move Cond-ExecuteNdx(cond-ptr) to p-StartExecutionNdx
           move Cond-ExecuteQty(cond-ptr) to FAExecutionQty
           move Cond-ResultNdx(cond-ptr) to FAFinalResultNdx

           call 'ExecForm' using
             Formula-Area
             Formula-Execution-Area
             Pre-Field-Table-Area,
v0854        Pre-Chain-Table-Area,
             Field-Table-Area, Cond-Table-Area
             ws-offset
             In-Buffer

           evaluate true
             when Formula-Result-Code = 0
               set FAEntry to Cond-ResultNdx(Cond-ptr)
               set FARinx  to FAResultNdx(FAEntry)
      *        'format answer to the vField's format
               evaluate true
                 when RAType-Int(FARinx)
                   move Pret-Length-Byte(PretInx) to uvwlen
                   move 1                         to uv-Pos
                   evaluate true
                     when Type-CH(PretInx)
                     when Type-ZD(PretInx)
                       move RAValInt(FARInx) to uv-value-num18
                       compute uv-POS = (18 - uvwLen) + 1
                     when Type-PD(PretInx)
                       move RAValInt(FARInx) to uv-value-PD18
                       compute uv-POS = (10 - uvwLen) + 1
                     when Type-BIN(PretInx)
                     when Type-BIS(PretInx)
                       move RAValInt(FARInx) to uv-value-BIN18
                       compute uv-POS = (8 - uvwLen) + 1
                   end-evaluate
                   move uv-value(uv-Pos:uvwLen)
                     to v-Buffer(uvvCur:uvwLen)
                 when RAType-Text(FARinx)
                 when RAType-Hex(FARinx)
                 when RAType-BIN(FARinx)
                   move RAValText(FARInx)(1:RALength(FARInx))
                     to v-Buffer(uvvCur:Pret-Length-Byte(PretInx))
                 when other
                  display 'Internal Error:Execform return unknown type '
                          RAType(FARInx)
                          ' from RA Entry:' Cond-ResultNdx(Cond-ptr)
                  move 12 to return-code
                  stop run
               end-evaluate
               set uvisResolved to true
             when Formula-Result-Code < 0
               continue
             when other
               display 'UpdateV:trouble with execform '
                  Formula-Result-code
                  ':Cond#' Cond-ptr
                  ':rec#' ws-rec-cnt
               move 12 to return-code
               stop run
           end-evaluate
           .


      * This gets the starting byte location.
      * Start field found,
       Get-Start-Position.
           evaluate true
             when Func-is-Source(FunInx)
                 move FLDT-Start-Byte(uvFoundAt) to uvsPos
                 move FLDT-Start-Nib(uvFoundAt) to uvsNib
                 move FLDT-Start-Bit(uvFoundAt) to uvsBit
             when Func-is-Target(FunInx)
                 move FLDT-Target-Start(uvFoundAt) to uvsPos
           end-evaluate
           .
      * First occurrence(root) of this field is found so go to this
      *  root's LastGhost. Starting with, and including this LastGhost
      *  entry, go up the parent chain to find the ....
       Get-End-Position.
           perform Find-First-Ancestor
           evaluate true
             when Func-is-Source(FunInx)
              and uvField-Found
               move FLDT-Start-Byte(FldInx) to uvePos
               move FLDT-Start-Nib(FldInx) to uvsNib
               move FLDT-Start-Bit(FldInx) to uvsBit
               Subtract 1 from uvePos
             when Func-is-Target(FunInx)
              and uvField-Found
               compute uvePos = FLDT-Target-Start(FldInx) - 1
             when Func-is-Source(FunInx)
              and uvField-Not-Found
               move In-len to uvePos
             when Func-is-Target(FunInx)
              and uvField-Not-Found
               move Out-len to uvePos
           end-evaluate
           .
      * 'go up parent chain to find the first ancestor having a
      * ' sibling after it
       Find-First-Ancestor.
           set uvStart-Searching to true
           perform until uvStop-Searching
             evaluate true
               when Fldt-Parent(FldInx) = 0
      *          'at top of record (no more ancestors)
      *          ' point 1 past last field generated
                 set FldInx to Field-Cnt
                 move 0 to uvFoundAt
                 set uvStop-Searching to true
               when Fldt-NextSib(FldInx) = 0
      *          'No sibling go to the previous ancestor
                 set FldInx to Fldt-Parent(FldInx)
               when other
                 set FldInx to Fldt-NextSib(FldInx)
                 if Not-a-vField(FldInx)
                   set uvFoundAt to FldInx
                   set uvStop-Searching to true
                 end-if
             end-evaluate
           end-perform
           .
      *'We go to its final organizational child by following the
      *' NextSibling chain
       Find-end-of-Group.
           set uvStart-Searching to true
           set FldInx to uvFoundAt
           set uvItem-just-left to FldInx
      *    'save off the level of the first/main group
           move Fldt-Level(FldInx) to uv-Level
           if Fldt-NextSib(FldInx) > 0
      *      'go to the next table entry
             set FldInx to Fldt-NextSib(FldInx)
             set uvItem-just-left to FldInx
           end-if
           perform until uvStop-Searching
             evaluate true
               when Fldt-NextSib(FldInx) = 0
                 set uvStop-Searching to true
               when FLDT-Level(FldInx) <= uv-Level
                 set FldInx to uvItem-just-left
                 set uvStop-Searching to true
               when Fldt-NextSib(FldInx) > 0
                 set uvItem-just-left to FldInx
                 set FldInx to Fldt-NextSib(FldInx)
             end-evaluate
           end-perform
           .
       Resolve-Base-Addr.
           set uvFindFld-Base     to true
           perform FindFldt
           set uvFindFld-Normal   to true
           evaluate true
             when uvField-Found
               evaluate true
                 when Func-is-Source(FunInx)
                   move FLDT-Start-Byte(uvFoundAt) to uvBase-Addr
                   move FLDT-Start-Nib(uvFoundAt) to uvsNib
                   move FLDT-Start-Bit(uvFoundAt) to uvsBit
                 when Func-is-Target(FunInx)
                   move FLDT-Target-Start(uvFoundAt) to uvBase-Addr
               end-evaluate
               if uvBase-Addr > 0
                 Subtract 1 from uvBase-Addr
               end-if
             when other
               move zeroes to uvBase-Addr
           end-evaluate
           if trace-on
             display 'TRACE:Base-Addr=' uvBase-Addr
              ':FoundAt=' uvFoundAt
              ':UsedPret=' uvUsedPret
              ':Search=' uvSearch-Field
           end-if
           .
      *----------------------------------------------------------------
      * FIELD
      *----------------------------------------------------------------
       UpdateV-Field.
           move FT-Field(FunInx) to uvSearch-Field
                                        uvUsedPret
           perform FindFldt
           if uvField-Found
             if FLDT-Length(uvFoundAt) > 0
               move FLDT-Field(uvFoundAt) to uvFoundPret
               perform UpdateV-Field-Constant
               perform Grab-and-format-data
               move uvDL-Value
                    (FT-Field-Start(FunInx):FT-Length(FunInx))
                 to v-Buffer(uvvCur:uvvLen)
             else
               move low-values to v-Buffer(uvvCur:uvvLen)
             end-if
v00871       if Discard-Nothing
               set uvisResolved to true
v00871       end-if
           else
             move spaces     to v-Buffer(uvvCur:uvvLen)
           end-if
           .
       Grab-and-format-data.
           move Fldt-Start-Byte(uvFoundAt) to gbPos
           move Fldt-Start-Nib(uvFoundAt)  to gbPosNib
           move Fldt-Start-Bit(uvFoundAt)  to gbPosBit
           move Fldt-Length-Byte(uvFoundAt) to gbLen
           move Fldt-Length-Nib(uvFoundAt)  to gbLenNib
           move Fldt-Length(uvFoundAt)      to gbLenBit
           move Pret-Type(uvFoundPret)      to gbType
           evaluate true
             when NORMAL-vField(uvFoundPret)
             when Occur-vField(uvFoundPret)
             when ODO-vField(uvFoundPret)
               move 1 to gbBuffer
             when other
               move 0 to gbBuffer
           end-evaluate
v0854      move uvFoundPret to gbField
           perform GetBuff
           move ws-Value to uv-Value
           IF Discard-Something
             add 1 to uvFieldValueInvalid-cnt
             if uvFieldValueInvalid-cnt > 10
               if uvFieldValueInvalid-cnt = 11
                 display 'UpdateV:FIELD function value is invalid '
                         'messages now suspended (>10)'
               end-if
             else
               set uvSub to FunInx
               display 'UpdateV:FIELD function value is invalid'
                ':Func#=' uvSub
                ':pret=' uvPret
                ':UsedPret=' uvFoundPret
                ':rec#=' ws-rec-cnt
             end-if
           end-if
      *Take the uv-Value and present it in an FDD format, if needed
      *first, determine the length
           evaluate true
             when Type-BIT (uvFoundPret)
               compute uv-Bytes = FldT-Length(uvFoundAt)
             when Type-Nib (uvFoundPret)
             when Type-PD-NEC4(uvFoundPret)
               compute uv-Bytes = FldT-Length(uvFoundAt) / 4
             when other
               compute uv-Bytes = FldT-Length(uvFoundAt) / 8
           end-evaluate
      *Now, reformat the field as needed base on type
           evaluate true
             when Type-Ch (uvFoundPret)
               if OPT-vFieldCheckAPI-Yes
                  and (API-VGFBCNV1-1(uvFoundPret)
                   or API-VGXBCNV1-1(uvFoundPret)
                   or API-VGFBCNV1-2(uvFoundPret)
                   or API-VGXBCNV1-2(uvFoundPret))
                   perform UpdateV-Field-API
               else
                   move uv-Value to uvDL-Value
               end-if
             when Type-Bit (uvFoundPret)
               move uv-Value to uvDL-Value
             when TYPE-NIB    (uvFoundPret)
             when TYPE-PD-NEC4(uvFoundPret)
               compute uv-Value-num18 = uv-Value-num18
                                      + uvConstantValue
               compute uv-Pos = (18 - uv-Bytes) + 1
               move uv-Value(uv-Pos:uv-Bytes) to uvDL-Value
             when Type-ZD(uvFoundPret)
               compute uv-Pos = (18 - uv-Bytes) + 1
               evaluate uv-Value(20:1)
                 when 'S'
                  compute uv-EDIT-NUM18S = uv-Value-num18s
                                          + uvConstantValue
v0850             move uv-EDIT-NUM18S(uv-Pos:uv-Bytes + 1)
                    to uvDL-Value(1:uv-Bytes + 1)
                 when 'U'
                   compute uv-Value-num18 = uv-Value-num18
                                          + uvConstantValue
v0850              move uv-Value-NUM18(uv-Pos:uv-Bytes)
                     to uvDL-Value(1:uv-Bytes)
               end-evaluate
             when Type-PD(uvFoundPret)
v0854          compute uvwLen = (uv-Bytes * 2) - 1
v0852          compute uv-Pos = (18 - uvwLen) + 1
               evaluate uv-Value(20:1)
                 when 'S'
                  compute uv-EDIT-NUM18S = uv-Value-num18s
                                          + uvConstantValue
v0852             move uv-EDIT-NUM18S(uv-Pos:uvwLen + 1)
v0852               to uvDL-Value(1:uvwLen + 1)
                 when 'U'
                   compute uv-Value-num18 = uv-Value-num18
                                          + uvConstantValue
v0852              move uv-Value-NUM18(uv-Pos:uvwLen)
v0852                to uvDL-Value(1:uvwLen)
               end-evaluate
             when Type-PD-NEC(uvFoundPret)
               compute uvwLen = uv-Bytes * 2
               compute uv-Pos = (18 - uvwLen) + 1
               evaluate uv-Value(20:1)
                 when 'S'
                   compute uv-EDIT-NUM18S = uv-Value-num18s
                                          + uvConstantValue
v0850              move uv-EDIT-NUM18S(uv-Pos:uvwLen + 1)
                     to uvDL-Value(1:uvwLen + 1)
                 when 'U'
                   compute uv-Value-num18 = uv-Value-num18
                                          + uvConstantValue
v0850              move uv-Value-NUM18(uv-Pos:uvwLen)
                     to uvDL-Value(1:uvwLen)
               end-evaluate
             when Type-BIS(uvFoundPret)
             when Type-BIN(uvFoundPret)
               evaluate uv-Bytes
                 when 1 THRU 2 move 5  to uvwLen
                 when 3 THRU 4 move 10 to uvwLen
                 when 5 THRU 8 move 18 to uvwLen
               end-evaluate
               compute uv-Pos = (18 - uvwLen) + 1
               evaluate uv-Value(20:1)
                 when 'S'
                   compute uv-EDIT-NUM18S = uv-Value-num18s
                                          + uvConstantValue
v0850              move uv-EDIT-NUM18S(uv-Pos:uvwLen + 1)
v0850                to uvDL-Value(1:uvwLen + 1)
                 when 'U'
                   compute uv-Value-num18 = uv-Value-num18
                                          + uvConstantValue
v0850              move uv-Value-Num18(uv-Pos:uvwLen)
v0850                to uvDL-Value(1:uvwLen)
               end-evaluate
           end-evaluate
           .
       UpdateV-Field-Constant.
           move FT-Field-Constant(FunInx) to uvConstantValue
           if Field-Subtract-Constant(FunInx)
              multiply -1 by uvConstantValue
           end-if
           .
v0851  UpdateV-Field-API.
           evaluate true
             when API-VGFBCNV1-1(uvFoundPret)
             when API-VGXBCNV1-1(uvFoundPret)
                  perform UpdateV-Field-API-CharCNV-1
             when API-VGFBCNV1-2(uvFoundPret)
             when API-VGXBCNV1-2(uvFoundPret)
                  perform UpdateV-Field-API-CharCNV-2
           end-evaluate
           IF Discard-Something
             set uvSub to FunInx
             display 'UpdateV:vField API call error'
              ':API Code-Return-code=' Code-Return-code
              ':API Pret=' uvFoundPret
              ':FunPtr=' uvSub
              ':pret=' uvPret
             move 12 to return-code
             stop run
           end-if
           .
v0851  UpdateV-Field-API-CharCNV-1.
           add 1 to CAT-num-api-VGFBCNV1
v00875     move 'IKOUNTOI' to Code-Conv-Process-Class
v00895     evaluate true
v00895       when OPT-CnvDiscard-Yes
v00895         move ' '  to Code-Conv-Request-Option
v00895         move ' '  to one-Byte-Code
v00895         move '  ' to two-Byte-Code
v00895       when OPT-CnvDiscard-No
v00895         move '1'  to Code-Conv-Request-Option
v00895         move ' '  to one-Byte-Code
v00895         move OPT-CnvExceptionCode-Hex to two-Byte-Code
v00895     end-evaluate
           move uv-Value to Target-Data-Storage(1:gbLen)
           move gbLen    to Code-Conv-Target-Data-Length
           compute Post-Code-Conv-Data-Length =
                      function length(Post-conv-Data-Storage)
           set Target-Data-Storage-Address
             to address of Target-Data-Storage
           set Post-conv-Data-Storage-Address
             to address of Post-conv-Data-Storage
v00872     move OPT-MaxErrorNumberCodeAPI
v00872       to Maximum-Number-of-error-entry
           call KampoAPI   using Code-Conv-Request-Packet
v00872                           CodeConvErrorInfo
           evaluate true
             when Code-Return-Code = '0000'
                  move Post-conv-Data-Storage(1:gbLen)
                    to uvDL-Value(1:gbLen)
             when Code-Return-Code = '9600'
             when Code-Return-Code = '9620'
             when Code-Return-Code = '9690'
             when Code-Return-Code = '9990'
                  set Discard-API-VGFBCNV1-RC-not-00 to true
             when Opt-CnvDiscard-Yes and Code-Return-Code = '9610'
                  set Discard-API-VGFBCNV1-RC-not-00 to true
                  add 1 to CAT-num-api-VGFBCNV1-err
             when Opt-CnvDiscard-No  and Code-Return-Code = '9610'
                  add 1 to CAT-num-api-VGFBCNV1-err
                  perform API-Log-Error
                  move Post-conv-Data-Storage(1:gbLen)
                    to uvDL-Value(1:gbLen)
                  move '0000' to Code-Return-Code
             when other
                  set Discard-API-VGFBCNV1-RC-not-00 to true
                  add 1 to CAT-num-api-VGFBCNV1-err
           end-evaluate
           .
v0851  UpdateV-Field-API-CharCNV-2.
           add 1 to CAT-num-api-VGFBCNV1
v00875     move 'IKOUNTOI' to Code-Conv-Process-Class
v00895     evaluate true
v00895       when OPT-CnvDiscard-Yes
v00895         move ' '  to Code-Conv-Request-Option
v00895         move ' '  to one-Byte-Code
v00895         move '  ' to two-Byte-Code
v00895       when OPT-CnvDiscard-No
v00895         move '1'  to Code-Conv-Request-Option
v00895         move ' '  to one-Byte-Code
v00895         move OPT-CnvExceptionCode-Hex to two-Byte-Code
v00895     end-evaluate

      * determine TARGET length
           move gbLen to mg-Len
           compute mg-Len2 = mg-Len + 2 + 1
           compute mg-Len3 = mg-Len + 4
           move x'3f75'            to Target-Data-Storage(1:2)
           move uv-Value(1:mg-Len) to Target-Data-Storage(3:mg-Len)
           move x'3f76'            to Target-Data-Storage(mg-Len2:2)
           MOVE mg-Len3            to Code-Conv-Target-Data-Length
           compute Post-Code-Conv-Data-Length =
                   function length(Post-conv-Data-Storage)
           set Target-Data-Storage-Address
             to address of Target-Data-Storage
           set Post-conv-Data-Storage-Address
             to address of Post-conv-Data-Storage
v00872     move OPT-MaxErrorNumberCodeAPI
v00872       to Maximum-Number-of-error-entry

           call KampoAPI   using Code-Conv-Request-Packet
v00872                           CodeConvErrorInfo
           evaluate true
             when Code-Return-Code = '0000'
                  move 1 to mgBit-Basis
                  compute mg-Len2 =
                    Pret-MIG-Len(uvFoundPret) / mgBit-Basis
+                 move Post-conv-Data-Storage(2:mg-Len)
                    to uvDL-Value(1:mg-Len2)
             when Code-Return-Code = '9600'
             when Code-Return-Code = '9620'
             when Code-Return-Code = '9690'
             when Code-Return-Code = '9990'
                  set Discard-API-VGFBCNV1-RC-not-00 to true
             when Opt-CnvDiscard-Yes and Code-Return-Code = '9610'
                  set Discard-API-VGFBCNV1-RC-not-00 to true
                  add 1 to CAT-num-api-VGFBCNV1-err
             when Opt-CnvDiscard-No  and Code-Return-Code = '9610'
                  add 1 to CAT-num-api-VGFBCNV1-err
                  perform API-Log-Error
                  move 1 to mgBit-Basis
                  compute mg-Len2 =
                    Pret-MIG-Len(uvFoundPret) / mgBit-Basis
+                 move Post-conv-Data-Storage(2:mg-Len)
                    to uvDL-Value(1:mg-Len2)
                  move '0000' to Code-Return-Code
             when other
                  set Discard-API-VGFBCNV1-RC-not-00 to true
                  add 1 to CAT-num-api-VGFBCNV1-err
           end-evaluate
           .
      *----------------------------------------------------------------
      * TimeStamp
      *----------------------------------------------------------------
       UpdateV-TimeStamp.
           if DB2-TimeStamp(FunInx)
              move DB2-TimeStamp-Value     to v-Buffer(uvvCur:uvvLen)
           end-if
           if ODBC-TimeStamp(FunInx)
              move ODBC-TimeStamp-Value    to v-Buffer(uvvCur:uvvLen)
           end-if
           set uvisResolved to true
           .
v00872*----------------------------------------------------------------
      * COPY
      *----------------------------------------------------------------
       UpdateV-Copy.
           move FT-Copy-Length(FunInx) to uvcLen
           move FT-Copy-Value(FunInx)(1:uvcLen)
             to v-Buffer(uvvCur:uvcLen)
      *    propagated move...
           if FT-Copy-Repeat(FunInx) > 1
             move v-Buffer(uvvCur:uvvLen)
               to v-Buffer(uvvCur + uvcLen:uvvLen - uvcLen)
           end-if
           set uvisResolved to true
           .

      * Find the FLD table entry, via the chain with index
      * Input: uvSearch-Field - Field Number to search for
      *        uvUsedPret - Function's Pret Field Number
      *        FPtr
      *        FunInx
      * Output: uvFoundAt
       FindFldt.
           move zeroes to uvFoundAt
           evaluate true
             when FT-Hash(FunInx) = 0
              and Table-Field(uvUsedPret)
               move low-values to uvSearch-Tbl
               move 4 to uvwLen
               set TblInx to Pret-Table(uvUsedPret)
               if Table-is-Group(TblInx)
                 move 0 to uvSearch-Index(1)
               else
                 move 1 to uvSearch-Index(1)
               end-if
             when FT-Hash(FunInx) > 0
               move FLDT-INDEX-TABLE(uvFPtr) to uvSearch-Tbl
               compute uvwLen = (FT-Hash(FunInx) * 4)
             when other
               move low-values to uvSearch-Tbl
               move 4 to uvwLen
           end-evaluate

           if Program-Pulling-Data-Field
             move Pret-LastField(uvUsedPret) to uvFoundAt
           else
             evaluate true
               when uvFindFld-Normal
                   if Fldt-UsedNextInChain(uvFPtr) > 0
                     move Fldt-UsedNextInChain(uvFPtr) to sub
                     if Fldt-Next(sub) > 0
HK1030                 if FT-Hash(FunInx) > 0
HK1030                   move Fldt-Next(sub) to sub2
HK1030                   if FLDT-INDEX-TABLE(sub2)(1:uvwLen) =
HK1030                      uvSearch-Tbl(1:uvwLen)
HK1030                     move Fldt-Next(sub) to uvFoundat
HK1030                   else
HK1030                     move 0 to uvFoundAt
HK1030                   end-if
HK1030                 else
                         move Fldt-Next(sub) to uvFoundat
                       end-if
                     else
                       move 0 to uvFoundAt
                     end-if
                   else
HK1027               if FT-Hash(FunInx) > 0
HK1027                 perform FindFldt-with-chain
HK1027               else
                       move Pret-StartField(uvUsedPret) to uvFoundAt
                     end-if
                   end-if
               when uvFindFld-Base
                 perform FindFldt-with-chain
             end-evaluate
           end-if

           .
      *This will find the Field using the chain method. Normally this
      * would be the BASE field.
       FindFldt-with-chain.
           move zeroes to uvFoundAt
           if Pret-StartField(uvSearch-Field) > 0
             set uvStart-Searching to true
             set FldInx to Pret-StartField(uvSearch-Field)
             perform until uvStop-Searching
               if FLDT-INDEX-TABLE(FldInx)(1:uvwLen) =
                  uvSearch-Tbl(1:uvwLen)
                  set uvFoundAt to FldInx
                  set uvStop-Searching to true
               else
                 if Fldt-Next(FldInx) > 0
                   set FldInx to Fldt-Next(FldInx)
                 else
                   set uvStop-Searching to true
                 end-if
               end-if
             end-perform
           end-if
           .
      * This locates the closest PRET
       FindFldt-Closest.
           move zeroes to uvClosestFound
v0854      compute tPret = uvSearch-Field - 1
           set uvStart-Searching to true
           perform until uvStop-Searching
             evaluate true
v0854          when tPret   < RT-Start-PreT(rtInx)
                 set uvStop-Searching to true
v0854          when Pret-LastField(tPret)   > 0
v0854            move Pret-LastField(tPret)   to uvClosestFound
                 set uvStop-Searching to true
               when other
v0854            subtract 1 from tPret
             end-evaluate
           end-perform
           if uvClosestFound = 0
             display 'updatev:FindFldt-Closest:Logic error.'
               ':rec#' ws-rec-cnt
               ':uvSearch-Field=' uvSearch-Field
             move 12 to return-code
             stop run
           else
             if trace-on
               set uvSub to FunInx
               display 'TRACE:Closest found.'
                  ':func=' uvSub ':rec#=' ws-rec-cnt
                  ':Closest=' uvClosestFound
                  ':search-=' uvSearch-Field
             end-if
           end-if
           .
      *----end of UpdateV--------
      *end program updatev.

      *This program will create a migrated record for each data field.
      *Input:
      *  Command - pic 9(4) comp; Command indicator.
      *                 0000 Open-MIG-File
      *                 0001 Move-Source-to-Target
      *                 0002 Write-MIG-Record
      *                 0003 Close-MIG-File
      *  Migrate-Stats  - structure as defined in LnkMig copybook.
      *  Pre-field-data-area - structure as defined in LnkmgPreT.
      *Output: depending on Command indicator.
      * 0000 MIG-File is opened and ready at byte 1, bit 1.
      * 0001 output buffer is populated with source information
      *      Data Field structure is updated with target location
      * 0002 output buffer is written.
      * 0003 MIG-File is closed and stats are reported.
      *
      *Return-Code:
      *  00 - all is okay
      *  11 - Error in API-VGFBCNV1
      *Fields passed to routine:
      * MIG-Write-Command    pic 9(4) comp.
      *  88 Open-MIG-File                value 0.
      *  88 Move-Source-to-Target        value 1.
      *  88 Write-MIG-Record             value 2.
      *  88 Close-MIG-File               value 3.
      *  88 Report-Stats-Back            value 4.
      *  88 Move-Table-to-Target         value 5.
      *  88 Start-Target-Record          value 6.
      *  88 Resolve-vFields              value 7.
      * FldT                 pic 9(5) comp.
      * in-Buffer            PIC X(32756).
      * v-Buffer             PIC X(32756).
      *     lnkMig.
      *     lnkmgPreT.
      *     lnkFld.
      *     lnkFunc.
      *     lnkDisc.
      *     lnkOpt.
      *     lnkTbl.
      *     lnkRec.
      *     lnkcnt.
      *     lnkdef.
      *----------------------------------------------------------------
      *change-history.  (be sure to change mgVersion)
      *  Date----- In- Ver---- Remarks----------------------------------
      * 2017/06/08 hk  v0.3.26 added DATE() and removed old comments
      *----------------------------------------------------------------
       Migrate.
           move 00 to return-code
           evaluate true
            when Open-MIG-File         perform Open-the-MIG-File
            when Close-MIG-File        perform Close-The-MIG-File
            when Resolve-vFields       perform Resolve-the-UnResolved
            when other
             perform Start-the-Target-Record

      *      'Move the fields to the migrate file.
      *      'Attempt to move(migrate) every field mapped. Some fields
      *      ' could have major errors some will be minor. Major errors
      *      ' are noted as Discard entry and some as a Log entry.
v00883*      '8/13/2015-Discard 3(out of buffer space) will cause to
v00883*      '    stop processing the fields.
             move zeroes to mgFirst-discard-code
v00883       set Discard-Nothing to true
             perform varying FldInx from 1 by 1
                       until (FldInx > Field-Cnt)
v00883                    or (Discard-CANT-HOLD-ENTIRE-FIELD)
v00892                    or (Discard-API-QGGFDRC1-RC-not-00)
v00892                    or (Discard-API-EEOR-RC-not-00)
               set PretInx to Fldt-Field(FldInx)
               evaluate true
                 when Table-Field(PretInx)
                       perform Move-The-Table-to-Target
                 when Record-Field(PretInx)
                       continue
                 when Other
                       if MIG-Event-Requested(PretInx)
                         perform Move-The-Source-to-Target
                       end-if
               end-evaluate
               if Discard-Something
                 if mgFirst-Discard-code = 0
                   move discard-code to mgFirst-Discard-code
                 end-if
                 perform Store-the-Discard
v00883           if not Discard-CANT-HOLD-ENTIRE-FIELD
v00883             set Discard-Nothing to true
v00883           end-if
               end-if
             end-perform
             move mgFirst-Discard-Code to Discard-Code
      *      'Write a migrate record only if there are NO discards
             if Discard-Nothing
               set Write-Mig-Record to true
               perform Write-the-MIG-record thru wtmr-exit
v00885         if Discard-Something
v00885           if mgFirst-Discard-code = 0
v00885             move discard-code to mgFirst-Discard-code
v00885           end-if
v00885           move RT-Start-Pret(Record-to-Process) to Pret
v00885           move Pret-StartField(Pret) to FldT
v00885           set FldInx to FldT
v00885           perform Store-the-Discard
v00885         end-if
               move zeroes to return-code
             end-if
           end-evaluate
           .
       Store-the-Discard.
           set sub to fldinx
v00860     set Fldt to FldInx
           evaluate true
            when Discard-API-VGFBCNV1-RC-not-00
      *      'store additional information for the discard 11 KampoAPI
             if (code-return-code = 9610)
             and OPT-MaxErrorNumberCodeAPI > 0
               perform varying n from 1 by 1
                        until n > Number-of-error-conversion
                 call 'NOTEDISC' using
                   Fldt-Field(FldInx), Fldt-Start-Byte(FldInx),
                   Discard-Table-Area, Fldt
                 move code-return-code         to DT-Data1(Discard-Cnt)
                 move CCEI-Character-Code(n)   to DT-Data2x(Discard-Cnt)
                 move CCEI-Offset(n)           to DT-Data3(Discard-Cnt)
               end-perform
             else
              call 'NOTEDISC' using
                 Fldt-Field(FldInx), Fldt-Start-Byte(FldInx),
                 Discard-Table-Area, Fldt
              move code-return-code             to DT-Data1(Discard-Cnt)
              move Post-Code-Conv-Data-Length   to DT-Data2(Discard-Cnt)
              move Code-Inconvert-Detection-Pos to DT-Data3(Discard-Cnt)
             end-if

            when Discard-API-QGGFDRC1-RC-not-00
              call 'NOTEDISC' using
                 Fldt-Field(FldInx), Fldt-Start-Byte(FldInx),
                 Discard-Table-Area, Fldt
              move RCM-Return-Code  to DT-Data1(Discard-Cnt)
              move RCM-Reason-Code  to DT-Data2-3(Discard-Cnt)
v00892        add 1 to discard-count(Discard-code)
v00892        move 8 to return-code
v00892        go to End-Program

v00886      when Discard-API-EEOR-RC-not-00
              call 'NOTEDISC' using
                 Fldt-Field(FldInx), Fldt-Start-Byte(FldInx),
                 Discard-Table-Area, Fldt
              move OE-Return-code   to DT-Data1(Discard-Cnt)
              move spaces           to DT-Data2-3(Discard-Cnt)

            when other
              call 'NOTEDISC' using
                 Fldt-Field(FldInx), Fldt-Start-Byte(FldInx),
                 Discard-Table-Area, Fldt
           end-evaluate

           add 1 to fields-in-error
           if First-Field-in-Error = 0
              move Fldt-Field(FldInx) to First-Field-in-Error
              move Discard-code       to First-Fields-Error
              set sub to fldinx
           end-if
           add 1 to discard-count(Discard-code)
           move zeroes to return-code
           .
       Open-the-MIG-File.
           if MIG-NOT-OPEN
             move mgVersion to Mig-Version
             Display 'Migrate v' mgVersion
             Display 'Migrate:Max record size set to ' mgMax-Out-Len
             open output MIG-FILE
             if mgIO-STATUS not = '00'
               display 'Migrate:no MIG file (' mgIO-STATUS ')'
               move 12 to return-code
               stop run
             else
               set MIG-OPEN to true
               perform Start-the-Target-Record
             end-if
v00886       open output EEO-File
v00886       if EEOR-IO-STATUS not = '00'
v00886         display 'Migrate no EEOR file (' EEOR-IO-STATUS ')'
v00886         move 12 to return-code
v00886         stop run
v00886       end-if
v00886       move zeroes      to ms-rec-out
v00886                        ms-largest-rec-size
v00886                        ms-largest-rec-location
v00886                        ms-shortest-rec-location
v00886                        ms-num-fields-moved
v00886                        ms-num-bytes-moved
v00886                        CAT-num-api-VGFBCNV1
v00895                        CAT-num-api-VGFBCNV1-err
v00886                        CAT-num-api-SHIFT
v00886                        CAT-num-api-KAMPOYR
v00886                        CAT-num-API-IKEEORE0
v00886                        CAT-num-API-EEOR
v00886                        ms-num-default-natural
v00886                        ms-num-default-value
v00886       move 999999999 to ms-shortest-rec-size
           end-if
           .
       Start-the-Target-Record.
           move 1 to Cursor
           move mgMax-Out-Len to Out-Len
           set OUT-LEN-Field-is-UnResolved to true
           .
       Close-The-MIG-File.
           if MIG-OPEN
             close MIG-File
v00886             EEO-File
             set MIG-NOT-OPEN to true
             display 'Migrate:---------Migrate Stats-----------------*'
             display '# of Records Written.....' ms-rec-out
             display '# of Records EEOR........' CAT-num-API-EEOR
             display '# of Records Divided.....' CAT-num-API-divided
             display 'Longest record length....' ms-largest-rec-size
                ' located at record # ' ms-largest-rec-location
             display 'Shortest record length...' ms-shortest-rec-size
                ' located at record # ' ms-shortest-rec-location
             display '# of Fields moved........' ms-num-fields-moved
             display '# of Bytes moved.........' ms-num-bytes-moved
             if ms-rec-out > 0
               compute ms-average-rec-size = ms-num-bytes-moved
                                           / ms-rec-out
v00886         display 'Average record size......' ms-average-rec-size
             else
               move zeroes to ms-average-rec-size
v00886         display 'Average record size......--n/a--'
             end-if
             display 'API VGFBCNV1 called......' CAT-num-api-VGFBCNV1
v00895      display 'API VGFBCNV1 errors......' CAT-num-api-VGFBCNV1-err
             display 'API SHIFT    called......' CAT-num-api-SHIFT
             display 'API KAMPOYR  called......' CAT-num-api-KAMPOYR
             display 'API QGGFDRC1 called......' CAT-num-api-QGGFDRC1
v00886       display 'API IKEEORE0 called......' CAT-num-api-IKEEORE0
             display '# of Natural defaults....' ms-num-default-natural
             display '# of Value defaults......' ms-num-default-value
             display 'Migrate:---end of Migrate Stats----------------*'

           end-if
           .
       Write-the-MiG-record.
           if MIG-NOT-OPEN then go to wtmr-exit end-if
           if cursor = 1 then
             if trace-on
              display 'Trace:Migrate:Nothing moved to target'
               ':Src rec#=' ws-rec-cnt
             end-if
             go to wtmr-exit
           end-if
      * note. update all internal system fields before resolving
           compute out-len = cursor - 1
           set OUT-LEN-Field-is-Resolved to true
      *    'try one more time to resolve the vField functions
           perform Resolve-the-UnResolved
           add 1 to RT-Migrate(Rec-Ptr)

v00867*    Kampo Decision Chart, Normally the 'Other' should be executd
v00867     evaluate true
v00886*      ' API-EEOR
v00887       when DLL-OutputEdit-Yes
v00886        and out-len > OPT-EEORSize
v00886            perform EEOR-Write
v00867
v00867*      ' API-Yes, PTable-Yes, Prefix-Yes
v00867       when opt-RecordPrefixAPICall-yes
v00867        and RT-PTable(Rec-Ptr)   not = spaces
v00867        and OPT-RecordPrefixEndField not = spaces
v00867            perform API-Prefix-and-Write
v00867
v00867*      ' API-Yes, PTable-Yes, Prefix-No
v00867       when opt-RecordPrefixAPICall-yes
v00867        and RT-PTable(Rec-Ptr)   not = spaces
v00867        and OPT-RecordPrefixEndField = spaces
v00878            perform API-Prefix-and-Write-No-DLL
v00867
v00867*      ' API-Yes, PTable-No , Prefix-Yes
v00867       when opt-RecordPrefixAPICall-yes
v00867        and RT-PTable(Rec-Ptr)       = spaces
v00867        and OPT-RecordPrefixEndField Not = spaces
v00867            perform DLL-Prefix-and-Write
v00867
v00867*      ' API-Yes, PTable-No , Prefix-No
v00867       when opt-RecordPrefixAPICall-yes
v00867        and RT-PTable(Rec-Ptr)       = spaces
v00867        and OPT-RecordPrefixEndField = spaces
v00867            perform No-Prefix-just-Write
v00867
v00867*      ' API-No , PTable-Yes, Prefix-Yes
v00867       when opt-RecordPrefixAPICall-no
v00867        and RT-PTable(Rec-Ptr)   not = spaces
v00867        and OPT-RecordPrefixEndField not = spaces
v00867            perform DLL-Prefix-and-Write
v00867
v00867*      ' API-No , PTable-Yes, Prefix-No
v00867       when opt-RecordPrefixAPICall-no
v00867        and RT-PTable(Rec-Ptr)   not = spaces
v00867        and OPT-RecordPrefixEndField = spaces
v00867            perform No-Prefix-just-Write
v00867
v00867*      ' API-No , PTable-No , Prefix-Yes
v00867       when opt-RecordPrefixAPICall-no
v00867        and RT-PTable(Rec-Ptr)       = spaces
v00867        and OPT-RecordPrefixEndField not = spaces
v00867            perform DLL-Prefix-and-Write
v00867
v00867*      ' API-No , PTable-No , Prefix-No
v00867       when opt-RecordPrefixAPICall-no
v00867        and RT-PTable(Rec-Ptr)       = spaces
v00867        and OPT-RecordPrefixEndField = spaces
v00867            perform No-Prefix-just-Write
v00867
v00867       when other
                  perform No-Prefix-just-Write
v00867     end-evaluate

           if OPT-LOGShowMigrate-yes
             move 'MIGRATE'            to LM-Reason-Text
             move ws-rec-cnt           to LM-Rec-Cnt
             move RT-Field(rec-ptr)    to Pret
             move Pret-FieldName(Pret) to LM-Record-Name
             move ms-rec-out           to LM-Rec-Out
             move Out-Len              to LM-Out-Len
             write LOG-RECORD from LOG-MIGRATE
           end-if
           move 1 to Cursor
           move mgMax-Out-Len to Out-Len
           .
       wtmr-exit.
           exit.

       No-Prefix-Just-Write.
v00867*    'No Prefix is built, just write migrate record
           if Trace-On
             display 'Mig:No-Prefix-Just-Write'
                ':Bytes(1:' Out-Len ')'
           end-if

v00885*    ' confirm record length is valid
v00885*    '   because migrate buffer maybe larger than record
v00885     if out-len > mgMax-Out-Len
v00885     or out-len = 0
v00885       set Discard-CANT-HOLD-ENTIRE-FIELD to true
              if mgFirst-Overflow = 0
                move 5 to mgFirst-Overflow
                perform ShowOverflow
              end-if
v00885     end-if

v00885     if Discard-Nothing then
             write MIG-Record
v00872         from mgBuffer(1:out-len)
             evaluate mgIO-STATUS
               when 00
v00886           perform Update-Migrate-Stats
               when 34
                   display 'Migrate:Out of space. File Status:'
                    mgIO-STATUS
                    ':rec-out=' ms-rec-out
                   move 12 to return-code
                   stop run
                 when other
                   display 'Migrate:Write Error. File Status:'
                    mgIO-STATUS
                    ':Src#=' ws-rec-cnt
                    ':rec-out=' ms-rec-out
                    ':len=' out-len
                   move 12 to return-code
                   stop run
             end-evaluate
v00885     else
v00885       if Trace-On
v00885         display 'TRACE:MIG write discard error of ' discard-code
v00885           ':out-len=' out-len
v00885           ':Src#=' ws-rec-cnt
v00885           ':Tgt#=' ms-rec-out
v00885           ':RecType=' Rec-Ptr
v00885           ':API=' OPT-RecordPrefixAPICall-sw
v00885           ':Prefix=' Opt-RecordPrefixEndField
v00885       end-if
v00885     end-if
           .
v00867*Kampo's API split routine
v00867*Interface to Kampo API record prefix and split > 28K record
v00867* Mig-Record is the 'source'
v00867* Split-Buffer is the 'target'
v00878* 'There is DLL prefix so need to remove the prefix first
v00867 API-Prefix-and-Write.
           move 'DIVI'                to RCM-Operator-Code
           move RT-PTable(Rec-Ptr)    to RCM-Physical-Table-Name
      *    ' determine position after prefix
v00880     move RT-PrefixEnd(Rec-Ptr) to mgPret
v00880     move Pret-StartField(mgPret) to mgFldt
v00880     compute mgPos = FLDT-Target-Start(mgFldt)
v00880                   + FLDT-Target-Length(mgFldt)
      *    ' determine length without prefix
           compute RCM-Source-Record-Length mgLen
                   = (Cursor - 1) - (mgPos - 1)
           if mgPos = 0
             display 'Engine:Logic error in APAW:Pos=0:#=' ws-rec-cnt
             move 12 to return-code
             stop run
           end-if
           if mgLen = 0
             display 'Engine:Logic error in APAW:Len=0:#=' ws-rec-cnt
             move 12 to return-code
             stop run
           end-if

v00878*    ' move data without the DLL prefix to the api buffer
v00878     move mgBuffer(mgPos:mgLen)   to API-buffer

           set RCM-Source-Record-Address
            to address of API-buffer
           set RCM-Record-Length-Area-Address
            to address of Split-Length-Area
           move zeroes                to RCM-Number-of-Target-Record
           set RCM-Target-Record-Area-Address
             to address of Split-buffer-Area
           move 'DB'                  to RCM-Filler-DB
           set RCM-Filler-no-use to null
           set RCM-Return-Code-Normal to true
           set RCM-Reason-Code-Normal to true
           call  QGGFDRC1  using Record-Conv-Main
           if Trace-On
             display 'Mig:API-Prefix-and-Write'
                 ':Buf(' mgPos ':' mgLen ')'
                 ':mRC=' RCM-Return-Code
                 ':#tgt=' RCM-Number-of-Target-Record
                 ':mgIO-Status=' mgIO-Status
           end-if
v00892     if RCM-Return-Code-Normal
v00880       compute CAT-num-API-divided =
v00880           CAT-num-API-divided + (RCM-Number-of-Target-Record - 1)
v00892       add 1 to CAT-num-api-QGGFDRC1
v00892     end-if
           perform varying mgN from 1 by 1
              until mgN > RCM-Number-of-Target-Record
                 or RCM-Return-Code-Abnormal
                 or mgIO-Status > '00'
             move Split-Length(mgN)            to Out-Len
             move Split-Buffer(mgN)(1:Out-Len) to Mig-record(1:Out-Len)
             if Trace-On
               display 'Mig:API-Prefix-and-Write'
                 ':bytes(1:' Out-Len ')'
                 ':mRC=' RCM-Return-Code
                 ':mgIO-Status=' mgIO-Status
                 ':Number=' RCM-Number-of-Target-Record
             end-if
v00886*    ' confirm record length is valid
v00886*    '   because migrate buffer maybe larger than record
v00886       if out-len > mgMax-Out-Len
v00886       or out-len = 0
v00886         set Discard-CANT-HOLD-ENTIRE-FIELD to true
               if mgFirst-Overflow = 0
                move 6 to mgFirst-Overflow
                perform ShowOverflow
               end-if
v00886       end-if
v00868       if Discard-Nothing
               write Mig-record
               evaluate mgIO-STATUS
                 when 00 continue
                 when 34
                      display 'Migrate(1):Out of space. File Status:'
                       mgIO-STATUS
                       ':rec-out=' ms-rec-out
                      move 12 to return-code
                      stop run
                 when other
                    display 'Migrate(1):Write Error. File Status:'
                      mgIO-STATUS
                      ':len=' out-len
                       ':rec-out=' ms-rec-out
                      move 12 to return-code
                      stop run
               end-evaluate
v00886         perform Update-Migrate-Stats
v00886       end-if
           end-perform
           if not RCM-Return-Code-Normal
v00882       if Trace-On
               display 'MigrateS:Return-code=' RCM-Return-Code
                 ':Reason=' RCM-Reason-Code
v00882       end-if
             set Discard-API-QGGFDRC1-RC-not-00 to true
             if mgFirst-Discard-code = 0
                move discard-code to mgFirst-Discard-code
             end-if
           end-if
           .
v00878*Kampo's API split routine
v00878*Interface to Kampo API record prefix and split > 28K record
v00878* Mig-Record is the 'source'
v00878* Split-Buffer is the 'target'
v00878* 'No DLL prefix so just pass this onto the API routine
v00878 API-Prefix-and-Write-No-DLL.
           move 'DIVI'                to RCM-Operator-Code
           move RT-PTable(Rec-Ptr)    to RCM-Physical-Table-Name
           compute RCM-Source-Record-Length mgLen = Cursor - 1

           move mgBuffer(1:mgLen)   to API-buffer

           set RCM-Source-Record-Address
            to address of API-buffer
           set RCM-Record-Length-Area-Address
            to address of Split-Length-Area
           move zeroes                to RCM-Number-of-Target-Record
           set RCM-Target-Record-Area-Address
             to address of Split-buffer-Area
           move 'DB'                  to RCM-Filler-DB
           set RCM-Filler-no-use to null
           set RCM-Return-Code-Normal to true
           set RCM-Reason-Code-Normal to true
           call  QGGFDRC1  using Record-Conv-Main
           if Trace-On
             display 'Mig:API-Prefix-and-Write-No-DLL'
                 ':Buf(1:' Cursor ')'
                 ':mRC=' RCM-Return-Code
                 ':#tgt=' RCM-Number-of-Target-Record
                 ':mgIO-Status=' mgIO-Status
           end-if
v00892     if RCM-Return-Code-Normal
v00880       compute CAT-num-API-divided =
v00880           CAT-num-API-divided + (RCM-Number-of-Target-Record - 1)
v00892       add 1 to CAT-num-api-QGGFDRC1
v00892     end-if
           perform varying mgN from 1 by 1
              until mgN > RCM-Number-of-Target-Record
                 or RCM-Return-Code-Abnormal
                 or mgIO-Status > '00'
             move Split-Length(mgN)            to Out-Len
             move Split-Buffer(mgN)(1:Out-Len) to Mig-record(1:Out-Len)
             if Trace-On
               display 'Mig:API-Prefix-and-Write-No-DLL'
                 ':bytes(1:' Out-Len ')'
                 ':mRC=' RCM-Return-Code
                 ':mgIO-Status=' mgIO-Status
                 ':Number=' RCM-Number-of-Target-Record
             end-if
v00886*    ' confirm record length is valid
v00886*    '   because migrate buffer maybe larger than record
v00886       if out-len > mgMax-Out-Len
v00886       or out-len = 0
v00886         set Discard-CANT-HOLD-ENTIRE-FIELD to true
               if mgFirst-Overflow = 0
                move 7 to mgFirst-Overflow
                perform ShowOverflow
               end-if
v00886       end-if
v00886       if Discard-Nothing
               write Mig-record
               evaluate mgIO-STATUS
                 when 00 continue
                 when 34
                      display 'Migrate(2):Out of space. File Status:'
                       mgIO-STATUS
                       ':rec-out=' ms-rec-out
                      move 12 to return-code
                      stop run
                 when other
                    display 'Migrate(2):Write Error. File Status:'
                      mgIO-STATUS
                       ':out-len=' out-len
                       ':rec-out=' ms-rec-out
                      move 12 to return-code
                      stop run
               end-evaluate
v00886         perform Update-Migrate-Stats
v00886       end-if
           end-perform
           if not RCM-Return-Code-Normal
v00882       if trace-on
             display 'MigrateS:Return-code=' RCM-Return-Code
               ':Reason=' RCM-Reason-Code
v00882       end-if
             set Discard-API-QGGFDRC1-RC-not-00 to true
             if mgFirst-Discard-code = 0
                move discard-code to mgFirst-Discard-code
             end-if
           end-if
           .
v00867*Kampo's create a DLL prefix to migrate record and write it
v00867* ** At present this is the same as No-Prefix-just-write. If there
v00867* were no prefix defined; then there are no prefixes to write.
v00867 DLL-Prefix-and-Write.
           if Trace-On
             display 'TRACE:Mig:DLL-Prefix-and-Write'
                 ':Buf(1:' Out-len ')'
           end-if
v00886*    ' confirm record length is valid
v00886*    '   because migrate buffer maybe larger than record
v00886     if out-len > mgMax-Out-Len
v00886     or out-len = 0
v00886       set Discard-CANT-HOLD-ENTIRE-FIELD to true
             if mgFirst-Overflow = 0
                move 8 to mgFirst-Overflow
                perform ShowOverflow
             end-if
v00886     end-if
v00886     if Discard-Nothing
             write Mig-record
v00872           from mgBuffer(1:out-len)
             evaluate mgIO-STATUS
                 when 00 continue
                 when 34
                      display 'Migrate(3):Out of space. File Status:'
                       mgIO-STATUS
                       ':rec-out=' ms-rec-out
                      move 12 to return-code
                      stop run
                 when other
                    display 'Migrate(3):Write Error. File Status:'
                      mgIO-STATUS
                       ':out-len=' out-len
                       ':rec-out=' ms-rec-out
                      move 12 to return-code
                      stop run
             end-evaluate
             perform Update-Migrate-Stats
v00886     end-if
           .
v00886*Kampo's API EEOR routine
v00886 EEOR-Write.
           compute OE-record-length = out-len - DLL-OutputEditOffset
           compute mgPos = DLL-OutputEditOffset + 1
           move mgBuffer(mgPos:OE-Record-Length) to API-Buffer
           set OE-record-area-address     to address of API-Buffer
           set EEOR-record-length-address
            to address of EEOR-RECORD-LEN(1)
           set EEOR-record-address
            to address of EEOR-RECORD(1)
v00893     move 0 to X
v00893     perform varying N from 1 by 1
v00893               until X > 0
v00893                  or N > length of Infile-DSN
v00893       if Infile-DSN(N:1) = '.'
v00893         compute X = N + 1
v00893       end-if
v00893     end-perform
v00893     if X > 0
v00893     and X <= length of InFile-DSN
v00893       move InFile-DSN(X:) to OE-external-file-name
v00893     else
v00893       move InFile-DSN     to OE-external-file-name
v00893     end-if
           move ws-rec-cnt to ws-num9
           move ws-num9    to OE-record-number
           move '0000'     to OE-Return-code
           move zeroes     to OE-record-size
           move zeroes     to Number-of-EEOR-record
           call  IKEEORE0  using IKEEORE0-IO-area
           if OE-Return-code not = '0000'
             set Discard-API-EEOR-RC-not-00 to true
v00892     else
v00892       add 1 to CAT-num-api-IKEEORE0
           end-if
           if Discard-Nothing
             compute out-len = DLL-OutputEditOffset + OE-Record-Size
             if DLL-OutputEditOffset > 0
               move mgBuffer(1:DLL-OutputEditOffset)
                 to Mig-Record(1:DLL-OutputEditOffset)
             end-if
             move API-Buffer(1:OE-Record-size)
               to Mig-Record(DLL-OutputEditOffset + 1:OE-Record-Size)
           end-if
      *    ' write the MIG record less the bytes from this EEOR API
      *    ' confirm record length is valid
      *    '   because migrate buffer maybe larger than record
           if Discard-Nothing
           and (out-len > mgMax-Out-Len
               or out-len = 0)
               set Discard-CANT-HOLD-ENTIRE-FIELD to true
               if mgFirst-Overflow = 0
                move 9 to mgFirst-Overflow
                perform ShowOverflow
               end-if
           end-if
           if Discard-Nothing
               write Mig-record
               evaluate mgIO-STATUS
                 when 00 continue
                 when 34
                      display 'Migrate(4):Out of space. File Status:'
                       mgIO-STATUS
                       ':rec-out=' ms-rec-out
                      move 12 to return-code
                      stop run
                 when other
                    display 'Migrate(4):Write Error. File Status:'
                      mgIO-STATUS
                      ':len=' out-len
                       ':rec-out=' ms-rec-out
                      move 12 to return-code
                      stop run
               end-evaluate
               perform Update-Migrate-Stats
           end-if
      *    'write the EEOR record (remaining bytes of the migbuffer)
           perform varying mgN from 1 by 1
              until mgN > Number-of-EEOR-Record
                 or OE-Return-code not = '0000'
                 or EEOR-IO-Status not = '00'
             move EEOR-RECORD-LEN(mgN)        to Out-Len
             move EEOR-Record(mgN)(1:Out-Len) to EEO-Record
             write EEO-Record
             evaluate EEOR-IO-Status
                 when 00 continue
                 when 34
                      display 'EEOR:Out of space. File Status:'
                       EEOR-IO-STATUS
                       ':src#=' ws-rec-cnt
                      move 12 to return-code
                      stop run
                 when other
                    display 'EEOR:Write Error. File Status:'
                      EEOR-IO-STATUS
                      ':len=' out-len
                       ':src#=' ws-rec-cnt
                      move 12 to return-code
                      stop run
             end-evaluate
             add 1 to CAT-num-API-EEOR
             move 'EEOR   '            to LE-Reason-Text
             move ws-rec-cnt           to LE-Rec-Cnt
             move mgN                  to LE-Rec-Num
             write LOG-RECORD from LOG-EEOR
           end-perform
           .
v00886*-----------------------------------------------------------------
v00886* Gather the migration statistics
v00886*-----------------------------------------------------------------
v00886 Update-Migrate-Stats.
           add 1 to ms-rec-out
           add out-Len to ms-num-bytes-moved
           if out-len > ms-largest-rec-size
             move out-len       to ms-largest-rec-size
             move ms-rec-out    to ms-largest-rec-location
           end-if
           if out-len < ms-shortest-rec-size
             move out-len       to ms-shortest-rec-size
             move ms-rec-out    to ms-shortest-rec-location
           end-if
           if Trace-On
             display 'TRACE:Mig:Src#=' ws-rec-cnt
                ':Tgt#=' ms-rec-out
                ':RecType=' Rec-Ptr
                ':API=' OPT-RecordPrefixAPICall-sw
                ':Prefix=' Opt-RecordPrefixEndField
           end-if
           .
      *----------------------------------------------------------------
      * Loop through the Field-Table for v-fields that are unresolved.
      * Efficiency Note. Maybe we should count the number of unresolved
      *  fields while processing earlier instead of the count loop
      *  below.
      *----------------------------------------------------------------
       Resolve-the-UnResolved.
           compute out-len = cursor - 1
           set mgResolve-Mode to true
           move zeroes to mgResolve-Attempts
           set rtInx to rec-ptr
      *    'chain from unresolved vfield to unresolved vfield
      *    ' attempting to resolve the vfield
      *    'vfield-start could be changed to 0 within this loop
           if vField-Start > 0
             if Trace-On
                  display
                    'Resol Field <Start.Lengt>'
                    ' -Pret-'
                    ' Field name--------------------'
                    ' -Fun# FTot SW FRes'
             end-if
             perform test after until mgNothing-was-Resolved
                                   or vField-Start = 0
               move zeroes to uvResolved
               set mgNothing-was-Resolved to true
               set mgStart-Searching to true
               set FldInx to vField-Start
               perform until mgStop-Searching
                 set PretInx to Fldt-Field(FldInx)
                 perform Attempt-to-Resolve-Field
                 if FLDT-vNext(FldInx) > 0
                   set FldInx to FLDT-vNext(FldInx)
                 else
                   set mgStop-Searching to true
                 end-if
               end-perform
               add 1 to mgResolve-Attempts
             end-perform
           end-if
           set mgNormal-Mode to true
           .
       Attempt-to-Resolve-Field.
           move Pret-FUNC(PretInx) to mgFunc
           set mgFPtr to FldInx
           move FLDT-Start-Byte(FldInx) to mg-POS
           move mg-POS to uvCursor
           set uvFptr to FldInx
           move Fldt-Field(FldInx) to uvPret
           perform UpdateV
           if uvResolved = 1
              set mgSomething-was-resolved to true
HK1221*       perform Resolve-the-Target-Field
           end-if
      *    'no matter what the target is as resolved as it can be
      *    ' since by this time all tables/fields are created
HK1221     perform Resolve-the-Target-Field
           if Trace-on
             move 'Resol' to Trace-Event
             move mgFPtr to ws-num5a
             move Fldt-Start(FldInx) to ws-num5b
             move mgFunc to ws-num5c
             move uvResolved to tr-num2a
             display Trace-Event
                 ' ' ws-num5a
                 ' <' ws-num5b '.     >'
                 ' ' uvPRET
                 ' ' Pret-FieldName(uvPret)
                 ' ' ws-num5c
                 ' ' Func-Total(mgFunc)
                 ' ' tr-num2a
                 ' ' Fldt-Functions-Resolved(FldInx)
           end-if
           .
       Resolve-the-Target-Field.
           set Discard-Nothing to true
           move zeroes to mgResolve-Attempts
           if Program-Migrating
             if FldT-Target-Start(FldInx) > 0
               move Cursor to mgSave-Cursor
               move FldT-Target-Start(FldInx)   to Cursor
               perform Move-The-Source-to-Target
               move mgSave-Cursor to Cursor
             end-if
           end-if
           .
      * Move the current cursor position to the Group/Table's start
      * position. This does not really move any 'data'.
       Move-The-Table-to-Target.
           If Table-Field(PretInx)
             if FLDT-Target-Start(FldInx) = 0
               move Cursor        to FLDT-Target-Start(FldInx)
             end-if
           end-if
           .
       Move-The-Source-to-Target.
           if MIG-NOT-OPEN then
             display 'Migrate:Logic err, File not open, tried to write'
             move 12 to return-code
             stop run
           end-if
           if Fldt-Field(FldInx) = zeroes
             display 'Migrate:Pret is Zeroes!'
              ':stage=' Program-Stage-sw
              ':cmd=' Migrate-Command
             move 12 to return-code
             stop run
           end-if
           set PretInx to Fldt-Field(FldInx)
           move zeroes                 to mg-Len
           move Fldt-Start(FldInx)     to mg-POS
           move zeroes                 to mgKAMPOYR-value

           evaluate true
             when No-API(PretInx)   perform Standard-Move
             when API-VGFBCNV1-1(PretInx)
             when API-VGXBCNV1-1(PretInx)
                                    perform API-CharCNV-1 thru AC1-exit
             when API-VGFBCNV1-2(PretInx)
             when API-VGXBCNV1-2(PretInx)
                                    perform API-CharCNV-2 thru AC2-exit
             when API-SHIFT(PretInx)  perform API-SHIFT-code
             when API-KAMPOYR(PretInx) perform API-KAMPOYR-code
v00867                                 perform Standard-Move
             when other
               set sub to PretInx
               display 'Engine:Migrate:internal Error:'
                 'Unknown API switch:' PRET-MIG-API(PretInx)
                 ':rec#=' ws-rec-cnt
                 ':pret=' sub
               move 12 to return-code
               stop run
           end-evaluate

           if Discard-Nothing and mgNormal-mode then
             move Cursor             to FLDT-Target-Start(FldInx)
             move mg-Len             to FLDT-Target-Length(FldInx)
             compute Cursor = Cursor + mg-Len
             add 1 to ms-num-fields-moved
           end-if
           .

       Standard-Move.
v0854      perform Validate-Buffer-Pointers
           if Discard-Nothing
             if Pret-type(PretInx) = Pret-Mig-Type(PretInx)
                and Pret-length(PretInx) = (Pret-Mig-Len(PretInx)  * 8)
v00858          and Not-DataValidationNumeric
v01000          and Pret-Date-FMT(PretInx) = Pret-Mig-Date-FMT(PretInx)
               perform Straight-Move
             else
               perform Convert-Types-Move
             end-if
           end-if
           .
       Convert-Types-Move.
           evaluate true
               when Pret-Date-FMT(PretInx) > 0
                and Pret-MIG-Date-FMT(PretInx) > 0
                  perform Convert-Date
               when MIG-TYPE-CH(PretInx)     perform Move-to-CH
               when MIG-TYPE-PD(PretInx)     perform Move-to-PD
               when MIG-TYPE-ZD(PretInx)     perform Move-to-ZD
               when MIG-TYPE-BIN(PretInx)    perform Move-to-BIN
               when MIG-TYPE-BIS(PretInx)    perform Move-to-BIS
CW0315         when MIG-TYPE-LS(PretInx)     perform Move-to-LS
CW0315         when MIG-TYPE-TS(PretInx)     perform Move-to-TS
      *n/a     when MIG-TYPE-BIT(PretInx)
      *n/a     when MIG-TYPE-NIB(PretInx)
      *n/a     when MIG-TYPE-PD-NEC(PretInx)
      *n/a     when MIG-TYPE-PD-NEC4(PretInx)
           end-evaluate
           .
       Straight-Move.
           move FLDT-Start-Byte(FldInx) to mg-POS
v00855     evaluate true
v00855       when (Type-PD(PretInx)
v00855          or Type-PD-NEC(PretInx)
v00855          or Type-PD-NEC4(PretInx))
v00855        and in-Buffer(mg-POS:mg-Len) = spaces
v00855          set Discard-NOT-NUMERIC-FIELD to true
             when other
                perform Move-Buffer-to-record
           end-evaluate
           .
      * This can move either from the buffers or from formatted data
      * depending on the type of the source of data.
      * vfields will always come from the v-buffer
       Move-to-CH.
           move FLDT-Start-Byte(FldInx) to mg-POS
           move FLDT-Length-Byte(FldInx) to mgsLen
           perform Compute-mg-Bytes
           evaluate true
             when mgsLen = 0
                 perform Default-Target
             when NORMAL-vField(PretInx)
             when Occur-vField(PretInx)
             when ODO-vField(PretInx)
               move v-Buffer(mg-POS:mgsLen)
v00872           to mgBuffer  (Cursor:mg-Len)
             when TYPE-CH(PretInx)
                move in-Buffer(mg-POS:mgsLen)
v00872            to mgBuffer  (Cursor:mg-Len)
             when Other
                perform Get-Numeric-Value
                if DataValidationNumeric
                and Discard-Something
                  continue
                else
                  if Type-Bit(PretInx)
                    move 1 to mg-POS
                  else
                    compute mg-POS = (18 - mg-Bytes) + 1
                  end-if
                  move mg-Value(mg-POS:mg-Bytes)
v00872              to mgBuffer  (Cursor:mg-Len)
                end-if
           end-evaluate
           .
       Move-to-PD.
           move FLDT-Start-Byte(FldInx) to mg-POS
           move FLDT-Length-Byte(FldInx) to mgsLen
           perform Compute-mg-Bytes
           evaluate true
             when Fldt-Length(FldInx) = 0
               perform Default-Target
             when Opt-FixPDWhenLowHigh-No
v00867         or Is-a-vField(FldInx)
      *        'get and format the source
               perform Get-Numeric-Value
v00859         if DataValidationNumeric
v00859         and Discard-Something
v00859           continue
v00859         else
v00859           set Discard-Nothing to true
                 if FLDT-is-Signed(FldInx)
                   move mg-Value-num18S to mg-PDs-value
                 else
                   move mg-Value-num18 to mg-PD-value
                 end-if
                 compute mg-POS = (length of mg-PD-valuex - mg-Len) + 1
                 move mg-PD-valuex(mg-POS:mg-Len)
v00872             to mgBuffer  (Cursor:mg-Len)
v00859         end-if

v0854        when Opt-FixPDWhenLowHigh-Yes
v0854         and Type-PD(PretInx)
v0854         and in-Buffer(mg-Pos:mgsLen) = low-values
v0854           compute mgLastPos = (Cursor + mg-Len) - 1
v00872          move low-values to mgBuffer  (Cursor:mg-Len - 1)
v00872          move x'0C'      to mgBuffer  (mgLastPos:1)
v0854           add 1 to FixPDWhenLowHigh-cnt
v0854           if FixPDWhenLowHigh-rec = 0
v0854             move ws-rec-cnt to FixPDWhenLowHigh-rec
v0854             set FixPDWhenLowHigh-Field to PretInx
v0854           end-if
v00860          set mgPret to PretInx
v00860          set mgFldt to FldInx
v00860          set mgCode-LowVal  to true
v00860          call 'NoteLog' using
v00860               mgPret, mgFldt, mgCode, Log-Table-Area

v0854        when Opt-FixPDWhenLowHigh-Yes
v00882        and Type-PD-NEC(PretInx)
v0854         and in-Buffer(mg-Pos:mgsLen) = high-values
v0854           compute mgLastPos = (Cursor + mg-Len) - 1
v00872          move all x'99' to mgBuffer  (Cursor:mg-Len - 1)
v00872          move x'9C'     to mgBuffer  (mgLastPos:1)
v0854           add 1 to FixPDWhenLowHigh-cnt
v0854           if FixPDWhenLowHigh-rec = 0
v0854             move ws-rec-cnt to FixPDWhenLowHigh-rec
v0854             set FixPDWhenLowHigh-Field to PretInx
v0854           end-if
v00860          set mgPret to PretInx
v00860          set mgFldt to FldInx
v00860          set mgCode-HighVal to true
v00860          call 'NoteLog' using
v00860               mgPret, mgFldt, mgCode, Log-Table-Area

v00882       when Opt-FixPDWhenLowHigh-Yes
              and (Type-PD(PretInx))
              and in-Buffer(mg-Pos:mgsLen) = high-values
                move in-Buffer(mg-POS:mgsLen)
                  to mgBuffer  (Cursor:mg-Len)
                add 1 to FixPDWhenLowHigh-cnt
                if FixPDWhenLowHigh-rec = 0
                  move ws-rec-cnt to FixPDWhenLowHigh-rec
                  set FixPDWhenLowHigh-Field to PretInx
                end-if
                set mgPret to PretInx
                set mgFldt to FldInx
                set mgCode-HighVal-Kept to true
                call 'NoteLog' using
                     mgPret, mgFldt, mgCode, Log-Table-Area

             when other
      *        'get and format the source
               perform Get-Numeric-Value
v00859         if DataValidationNumeric
v00859         and Discard-Something
v00859           continue
v00859         else
v00859           set Discard-Nothing to true
                 if FLDT-is-Signed(FldInx)
                   move mg-Value-num18S to mg-PDs-value
                 else
                   move mg-Value-num18 to mg-PD-value
                 end-if
                 compute mg-POS = (length of mg-PD-valuex - mg-Len) + 1
                 move mg-PD-valuex(mg-POS:mg-Len)
v00872             to mgBuffer  (Cursor:mg-Len)
               end-if
           end-evaluate
           .
       Move-to-ZD.
v00857     move FLDT-Length-Byte(FldInx) to mgsLen
v00857     move FLDT-Start-Byte(FldInx) to mg-POS
           perform Compute-mg-Bytes

v00860*    'check ZD for non 0-9, space values; results used later
v00860     move 0 to mgBadZD
v00860     if DataValidationNumeric
v00860     and Opt-FixZDWhenLowHigh-Yes
v00860     and Type-ZD(PretInx)
v00860       compute mgLastPos = (mg-Pos + mgsLen) - 1
v00860       perform varying mgN from mg-Pos by 1
v00860         until mgN > mgLastPos or mgBadZD > 1
v00869          evaluate true
v00869           when in-buffer(mgN:1) = 0 or 1 or 2 or 3 or 4 or 5 or 6
v00869                            or 7 or 8 or 9
v00869             continue
v00869           when in-buffer(mgN:1) = space
v00869             move 1 to mgBadZD
v00870           when mgN = mgLastPos
MFMFMF            and (in-buffer(mgN:1) >= x'C0' and <= x'C9')
PCPCPC*           and (in-buffer(mgN:1) >= x'30' and <= x'39')
v00870             continue
v00870           when mgN = mgLastPos
MFMFMF            and (in-buffer(mgN:1) >= x'D0' and <= x'D9')
PCPCPC*           and (in-buffer(mgN:1) >= x'70' and <= x'79')
v00870             continue
v00869           when other
v00869             move 2 to mgBadZD
v00869          end-evaluate
v00860       end-perform
v00860     end-if

      *    'check the type of validation to perform, if any
           evaluate true
             when FLDT-Length(FldInx) = 0
              perform Default-Target

             when opt-FixZDWhenLowHigh-No
v00867         or Is-a-vField(FldInx)
      *        'get and format the source
               perform Get-Numeric-Value
v00859         if DataValidationNumeric
v00859         and Discard-Something
v00859           continue
v00859         else
v00859           set Discard-Nothing to true
      *          'special case: From PD-NEC or PD-NEC4 to ZD
      *          ' then make it unsigned value
                 if TYPE-PD-NEC(PretInx) or TYPE-PD-NEC4(PretInx)
                   if FLDT-is-Signed(FldInx)
                     move mg-Value-num18S to mg-ZD-value
                   else
                     move mg-Value-num18 to mg-ZD-value
                   end-if
                 else
                   if FLDT-is-Signed(FldInx)
                     move mg-Value-num18S to mg-ZDs-value
                   else
                     move mg-Value-num18 to mg-ZD-value
                   end-if
                 end-if
                 compute mg-POS = (length of mg-ZD-value - mg-Len) + 1
                 move mg-ZD-valuex(mg-POS:mg-Len)
v00872             to mgBuffer  (Cursor:mg-Len)
               end-if

v0854        when Opt-FixZDWhenLowHigh-Yes
v0854         and  Type-ZD(PretInx)
v00857        and in-Buffer(mg-Pos:mgsLen) = low-values
v00872         move zeroes to mgBuffer  (Cursor:mg-Len)
v0854          add 1 to FixZDWhenLowHigh-cnt
v0854          if FixZDWhenLowHigh-rec = 0
v0854            move ws-rec-cnt to FixZDWhenLowHigh-rec
v0854            set FixZDWhenLowHigh-Field to PretInx
v0854          end-if
v00860         set mgPret to PretInx
v00860         set mgFldt to FldInx
v00860         set mgCode-LowVal to true
v00860         call 'NoteLog' using
v00860              mgPret, mgFldt, mgCode, Log-Table-Area

v0854        when Opt-FixZDWhenLowHigh-Yes
v00882        and (Type-PD-NEC4(PretInx)
v00882          or Type-PD-NEC (PretInx))
v00857        and in-Buffer(mg-Pos:mgsLen) = high-values
v00872          move all '9' to mgBuffer  (Cursor:mg-Len)
v0854          add 1 to FixZDWhenLowHigh-cnt
v0854          if FixZDWhenLowHigh-rec = 0
v0854            move ws-rec-cnt to FixZDWhenLowHigh-rec
v0854            set FixZDWhenLowHigh-Field to PretInx
v0854          end-if
v00860         set mgPret to PretInx
v00860         set mgFldt to FldInx
v00860         set mgCode-HighVal to true
v00860         call 'NoteLog' using
v00860              mgPret, mgFldt, mgCode, Log-Table-Area

v00882       when Opt-FixZDWhenLowHigh-Yes
              and (Type-ZD(PretInx))
              and in-Buffer(mg-Pos:mgsLen) = high-values
               move in-Buffer(mg-POS:mgsLen)
                 to mgBuffer  (Cursor:mg-Len)
               add 1 to FixZDWhenLowHigh-cnt
               if FixZDWhenLowHigh-rec = 0
                 move ws-rec-cnt to FixZDWhenLowHigh-rec
                 set FixZDWhenLowHigh-Field to PretInx
               end-if
               set mgPret to PretInx
               set mgFldt to FldInx
               set mgCode-HighVal-Kept to true
               call 'NoteLog' using
                    mgPret, mgFldt, mgCode, Log-Table-Area

v0854        when Opt-FixZDWhenLowHigh-Yes
v0854         and Type-ZD(PretInx)
v00857        and in-Buffer(mg-Pos:mgsLen) = spaces
v00872          move spaces  to mgBuffer  (Cursor:mg-Len)
v0854           add 1 to FixZDWhenLowHigh-cnt
v0854           if FixZDWhenLowHigh-rec = 0
v0854             move ws-rec-cnt to FixZDWhenLowHigh-rec
v0854             set FixZDWhenLowHigh-Field to PretInx
v0854           end-if
v00860          set mgPret to PretInx
v00860          set mgFldt to FldInx
v00860          set mgCode-Space   to true
v00860          call 'NoteLog' using
v00860               mgPret, mgFldt, mgCode, Log-Table-Area

v00869       when Opt-FixZDWhenLowHigh-Yes
v00869        and Type-ZD(PretInx)
v00869        and mgBadZD = 0
v00869*         'perfect ZD field
v00869          move in-buffer(mg-pos:mgsLen)
v00872            to mgBuffer  (Cursor:mg-Len)

v00860       when Opt-FixZDWhenLowHigh-Yes
v00860        and Type-ZD(PretInx)
v00869        and mgBadZD = 1
v00869*         'it is still okay ZD even with spaces, just move & logit
v00860          move in-buffer(mg-pos:mgsLen)
v00872            to mgBuffer  (Cursor:mg-Len)
v00860          add 1 to FixZDWhenLowHigh-cnt
v00860          if FixZDWhenLowHigh-rec = 0
v00860            move ws-rec-cnt to FixZDWhenLowHigh-rec
v00860            set FixZDWhenLowHigh-Field to PretInx
v00860          end-if
v00860          set mgPret to PretInx
v00860          set mgFldt to FldInx
v00860          set mgCode-Space   to true
v00860          call 'NoteLog' using
v00860               mgPret, mgFldt, mgCode, Log-Table-Area

v00869       when Opt-FixZDWhenLowHigh-Yes
v00869        and Type-ZD(PretInx)
v00869        and mgBadZD = 2
v00869*        'bad source ZD numeric
v00869         set Discard-NOT-NUMERIC-FIELD to true

v0854        when other
      *        'get and format the source
               perform Get-Numeric-Value
v00859         if DataValidationNumeric
v00859         and Discard-Something
v00859           continue
v00859         else
v00859           set Discard-Nothing to true
      *          'special case: From PD-NEC or PD-NEC4 to ZD
      *          ' then make it unsigned value
                 if TYPE-PD-NEC(PretInx) or TYPE-PD-NEC4(PretInx)
                   if FLDT-is-Signed(FldInx)
                     move mg-Value-num18S to mg-ZD-value
                   else
                     move mg-Value-num18 to mg-ZD-value
                   end-if
                 else
                   if FLDT-is-Signed(FldInx)
                     move mg-Value-num18S to mg-ZDs-value
                   else
                     move mg-Value-num18 to mg-ZD-value
                   end-if
                 end-if
                 compute mg-POS = (length of mg-ZD-value - mg-Len) + 1
                 move mg-ZD-valuex(mg-POS:mg-Len)
v00872             to mgBuffer  (Cursor:mg-Len)
v00859         end-if
           end-evaluate
           .
       Move-to-BIN.
           perform Compute-mg-Bytes
           if FldT-Length(FldInx) = 0
               perform Default-Target
           else
      * get and format the source
             perform Get-Numeric-Value
v00859       set Discard-Nothing to true
             move mg-Value-num18  to mg-BN-value
             perform Validate-Buffer-Pointers
             if Discard-Nothing
               compute mg-POS = (length of mg-BN-valuex - mg-Len) + 1
               move mg-BN-valuex(mg-POS:mg-Len)
v00872           to mgBuffer  (Cursor:mg-Len)
             end-if
           end-if
           .
       Move-to-BIS.
           perform Compute-mg-Bytes
           if FldT-Length(FldInx) = 0
               perform Default-Target
           else
      * get and format the source
             perform Get-Numeric-Value
v00854       set Discard-Nothing to true
             move mg-Value-num18s to mg-BN-value
             perform Validate-Buffer-Pointers
             if Discard-Nothing
               compute mg-POS = (length of mg-BN-valuex - mg-Len) + 1
               move mg-BN-valuex(mg-POS:mg-Len)
v00872           to mgBuffer  (Cursor:mg-Len)
             end-if
           end-if
           .
CW0315 Move-to-LS.
CW0315     move FLDT-Length-Byte(FldInx) to mgsLen
CW0315     move FLDT-Start-Byte(FldInx) to mg-POS
CW0315     perform Compute-mg-Bytes
CW0315
CW0315*    'check the type of validation to perform, if any
CW0315     evaluate true
CW0315       when FLDT-Length(FldInx) = 0
CW0315        MOVE 0 TO MG-LS-VALUE
CW0315        compute mg-POS = (length of mg-LS-value - mg-Len) + 1
CW0315        move mg-LS-valuex(1:1)
CW0315             to mgBuffer  (Cursor:1)
CW0315        move mg-LS-valuex(mg-POS + 1:mg-Len - 1)
CW0315             to mgBuffer  (Cursor + 1:mg-Len - 1)
CW0315       when OTHER
CW0315*        'get and format the source
CW0315         perform Get-Numeric-Value
CW0315         if DataValidationNumeric
CW0315         and Discard-Something
CW0315           continue
CW0315         else
CW0315           set Discard-Nothing to true
CW0315           move mg-Value-num18S to mg-LS-value
CW0315           compute mg-POS = (length of mg-LS-value - mg-Len) + 1
CW0315           move mg-LS-valuex(1:1)
CW0315                to mgBuffer  (Cursor:1)
CW0315           move mg-LS-valuex(mg-POS + 1:mg-Len - 1)
CW0315             to mgBuffer  (Cursor + 1:mg-Len - 1)
CW0315         end-if
CW0315     end-evaluate.
CW0315     .
CW0315 Move-to-TS.
CW0315     move FLDT-Length-Byte(FldInx) to mgsLen
CW0315     move FLDT-Start-Byte(FldInx) to mg-POS
CW0315     perform Compute-mg-Bytes
CW0315
CW0315*    'check the type of validation to perform, if any
CW0315     evaluate true
CW0315       when FLDT-Length(FldInx) = 0
CW0315        MOVE 0 TO MG-TS-VALUE
CW0315        compute mg-POS = (length of mg-TS-value - mg-Len) + 1
CW0315        move mg-TS-valuex(mg-POS:mg-Len)
CW0315             to mgBuffer  (Cursor:mg-Len)
CW0315       when OTHER
CW0315         perform Get-Numeric-Value
CW0315         if DataValidationNumeric
CW0315         and Discard-Something
CW0315           continue
CW0315         else
CW0315           set Discard-Nothing to true
CW0315           move mg-Value-num18S to mg-TS-value
CW0315           compute mg-POS = (length of mg-TS-value - mg-Len) + 1
CW0315           move mg-TS-valuex(mg-POS:mg-Len)
CW0315             to mgBuffer  (Cursor:mg-Len)
CW0315         end-if
CW0315     end-evaluate.
CW0315     .
      * This will convert the source date to a different date format
      * and will also format to target's usage.
      * first step is to build a null-terminated date string which
      * represents the source and target formats. Such as:
      * 'MMDDYY{COMP}<45>CCYYMMDD{COMP-3}'
      * Refer to DateC module for more documentation.
v01000 Convert-Date.
           move Pret-Date-Fmt(PretInx) to mgSubS
           move Pret-MIG-Date-Fmt(PretInx) to mgSubT
           move 1 to wPos
           move spaces to mgFMT
      *    ' describe the source format and usage
           move DateFmt-Text(mgSubS)(1:DateFmt-Length(mgSubS))
             to mgFmt(wPos:DateFmt-Length(mgSubS))
           add DateFmt-Length(mgSubs) to wPos
           evaluate true
             when Type-BIN(PretInx)
               move '{COMP}' to mgFMT(wPos:6)
               add 6 to wPos
             when Type-BIS(PretInx)
               move '{COMPS}' to mgFMT(wPos:7)
               add 7 to wPos
             when Type-PD(PretInx)
              and FLDT-is-UnSigned(FldInx)
               move '{COMP-3}' to mgFMT(wPos:8)
               add 8 to wPos
             when Type-PD(PretInx)
              and FLDT-is-Signed(FldInx)
               move '{COMP-3S}' to mgFMT(wPos:9)
               add 9 to wPos
             when Type-ZD(PretInx)
              and FLDT-is-Signed(FldInx)
               move '{S}' to mgFMT(wPos:3)
               add 8 to wPos
             when Type-ZD(PretInx)
               continue
           end-evaluate
      *    ' we may need to add a DATE WINDOW option here (ie. <45)
           move '>' to mgFMT(wPos:1)
           add 1 to wPos
      *    ' describe the target format and usage
           move DateFmt-Text(mgSubT)(1:DateFmt-Length(mgSubT))
             to mgFmt(wPos:DateFmt-Length(mgSubT))
           add DateFmt-Length(mgSubT) to wPos
           evaluate true
             when MIG-Type-BIN(PretInx)
               move '{COMP}' to mgFMT(wPos:6)
               add 6 to wPos
             when MIG-Type-BIS(PretInx)
               move '{COMPS}' to mgFMT(wPos:7)
               add 7 to wPos
             when MIG-Type-PD(PretInx)
              and FLDT-is-UnSigned(FldInx)
               move '{COMP-3}' to mgFMT(wPos:8)
               add 8 to wPos
             when MIG-Type-PD(PretInx)
              and FLDT-is-Signed(FldInx)
               move '{COMP-3S}' to mgFMT(wPos:9)
               add 9 to wPos
             when MIG-Type-ZD(PretInx)
              and FLDT-is-Signed(FldInx)
               move '{S}' to mgFMT(wPos:3)
               add 8 to wPos
             when MIG-Type-ZD(PretInx)
               continue
           end-evaluate
           move '>' to mgFMT(wPos:1)
           add 1 to wPos
           move x'00' to mgFMT(wPos:1)
           add 1 to wPos
      *    ' Get the byte positions and lengths for Source & Target
           move FLDT-Start-Byte(FldInx) to mg-POS
           move FLDT-Length-Byte(FldInx) to mgsLen
      *    ' Do the actual convert of date and put to target buffer
           call 'DateC' using mgFMT
             by reference in-Buffer(mg-POS:mgsLen)
                          mgBuffer (Cursor:mg-Len)
           .

       Move-Buffer-to-record.
           evaluate true
             when FldT-Length(FldInx) = 0
               perform Default-Target

             when NORMAL-vField(PretInx)
             when Occur-vField(PretInx)
             when ODO-vField(PretInx)
               move v-Buffer(mg-POS:mg-Len)
v00872           to mgBuffer  (Cursor:mg-Len)
             when other
*!            move in-Buffer(mg-POS:mg-Len)
v00872          to mgBuffer  (Cursor:mg-Len)
           end-evaluate
           .
      *The source length is zero so move a default to the target.
       Default-Target.
v00872     move spaces to mgBuffer  (Cursor:mg-Len)
           if Pret-Default(PretInx) > 0
      *Have a default value
             add 1 to ms-num-default-value
             move Pret-Default(PretInx) to Default-ptr
             if Default-Hex(default-ptr)
             and Default-Length(default-ptr) = 1
v00872         inspect mgBuffer  (Cursor:mg-Len) converting
                 ' ' to Default-Value(Default-ptr)(1:1)
             else
               move Default-Value(Default-ptr)
v00872           to mgBuffer  (Cursor:mg-Len)
             end-if
           else
      *No default value specified, set to natural default
             add 1 to ms-num-default-natural
             evaluate true
               when MIG-TYPE-CH(PretInx)
                    continue
               when MIG-TYPE-PD(PretInx)
                    compute mg-POS = (10 - mg-Len) + 1
                    move zeroes to mg-PD-value
                    move mg-PD-valuex(mg-POS:mg-Len)
v00872                to mgBuffer  (Cursor:mg-Len)
               when MIG-TYPE-ZD(PretInx)
                    compute mg-POS = (18 - mg-Len) + 1
                    move zeroes     to mg-ZD-value
                    move mg-ZD-valuex(mg-POS:mg-Len)
v00872                to mgBuffer  (Cursor:mg-Len)
               when MIG-TYPE-BIN(PretInx)
               when MIG-TYPE-BIS(PretInx)
               when MIG-TYPE-NIB(PretInx)
               when MIG-TYPE-PD-NEC(PretInx)
               when MIG-TYPE-PD-NEC4(PretInx)
                    compute mg-POS = (8 - mg-Len) + 1
                    move zeroes to mg-BN-value
                    move mg-BN-valuex(mg-POS:mg-Len)
v00872                to mgBuffer  (Cursor:mg-Len)
               when MIG-TYPE-BIT(PretInx)
                    compute mg-POS = (32 - mg-Len) + 1
                    move zeroes     to mg-BIT-value
                    move mg-BIT-valuex(mg-POS:mg-Len)
v00872                to mgBuffer  (Cursor:mg-Len)
CW0315         when MIG-TYPE-LS(PretInx)
CW0316              compute mg-POS =
CW0316                          (length of mg-LS-value - mg-Len) + 1
CW0315              move zeroes     to mg-LS-value
CW0316              move mg-LS-valuex(1:1)
CW0316                to mgBuffer  (Cursor:1)
CW0316              move mg-LS-valuex(mg-POS + 1:mg-Len - 1)
CW0316                to mgBuffer  (Cursor + 1:mg-Len - 1)
CW0315         when MIG-TYPE-TS(PretInx)
CW0316              compute mg-POS =
CW0316                          (length of mg-TS-value - mg-Len) + 1
CW0315              move zeroes     to mg-TS-value
CW0315              move mg-TS-valuex(mg-POS:mg-Len)
CW0315                to mgBuffer  (Cursor:mg-Len)
             end-evaluate
           end-if
           .
      *-----------------------------------------------------------------
      * This will ensure that the various pointers into the buffer
      * areas are valid (i.e., cursor and length of field)
      *-----------------------------------------------------------------
       Validate-Buffer-Pointers.
v0854      move 1 to mgBit-Basis
v0854      compute mg-Len = Pret-MIG-Len(PretInx) / mgBit-Basis
           evaluate true
v00887      when (mg-Len < 1 or > 32768)
              if mgFirst-Overflow = 0
                move 1 to mgFirst-Overflow
                perform ShowOverflow
              end-if
              set Discard-CANT-HOLD-ENTIRE-FIELD to true
            when (cursor < 1 or > out-len)
v00878       and OPT-RecordPrefixAPICall-No
              if mgFirst-Overflow = 0
                move 2 to mgFirst-Overflow
                perform ShowOverflow
              end-if
              set Discard-CANT-HOLD-ENTIRE-FIELD to true
v00872      when (Cursor + mg-Len - 1) > mgMax-Buffer-Len
              if mgFirst-Overflow = 0
                move 3 to mgFirst-Overflow
                perform ShowOverflow
              end-if
              set Discard-CANT-HOLD-ENTIRE-FIELD to true
           end-evaluate
           .
       ShowOverflow.
           set sub to PretInx
           display 'Migrate:******** First Overflow *************'
v00884     display 'Migrate:* Pointer invalid:Prefield=' sub
             ':type=' Pret-MIG-TYPE(PretInx)
             ':cursor=' cursor
             ':len=' mg-len
             ':Stage=' Program-Stage-sw
             ':OV=' mgFirst-Overflow
           display 'Migrate:*  '
             ':OutputEdit=' DLL-OutputEdit-Sw
             ':APICall=' OPT-RecordPrefixAPICall-sw
             ':EEORSize=' OPT-EEORSize
           display 'Migrate:*  '
             ':in-rec#=' WS-REC-CNT
             ':in-len=' IN-LEN
           display 'Migrate:*  '
             ':out-rec#=' ms-rec-out
             ':out-len=' out-len
           display 'Migrate:*************************************'
           .

       Get-Numeric-Value.
      * Value from the buffer/vBuffer and format to 9(18)/s9(18)

           move FLDT-Start-Byte(FldInx) to mg-POS
           evaluate true
             when NORMAL-vField(PretInx)
             when Occur-vField(PretInx)
             when ODO-vField(PretInx)
               evaluate true
                 when Type-Ch (PretInx)
                   if v-Buffer(mg-POS:mg-Bytes) is numeric
                     move v-Buffer(mg-POS:mg-Bytes)
                       to mg-Value-num18
                   else
                     move zeroes                to mg-Value-num18
                   end-if
                   move 'U' to mg-Value(20:1)
                   set Discard-Nothing to true
                 when other
                  move 1 to gbBuffer
                  move Fldt-Start-Byte(FldInx) to gbPos
                  move Fldt-Start-Nib(FldInx)  to gbPosNib
                  move Fldt-Start-Bit(FldInx)  to gbPosBit
                  move Fldt-Length-Byte(FldInx) to gbLen
                  move Fldt-Length-Nib(FldInx)  to gbLenNib
                  move Fldt-Length(FldInx)      to gbLenBit
                  move Pret-Type(PretInx)     to gbType
v0854             move Fldt-Field(FldInx) to gbField
                  perform GetBuff
                  move ws-value to mg-Value
               end-evaluate
             when other
               evaluate true
                 when Type-Ch (PretInx)
                   if in-Buffer(mg-POS:mg-Bytes) numeric
                     move in-Buffer(mg-POS:mg-Bytes) to mg-Value-num18
                   else
                     move zeroes to mg-Value-num18
                   end-if
                   move 'U' to mg-Value(20:1)
                   set Discard-Nothing to true
                 when other
                  move 0 to gbBuffer
                  move Fldt-Start-Byte(FldInx) to gbPos
                  move Fldt-Start-Nib(FldInx)  to gbPosNib
                  move Fldt-Start-Bit(FldInx)  to gbPosBit
                  move Fldt-Length-Byte(FldInx) to gbLen
                  move Fldt-Length-Nib(FldInx)  to gbLenNib
                  move Fldt-Length(FldInx)      to gbLenBit
                  move Pret-Type(PretInx)     to gbType
v0854             move Fldt-Field(FldInx) to gbField
                  perform GetBuff
                  move ws-value to mg-Value
               end-evaluate
           end-evaluate
           move mg-Value(20:1) to FLDT-sign-Sw(FldInx)
           if API-KAMPOYR(PretInx)
             evaluate true
               when FLDT-is-Signed(FldInx)
                    add mgKAMPOYR-value to mg-Value-num18s
               when FLDT-is-Unsigned(FldInx)
                    add mgKAMPOYR-value to mg-Value-num18
             end-evaluate
           end-if
           .
       Compute-mg-Bytes.
           evaluate true
             when Type-BIT (PretInx)
                  compute mg-Bytes = Fldt-Length(FldInx)
             when Type-BIN(PretInx)
             when Type-BIS(PretInx)
                  evaluate Fldt-Length(FldInx)
                    when 1 THRU 2 move 5  to mg-Bytes
                    when 3 THRU 4 move 10 to mg-Bytes
                    when 5 THRU 8 move 18 to mg-Bytes
                  end-evaluate
             when Type-Nib (PretInx)
             when Type-PD-NEC4(PretInx)
                  compute mg-Bytes = Fldt-Length(FldInx) / 4
             when other
                  compute mg-Bytes = Fldt-Length(FldInx) / 8
           end-evaluate
           .

       API-CharCNV-1.
           if FLDT-Length(FldInx) = zeroes
             move 1 to mgBit-Basis
             compute mg-Len = Pret-MIG-Len(PretInx) / mgBit-Basis
             perform Default-Target
             go to AC1-exit
           end-if
           add 1 to CAT-num-api-VGFBCNV1
v00875     move 'IKOUNTOI' to Code-Conv-Process-Class
v00895     evaluate true
v00895       when OPT-CnvDiscard-Yes
v00895         move ' '  to Code-Conv-Request-Option
v00895         move ' '  to one-Byte-Code
v00895         move '  ' to two-Byte-Code
v00895       when OPT-CnvDiscard-No
v00895         move '1'  to Code-Conv-Request-Option
v00895         move ' '  to one-Byte-Code
v00895         move OPT-CnvExceptionCode-Hex to two-Byte-Code
v00895     end-evaluate

           move 1 to mgBit-Basis
v0854      compute mgsLen = Pret-MIG-Len(PretInx) / mgBit-Basis
v0854      if mgsLen = 0 then go to AC1-exit end-if

           move FLDT-Start-Byte(FldInx) to mg-POS
           perform Validate-Buffer-Pointers
           evaluate true
             when NORMAL-vField(PretInx)
             when Occur-vField(PretInx)
             when ODO-vField(PretInx)
v0854         move  v-Buffer(mg-POS:mgsLen)
v0854           to Target-Data-Storage(1:mgsLen)
             when other
v0854         move in-Buffer(mg-POS:mgsLen)
v0854           to Target-Data-Storage(1:mgsLen)
           end-evaluate
v0854      MOVE mgsLen        TO Code-Conv-Target-Data-Length
           COMPUTE Post-Code-Conv-Data-Length =
                      FUNCTION LENGTH(Post-conv-Data-Storage)
           SET Target-Data-Storage-Address
             TO ADDRESS OF Target-Data-Storage
           SET Post-conv-Data-Storage-Address
             TO ADDRESS OF Post-conv-Data-Storage
v00872     move OPT-MaxErrorNumberCodeAPI
v00872       to Maximum-Number-of-error-entry
           CALL KampoAPI   USING Code-Conv-Request-Packet
v00872                           CodeConvErrorInfo
           evaluate true
             when Code-Return-Code = '0000'
                  move Post-conv-Data-Storage(1:mgsLen)
                    to mgBuffer  (Cursor:mgsLen)
                  move mgsLen to mg-Len
             when Code-Return-Code = '9600'
             when Code-Return-Code = '9620'
             when Code-Return-Code = '9690'
             when Code-Return-Code = '9990'
                  set Discard-API-VGFBCNV1-RC-not-00 to true
             when Opt-CnvDiscard-Yes and Code-Return-Code = '9610'
                  set Discard-API-VGFBCNV1-RC-not-00 to true
                  add 1 to CAT-num-api-VGFBCNV1-err
             when Opt-CnvDiscard-No  and Code-Return-Code = '9610'
                  add 1 to CAT-num-api-VGFBCNV1-err
                  perform API-Log-Error
                  move Post-conv-Data-Storage(1:mgsLen)
                    to mgBuffer  (Cursor:mgsLen)
                  move mgsLen to mg-Len
                  move '0000' to Code-Return-Code
             when other
                  set Discard-API-VGFBCNV1-RC-not-00 to true
                  add 1 to CAT-num-api-VGFBCNV1-err
           end-evaluate
           .
       AC1-exit.
           exit.


       API-CharCnv-2.
           if FldT-Length(FldInx) = zeroes
             compute mg-Len = Pret-MIG-Len(PretInx) / mgBit-Basis
             perform Default-Target
             go to AC2-exit
           end-if
           add 1 to CAT-num-api-VGFBCNV1
v00875     move 'IKOUNTOI' to Code-Conv-Process-Class
v00895     evaluate true
v00895       when OPT-CnvDiscard-Yes
v00895         move ' '  to Code-Conv-Request-Option
v00895         move ' '  to one-Byte-Code
v00895         move '  ' to two-Byte-Code
v00895       when OPT-CnvDiscard-No
v00895         move '1'  to Code-Conv-Request-Option
v00895         move ' '  to one-Byte-Code
v00895         move OPT-CnvExceptionCode-Hex to two-Byte-Code
v00895     end-evaluate


      * determine TARGET length
v0854      move FLDT-Length-Byte(FldInx) to mgsLen
v0854      if mgsLen = 0 then go to AC2-exit end-if

+          compute mg-Len2 = mgsLen + 2 + 1
+          compute mg-Len3 = mgsLen + 4
           move FLDT-Start-Byte(FldInx) to mg-POS
           perform Validate-Buffer-Pointers
           evaluate true
             when NORMAL-vField(PretInx)
             when Occur-vField(PretInx)
             when ODO-vField(PretInx)
              move x'3f75'
                to Target-Data-Storage(1:2)
v0854         move  v-Buffer(mg-POS:mgsLen)
v0854           to Target-Data-Storage(3:mgsLen)
              move x'3f76'
                to Target-Data-Storage(mg-Len2:2)
             when other
              move x'3f75'
                to Target-Data-Storage(1:2)
v0854         move in-Buffer(mg-POS:mgsLen)
v0854           to Target-Data-Storage(3:mgsLen)
              move x'3f76'
                to Target-Data-Storage(mg-Len2:2)
           end-evaluate
           MOVE mg-Len3       TO Code-Conv-Target-Data-Length
           COMPUTE Post-Code-Conv-Data-Length =
                      FUNCTION LENGTH(Post-conv-Data-Storage)
           SET Target-Data-Storage-Address
             TO ADDRESS OF Target-Data-Storage
           SET Post-conv-Data-Storage-Address
             TO ADDRESS OF Post-conv-Data-Storage
v00872     move OPT-MaxErrorNumberCodeAPI
v00872       to Maximum-Number-of-error-entry
           CALL KampoAPI   USING Code-Conv-Request-Packet
v00872                           CodeConvErrorInfo
           evaluate true
             when Code-Return-Code = '0000'
                  move 1 to mgBit-Basis
                  compute mg-Len2 = Pret-MIG-Len(PretInx) / mgBit-Basis
                  move Post-conv-Data-Storage(2:mgsLen)
                    to mgBuffer (Cursor:mg-Len2)
                  move mg-Len2 to mg-Len
             when Code-Return-Code = '9600'
             when Code-Return-Code = '9620'
             when Code-Return-Code = '9690'
             when Code-Return-Code = '9990'
                  set Discard-API-VGFBCNV1-RC-not-00 to true
             when Opt-CnvDiscard-Yes and Code-Return-Code = '9610'
                  set Discard-API-VGFBCNV1-RC-not-00 to true
                  add 1 to CAT-num-api-VGFBCNV1-err
             when Opt-CnvDiscard-No  and Code-Return-Code = '9610'
                  add 1 to CAT-num-api-VGFBCNV1-err
                  perform API-Log-Error
                  move 1 to mgBit-Basis
                  compute mg-Len2 = Pret-MIG-Len(PretInx) / mgBit-Basis
                  move Post-conv-Data-Storage(2:mgsLen)
                    to mgBuffer (Cursor:mg-Len2)
                  move mg-Len2 to mg-Len
                  move '0000' to Code-Return-Code
             when other
                  set Discard-API-VGFBCNV1-RC-not-00 to true
                  add 1 to CAT-num-api-VGFBCNV1-err
           end-evaluate
           .
       AC2-exit.
           exit.

v00895* Record the API error onto the LOG file
v00895 API-Log-Error.
           move 'DISCARD'                  to LD-Message
           move spaces                     to LD-Kampo-Extra-Details
           move WS-REC-CNT                 to LD-REC-CNT
           move Fldt-Field(FldInx)         to LD-FIELD
           move PRET-StartofChain(PretInx) to Fldt
           if Pret-Dims(LD-Field) = 0
             move PRET-FIELDNAME(LD-Field) to LD-Field-Name
           else
             move spaces                   to LD-Field-Name
             call 'FmtInd' using
                  FLDT-INDEX-TABLE(FldInx), Pret-Dims(LD-Field), Indice
             string PRET-FIELDNAME(LD-Field) delimited by SPACES
                      Indice                 delimited by SPACES
               into LD-Field-Name
             end-string
           end-if
           move FLDT-Start-Byte(FldInx)    to LD-POS
           move Pret-Length-Byte(LD-FIELD) to LD-Length
           move 11                         to LD-REASON-CODE
           move 'NO-CODE'                  to LD-Message
           move 'API-VGXBCNV9 RC = '       to LDKM-Lit2
           move code-return-code           to LDKM-Data1
           move ' CharCd:'                 to LDKM-Lit3
           move Log-Discard-kampo-message  to LD-Reason-Msg
           if OPT-MaxErrorNumberCodeAPI > 0
             perform varying n from 1 by 1
                       until n > Number-of-error-conversion
               move "x'0000'" to LDKM-Data2x
               move spaces to WS-Hex
               move 2 to tSize
               call 'HEXSHOW' USING
                     CCEI-Character-Code(n)(1:2), tSize, WS-Hex
               move ws-hex(1:4)            to LDKM-Data2(3:4)
               move CCEI-Offset(n)         to LDKM-Data3
               move ' POS:'                to LDKM-Lit1
               write LOG-RECORD from LOG-DISCARD
             end-perform
           else
             move "x'0000'"                to LDKM-Data2x
             move spaces to WS-Hex
             move 2 to tSize
             call 'HEXSHOW' USING
                 Post-Code-Conv-Data-LengthX(1:2), tSize, WS-Hex
             move ws-hex(1:4)              to LDKM-Data2(3:4)
             move Code-Inconvert-Detection-Pos to LDKM-Data3
             move ' POS:'                  to LDKM-Lit1
             write LOG-RECORD from LOG-DISCARD
           end-if
v00895     .
       API-SHIFT-code.
           add 1 to CAT-num-api-SHIFT
           move 1 to mgBit-Basis
           compute mg-Len = Pret-MIG-Len(PretInx) / mgBit-Basis
           move FLDT-Start-Byte(FldInx) to mg-POS
           evaluate in-Buffer(mg-POS:2)
v00872       when x'3F75' move x'0E' to mgBuffer  (Cursor:mg-Len)
v00872       when x'3F76' move x'0F' to mgBuffer  (Cursor:mg-Len)
             when other  Perform Convert-Types-Move
           end-evaluate
           .
       API-KAMPOYR-code.
           add 1 to CAT-num-api-KAMPOYR
           move 1882 to mgKAMPOYR-value
           .
      *End program Migrate.


      *This will determine the length of a field in bits.
      *If the Length is specified, use that length.
      *The only time the length should not be specified is on
      * variable coded fields like varchar or an ODO table.
      * For those fields, it may be necessary to read ahead to
      * find the length field for this field/table. For this
      * the 'getValue' routine is caled.
      *It is possible to return a length of zero with an error.
      * This means the field really has no length, aka no data.
      * Return-code:
      *  00 - all is okay
      *  01 - Warning. The offset length value is zero
      *Parms passed to and fro:
      * pointer to field in question
      *    PRET    PIC 9(6) COMP.
      * return the length
      *    WS-LEN  PIC S9(9) COMP.
      * aka sCursor
      *    sCursor PIC 9(9) COMP.
      *    IN-BUFFER            PIC X(32756).
      *     LNKPRET.
      *     LNKTBL.
      *     LNKFLD.
      *     LnkCnt.
      *     lnkDisc.
      *     LnkOpt.
      *PROCEDURE DIVISION using
      *    PRET, WS-LEN, sCursor
      *    IN-BUFFER
      *    PRE-FIELD-TABLE-AREA
      *    TABLE-TABLE-AREA
      *    FIELD-TABLE-AREA
      *    Counters-and-totals
      *    Discard-Table-Area
      *    Options-in-Effect
      *    .
       GetLen.
           move zeroes to return-code
           move zeroes to ws-len
           evaluate true
              when NORMAL-FIELD(PRET)
              when ODO-FIELD   (PRET)
              when OCCUR-FIELD (PRET)
                if Pret-Length(Pret) > zeroes
v00894          and File-CSV-No(1)
                   move Pret-Length(PRET)   TO WS-LEN
                else
                   perform PI-GET-LENGTH-OF-FIELD
                end-if
              when TABLE-ODO(PRET)
              when TABLE-OCCUR(PRET)
                   perform PI-GET-LENGTH-OF-TABLE
              when other
                 display 'ENGINE:GETLEN:DONT KNOW WHAT TO DO WITH THIS '
                 PRET-ENTRY-TYPE(PRET) ' FIELD TYPE'
                 ':' PRET
           end-evaluate
           .
      * NUMBER - THE LENGTH IS A NUMBER IN ANOTHER FIELD???
      * OFFSET - THE LENGTH IS COMPUTED FROM AN OFFSET CALCULATION
      *        = (addrOf(Len)+Valueof(Len)) - Addrof(Data)
      * VARCHAR - THE LENGTH IS COMPUTED IN THE VALUE OF ANOTHER FIELD
      *        = (VALUE OF(REF FIELD)
       PI-GET-LENGTH-OF-FIELD.
           evaluate true
v00894         when File-CSV-Yes(1)
v00894           perform Get-CSV-Length
               WHEN LENGTH-FIELD-OFFSET(PRET)
                    perform Get-glValueOf-Len
                    compute glValueOf-Len = gl-Value-num18s * 8
                    if glValueOf-Len > 0
                      move FLDT-Start(glFPtr) to glAddrOf-Len
                      move sCursor          to glAddrOf-Data
                      compute ws-len =
                        (glAddrOf-Len + glValueOf-Len) - glAddrOf-Data
                    else
                      move zeroes to ws-len
                      move 1 to return-code
                    end-if
               WHEN LENGTH-FIELD-VARCHAR(PRET)
                    move PRET-LENGTH-FIELD(PRET) to glSearchField
                    if Pret-LastField(glSearchField) > 0
                      move Pret-LastField(glSearchField) to glFPtr
                      move Fldt-LastGhost(glFPtr) to glFPtr
                    else
                      display 'GetLen:varchar length field not gen'
                       ':=' PRET-LENGTH-FIELD(PRET) ':Pret=' Pret
                      move 12 to return-code
                      stop run
                    end-if
                    move glFPtr to gvPtr
                    perform GetValue
                    move ws-value to gl-Value
                    if Discard-Something
                      display 'GetLen:varchar length value not valid'
                       ':=' PRET-LENGTH-FIELD(PRET) ':Pret=' Pret
                       ':value=' gl-Value
                      move 12 to return-code
                      stop run
                    end-if
                    evaluate gl-Value(20:1)
                      when 'S' move gl-Value-NUM18S to WS-LEN
                      when 'U' move gl-Value-NUM18  to WS-LEN
                    end-evaluate
                    compute WS-LEN = WS-LEN * 8
               WHEN LENGTH-FIELD-NUMBER(PRET)
                    MOVE PRET-LENGTH-FIELD(PRET)   TO WS-LEN
           end-evaluate
           .

      *Analyze the in-buffer for the next delimiter
v00894 Get-CSV-Length.
v00894     divide sCursor by 8 giving gbPos remainder gbPosBit
v00894     if gbPosBit = 0 then
v00894       move 8 to gbPosBit
v00894     else
v00894       add 1 to gbPos
v00894     end-if
v00894     perform varying gbEnd from gbPos by 1
v00894       until gbEnd > in-Len
v00894          or in-buffer(gbEnd:FT-Delim-Len(1))
v00894             = FT-Delim(1)(1:FT-Delim-Len(1))
v00894     end-perform
v00894     compute ws-len = (gbEnd - gbPos) * 8
v00894     .

       Get-glValueOf-Len.
           move PRET-LENGTH-FIELD(PRET) to glSearchField
           if Pret-LastField(glSearchField) > 0
              move Pret-LastField(glSearchField) to glFPtr
              move Fldt-LastGhost(glFPtr) to glFPtr
           else
              display 'GetLen:Offset field length not generated'
                ':=' PRET-LENGTH-FIELD(PRET) ':Pret=' Pret
              move 12 to return-code
              stop run
           end-if
           move glFPtr to gvPtr
           perform GetValue
           move ws-value to gl-Value
           if Discard-Something
             display 'GetLen:offset length value not valid'
               ':=' PRET-LENGTH-FIELD(PRET) ':Pret=' Pret
               ':value=' gl-Value
             move 12 to return-code
             stop run
           end-if
           evaluate gl-Value(20:1)
             when 'S' move gl-Value-NUM18S to WS-LEN
             when 'U' move gl-Value-NUM18  to WS-LEN
           end-evaluate
           move WS-LEN   to gl-Value-NUM18S
           .

       PI-GET-LENGTH-OF-TABLE.
      *This will get the length in bits/bytes of the table
      * based on LIMIT or LENGTH parameter and whether we need to
      * offset from a BASE address and if we need to Add/Subtract
      * a constant value. Only LIMIT allows for a COMPUTE.
           MOVE PRET-TABLE(PRET)   TO glTblPtr
           if Table-Limit-Base-Field(glTblPtr) > 0
             perform Set-Base
           else
             move zeroes to gl-Base
           end-if
           move zeroes to gl-Value-num18s
           if (Table-Limit-Set(glTblPtr)
           or  Table-Length-Set(glTblPtr))
           and Table-Limit-No-Compute(glTblPtr)
             perform PI-LimitLength-NoCompute
           end-if
           IF TABLE-LIMIT-SET(glTblPtr) AND
              (TABLE-LIMIT-ADD(glTblPtr) OR
               TABLE-LIMIT-SUBTRACT(glTblPtr))
             perform PI-Limit-Compute
           END-IF
           .
       PI-LimitLength-NoCompute.
           IF TABLE-LIMIT-FIELD1-IS-VALUE(glTblPtr)
             MOVE TABLE-LIMIT-FIELD1(glTblPtr) TO gl-Value-NUM18S
           ELSE
             move TABLE-LIMIT-FIELD1(glTblPtr) to glSearchField
             if Pret-LastField(glSearchField) > 0
                move Pret-LastField(glSearchField) to glFPtr
                move Fldt-LastGhost(glFPtr) to glFPtr
             else
                display 'GetLen:Table Length field not generated'
                ':Length Field=' Table-Limit-Field1(glTblPtr)
                ':Pret=' Pret
                move 12 to return-code
                stop run
             end-if
             move glFPtr to gvPtr
             perform GetValue
             move ws-value to gl-Value
             IF Discard-Something
                display 'GetLen:Length of Table value invalid'
                ':Length Field=' PRET-LENGTH-FIELD(PRET) ':Pret=' Pret
                 ':' gl-Value
                move 12 to return-code
                stop run
             END-IF
             evaluate gl-Value(20:1)
               when 'S' move gl-Value-NUM18S to WS-LEN
               when 'U' move gl-Value-NUM18    to WS-LEN
             end-evaluate
             move WS-LEN            TO gl-Value-NUM18s
           end-if
           move gl-Value-num18s to glEditNumber
           if Table-Limit-Set(glTblPtr)
             COMPUTE WS-LEN =
                  (((gl-Value-NUM18S * 8) + gl-Base) - sCursor)
           end-if
           if Table-Length-Set(glTblPtr)
             compute ws-len = gl-Value-num18s * 8
           end-if
           move ws-len to TR-NUM9AS
           if ws-len < 0 then move 0 to ws-len end-if
           .
       PI-Limit-Compute.
           IF TABLE-LIMIT-FIELD1-IS-VALUE(glTblPtr)
             MOVE TABLE-LIMIT-FIELD1(glTblPtr) TO gl-Value-NUM18s
           else
             move TABLE-LIMIT-FIELD1(glTblPtr) to glSearchField
             if Pret-LastField(glSearchField) > 0
                move Pret-LastField(glSearchField) to glFPtr
                move Fldt-LastGhost(glFPtr) to glFPtr
             else
                display 'GetLen:Table Limit field not generated'
                  ':' TABLE-LIMIT-FIELD1(glTblPtr)
                  ':Tbl=' glTblPtr
                move 12 to return-code
                stop run
             end-if
             move glFPtr to gvPtr
             perform GetValue
             move ws-value to gl-Value
             if Discard-Something
                display 'GetLen:Table Limit value invalid'
                  ':' TABLE-LIMIT-FIELD1(glTblPtr)
                  ':Tbl=' glTblPtr
                  ':' gl-Value
                move 12 to return-code
                stop run
             end-if
           END-IF
           compute gl-Number1 = gl-Value-NUM18S * 8
           IF TABLE-LIMIT-FIELD2-IS-VALUE(glTblPtr)
             MOVE TABLE-LIMIT-FIELD2(glTblPtr) TO gl-Value-NUM18s
           ELSE
             move TABLE-LIMIT-FIELD2(glTblPtr) to glSearchField
             if Pret-LastField(glSearchField) > 0
                move Pret-LastField(glSearchField) to glFPtr
                move Fldt-LastGhost(glFPtr) to glFPtr
             else
                DISPLAY 'GetLen: Table limit field2 not generated'
                ':FIELD=' TABLE-LIMIT-FIELD2(glTblPtr)
                ':Pret=' Pret
                move 12 to return-code
                STOP RUN
             END-IF
             move glFPtr to gvPtr
             perform GetValue
             move ws-value to gl-Value
             IF Discard-Something
                DISPLAY 'ENGINE:GETLEN:ARRGH! Get-Length-of-Table3'
                 ':' gl-Value
                STOP RUN
             END-IF
             evaluate gl-Value(20:1)
               when 'S' move gl-Value-NUM18S to WS-LEN
               when 'U' move gl-Value-NUM18    to WS-LEN
             end-evaluate
             move WS-LEN to gl-Value-NUM18s
           end-if
           compute gl-Number2 = gl-Value-NUM18S * 8
           IF TABLE-LIMIT-ADD(glTblPtr)
             COMPUTE WS-LEN =
              ((gl-Number1 + gl-Number2) - sCursor)
           ELSE
             COMPUTE WS-LEN =
              ((gl-Number1 - gl-Number2) - sCursor)
           END-IF
           if ws-len < 0 then move 0 to ws-len end-if
           .
      * Set the Base address reference by searching FLDT table.
       Set-Base.
           move Table-Limit-Base-FIELD(glTblPtr) to glSearchField
           if Pret-LastField(glSearchField) > 0
              move Pret-LastField(glSearchField) to glFPtr
              move Fldt-LastGhost(glFPtr) to glFPtr
           else
              DISPLAY 'getlen:SET-BASE:not found:Pret=' pret
              move 12 to return-code
              stop run
           end-if
           if FLDT-START(glFPtr) = 0
             display 'engine:getlen:set-base start position not set'
             move 12 to return-code
             stop run
           end-if
           compute gl-Base = FLDT-START(glFPtr)
           .
      *END PROGRAM GETLEN.
      *--------------------------------------------------------------
      *This will grab the btyes from buffer.
      *INPUT:IN-BUFFER
      *     :gvPtr; POINTER TO WHICH FIELD TO GET
      *     :FLDT-START(); START BYTE
      *     :FLDT-LENGTH(); LENGTH OF BYTES
      *     :FLDT-FIELD(); POINTER TO PREFIELD TALBE
      *OUTPUT: WS-VALUE; HUMAN READABLE VIEW OF THE DATA
      *      : Discard-Code; IF FIELD IS IN ERROR Code > 0
      *           Code=1 NUMERIC FIELD IS NOT NUMERIC
      *--------------------------------------------------------------
      * This are the fields based to this routine
      *    gvPtr     PIC 9(5) COMP.
      *    WS-VALUE  PIC X(256).
      *    IN-BUFFER PIC X(32756).
      *     LNKPRET.
      *     LNKFLD.
      *     LnkCnt.
      *     LnkDisc.
      *     LnkOpt.
      *--------------------------------------------------------------
      *PROCEDURE DIVISION USING gvPtr, WS-VALUE
      *    IN-BUFFER
      *    PRE-FIELD-TABLE-AREA
      *    FIELD-TABLE-AREA
      *    Counters-and-totals
      *    Discard-Table-Area
      *    Options-in-Effect
      *    .
       GetValue.
           set Discard-Nothing to true
           move SPACES TO WS-VALUE
           if gvPtr = ZEROES
             display 'engine:GetValue error: no gvPtr'
             move 12 to return-code
             stop run
           end-if
           move 0 to gbBuffer
           if Is-a-vField(gvPtr)
             move 1 to gbBuffer
           end-if
           move Fldt-Start-Byte(gvPtr) to gbPos
           move Fldt-Start-Nib(gvPtr)  to gbPosNib
           move Fldt-Start-Bit(gvPtr)  to gbPosBit
           move Fldt-Length-Byte(gvPtr) to gbLen
           move Fldt-Length-Nib(gvPtr)  to gbLenNib
           move Fldt-Length(gvPtr)      to gbLenBit
           move Fldt-Field(gvPtr) to gvPret
           move Pret-Type(gvPret)       to gbType
v0854      move gvPret to gbField
           perform GetBuff
           IF Discard-Something
             if DataValidationNumeric
               ADD 1 TO FIELDS-IN-ERROR
               if First-Field-in-Error = 0
                 move gvPtr to First-Field-in-Error
                 move Discard-Code to First-Fields-Error
               end-if
             end-if
           END-IF
           .
      *END PROGRAM GETVALUE.

      *THIS WILL GRAB from buffer AND FORMAT TO HUMAN READABLE FORMAT.
      *INPUT
      *  :IN-BUFFER
      *  :Position in buffer in byte basis
      *     :Byte location
      *     :Nibble (within the first byte)
      *     :Bit (within the first byte)
      *  :Length in buffer
      *     :Bytes
      *     :Nibble
      *     :Bit
      *  :Type
      *OUTPUT: ws-Value; HUMAN READABLE VIEW OF THE DATA
      *      : RETURN-CODE; IF FIELD IS IN ERROR RC>0
      *           RC=1 NUMERIC FIELD IS NOT NUMERIC
      *           RC=3 not starting at bit 1 of byte type field
      *           RC=4 length not all 8 bits of byte type field
      *     LnkDisc.
      *PROCEDURE DIVISION USING gbPos, gbLen, gbType,
      *    ws-Value
      *    IN-BUFFER
      *    Discard-Table-Area
      *    .
       GetBuff.
           set Discard-Nothing to true
           EVALUATE TRUE
             WHEN gbTYPE-CH
              evaluate true
               when gbPosBit not = 1
                 set Discard-More-DLL-than-Data to true
               when other
                if gbBuffer = 0
v00858            move in-buffer(gbPos:gbLen) TO ws-Value
                else
v00858            move v-buffer(gbPos:gbLen) TO ws-Value
                end-if
              end-evaluate

      *come back to bits
             WHEN gbTYPE-BIT
                 if gbBuffer = 0
                   move In-Buffer(gbPos:1) to gb-Index-2
                 else
                   move  v-Buffer(gbPos:1) to gb-Index-2
                 end-if
                 move low-value to gb-index-1
                 add 1 to gb-Index
                 move BT-Bits(gb-Index)(gbPosBit:gbLenBit) to ws-Value

             WHEN gbTYPE-ZD
               evaluate true
                 when gbPosBit not = 1
                   set Discard-More-DLL-than-Data to true
                 when other
                   move zeroes          TO gb-Value-NUM18s
                   if gbBuffer = 0
                     move in-Buffer((gbPos + gbLen) - 1:1)
                       to gbLast-Byte
                   else
                     move  v-Buffer((gbPos + gbLen) - 1:1)
                       to gbLast-Byte
                   end-if
      *            'check valid ZD sign byte. It is either:
      *            ' Unsigned
      *            ' Signed Positive
      *            ' Signed Negative
                   evaluate true
MFMFMF               when gbLast-Byte >= x'F0' and <= x'F9'
PCPCPC*              when gbLast-Byte >= x'30' and <= x'39'
                          move gbNot-Signed to gbSignSignal
                          if gbBuffer = 0
                            call 'TODISP' using gbUSAGE-ZD,
                                  IN-BUFFER(gbPos:gbLen),
                                  gb-Value-NUM18, gbSignSignal
                                  gbLen
                          else
                            call 'TODISP' using gbUSAGE-ZD,
                                   v-BUFFER(gbPos:gbLen),
                                  gb-Value-NUM18, gbSignSignal
                                  gbLen
                          end-if
MFMFMF               when gbLast-Byte >= x'C0' and <= x'C9'
MFMFMF               when gbLast-Byte >= x'D0' and <= x'D9'
PCPCPC*              when gbLast-Byte >= x'30' and <= x'39'
PCPCPF*              when gbLast-Byte >= x'70' and <= x'79'
                          move gbSigned to gbSignSignal
                          if gbBuffer = 0
                            call 'TODISP' using gbUSAGE-ZD,
                                  IN-BUFFER(gbPos:gbLen),
                                  gb-Value-NUM18s, gbSignSignal
                                  gbLen
                          else
                            call 'TODISP' using gbUSAGE-ZD,
                                   v-BUFFER(gbPos:gbLen),
                                  gb-Value-NUM18s, gbSignSignal
                                  gbLen
                          end-if
                     when other
                          move 1 to return-code
                   end-evaluate
                   if Return-Code > 0
                     set Discard-NOT-NUMERIC-FIELD to true
                     move zeroes     TO gb-Value-NUM18s
v0850                move gb-Value-area(1:20) to ws-Value(1:20)
                     move 'S' to ws-Value(20:1)
                   else
v0850                move gb-Value-area(1:20) to ws-Value(1:20)
                     evaluate gbSignSignal
                       when gbSigned
                         move 'S' to ws-Value(20:1)
                       when gbNot-signed
                         move 'U' to ws-Value(20:1)
                     end-evaluate
                   end-if
               end-evaluate

             WHEN gbTYPE-PD
               evaluate true
                 when gbPosBit not = 1
                   set Discard-More-DLL-than-Data to true
                 when other
                   if gbBuffer = 0
                     move in-Buffer((gbPos + gbLen) - 1:1)
                       to gbLast-Byte
                   else
                     move  v-Buffer((gbPos + gbLen) - 1:1)
                       to gbLast-Byte
                   end-if
                   compute gbLast-Byte-Num = (gbLast-Byte-Num * 16)
                   evaluate true
                     when Byte-Unsigned
                       move gbNot-Signed to gbSignSignal
                       if gbBuffer = 0
                         call 'TODISP' using gbUSAGE-PD,
                                  IN-BUFFER(gbPos:gbLen),
                                  gb-Value-NUM18S, gbSignSignal
                                  gbLen
                       else
                         call 'TODISP' using gbUSAGE-PD,
                                   v-BUFFER(gbPos:gbLen),
                                  gb-Value-NUM18S, gbSignSignal
                                  gbLen
                       end-if
                     when Byte-Positive
                       or Byte-Negative
                       move gbSigned to gbSignSignal
                       if gbBuffer = 0
                         call 'TODISP' using gbUSAGE-PD,
                                  IN-BUFFER(gbPos:gbLen),
                                  gb-Value-NUM18S, gbSignSignal
                                  gbLen
                       else
                         call 'TODISP' using gbUSAGE-PD,
                                   v-BUFFER(gbPos:gbLen),
                                  gb-Value-NUM18S, gbSignSignal
                                  gbLen
                       end-if
                     when other
                       move 1 to return-code
                   end-evaluate
                   if Return-Code > 0
                     set Discard-NOT-NUMERIC-FIELD to true
                     move zeroes     to gb-Value-NUM18S
v0850                move gb-Value-area(1:20) to ws-Value(1:20)
                     move 'S'        to ws-Value(20:1)
                   else
v0850                move gb-Value-area(1:20) TO ws-Value(1:20)
                     evaluate gbSignSignal
                       when gbNot-Signed move 'U' to ws-Value(20:1)
                       when gbSigned  move 'S' to ws-Value(20:1)
                     end-evaluate
                   end-if
               end-evaluate

             WHEN gbTYPE-BIN
               evaluate true
                 when gbPosBit not = 1
                   set Discard-More-DLL-than-Data to true
                 when other
                   MOVE ZEROES          TO gb-Value-NUM18
                   if gbBuffer = 0
                     CALL 'TODISP'         USING gbUSAGE-BIN
                                  IN-BUFFER(gbPos:gbLen),
                                   gb-Value-NUM18, gbNOT-SIGNED
                                   gbLen
                   else
                     CALL 'TODISP'         USING gbUSAGE-BIN
                                   v-BUFFER(gbPos:gbLen),
                                   gb-Value-NUM18, gbNOT-SIGNED
                                   gbLen
                   end-if
                   IF Return-Code > 0
                     set Discard-NOT-NUMERIC-FIELD to true
                     MOVE ZEROES TO gb-Value-NUM18
                   END-IF
v0850              move gb-Value-area(1:20) to ws-Value(1:20)
                   move 'U'        to ws-Value(20:1)
               end-evaluate

             WHEN gbTYPE-BIS
               evaluate true
                 when gbPosBit not = 1
                   set Discard-More-DLL-than-Data to true
                 when other
                   MOVE ZEROES          TO gb-Value-NUM18S
                   if gbBuffer = 0
                     CALL 'TODISP'         USING gbUSAGE-BIN
                                  IN-BUFFER(gbPos:gbLen),
                                   gb-Value-NUM18S, gbSIGNED
                                   gbLen
                   else
                     CALL 'TODISP'         USING gbUSAGE-BIN
                                   v-BUFFER(gbPos:gbLen),
                                   gb-Value-NUM18S, gbSIGNED
                                   gbLen
                   end-if
                   IF Return-Code > 0
                     set Discard-NOT-NUMERIC-FIELD to true
                     MOVE ZEROES   TO gb-Value-NUM18S
                   END-IF
v0850              move gb-Value-area(1:20) TO ws-Value(1:20)
                   move 'S'        to ws-Value(20:1)
               end-evaluate

             WHEN gbTYPE-PD-NEC
               evaluate true
                 when gbPosBit not = 1
                   set Discard-More-DLL-than-Data to true
                 when other
                   if gbBuffer = 0
                     call 'HEXSHOW' USING
                        IN-BUFFER(gbPos:gbLen), gbLen, gb-Hex
                   else
                     call 'HEXSHOW' USING
                         v-BUFFER(gbPos:gbLen), gbLen, gb-Hex
                   end-if
v00860             if gb-Hex(1:gbLen * 2) not numeric
                     set Discard-NOT-NUMERIC-FIELD to true
                     MOVE ZEROES  TO gb-Value-NUM18
                   else
v00860               MOVE gb-Hex(1:gbLen * 2) TO gb-Value-NUM18S
                   end-if
v0850              move gb-Value-area(1:20) TO ws-Value(1:20)
                   move 'S'       to ws-Value(20:1)
               end-evaluate

             WHEN gbTYPE-PD-NEC4
              evaluate true
               when gbPosBit = 1 or 5
                COMPUTE gbwLen = gbLen + 1
                if gbBuffer = 0
                 CALL 'HEXSHOW' USING
                          IN-BUFFER(gbPos:gbwLen), gbwLen, gb-Hex
                else
                 CALL 'HEXSHOW' USING
                           v-BUFFER(gbPos:gbwLen), gbwLen, gb-Hex
                end-if
                MOVE gb-Hex(gbPosNib:gbLenNib) TO gb-Value-NUM18
                IF gb-Value-NUM18 NOT NUMERIC
                  set Discard-NOT-NUMERIC-FIELD to true
                  MOVE ZEROES      TO gb-Value-NUM18
                END-IF
v0850           move gb-Value-area(1:20) to ws-Value(1:20)
                move 'U'             to ws-Value(20:1)
               when other
                set Discard-More-DLL-than-Data to true
              end-evaluate

           END-EVALUATE
           .
      *END PROGRAM GetBuff.

       Allocate-Table-Space.
      *---------------------------------------------------------------
      * This will create a storage area unto which all major tables
      * will be allocated from (heap space).
      * This will be done by using the Dynamic Storage Callable Services
      * : CEECRHP - Create user heap
      * : CEEGTST - obtain storage from user heap
      *---------------------------------------------------------------
           move 0 TO HEAPID
           move 1 TO HPSIZE
           move 0 TO INCR
           move 0 TO OPTS
MFMFMF     call "CEECRHP" using HEAPID, HPSIZE, INCR, OPTS, FC.
MFMFMF     if not CEE000 of FC THEN
MFMFMF       display 'Engine:Allocate Table Space error:CEECRHP:FC=' FC
MFMFMF       move 16 to return-code
MFMFMF       stop run
MFMFMF     end-if
      *--Pre-Field-Table-Area (Pret)
      *    'always allocate at least one entry
           if Opt-DefinedFields = 0
             move 1 to Opt-DefinedFields
           end-if
           compute NBytes = (length of PreField-Table(1)
                          * Opt-DefinedFields) + 12
MFMFMF     call "CEEGTST" using HEAPID, NBYTES, ADDRSS, FC
MFMFMF     IF not CEE000 of FC THEN
MFMFMF       display 'Engine:Allocate Error:PRET:CEEGTST:FC=' FC
MFMFMF       move 16 to return-code
MFMFMF       stop run
MFMFMF     end-if
MFMFMF     set ADDRESS OF Pre-Field-Table-area to Addrss
PCPCPC*    set ADDRESS OF Pre-Field-Table-area
PCPCPC*        ADDRESS OF Storage-Pret
           move Opt-DefinedFields to PREFIELD-MAX
           move 0      to PREFIELD-PTR
           move 0      to PREFIELD-CNT
           move 'Pre-Field-Table'  to DMA-Table
           move NBytes             to DMA-Bytes
           move PreField-Max       to DMA-Entries
           display Display-Memory-Allocated
      *--Pre-Chain-Table-Area (chain fields for Pret)
      *    'always allocate at least one entry
           if Opt-DefinedFields = 0
             move 1 to Opt-DefinedFields
           end-if
           compute NBytes = (length of PreChain-Table(1)
                          * Opt-DefinedFields)
MFMFMF     call "CEEGTST" using HEAPID, NBYTES, ADDRSS, FC
MFMFMF     IF not CEE000 of FC THEN
MFMFMF       display 'Engine:Allocate Error:PreChain:CEEGTST:FC=' FC
MFMFMF       move 16 to return-code
MFMFMF       stop run
MFMFMF     end-if
MFMFMF     set ADDRESS OF Pre-Chain-Table-area to Addrss
PCPCPC*    set ADDRESS OF Pre-Chain-Table-area
PCPCPC*        ADDRESS OF Storage-Pre-Chain
           move 'Pre-Chain-Table'  to DMA-Table
           move NBytes             to DMA-Bytes
           move PreField-Max       to DMA-Entries
           display Display-Memory-Allocated
      *--Table-Table-Area (Tbl)
      *    'always allocate at least one entry
           if Opt-DefinedTables = 0
             move 1 to Opt-DefinedTables
           end-if
           compute NBytes = (length of Table-Table(1)
                          * Opt-DefinedTables) + 12
MFMFMF     call "CEEGTST" using HEAPID, NBYTES, ADDRSS, FC
MFMFMF     IF not CEE000 of FC THEN
MFMFMF       display 'Engine:Allocate Error:TBL:CEEGTST:FC=' FC
MFMFMF       move 16 to return-code
MFMFMF       stop run
MFMFMF     end-if
MFMFMF     set ADDRESS OF Table-Table-area to Addrss
PCPCPC*    set ADDRESS OF Table-Table-area
PCPCPC*        ADDRESS OF Storage-Table
           move Opt-DefinedTables to Table-Max
           move 0      to Table-Ptr
           move 0      to Table-Cnt
           move 'Table-Table    '  to DMA-Table
           move NBytes             to DMA-Bytes
           move Table-Max          to DMA-Entries
           display Display-Memory-Allocated
      *--Field-Table-Area (FLD)
      *    'always allocate at least one entry
           if Opt-MaxRecordFields = 0
              move 1 to Opt-MaxRecordFields
           end-if
           compute NBytes = (length of Field-Table(1)
                          * Opt-MaxRecordFields) + 8
MFMFMF     call "CEEGTST" using HEAPID, NBYTES, ADDRSS, FC
MFMFMF     IF not CEE000 of FC THEN
MFMFMF       display 'Engine:Allocate Error:FLD:CEEGTST:FC=' FC
MFMFMF       move 16 to return-code
MFMFMF       stop run
MFMFMF     end-if
MFMFMF     set ADDRESS OF Field-Table-area to Addrss
PCPCPC*    set ADDRESS OF Field-Table-area
PCPCPC*        ADDRESS OF Storage-Fld
           move Opt-MaxRecordFields to FIELD-MAX
           move 0      to FIELD-CNT
           move 'Field-Table    '  to DMA-Table
           move NBytes             to DMA-Bytes
           move Field-Max          to DMA-Entries
           display Display-Memory-Allocated
      *--Cond-Table-Area  (COND)
      *    'always allocate at least one entry
           if Opt-DefinedConds = 0
              move 1 to Opt-DefinedConds
           end-if
           compute NBytes = (length of Cond-Table(1)
                             * Opt-DefinedConds) + 20
MFMFMF     call "CEEGTST" using HEAPID, NBYTES, ADDRSS, FC
MFMFMF     IF not CEE000 of FC THEN
MFMFMF       display 'Engine:Allocate Error:COND:CEEGTST:FC=' FC
MFMFMF       move 16 to return-code
MFMFMF       stop run
MFMFMF     end-if
MFMFMF     set ADDRESS OF Cond-Table-area to Addrss
PCPCPC*    set ADDRESS OF Cond-Table-area
PCPCPC*        ADDRESS OF Storage-Cond
           move Opt-DefinedConds to Cond-MAX
           move 0      to Cond-CNT
           move 0      to Cond-Ptr
           move 0      to TotalExecutionQty
           move 0      to TotalEntryQty
           move 'Cond-Table     '  to DMA-Table
           move NBytes             to DMA-Bytes
           move Cond-Max           to DMA-Entries
           display Display-Memory-Allocated
      *--Func-Table-Area  (Func)
      *    'always allocate at least one entry
           if Opt-DefinedFuncs = 0
              move 1 to Opt-DefinedFuncs
           end-if
           compute NBytes = (length of Func-Table(1)
                             * Opt-DefinedFuncs) + 12
MFMFMF     call "CEEGTST" using HEAPID, NBYTES, ADDRSS, FC
MFMFMF     IF not CEE000 of FC THEN
MFMFMF       display 'Engine:Allocate Error:FUNC:CEEGTST:FC=' FC
MFMFMF       move 16 to return-code
MFMFMF       stop run
MFMFMF     end-if
MFMFMF     set ADDRESS OF Func-Table-area to Addrss
PCPCPC*    set ADDRESS OF Func-Table-area
PCPCPC*        ADDRESS OF Storage-Func
           move Opt-DefinedFuncs to Func-MAX
           move 0      to Func-CNT
           move 0      to Func-Ptr
           move 'Func-Table     '  to DMA-Table
           move NBytes             to DMA-Bytes
           move Func-Max           to DMA-Entries
           display Display-Memory-Allocated
      *--Formula-Area
      *    'always allocate at least one entry
           if Opt-DefinedOps   = 0
              move 1 to Opt-DefinedOps
           end-if
           compute N      = length of m-FAS + 8
                          + (length of FAEntries(1) * Opt-DefinedOps)
              on size error
                display 'Engine:Error computing FAEntries'
                 ':#Ops=' Opt-DefinedOps
                 ':len=' length of FAEntries(1)
                 ':FASlen=' length of m-FAS
                 ':Nbytes=' N
                move 12 to return-code
                stop run
           end-compute
           if N > 4294967295
             compute NLimit = (4294967295 - (Length of m-FAS + 8))
                               / length of FAEntries(1)
             display 'Engine:Current technical restriction limiting'
              ' the allocation of Formula-Array to ' NLimit
             move 12 to return-code
             stop run
           end-if
           move N to NBytes
MFMFMF     call "CEEGTST" using HEAPID, NBYTES, ADDRSS, FC
MFMFMF     IF not CEE000 of FC THEN
MFMFMF       display 'Engine:Allocate Error:OPS :CEEGTST:FC=' FC
MFMFMF       move 16 to return-code
MFMFMF       stop run
MFMFMF     end-if
MFMFMF     set ADDRESS OF Formula-Area    to Addrss
PCPCPC*    set ADDRESS OF Formula-Area
PCPCPC*        ADDRESS OF Storage-Formula
           move Opt-DefinedOps to UBoundFAEntries
           move 'Formula-Area   '  to DMA-Table
           move NBytes             to DMA-Bytes
           move Opt-DefinedOps     to DMA-Entries
           display Display-Memory-Allocated
      *--Formula-Execution-Area
           compute NBytes = length of FAExecutionList(1)
                          * Opt-DefinedOps
MFMFMF     call "CEEGTST" using HEAPID, NBYTES, ADDRSS, FC
MFMFMF     IF not CEE000 of FC THEN
MFMFMF       display 'Engine:Allocate Error:OPS2:CEEGTST:FC=' FC
MFMFMF       move 16 to return-code
MFMFMF       stop run
MFMFMF     end-if
MFMFMF     set ADDRESS OF Formula-Execution-Area to Addrss
PCPCPC*    set ADDRESS OF Formula-Execution-Area
PCPCPC*        ADDRESS OF Storage-FormExec
           move Opt-DefinedOps to UBoundFAExecutionList
           move 'Formula-Execution-Area'  to DMA-Table
           move NBytes             to DMA-Bytes
           move Opt-DefinedOps     to DMA-Entries
           display Display-Memory-Allocated
v00860*--Log-Table-Area (Log)
      *    'always allocate at least one entry
           if Opt-DefinedFields = 0
             move 1 to Opt-DefinedFields
           end-if
v00878*    ' presume 100 log entries per field (to handle occurs)
v00878     compute NBytes = (length of Log-Table(1)
v00878                    * (Opt-DefinedFields * 100)) + 12
MFMFMF     call "CEEGTST" using HEAPID, NBYTES, ADDRSS, FC
MFMFMF     IF not CEE000 of FC THEN
MFMFMF       display 'Engine:Allocate Error:LOG:CEEGTST:FC=' FC
MFMFMF       move 16 to return-code
MFMFMF       stop run
MFMFMF     end-if
MFMFMF     set ADDRESS OF Log-Table-area to Addrss
PCPCPC*    set ADDRESS OF Log-Table-Area
PCPCPC*        ADDRESS OF Storage-Log
v00878     compute Log-Max = Opt-DefinedFields * 100
           move 0      to Log-Ptr
           move 0      to Log-Cnt
v00878     move 0      to Log-Highest-Used
           move 'Log-Table'        to DMA-Table
           move NBytes             to DMA-Bytes
           move Log-Max            to DMA-Entries
           display Display-Memory-Allocated

      *    ' Allocate the table space for the Alt1 File by reading
      *    '  and counting the number of non-comment records. We'll add
      *    '  that count with the count of AltName1 options found on the
      *    '  DLL cards (from the SumDLL routine).
           move 0 to Tally
v00890     if Opt-UseAlt-Yes
             open input Alt1-File
             perform until IO-STATUS not = '00'
               read Alt1-File
                 at end move '10' to IO-STATUS
                 not at end
                   if Alt1-File-Record(1:1) not = '*'
                   and Alt1-File-Record(1:2) not = '/*'
                     add 1 to Tally
                   end-if
               end-read
             end-perform
             close Alt1-File
           end-if
           if Tally = 0 then
             move 1 to Tally
           end-if
v00900*--Alt1-Table-Area (Alt1)
           compute NBytes =
           (length of Alt1-Table(1) * (Tally + OPT-Alt1-DLL-CNT)) + 12
MFMFMF     call "CEEGTST" using HEAPID, NBYTES, ADDRSS, FC
MFMFMF     IF not CEE000 of FC THEN
MFMFMF         display 'Engine:Allocate Error:Alt1:CEEGTST:FC=' FC
MFMFMF         move 16 to return-code
MFMFMF         stop run
MFMFMF     end-if
MFMFMF     set ADDRESS OF Alt1-Table-area to Addrss
PCPCPC*    set ADDRESS OF Alt1-Table-area
PCPCPC*        ADDRESS OF Storage-Alt1
           move high-values to Alt1-Table-Area
           move 0    to Alt1-PTR
           move Tally to Alt1-Cnt
           compute Alt1-Total = Alt1-Cnt + OPT-Alt1-DLL-CNT
           move 'Alt1-Table'       to DMA-Table
           move NBytes             to DMA-Bytes
           move Alt1-Total         to DMA-Entries
           display Display-Memory-Allocated

      *    ' Allocate the table space for the Alt2 File by reading
      *    '  and counting the number of non-comment records. We'll add
      *    '  that count with the count of AltName1 options found on the
      *    '  DLL cards (from the SumDLL routine).
           move 0 to Tally
v00890     if Opt-UseAlt-Yes
             open input Alt2-File
             perform until IO-STATUS not = '00'
               read Alt2-File
                 at end move '10' to IO-STATUS
                 not at end
                   if Alt2-File-Record(1:1) not = '*'
                   and Alt2-File-Record(1:2) not = '/*'
                     add 1 to Tally
                   end-if
               end-read
             end-perform
             close Alt2-File
           end-if
           if Tally = 0 then
             move 1 to Tally
           end-if
v00900*--Alt2-Table-Area (Alt2)
           compute NBytes =
           (length of Alt2-Table(1) * (Tally + OPT-Alt2-DLL-Cnt)) + 12
MFMFMF     call "CEEGTST" using HEAPID, NBYTES, ADDRSS, FC
MFMFMF     IF not CEE000 of FC THEN
MFMFMF         display 'Engine:Allocate Error:Alt2:CEEGTST:FC=' FC
MFMFMF         move 16 to return-code
MFMFMF         stop run
MFMFMF     end-if
MFMFMF     set ADDRESS OF Alt2-Table-area to Addrss
PCPCPC*    set ADDRESS OF Alt2-Table-area
PCPCPC*        ADDRESS OF Storage-Alt2
           move high-values to Alt2-Table-Area
           move 0    to Alt2-PTR
           move Tally to Alt2-Cnt
           compute Alt2-Total = Alt2-Cnt + OPT-Alt2-DLL-CNT
           move 'Alt2-Table'       to DMA-Table
           move NBytes             to DMA-Bytes
           move Alt2-Total         to DMA-Entries
           display Display-Memory-Allocated
           .
      *end Allocate-Table-Space.

       Allocate-DAR-Space.
      *    'the size is determined by the PRET fields.
      *    'if DAR is NOT requested only 1 occurrence worth of bytes
      *    ' will be allocated.
      *--DAR-Table        (DAR)
           if DAR-Requested
             compute NBytes = length of dTable(1)
                            * Opt-DefinedFields
             move Opt-DefinedFields to N
           else
             compute NBytes = Length of dTable(1)
             move 1 to N
           end-if
MFMFMF     call "CEEGTST" using HEAPID, NBYTES, ADDRSS, FC
MFMFMF     IF not CEE000 of FC THEN
MFMFMF       display 'Engine:Allocate Error:DAR :CEEGTST:FC=' FC
MFMFMF       move 16 to return-code
MFMFMF       stop run
MFMFMF     end-if
MFMFMF     set ADDRESS OF DAR-Table       to Addrss
PCPCPC*    set ADDRESS OF DAR-Table
PCPCPC*        ADDRESS OF Storage-DAR
           move 'DAR-Table      '  to DMA-Table
           move NBytes             to DMA-Bytes
           move N                  to DMA-Entries
           display Display-Memory-Allocated
      *--Sample-Table
           if DAR-Requested
             compute NBytes = 4 + (length of sTable(1)
                                   * Opt-DefinedSamples)
             compute N = Opt-DefinedSamples
           else
             compute NBytes = 4 + length of sTable(1)
             move 1 to N
           end-if
MFMFMF     call "CEEGTST" using HEAPID, NBYTES, ADDRSS, FC
MFMFMF     IF not CEE000 of FC THEN
MFMFMF       display 'Engine:Allocate Error:DARS:CEEGTST:FC=' FC
MFMFMF       move 16 to return-code
MFMFMF       stop run
MFMFMF     end-if
MFMFMF     set ADDRESS OF Sample-Table    to Addrss
PCPCPC*    set ADDRESS OF Sample-Table
PCPCPC*        ADDRESS OF Storage-Sample
           move N to sMax
           move 'Sample-Table   '  to DMA-Table
           move NBytes             to DMA-Bytes
           move N                  to DMA-Entries
           display Display-Memory-Allocated
      *--Others-Table
      *    'Allocate 11 percent more than DAR occurrences
           if DAR-Requested
             compute NBytes = 4 + (length of oTable(1)
                                   * (Opt-DefinedFields * 1.11))
             compute N = Opt-DefinedFields * 1.11
           else
             compute NBytes = 4 + length of oTable(1)
             compute N = 1
           end-if
MFMFMF     call "CEEGTST" using HEAPID, NBYTES, ADDRSS, FC
MFMFMF     IF not CEE000 of FC THEN
MFMFMF       display 'Engine:Allocate Error:DARO:CEEGTST:FC=' FC
MFMFMF       move 16 to return-code
MFMFMF       stop run
MFMFMF     end-if
MFMFMF     set ADDRESS OF Others-Table    to Addrss
PCPCPC*    set ADDRESS OF Others-Table
PCPCPC*        ADDRESS OF Storage-Others
           move N to oMax
           move 'Other-Table    '  to DMA-Table
           move NBytes             to DMA-Bytes
           move N                  to DMA-Entries
           display Display-Memory-Allocated

v00856*--KeyList-Table
      *    'the array size will be same as #of Samples
           if DAR-Requested
             compute NBytes = 4 + (length of sTable(1)
                                   * Opt-DefinedSamples)
             compute N = Opt-DefinedSamples
           else
             compute NBytes = 4 + length of klTable(1)
             move 1 to N
           end-if
MFMFMF     call "CEEGTST" using HEAPID, NBYTES, ADDRSS, FC
MFMFMF     IF not CEE000 of FC THEN
MFMFMF       display 'Engine:Allocate Error:DARK:CEEGTST:FC=' FC
MFMFMF       move 16 to return-code
MFMFMF       stop run
MFMFMF     end-if
MFMFMF     set ADDRESS OF KeyList-Table   to Addrss
PCPCPC*    set ADDRESS OF KeyList-Table
PCPCPC*        ADDRESS OF Storage-KeyList
           move N to klMax
           move 'KeyList-Table  '  to DMA-Table
           move NBytes             to DMA-Bytes
           move N                  to DMA-Entries
           display Display-Memory-Allocated
           .

       Write-Trace.
      *--------------------------------------------------------------
      *This will write a trace record. At present only the
      * field-table is written.
      *Inputs:
      * Trace-Write-Command    pic 9(4) comp.
      *     Open-Trace-File              value 0.
      *     Write-Trace-FldT             value 1.
      *     Close-Trace-File             value 2.
      * FPtr                 pic 9(5) comp value 0.
      *    Field-Table-Area
      *    Pre-Field-Table-Area
      *    Table-Table-Area
      *    Counters-and-Totals
      *--------------------------------------------------------------
           evaluate true
            when Close-Trace-File
              if Trace-OPEN
                close Trace-File
                set Trace-NOT-OPEN to true
              end-if
            when Write-Trace-Fldt
              if Trace-NOT-OPEN
                open output Trace-FILE
                if IO-STATUS not = '00'
                  display 'Write-Trace:TRACE open error(' IO-STATUS ')'
                  move 12 to return-code
                  stop run
                else
                  set Trace-OPEN to true
                  move 32752 to Out-Len-T
                end-if
              end-if
              compute out-len-T = length of TRACE-FIELD-AREA
              move FLDT-Field (fptr)     to PreT
              if Record-Field(Pret)
                  move ws-rec-cnt to TFS-rec-cnt
                  write Trace-Record from Trace-Field-Start
                  write TRACE-Record from TRACE-FIELD-col
              end-if
              move FPtr                  to TFLDT-PTR
              move FLDT-FIELD   (fptr)   to TFLDT-FIELD
              move FLDT-Start-Byte(fptr) to sByte
              move FLDT-Start-Bit(fptr)  to sBit
              move sByte                 to TFLDT-START-BYTE
              move sBit                  to TFLDT-START-BIT
              move FLDT-Length-Byte(fptr) to lByte
              move 1                      to lBit
              move lByte                 to TFLDT-LENGTH-BYTE
              move lBit                  to TFLDT-LENGTH-BIT
              MOVE FLDT-Target-Start(fptr)  TO TFLDT-Target-Start
              MOVE FLDT-Target-Length(fptr) TO TFLDT-Target-Length
              evaluate true
                when Table-Field(Pret)
                  move Pret-Table(Pret) to TblPtr
                  compute ws-dims = Table-Dims(TblPtr)
                  call 'FmtInd' using
                    FLDT-INDEX-TABLE(fptr), ws-dims, Indice
                  move Pret-Hash(Pret)       to TFLDT-Hash
                  move FLDT-LEVEL(fptr)      to TFLDT-Level
                  move spaces to TFLDT-FieldName
                  string PRET-FIELDNAME(Pret)   delimited by SPACES
                         Indice                 delimited by SPACES
                    into TFLDT-FieldName
                  end-string
                  move FLDT-EntryLength(fptr) to TFLDT-EntryLength
                when OCCUR-FIELD(Pret)
                when ODO-FIELD(Pret)
                when OCCUR-vFIELD(Pret)
                when ODO-vFIELD(Pret)
                  move Pret-Table(Pret) to TblPtr
                  compute ws-dims = Table-Dims(Tblptr)
                  call 'FmtInd' using
                    FLDT-INDEX-TABLE(fptr), ws-dims, Indice
                  move Pret-Hash(Pret)       to TFLDT-Hash
                  move spaces to TFLDT-FieldName
                  move FLDT-LEVEL(fptr)     to TFLDT-Level
                  string PRET-FIELDNAME(Pret)   delimited by SPACES
                         Indice                 delimited by SPACES
                    into TFLDT-FieldName
                  end-string
                  move FLDT-EntryLength(fptr) to TFLDT-EntryLength
                when other
                  move '0'                  to TFLDT-Hash-x
                  move PRET-FIELDNAME(Pret) to TFLDT-FieldName
                  move FLDT-LEVEL(fptr)     to TFLDT-Level
                  move FLDT-EntryLength(fptr) to TFLDT-EntryLength
              end-evaluate
              move PRET-ENTRY-TYPE(Pret)    to TFLDT-Type
              move Pret-StartField(Pret)    to TFLDT-Start
              move Pret-LastField(Pret)     to TFLDT-Last
              move FLDT-Root(fptr)          to TFLDT-Root
              move FLDT-Next(fptr)          to TFLDT-Next
              move FLDT-Prior(fptr)         to TFLDT-Prior
              move FLDT-vNext(fptr)         to TFLDT-vNext
              move FLDT-vPrior(fptr)        to TFLDT-vPrior
              move FLDT-Parent(fptr)        to TFLDT-Parent
              move FLDT-NextSib(Fptr)       to TFLDT-NextSib
              move FLDT-PrevSib(Fptr)       to TFLDT-PrevSib
              move FLDT-FirstChild(FPtr)    to TFLDT-FirstChild
              move FLDT-LastChild(FPtr)     to TFLDT-LastChild
              move FLDT-LastGhost(FPtr)     to TFLDT-LastGhost
              move FLDT-UsedNextInChain(FPtr) to TFLDT-UsedNIC
              write Trace-Record from TRACE-FIELD-AREA
           end-evaluate
           .
      *END PROGRAM Write-Trace.

      *------ this is CHKCond, now a perform routine -----*
      *Change-History.
      * 2014/10/17 HK changed to be a perform instead of a call
      * 2014/08/12 HK use the new Formula2 (execform) routine
      * 2014/04/10 HK set to indeterminate on field beyond record size
      * 2014/02/11 HK allow for indeterminate find of variable
      *---------------------------------------------------------------
      *Description
      *Check to see if an expression is false or true.
      *if the result is zero it is false. If it is not zero it is true.
      *This is an adaptation of the formula program written in Excel
      * VBA by Craig Conrad. This is the execution portion of the code.
      * The validation of the syntax portion is in the Formula program.
      *Additional note. The VBA uses a concept of CLASS to define the
      * array. I have removed the concept and will be using 'old-style'
      * methodology to manipulate the array.
      *The Formula program creates an array holding all the expressions
      * so that this execution routine can run. All we need to do is
      * get the real data from the record.
      *--------------------------------------------------------------
      * Fields passed:
      *    Cond-Table-Area
      *    Condition-SW
      *    Formula-Area
      *    Formula-Execution-Area
      *    Counters-and-Totals
      *    Options-in-Effect
      *    Pre-Field-Table-Area
      *    Field-Table-Area
      *    ws-offset
      *    In-Buffer
      *    Discard-Table-Area
      *
       ChkCond.

           move Cond-ExecuteNdx(cond-ptr) to p-StartExecutionNdx
           move Cond-ExecuteQty(cond-ptr) to FAExecutionQty
           move Cond-ResultNdx(cond-ptr) to FAFinalResultNdx

           if trace-on then perform Trace-ChkCond end-if

           call 'ExecForm' using
             Formula-Area
             Formula-Execution-Area
             Pre-Field-Table-Area,
             Pre-Chain-Table-Area,
             Field-Table-Area, Cond-Table-Area
             ws-offset
             In-Buffer

           evaluate true
             when Formula-Result-Code = 0
HK1218         set FAEntry to Cond-ResultNdx(Cond-ptr)
HK1218         set FARinx  to FAResultNdx(FAEntry)
               If RAType-Int(FARinx)
                 if RAValInt(FARInx) = 0
                   set Condition-False to true
                 else
                   set Condition-True  to true
                 end-if
               Else
                display 'Chkcond:formula returning Non-INT field!'
                  '===> FINAL ==> ' RAValText(FARinx)
                  ' from RA Entry:' Cond-ResultNdx(Cond-ptr)
                move 12 to return-code
                stop run
               end-if
             when Formula-Result-Code < 0
               set Condition-False to true
             when other
               display 'Chkcond:trouble with execform '
                  Formula-Result-code
HK1216            ':Cond#' Cond-ptr
HK1216            ':rec#' ws-rec-cnt
               move 12 to return-code
               stop run
           end-evaluate
           if Trace-On
             move 'ChkCond' to Trace-Event
             display Trace-Event
                 ' rec#=' ws-rec-cnt
                 ' cond#=' cond-ptr
                 ' ' Condition-State(CONDITION-SW + 1)
           end-if
           .
       Trace-ChkCond.
           move 'TrCnd'   to Trace-Event
           display Trace-Event
             ' Cond-ptr=' cond-ptr
             ':StartExecute=' p-StartExecutionNdx
             ':ExecuteQty=' FAExecutionQty
           compute ExecutionStopat = p-StartExecutionNdx
                        + FAExecutionQty - 1
           display Chkcond-trace-header
           perform varying ELNdx from p-StartExecutionNdx by 1
                   until ELNdx > ExecutionStopAt
            set ctl-ELNdx to ELNdx
            set FAEntry to FAExecutionList(ELNdx)
            evaluate true
               when FAType-VAR(FAEntry)
                    move 'Var' to ctl-FAType
                    move FAVarName(FAEntry) to cvn-field, ws-num6
                    move Pret-FieldName(ws-num6) to cvn-FieldName
                    move ChkCond-Var-Name          to ctl-misc
               when FAType-FUNC(FAEntry)
                    move 'Func' to ctl-FAType
                    move FAFuncName(FAEntry) to ctl-misc
               when FAType-PAREN(FAEntry)
                    move 'Paren'              to ctl-FAType
                    move '('                  to ctl-misc
               when FAType-OPER(FAEntry)
                    move 'Oper' to ctl-FAType
                    move FAOperName(FAEntry) to ctl-misc
               when FAType-TEXT(FAEntry)
                    move 'Text' to ctl-FAType
                    move FAValText(FAEntry)   to ctl-misc
               when FAType-HEX(FAEntry)
                    move 'Hex' to ctl-FAType
                    move FAValText(FAEntry)   to ctl-misc
               when FAType-BIN(FAEntry)
                    move 'Bin' to ctl-FAType
                    move FAValText(FAEntry)   to ctl-misc
               when FAType-INT(FAEntry)
                    move 'Int' to ctl-FAType
                    move FAValInt(FAEntry)    to ws-num6
                    move ws-num6              to ctl-misc
               when FAType-SET(FAEntry)
                    move 'Set' to ctl-FAType
                    move '$Set'               to ctl-misc
            end-evaluate
            move FAPriority(FAEntry)     to ctl-FAPriority
            if FAParmQty(FAEntry) >= 1
              move FAParmNdx(FAEntry, 1) to ctl-FAParmNdx1
            else
              move zeroes                 to ctl-FAParmNdx1
            end-if
            if FAParmQty(FAEntry) >= 2
              move FAParmNdx(FAEntry, 2) to ctl-FAParmNdx2
            else
              move zeroes                 to ctl-FAParmNdx2
            end-if
            display chkcond-trace-line
           end-perform
           display '=== end of list ==='
           .
      *END PROGRAM CHKCOND.

      * Load the Alternate File to table. Must be in sequence by
      *  File ID and Field Name.
      *  Alt*-Cnt holds the counter of what is loaded from the ALT-file
      *  Alt*-Total holds the counter of what is loaded from the
      *    ALT-file PLUS AltName*() keywords from the DLL command
v00890 Load-the-Alternate-Names.
           open input Alt1-File
           if IO-STATUS not = '00'
             display 'Engine: Alt1 open error ' IO-STATUS
           end-if
           move 0 to Alt1-Ptr
           move low-values to Last-Alt-Key
           perform until IO-STATUS not = '00'
             read Alt1-File
               at end move '10' to IO-STATUS
               not at end
                 if Alt1-File-Record(1:1) not = '*'
                 and Alt1-File-Record(1:2) not = '/*'
                 and (Alt1-File-ID = spaces or
                      Alt1-File-ID = FT-ID(1))
                   perform LTAN-1-Process-Record
                 end-if
             end-read
           end-perform
           close Alt1-File
           display 'Engine: Alt1 records loaded ' Alt1-Ptr

           open input Alt2-File
           if IO-STATUS not = '00'
             display 'Engine: Alt2 open error ' IO-STATUS
           end-if
           move 0 to Alt2-Ptr
           move low-values to Last-Alt-Key
           perform until IO-STATUS not = '00'
             read Alt2-File
               at end move '10' to IO-STATUS
               not at end
                 if Alt2-File-Record(1:1) not = '*'
                 and Alt2-File-Record(1:2) not = '/*'
                 and (Alt2-File-ID = spaces or
                      Alt2-File-ID = FT-ID(1))
                   perform LTAN-2-Process-Record
                 end-if
             end-read
           end-perform
           close Alt2-File
           display 'Engine: Alt2 records loaded ' Alt2-Ptr
           .
v00890 LTAN-1-Process-Record.
           add 1 to Alt1-Ptr
           if Alt1-Ptr > Alt1-Cnt
             display 'Engine:Too many Alt1 records to load'
             move 12 to return-code
             stop run
           end-if
           if Alt1-File-Key not > Last-Alt-Key
             display
              'Engine:Alt1 Key out of sequence. Record # ' Alt1-Ptr
             move 12 to return-code
             stop run
           end-if
           move Alt1-File-Key           to Last-Alt-Key
           inspect Alt1-File-Key
               converting 'abcdefghijklmnopqrstuvwxyz'
                       to 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
           move Alt1-File-ID            to Alt1-ID(Alt1-Ptr)
           move Alt1-File-FieldName     to Alt1-FieldName(Alt1-Ptr)
           move Alt1-File-AlternateName to Alt1-AlternateName(Alt1-Ptr)
           .
v00354 LTAN-2-Process-Record.
           add 1 to Alt2-Ptr
           if Alt2-Ptr > Alt2-Cnt
             display 'Engine:Too many Alt2 records to load'
             move 12 to return-code
             stop run
           end-if
           if Alt2-File-Key not > Last-Alt-Key
             display
              'Engine:Alt2 Key out of sequence. Record # ' Alt2-Ptr
                    ws-num6
             move 12 to return-code
             stop run
           end-if
           move Alt2-File-Key           to Last-Alt-Key
           inspect Alt2-File-Key
               converting 'abcdefghijklmnopqrstuvwxyz'
                       to 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
           move Alt2-File-ID            to Alt2-ID(Alt2-Ptr)
           move Alt2-File-FieldName     to Alt2-FieldName(Alt2-Ptr)
           move Alt2-File-AlternateName to Alt2-AlternateName(Alt2-Ptr)
           .


      *--------------------------------------------------------------
      *  Below here are Internal sub-routines
      *--------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     ShowLocation COMMON.                             R3
      * Show the value of the cursor in byte basis
      * call 'ShowLocation' using <showtext>, <cursor>
      *  where <showtext> is the text to display first
      *        <cursor> is 9(9) comp which is the cursor coordinate
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       77  ShowText-Max    pic 9(4) comp value 0.
       77  ShowText-Length pic 9(4) comp value 0.
       77  ShowByte        pic 9(5) comp value 0.
       77  ShowNib         pic 9         value 0.
       77  ShowBit         pic 9         value 0.
       77  ShowCoord       pic 9(9)      value 0.
       LINKAGE SECTION.
       77  ShowText   PIC x(20).
       77  ShowCursor PIC 9(9) COMP.
       PROCEDURE DIVISION using ShowText, ShowCursor.
           move length of ShowText to ShowText-Max
           perform varying ShowText-Length from 1 by 1
                 until ShowText-Length > ShowText-Max
                    or ShowText(ShowText-Length:1) = x'00'
           end-perform
           subtract 1 from ShowText-Length
           if ShowText-Length = 0 goback end-if
           if ShowCursor = 0 goback end-if
           move ShowCursor to ShowCoord
           move zeroes to ShowByte, ShowNib, ShowBit
           if ShowCursor = 0 goback end-if
           divide ShowCursor by 8 giving ShowByte remainder ShowBit
           if ShowBit = 0 then
             move 8 to ShowBit
           else
             add 1 to ShowByte
           end-if
           if ShowBit > 4
             move 2 to ShowNib
           else
             move 1 to ShowNib
           end-if
           display ShowText(1:ShowText-Length)
             ':@=' ShowCoord
             ' (' ShowByte
             ':n' ShowNib
             ':b' ShowBit ')'
           GOBACK
           .
       END PROGRAM ShowLocation.
      *--------------------------------------------------------------
       copy LoadOpt.
      *--------------------------------------------------------------
       copy HKSys.
       copy ExecForm.
       copy SumDLL.
       copy LoadDLL.
       copy DAR.
       copy Ratify.
       copy Len.
       copy DateC.
       copy SetError.
      *--------------------------------------------------------------
       END PROGRAM ENGINE.


      *--------------------------------------------------------------
      *--       External Sub-Routines                              --
      *--------------------------------------------------------------
MFMFMF copy CCDSDate.
v00890 copy MAP.
       copy Sample.
       copy NoteDisc.
       copy NoteLog.
       copy FldName.
       copy Formula.
       copy FmtInd.
       copy FmtHash.
       copy Parse.
       copy ParseKey.
       copy NegValue.
       copy HexShow.
       copy ToHex.
       copy ToDisp.
       copy InStr.
       copy Len.
       copy LenStr.
       copy Ltrim.
       copy UCase.
