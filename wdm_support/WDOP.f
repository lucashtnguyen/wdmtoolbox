C
C
C
    SUBROUTINE Findex(filename)
C    !Author: Jack Lisin
C    !Date Developed: 06/10/2019
C    !Developed for: Geosyntec Consultants
        IMPLICIT NONE
        CHARACTER(LEN=*),INTENT(INOUT)::filename
        CHARACTER(LEN=50)::dummyI
        LOGICAL::Fcheck
        INTEGER::n=0, TheSTAT
        INQUIRE(FILE='commonblock_FI.txt',EXIST=Fcheck)
        
        IF(Fcheck) THEN
            OPEN(UNIT=9999,FILE='commonblock_FI.txt',STATUS='OLD',IOSTAT=TheSTAT)
            IF(TheSTAT/=0) STOP 'FILE ERROR In COMMONBLOCK_FI'
            REWIND(9999)
            READ(9999,*)n
            n=n+1
            REWIND(9999)
            WRITE(9999,*) n
            CLOSE(9999)
            WRITE(dummyI,'(I0)') n
            filename((LEN_TRIM(filename)+1):LEN_TRIM(dummyI)+LEN_TRIM(filename)+1)=TRIM(dummyI)
        ELSE
            n=1
            OPEN(UNIT=9999,FILE='commonblock_FI.txt',STATUS='NEW', IOSTAT=TheSTAT)
            IF(TheSTAT/=0) STOP 'ERROR in COMMONBLOCK_FI'
            WRITE(9999,*) n
            WRITE(dummyI,'(I0)') n
            filename((LEN_TRIM(filename)+1):LEN_TRIM(dummyI)+LEN_TRIM(filename)+1)=TRIM(dummyI)
        END IF
        
    END SUBROUTINE 
c
c
c
      SUBROUTINE   WDBOPN
     I                    (WDMSFL,WDNAME,RONWFG,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     Open a WDM file.  File is opened as new or old, depending on
C     the value of RONWFG.  The common block related to the WDM record
C     buffer are initialized the first time this routine is called.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      WDMSFL,RONWFG,RETCOD
      CHARACTER(LEN=*) WDNAME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of the WDM file
C     WDNAME - name of the WDM file
C     RONWFG - read only/new file flag
C              0- normal open of existing WDM file,
C              1- open WDM file as read only (system dependent),
C              2- open new WDM file
C     RETCOD - return code
C               0 - successful open
C               1 - successful open, but invalid WDM file
C              <0 - error on open, -IOSTAT, compiler specific
C
C     + + + SAVES + + +
      INTEGER   INITFG
      SAVE INITFG
      INTEGER   RECRDL
      SAVE RECRDL
C
C     + + + LOCAL VARIABLES + + +
      INTEGER(4)IOS
      CHARACTER(LEN=50)::filename
      INTEGER::TheSTAT
      INTERFACE
	      SUBROUTINE Findex(filename)
		      IMPLICIT NONE
		      CHARACTER(LEN=*),INTENT(INOUT)::filename
	      END SUBROUTINE 
      END INTERFACE
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDBFIN, WDFLCK, WDCREA
C
C     + + + DATA INITIALIZATIONS + + +
      DATA INITFG/0/
      DATA RECRDL/0/
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
C
      IF (RECRDL.EQ.0) THEN
C       first time called, determine compiler flag specific
C       definition of RECL units in OPEN
C
C       create a small file and try to write different size strings
C!        OPEN(UNIT=WDMSFL,STATUS='SCRATCH',ACCESS='DIRECT',
C!     1       FORM='UNFORMATTED',RECL=4)
        filename="temporary.wdm"
        CALL Findex(filename)
        open (unit=WDMSFL, file=filename, &
       &  status='REPLACE', form='UNFORMATTED', access='DIRECT', recl=4, IOSTAT=TheSTAT)
        IF(TheSTAT/=0) STOP 'Error opening index file'
        WRITE(WDMSFL,REC=1,ERR=110) '1234567890123456'
        RECRDL= 512
        GOTO 100
 110    CONTINUE
        WRITE(WDMSFL,REC=1,ERR=120) '12345678'
        RECRDL= 1024
        GOTO 100
 120    CONTINUE
        WRITE(WDMSFL,REC=1,ERR=100) '1234'
        RECRDL= 2048
 100    CONTINUE
        CLOSE(WDMSFL,STATUS='DELETE')
      END IF
C
      IF (RONWFG.EQ.1) THEN
C       open file as 'read only'
        OPEN (UNIT=WDMSFL,FILE=WDNAME,STATUS='OLD',
     1        ACCESS='DIRECT',FORM='UNFORMATTED',RECL=RECRDL,
     2        ERR=10,IOSTAT=IOS)
      ELSE IF (RONWFG.EQ.2) THEN
C       open new wdm file
        OPEN (UNIT=WDMSFL,FILE=WDNAME,STATUS='NEW',
     1        ACCESS='DIRECT',FORM='UNFORMATTED',RECL=RECRDL,
     2        ERR=10,IOSTAT=IOS)
      ELSE
C       open file w/out 'read only'
        OPEN (UNIT=WDMSFL,FILE=WDNAME,STATUS='OLD',
     1        ACCESS='DIRECT',FORM='UNFORMATTED',RECL=RECRDL,
     2        ERR=10,IOSTAT=IOS)
      END IF
C     WDM file opened successfully
      IF (INITFG.EQ.0) THEN
C       first time called, initialize WDM record buffer
        CALL WDBFIN
        INITFG= 1
      END IF
      IF (RONWFG.EQ.2) THEN
C       new file, need to initialize it
        CALL WDCREA (WDMSFL)
      END IF
      IF (RETCOD.EQ.0) THEN
C       check WDM directory records
        CALL WDFLCK (WDMSFL,
     O               RETCOD)
      END IF
      GO TO 20
 10   CONTINUE
C       error on open, exact value of retcod may vary by system,
C       set it to a negative value for consistancy
        RETCOD= IOS
        IF (RETCOD.GT.0) RETCOD= -RETCOD
        IF (RETCOD.EQ.0) RETCOD= -1
 20   CONTINUE
C
      RETURN
      END
