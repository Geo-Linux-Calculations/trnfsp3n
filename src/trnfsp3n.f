       PROGRAM TRNFSP3N
C
C TRANSFORMS SP3 EPHEMERIS FROM THE CURRENT (ITRF) FRAME TO A NEW (E.G. NAD83)
C DATUM
C USAGE: TRNFSP3 INPUTFILE(SP3) OUTPUTFILE(SP3) TRANSFORMFILE 
C
C************************* SERVICE_ROUTINES module required for Lahey LF95
C************************* compilter to use IARC and GETARG routines.
C      USE SERVICE_ROUTINES !!! for F77 DOS and WINDOWS
C
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER*2 IARGC
      INTEGER*4 LENGTH,INP,IOT,ITR,IEI,IEO,ERROR
      CHARACTER*40 NAMR,REFNAME,LIN*80
C
      DIMENSION USAV(3), IMONTH(12)
      DATA IMONTH/0,31,60,91,121,152,182,213,244,274,305,335/
      DATA ITR,INP,IOT, IEI, IEO/97,98,99,51,52/
      CLK=0.D0
      IERP=0
C************************* IARGC & GETARG are routines to get the command
C************************* line parameter stings. Requires use of +U77
C************************* compiler switch for HP Fortran 77 and the use of
C************************* the SERVICE_ROUTINES module for the Lahey LF95
C************************* compiler under Linux or DOS.
C
C INTPUT FILE NAMR IS DETERMINED BY COMMAND-STRING PARAMETER 1.
C
      LENGTH = IARGC()
      IF (LENGTH.GT.0) THEN
        CALL GETARG(1,NAMR)
      ENDIF
      OPEN(INP,FILE=NAMR,ERR=120,STATUS='OLD',IOSTAT=ERROR)
C
C OUTPUT FILE NAMR IS DETERMINED BY COMMAND-STRING PARAMETER 2.
C
      IF (LENGTH.GT.1) THEN
        CALL GETARG(2,NAMR)
        OPEN(IOT,FILE=NAMR,ERR=120,IOSTAT=ERROR)
      ENDIF
C
C TRANSFORMATION FILE NAMR IS DETERMINED BY COMMAND-STRING PARAMETER 3.
C
      IF (LENGTH.GT.2) THEN
        CALL GETARG(3,NAMR)
        OPEN(ITR,FILE=NAMR,ERR=120,STATUS='OLD',IOSTAT=ERROR)
      ENDIF
C
C ERP input FILE (OPTIONAL)IS DETERMINED BY COMMAND-STRING PARAMETER 4.
C
      IF (LENGTH.GT.3) THEN
        CALL GETARG(4,NAMR)
        OPEN(IEI,FILE=NAMR,ERR=120,STATUS='OLD',IOSTAT=ERROR)
      ENDIF
C
C ERP output FILE (OPTIONAL)IS DETERMINED BY COMMAND-STRING PARAMETER 5.
C
      IF (LENGTH.GT.4) THEN
        CALL GETARG(5,NAMR)
        OPEN(IEO,FILE=NAMR,ERR=120,IOSTAT=ERROR)
      IERP=1
      ENDIF
      GOTO 121
C
C ERROR OPENING TRANSFORMATION FILE: FATAL
C
120   CONTINUE
      WRITE(6,'(/"ERROR IN OPENING FILE ",A40)') NAMR
      WRITE(6,'("IOSTAT = ",I4/)') ERROR
      STOP  0002
C
121   CONTINUE
C
C READ TRANSFORMATION PARAMETER FILE T1-3 (CM), D(PPB), R1-3(.001 ARC SEC)
C  AT EPOCH TRTIME(YEARS; E.G. 1995.0)
C  REFNAME - REFERNCE FRAME (DATUM) NAME (E.G. NAD83)
C
      READ(ITR,'(A40)') REFNAME
      READ(ITR,*) TRTIME, T1,T2,T3,D,R1,R2,R3
C
C READ RATE OF CHANGE ./Y
C
      READ(ITR,*) T1D,T2D,T3D,DD,R1D,R2D,R3D 
C
C READ FIRST LINE OF INPUT FILE TO GET FIRST EPOCH
C
      READ(INP,'(A80)') LIN
      READ(LIN,'(3X,I4,I3,I3)') IYR,IMO,IDY
      WRITE(IOT,'(A46,A5,A9)') LIN(1:46) , REFNAME , LIN(52:60)
      EPOCH= IYR+(IMONTH(IMO)+IDY-1)/365.25
C
C LOOP TO REACH FIRST POSITION: OUTPUT HEADER TEXT: DETERMINE SCALE
C
1000  CONTINUE
      READ(INP,'(A80)',END=901) LIN
      IF(LIN(1:1).EQ."P".OR.LIN(1:1).EQ."V".AND.LIN(2:2).EQ." ") THEN
        READ(LIN(6:80),*,END=1005) (USAV(K),K=1,3),CLK
1005    CONTINUE
C GET X,Y,X UNITS (KM,M,OR MM) FROM THE FIRST X,Y,Z LINE
        TSTSCL=SQRT(USAV(1)**2+USAV(2)**2+USAV(3)**2)/5.D3
        IF(TSTSCL.LT.1.D-3) THEN
          WRITE(IOT,'(A80)') LIN             
          GOTO 1000
        ENDIF
C X,Y,Z IN KM
        IF(TSTSCL.LT.20.0D0) SCALE= 1.0D-5
C X,Y,Z IN M
        IF(TSTSCL.GT.1.D3) SCALE= 1.0D-2
C X,Y,Z IN MM
        IF(TSTSCL.GT.1.D6) SCALE= 1.0D1
C
        T1T= (T1+T1D*(EPOCH-TRTIME))*SCALE 
        T2T= (T2+T2D*(EPOCH-TRTIME))*SCALE 
        T3T= (T3+T3D*(EPOCH-TRTIME))*SCALE 
        DT = (D + DD*(EPOCH-TRTIME))*1.0D-9
        R1T= (R1+R1D*(EPOCH-TRTIME))*4.848137D-09
        R2T= (R2+R2D*(EPOCH-TRTIME))*4.848137D-09
        R3T= (R3+R3D*(EPOCH-TRTIME))*4.848137D-09
        GO TO 1001
      ELSE
        WRITE(IOT,'(A80)') LIN             
        GOTO 1000
      ENDIF
C
C SECOND LOOP REACH END OF INPUT FILE: OUTPUT TRANSFORMED POSITIONS
C       
1050  CONTINUE
      READ(INP,'(A80)',END=901) LIN
      IF(LIN(1:1).EQ."P".OR.LIN(1:1).EQ."V".AND.LIN(2:2).EQ." ")
     *          GO TO 1051
      WRITE(IOT,'(A80)') LIN             
      GO TO 1050
1051  CONTINUE                                      
      READ(LIN(6:80),*,END=1055) (USAV(K),K=1,3),CLK
1055  CONTINUE
1001    IF(USAV(1).NE.0.0D0 ) THEN
C TRANSFORM SV XYZ TO A NEW REFERENCE FRAME (DATUM)
        USAV(1)= USAV(1)*(1.0D0+DT)+T1T-R3T*USAV(2)+R2T*USAV(3)
        USAV(2)= USAV(2)*(1.0D0+DT)+T2T+R3T*USAV(1)-R1T*USAV(3)
        USAV(3)= USAV(3)*(1.0D0+DT)+T3T-R2T*USAV(1)+R1T*USAV(2)
        ELSE
          CLK=999999.999999D0
        ENDIF
      IF(SCALE.EQ.1.0D-5)
     *   WRITE(IOT,1041) LIN(1:5),(USAV(K),K=1,3),CLK
      IF( SCALE.EQ.1.0D-2)
     *   WRITE(IOT,1042) LIN(1:5),(USAV(K),K=1,3)
      IF( SCALE.EQ.1.0D+1)
     *   WRITE(IOT,1043) LIN(1:5),(USAV(K),K=1,3)
      GOTO 1050
901   CONTINUE
C  transform ERP file if ERP output asked for (IERP=1)
      IF(IERP.EQ.1) call ERPRDWR(IEI, IEO, REFNAME, TRTIME,
     +                   R1,R2,R3, R1D, R2D, R3D)
      STOP 
1010  FORMAT(4X,I4,4I3,F11.7) 
1020  FORMAT(3X,I4,4I3,F11.7) 
1021  FORMAT('*',2X,I4,4I3,F11.7)
1030  FORMAT(3X,I2,3F13.5,3F12.8)
1041  FORMAT(A5,F13.6,3F14.6)
1042  FORMAT(A5,F13.3,3F14.3)
1043  FORMAT(A5,F13.0,3F14.0)
      END
C
      SUBROUTINE ERPRDWR(LFN002, LFN010, REFNAME, TRTIME,
     +                   R1,R2,R3, R1D, R2D, R3D)
C
C READ/WRITE IGS ERP FILE
C --------------------
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*80   TEXT, REFNAME*40
      INTEGER*4 XP,YP,UT1, LOD,XSIG,YSIG,UTSIG, LFN002, LFN010,
     1          LODSIG,NR,NF,NT, XRT,YRT, XRTSIG, YRTSIG
      PARAMETER (MAXERP=1000)
      IGSFRM=0
      XRT = 0
      XRTSIG = 0
      YRT = 0
      YRTSIG = 0
C
C FIND THE FIRST LINE WITH ERP VALUES
C -----------------------------------
      READ(LFN002,20,END=45)TEXT
      WRITE(LFN010,21)TEXT, REFNAME
21    FORMAT(A74,a6)
10    READ(LFN002,20,END=45)TEXT
      WRITE(LFN010,20)TEXT
20    FORMAT(A80)
        IF(TEXT(1:5).EQ.'  MJD') THEN
          IGSFRM=2
          READ(LFN002,20) TEXT 
          WRITE(LFN010,20)TEXT  
          GOTO 50
        END IF
        GOTO 10
C 
C PROBLEM: COULD NOT FIND ANY ERP VALUES FOR CENTER CNAME
C -------------------------------------------------------
45    WRITE(6,47) 
47    FORMAT(/,1X,'EOF IN ERP FILE: ERROR IN SR ERPRDWR',
     1       /,1X,'PROBLEM :  COULD NOT FIND ERP VALUES',
     2       /,1X,'CENTER  : ',A3,
     3       /,1X,'PROGRAM CONTINUES WITH NEXT ERP FILE ...',/)
      STOP

C ***************
C READ EOP VALUES
C ***************

50    DO 70 I=1,MAXERP
C       IF(CNAME.EQ.'IGS') THEN
C -----------
C CENTER: IGS
C -----------
          IF(IGSFRM.EQ.2) THEN

            READ(LFN002,*,END=100,ERR=100)XMJD
            BACKSPACE(LFN002)
            
C
C IGS FORMAT IS DIFFERENT BEFORE MJD=50243 (wk 857 day 0)
C ---------------------------------------------------
C
            IF(XMJD.GE.50243.D0) THEN
              READ(LFN002,*,END=100,ERR=100)XMJD,XP,YP,UT1,
     1                             LOD,XSIG,YSIG,UTSIG,
     2                             LODSIG,NR,NF,NT,
     3                             XRT,YRT, XRTSIG, YRTSIG
            ELSE
c read older format w/o PM rates
              READ(LFN002,*,END=100,ERR=100)XMJD,XP,YP,UT1,
     1                             LOD,XSIG,YSIG,UTSIG,
     2                             LODSIG,NR,NF,NT
            ENDIF
C transform ERP
      epoch= (xmjd+2400000.5)/365.2475 -4712.0 - trtime
c PM units (current 1E-5 arcsec possible future 1e-06 arcsec & 1e-07 sec)
      scle= 1.d02
      test=sqrt(dble(xp)**2+dble(yp)**2)
      if (test.gt.9.d04) scle=1.d03
      xp  = xp  + (r2 +r2d*epoch)*scle  
     1+ dsign(0.5d0,dble(xp))
      yp  = yp  + (r1 +r1d*epoch)*scle 
     1+ dsign(0.5d0,dble(yp))
      ut1 = ut1 - (r3 +r3d*epoch)*1.0d03/15.d0*scle/1.d02
     1 + dsign(0.5d0, dble(ut1))
C
C WRITE EOP VALUES
C ----------
               IF(scle.EQ.1.d02) THEN
c write the old format (1e-05 arcsec/1e-06 sec)
         WRITE(LFN010,23)XMJD,XP,YP,UT1,
     1                  LOD,XSIG,YSIG,UTSIG,
     2                  LODSIG,NR,NF,NT,
     3                  XRT,YRT, XRTSIG, YRTSIG
23    FORMAT(F8.2,2I7,I8,I6,2I5,2I7,I4,2I3,4I6)
               ELSE
         WRITE(LFN010,24)XMJD,XP,YP,UT1,
     1                  LOD,XSIG,YSIG,UTSIG,
     2                  LODSIG,NR,NF,NT,
     3                  XRT,YRT, XRTSIG, YRTSIG
24    FORMAT(F8.2,2I8,I9,I7,2I6,2I8,I4,2I3,4I7)
                END IF
c           END IF
C         
          END IF
C        ENDIF
70    CONTINUE
C
100   CONTINUE
      return
      end

