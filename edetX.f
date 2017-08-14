C ****************************************************************
C *
C *  EDET
C *
C *  Source code modified from sfaero.f as
C *  taken from NASA FLOPS code, circa 2000
C *
C *  This code implements methods described in:
C *    Feagin, Richard C. and Morrison, William D.
C *    "Delta Method, An Empirical Drag Build Up Technique"
C *    NASA CR-151971, December 1978
C
      PROGRAM EDET

      IMPLICIT DOUBLE PRECISION ( A-H, O-Z )

      COMMON / AERO / CDTAB(1200),CDPA(1000)
      COMMON / MISSA/ AITEK ,AR    ,CAM,CH,DB ,BODYLD,SBASE,
     1                SPI   ,SREF  ,SW25  ,TAPER,TC,VCMIN,CRUDF,
     2                CDCTAB(40)   ,CDF(42)      ,CDFCRD(10)   ,
     3                CDFTAB(40)   ,CDITAB(40,40),CDPTAB(40,40),
     4                CDREF(40)    ,CF(41)       ,CL(40)       ,
     5                CLBUF(40)    ,FF(40)       ,FR(40)       ,
     6                EM(40)       ,RN(40)       ,SWET(41)     ,
     7                TRU          ,TRL          ,EL(40)       ,
     8                ITAB  ,LAM   ,MNO   ,N     ,NCRUD
      COMMON /CONPON/ NAME(10)
      DIMENSION HTEMP(60), RECHK(60)
      CHARACTER*16 NAME
	  PARAMETER (CONSTPI = 3.141592654, CONSTRADTODEG = 57.29577951)

CLVBS Use explicit filenames instead of names based on "fort" units
      OPEN(UNIT=8, FILE='EDET.in', ACTION='READ')
      OPEN(UNIT=7, FILE='EDET.out', ACTION='WRITE')
CLVBE

      WRITE(7,*) '* DELTA METHOD FOR DRAG ESTIMATION'
      WRITE(7,*) '*'
      WRITE(7,*) '* INPUT FILE ECHO'

      CALL CCTOSS

      OPEN(UNIT=10, FILE='EDET.temp', ACTION='READ')
       
      WRITE(7,*) '*'
      WRITE(7,*) '* PARSED INPUTS'
      WRITE(7,*) '*'
      WRITE(7,*) '* WING GEOMETRY'
C                          1         2         3         4         5
C                 12345678901234567890123456789012345678901234567890
      WRITE(7,*) '*   S_REF        AR       TC% SW25(deg)    TAPER%'
      READ(10,*) SREF,AR,TC,SW25,TAPER

      WSPAN = SQRT(AR*SREF)
      WMAC = WSPAN / AR
 
      WRITE(7,1) SREF,AR,TC*100.,SW25,TAPER*100.
      WRITE(7,*) '*WINGSPAN       MAC'
      WRITE(7,1) WSPAN,WMAC	  
	  WRITE(7,26)
      READ(10,*) SWETW,CAM,AITEK,TRU,TRL,CDFCRD(1)
      WRITE(7,21) SWETW,CAM,AITEK,TRU*100,TRL*100,CDFCRD(1)

C     TRANSITION POINTS - USED FOR LAMINAR FLOW OPTION
      IF ((TRU.EQ.0.).AND.(TRL.EQ.0.)) THEN
         LAM=0
      ELSE
         LAM=1
      END IF

      WRITE(7,*) '*'
      WRITE(7,*) '* FUSELAGE GEOMETRY'
      WRITE(7,*) '*   S_WET     S_LEN  BODY_L/D     SBASE    DELCD0'

      READ(10,*) SWETF,BLENF,BODYLD,SBASE,CDFCRD(2)
      WRITE(7,23) SWETF,BLENF,BODYLD,SBASE,CDFCRD(2)

      SPI = CONSTPI * (BLENF/BODYLD)**2 / 4.0
CLVBS ADD COMPUTATION OF FUSELAGE DIAMETER TO WING SPAN RATIO 
CLVB  THIS HAS BEEN MYSTERIOUSLY MISSING
      DB = BLENF/BODYLD/WSPAN
CLVBE
      IF (SBASE.GT.SPI) THEN
         WRITE(7,*) '* SBASE > SPI! --- LIMITING SBASE TO SPI'
         SBASE=SPI
      ENDIF

CLVB      WRITE(7,*) '*  X-AREA (SPI)  1+SBASE/SPI'
      WRITE(7,*) '*XAREASPI  1+SB/SPI  FUSE_D/B'
CLVB      WRITE(7,1)  SPI, 1+(SBASE/SPI)
      WRITE(7,1) SPI,1+(SBASE/SPI),DB

      WRITE(7,*) '*'
      WRITE(7,*) '* CRUDFACTOR (EDET CR USES = 0.284)'
      READ(10,*) CRUDF
      WRITE(7,27) CRUDF

      WRITE(7,*) '*'
      WRITE(7,*) '* REFERENCE CONDITIONS'
CLVB  MAKE REF MACH AN INPUT
      WRITE(7,*) '* REF_ALT  REF_MACH REF_REMAC'
      READ(10,*) CH,VCMIN
CLVB  CALCULATE REFERENCE RE # 
      DTC = 0.
      CALL ATMO(CH,DTC,DELTA,THETA,ASTAR,T,REREF,HFT)
      REREF = REREF*VCMIN*WMAC
      WRITE(7,2) CH,VCMIN,REREF

    1 FORMAT(10F10.2)
    2 FORMAT(2F10.2,F10.0,F6.1,3X,F6.1,F8.4)
   21 FORMAT(2F10.2,F10.0,2F10.2,F10.4)
   23 FORMAT(4F10.2,F10.4)
   26 FORMAT(1X,'*   S_WET   CAMBER%    AITEK    UP_LAM%',
     *     '   LW_LAM%    DELCD0')
   27 FORMAT(F10.4)

C     POPULATE TABLES
      SWET(1)   = SWETW
      EL(1)     = WMAC
      FR(1)     = TC
CLVB      CDFCRD(1) = 0

      SWET(2)   = SWETF
      EL(2)     = BLENF
      FR(2)     = BODYLD
CLVB      CDFCRD(2) = 0

C     READ IN EXTRA COMPONENTS
      I=3
      N=2
   10 READ(10,3) NAME(I)
    3 FORMAT(A16)
    4 FORMAT(A16,3F10.2,F10.4)
      IF (NAME(I)(1:4).NE.'    ') THEN
         IF (I.EQ.3) THEN
            WRITE(7,*) '*'
            WRITE(7,*) '* EXTRA COMPONENTS'
            WRITE(7,*) '*'
            WRITE(7,*) '* COMPONENT         S_WET       LEN     ',
     &              'TC/FR DELTA_CD0'
         END IF
         READ(10,*) SWET(I), EL(I),FR(I),CDFCRD(I)
         WRITE(7,4) NAME(I),SWET(I),EL(I),FR(I),CDFCRD(I)
         N=I
         I=I+1
         GO TO 10
      END IF

      IF (N.GE.3) THEN
         NCRUD=N
      ELSE
         NCRUD=0
      END IF

      WRITE(7,*) '*'
      WRITE(7,*) '*'
      WRITE(7,*) '* **************************************************'
      WRITE(7,*) '*'
      WRITE(7,*) '* EDET RESULTS'

C     CALCULATE DESIGN LIFT COEFFICIENT
      CALL CLDESN (TC,AR,SW25,CAM,CLDES,CL,NUMCL)

C     CALCULATE DESIGN MACH NUMBER
CLVBS      DESM = FMDES
CLVB     Initializing DESM to zero forces calculation in MDESN
      DESM = 0
CLVBE
      CALL MDESN (TC,AR,SW25,AITEK,CLDES,DESM,EM,MNO,TAPER)
      WRITE(7,*) '*'
      WRITE(7,*) '* WING DESIGN CONDITIONS'
      WRITE(7,*) '*DES_MACH    DES_CL'
      WRITE(7,5) DESM, CLDES
    5 FORMAT(10F10.4)

C     ESTIMATE LOW-SPEED PITCHUP FROM NACA TN 1093
C     (* FROM T. TAKAHASHI 2013a VERSION)
      ARCRIT = 0.0012 * SW25**2 - 0.2417 * SW25 + 11.1
      WRITE(7,*) '*'
      WRITE(7,*) '* POST-STALL LONGITUDINAL STABILITY ESTIMATE'
      WRITE(7,*) '* (TAKAHASHI EDET2013a, NACA TN 1093)'
      WRITE(7,*) '* CRIT_AR   PITCH_BREAK'
      IF (AR.LE.ARCRIT) THEN
         WRITE(7,6) ARCRIT
      ELSE
         WRITE(7,7) ARCRIT
      END IF
    6 FORMAT(2X,F8.2,'        STABLE')
    7 FORMAT(2X,F8.2,'      UNSTABLE')

C     CALCULATE SKIN FRICTION DRAG COEFFICIENT AT CRUISE FOR PRINTING
      WRITE(7,*) '*'
      WRITE(7,*) '* CD0 BUILD-UP'
       
CLVB USE READ-IN VALUE INSTEAD      VCMIN = 0.6

      IPRINT=1
      CALL CDFF (CH,VCMIN,ANS,IPRINT)

C     ZERO OUT ARRAYS 
      DO I = 1,40
         CDFTAB(I) = 0D0
         CLBUF(I) = 0D0
         CDCTAB(I) = 0D0
         DO J = 1,40 
            CDPTAB(I,J) = 0D0
            CDITAB(I,J) = 0D0
         END DO
      END DO

C     ENCODE ELEMENT 1 OF DRAG COEFFICIENT TABLE CDTAB
      CDTAB(1) = 1000. * MNO + NUMCL
  
C     LIFT COEFFICIENTS ARE THE FIRST ROW OF THE TABLE
      DO 20 I = 1,NUMCL
         CDTAB(I + 1) = CL(I)
   20 CONTINUE
      NNNL = NUMCL + 1
  
C     SET UP CONSTANTS FOR FORWARD SWEPT WING 
      TH = (1D0 - TAPER) / (1D0 + TAPER) / AR
      COSA = 1D0 / SQRT(1D0 + (TAN(SW25 / CONSTRADTODEG)-3D0*TH)**2)
      COSB = 1D0 / SQRT(1D0 + (TAN(SW25 / CONSTRADTODEG) + TH) ** 2)

      WRITE(7,*) '*     NUMCL'
      WRITE(7,*) NUMCL
      WRITE(7,*) '*   NUMMACH'
      WRITE(7,*) MNO
      WRITE(7,*) '*      NALT'
      WRITE(7,*) 18

C     LOOP ON MACH NUMBER - FILL IN DRAG COEFFICIENTS 

      DO 50 I = 1, MNO
         NNNL = NNNL + 1

C        THE FIRST COLUMN IN EACH ROW IS THE MACH NUMBER 
         CDTAB(NNNL) = EM(I)

CLVBC        CAP DELM TO AN EQUIVALENT OF MACH 1.5
CLVB         IF (EM(I).GT.1.5) THEN
CLVB            DELM = 1.5 - DESM
C        CAP DELM TO AN EQUIVALENT OF MACH 2.0
         IF (EM(I).GT.2D0) THEN
            DELM = 2D0 - DESM
         ELSE
            DELM = EM(I) - DESM
         END IF

C        CALCULATE BUFFET ONSET CL          
         CALL BUFFET ( TC, DELM, SW25, AR, CAM, DELCLB )
         IF ( DELM .LE. 0.15 ) CLBUF(I) = MIN(DELCLB + CLDES,1.2)

C        GET SKIN FRICTION DRAG COEFFICIENTS  
CLVBS
CLVB     GET CDF AT REFERENCE RN (DIFFERENT ALT FOR EACH M) 			
         IF (ABS(EM(I)-VCMIN).LT..00001) THEN
CLVB        IF CURRENT MACH IS REFERENCE MACH, SKIP SEARCH
CLVB        AND USE THE REFERENCE ALTITUDE - AVOIDS NUMERICAL NOISE  
            HRECHK = CH
            GO TO 58
         END IF
         DO 57 K = 1,54
            HTEMP(K) = 2000D0 * (K - 11)
            CALL ATMO(HTEMP(K),DTC,DELTA,THETA,ASTAR,T,RETEMP,HFT)
            RECHK(K) = RETEMP*EM(I)*WMAC
            IF (K.GT.1) THEN
               IF (RECHK(K).LE.REREF) THEN
CLVB              THIS STEP CROSSED OVER THE REFERENCE RE, NOW LINEARLY
CLVB              INTERPOLATE THE EXACT ALT CORRESPONDING TO THE REF RE
                  HRECHK  = (HTEMP(K) - HTEMP(K-1)) / 
     1              (RECHK(K) - RECHK(K-1)) * (REREF - RECHK(K-1)) + 
     2              HTEMP(K-1)
                  GO TO 58
               END IF
            END IF
   57    CONTINUE
   58    CALL CDFF (HRECHK,EM(I),CDFTAB(I),0)
CLVBE
C        COMPRESSIBILITY EFFECTS 
         CALL CDCC ( AR, TC, CAM, SW25, SREF, DB, TAPER, SBASE, SPI,
     &               BODYLD, EM(I), DELM, CDCTAB(I) )
CLVB UNCOMMENT FOR DEBUG          write(7,*) EM(I),'CDCTAB',DELM,BODYLD,CDCTAB(I)
         DO 40 J = 1,NUMCL
            NNNL = NNNL + 1
            DELCL = CL(J) - CLDES
  
C           PRESSURE DRAG 
            CALL CDPP ( AR, TC, DELCL, DELM, CAM, CDPTAB(I,J) )
            CDITAB(I,J) = CL(J) * CL(J) / CONSTPI / AR
C           IF FORWARD SWEEP, ADD WARNER ROBINS FACTOR
            IF ( SW25 .LT. 0. ) THEN
               CAYT = ((1.1 - .11 / (1.1 - EM(I) * COSA)) / 
     &           (1.1 - .11 / (1.1 - EM(I) * COSB)) - 1.) ** 2 / 2.
               CDITAB(I,J) = CDITAB(I,J) + CAYT * CL(J)**2 
            END IF

C           TOTAL DRAG DUE TO LIFT INCLUDING PRESSURE DRAG
            CDTAB(NNNL) = CDPTAB(I,J) + CDITAB(I,J)

   40    CONTINUE
   50 CONTINUE

C     END LOOP DEFINING CDTAB ARRAY
C     RNCORR GENERATES CDPA ARRAY
      CALL RNCORRLVB(WMAC, REREF)

C     PRINT RESULTS
      WRITE(7,*) '*'
      WRITE(7,*) '* DRAG POLARS (@ REFERENCE CONDITIONS)'
      WRITE(7,*) '*'
      WRITE(7,*) '*MACH         ALPHA        CL        CD'
      NNNL = NUMCL + 1
      DO I = 1,MNO
         NNNL = NNNL + 1       
          CD0 = CDFTAB(I) + CDCTAB(I)
         DO J=1,NUMCL
            NNNL = NNNL + 1
C           ESTIMATE AN ALPHA USING PRANDTL-GLAUERT
C           (* FROM T. TAKAHASHI EDET 2013a VERSION,
C              WITH CORRECTIONS FOR M=1)
C           FIRST LOOK AT EQUIVALENT MACH #
            VMACHLE = EM(I)*COS(SW25/CONSTRADTODEG)
C           SUBSONIC LEADING EDGE
C           FIRST CALC 2D USING PRANDTL GLAUERT
            IF (VMACHLE .EQ. 1.0) THEN
               CLALFA = 0.15
            ELSE IF (VMACHLE .LT. 1.0) THEN
               CLALFA = 0.109 / SQRT(1.-VMACHLE**2)
               IF (CLALFA .GT. 0.15) CLALFA = 0.15
C              3-D CALCULATION
               CLALFA = CLALFA/(1.+CLALFA*CONSTRADTODEG/(CONSTPI*AR))
            ELSE
C              SUPERSONIC LEADING EDGE 
               CLALFA = (4. / CONSTRADTODEG) / SQRT(VMACHLE**2 - 1.)
               IF (CLALFA .GT. 0.15) CLALFA = 0.15
            END IF
            ALFA = CL(J) / CLALFA
CLVB        BASIC MACH-DEPENDENT POLARS
            WRITE(7, 51) EM(I), ALFA, CL(J), CDTAB(NNNL) + CD0
         END DO
      END DO
   51 FORMAT(F6.3,4X,3F10.4)

      WRITE(7,*) '*'
      WRITE(7,*) '*    MACH       CDF       CDC BUFFET_CL'
      DO I = 1,MNO
         IF(CLBUF(I) .EQ. 0.) WRITE(7, 90) EM(I), CDFTAB(I), CDCTAB(I)
         IF(CLBUF(I) .NE. 0.) WRITE(7, 90) EM(I), CDFTAB(I), CDCTAB(I)
     1                                            , CLBUF(I)
      END DO
   90 FORMAT ( F10.3,F10.5,F10.5,F10.3)
 
      WRITE(7,*) '*'
      WRITE(7,*) '* INDUCED DRAG'
      WRITE(7,*) '*  CL           CDI'
      DO I = 1,NUMCL
         WRITE(7,51) CL(I), CDITAB(1,I)
      END DO       

      WRITE(7,*) '*' 
      WRITE(7,*) '* PRESSURE DRAG COEFFICIENTS...CDP'
      WRITE(7,*) '*MACH            CL   DEL_CDP'
      DO I= 1,MNO
         DO J= 1,NUMCL
            WRITE(7,51) EM(I), CL(J), CDPTAB(I,J)
         END DO
      END DO
	  
      WRITE(7,*) '*'
      WRITE(7,*) '* ZERO-LIFT DRAG'
      WRITE(7,*) '*MACH           CDF       CDC       CDO'
      NNNL = NUMCL + 1
      DO I = 1,MNO
            WRITE(7, 51) EM(I),CDFTAB(I), CDCTAB(I), 
     &                    CDFTAB(I) + CDCTAB(I)
      END DO	  
      WRITE(7,*) '*END'
       
  199 STOP
      END

CCCCCCCCCCCCCCCCCCCCCCC  SUBROUTINE SEPARATOR  CCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CCTOSS

C     INPUT SUBROUTINE COMMENT CARD TOSSER

      CHARACTER*80 X

       OPEN(UNIT=10, FILE='EDET.temp', ACTION='WRITE')

 10   READ(8,100,END=20) X
      
      IF (X(1:1).NE.'*') THEN
       WRITE(10,*) X(1:78)
      END IF

      GO TO 10
 20   CLOSE(UNIT=10)
 100  FORMAT(A78)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCC  SUBROUTINE SEPARATOR  CCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE BUFFET ( TC, DELM, SW25, AR, CAM, DELCLB )
  
C      *************************************************************
C      *                                                           *
C      *  BUFFET LIFT COEFFICIENT SUBROUTINE                       *
C      *                                                           *
C      *************************************************************
C      TC      WING THICKNESS-CHORD RATIO 
C      DELM    DELTA ABOVE DESIGN MACH NUMBER 
C      SW25    WING QUARTER-CHORD SWEEP 
C      AR      WING ASPECT RATIO
C      CAM     WING CAMBER
C      DELCLB  DELTA CL FOR BUFFET ONSET
  
      IMPLICIT DOUBLE PRECISION ( A-H, O-Z )
      COMMON /AEROTB/ AR05 (132),AR1  (132),AR2  (132),AR4  (132),
     1                AR6  (120),ARS07(110),ARS08(110),ARS10(110),
     2                ARS12(110),ARS14(110),ARS16(110),ARS18(110),
     3                ARS20(110),AMDES (36),CMDES (32),HSMDES(28),
     4                BINT (154),BSUB  (90),BSUP (105),BUFT  (99),
     5                PCAR (190),PCW  (112)
	  PARAMETER (CONSTRADTODEG = 57.29577951)
  
      ART    = TC**(2./3.)
      FCLB   = TRP2( BUFT, ART, DELM )
      DELCLB = FCLB * AR * (1 + CAM/10.0) / COS(SW25/CONSTRADTODEG)
  
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCC  SUBROUTINE SEPARATOR  CCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CDCC ( AR, TC, CAM, SW25, SREF, DB, TAPER, SBASE, SPI,
     1                  BODYLD, EM, DELM, CDCTAB ) 

C      *************************************************************
C      *                                                           *
C      *  COMPRESSIBLE DRAG SUBROUTINE...CDCC                      *
C      *                                                           *
C      *************************************************************
C      AR      WING ASPECT RATIO
C      TC      WING THICKNESS-CHORD RATIO 

C      CAM     WING CAMBER
C      SW25    WING QUARTER-CHORD SWEEP 
C      SREF    WING REFERENCE AREA
C      DB      FUSELAGE DIAMETER TO WING SPAN RATIO 
C      TAPER   WING TAPER RATIO 
C      SBASE   AIRCRAFT BASE AREA 
C      SPI     FUSELAGE CROSS-SECTIONAL AREA
C      BODYLD  FUSELAGE LENGTH-DIAMETER RATIO 
C      EM      MACH NUMBER
C      DELM    DELTA ABOVE DESIGN MACH NUMBER 
C      CDCTAB  COMPRESSIBLE DRAG COEFFICIENT
  
      IMPLICIT DOUBLE PRECISION ( A-H, O-Z )
      COMMON /AEROTB/ AR05 (132),AR1  (132),AR2  (132),AR4  (132),
     1                AR6  (120),ARS07(110),ARS08(110),ARS10(110),
     2                ARS12(110),ARS14(110),ARS16(110),ARS18(110),
     3                ARS20(110),AMDES (36),CMDES (32),HSMDES(28),
     4                BINT (154),BSUB  (90),BSUP (105),BUFT  (99),
     5                PCAR (190),PCW  (112)
	  PARAMETER (CONSTRADTODEG = 57.29577951)
      DATA CD5 / 0.0 /
  
C     SUBSONIC
  
      IF ( DELM .GT. 0.05 ) GO TO 10
      TOC    = TC ** (2./3.)
      CD1    = TRP2( PCW,DELM,TOC )
      CDCWNG = MAX(CD1,0.0D0) * TC ** (5.0 / 3.0) * (1.0 + CAM / 10.0)
      SOS    = 1.0 + SBASE / SPI
      CD2    = TRP2( BSUB,EM,SOS )
      CDCFUS = MAX( CD2,0.0D0 ) * SPI / SREF * (1.0 / BODYLD ** 2 ) 
      CDCTAB = CDCWNG + CDCFUS
      RETURN
  
C     SUPERSONIC
   10 ART = AR * TAN(SW25/CONSTRADTODEG) + (1.0 - TAPER)/(1.0 + TAPER)
CLVB UNCOMMENT FOR DEBUG       write(*,*) 'ART=',ART
      CD3    = TRP2( PCAR,DELM,ART )
CLVB UNCOMMENT FOR DEBUG       write(*,*) 'CD3=',CD3
      CDCWNG = MAX(CD3,0.0D0) * TC ** (5.0 / 3.0) * (1.0 + CAM / 10.0)
CLVB UNCOMMENT FOR DEBUG       write(*,*) 'CDCWNG=',CDCWNG
      SOS    = 1.0 + SBASE / SPI
CLVB UNCOMMENT FOR DEBUG       write(*,*) 'SOS=',SOS
      CD4    = TRP2( BSUP,EM,SOS )
CLVB UNCOMMENT FOR DEBUG       write(*,*) 'CD4=',CD4
      CDCFUS = MAX( CD4,0.0D0 ) * SPI / SREF * (1.0 / BODYLD ** 2 ) 
CLVB UNCOMMENT FOR DEBUG       write(*,*) 'CDCFUS=',CDCFUS
      IF (  EM   .GE. 1.0 ) CD5 = TRP2( BINT,EM,DB )
      IF ( TAPER .EQ. 1.0 ) TAPERR = 0.5
      IF ( TAPER .NE. 1.0 ) TAPERR = TAPER
      CDCINT = CD5 / (1.0 - TAPERR) / COS( SW25 / CONSTRADTODEG )
CLVB UNCOMMENT FOR DEBUG       write(*,*) 'CDCINT=',CDCINT
      CDCTAB = CDCWNG + CDCFUS + CDCINT
      RETURN
  
      END

CCCCCCCCCCCCCCCCCCCCCCC  SUBROUTINE SEPARATOR  CCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CDFF (ALT, AMCH, ANS, IPRINT)

C      *************************************************************
C      *                                                           *
C      *  SKIN FRICTION SUBROUTINE                                 *
C      *                                                           *
C      *************************************************************
C      ALT       ALTITUDE 
C      AMCH      MACH NUMBER
C      ANS       SKIN FRICTION DRAG COEFFICIENT 
C      IPRINT    PRINT SWITCH
  
      IMPLICIT DOUBLE PRECISION ( A-H, O-Z )
      COMMON /CONPON/ NAME(10)
    
      COMMON / MISSA/ AITEK ,AR    ,CAM,CH,DB ,BODYLD,SBASE, 
     1                SPI   ,SREF  ,SW25  ,TAPER,TC,VCMIN,CRUDF,
     2                CDCTAB(40)   ,CDF(42)      ,CDFCRD(10)   ,
     3                CDFTAB(40)   ,CDITAB(40,40),CDPTAB(40,40),
     4                CDREF(40)    ,CF(41)       ,CL(40)       ,
     5                CLBUF(40)    ,FF(40)       ,FR(40)       ,
     6                EM(40)       ,RN(40)       ,SWET(41)     ,
     7                TRU          ,TRL          ,EL(40)       ,
     8                ITAB  ,LAM   ,MNO   ,N     ,NCRUD 
	  PARAMETER (CONSTRADTODEG = 57.29577951)
      CHARACTER*16 NAME
  
      DIMENSION F(25)
      DATA F /11*0.,4.34255, -1.14281, .171203, -.0138334, .621712D-3,
     1         .137442D-6, -.145532D-4, 2.94206, 7.16974, 48.8876,
     2         -1403.02, 8598.76, -15834.3, 4.275/
  
      CDF(42)  = 0.0
      ANS      = 0.0
      SWET(41) = 0.0
  
C     LOOP ON NUMBER OF COMPONENTS

      DO 20 I = 1,N
         IF ( SWET(I) .LT. 0. ) SWET(I) = 0.
         IF ( EL(I)   .LE. 0. ) THEN
            FF(I)  = 1.
            FR(I)  = 1.
            RN(I)  = 0.
            CF(I)  = 0.
            CDF(I) = 0.
            GO TO 20
         ENDIF
         CALL CFF ( ALT,EL(I),AMCH,CF(I),RN(I) )
CLVBS    IF(LAM.EQ.1) CF(I) = CF(I) - 0.5 * (CF(I) - 1.328/SQRT(RN(I)))
CLVB     IN CURRENT VERSION APPLY LAMINAR % TO WING ONLY 
         IF(LAM.EQ.1.AND.I.EQ.1) THEN
            CF(I) = CF(I) - 0.5 * (CF(I) - 1.328/SQRT(RN(I))) 
     1       *(TRU*(0.0064164 + TRU*(0.48087D-4 - 0.12234D-6 * TRU)) 
     2       + TRL*(0.0064164 + TRL*(0.48087D-4 - 0.12234D-6 * TRL)))
         ENDIF
CLVBE
C           FORM FACTOR FOR BODIES
         IF ( FR(I) .GT. 0.5 ) THEN
            FF(I) = F(12) + FR(I) * (F(13) + FR(I) *
     1      ( F(14) + FR(I) * (F(15) + FR(I) * ( F(16) + FR(I) * 
     2      ( F(17) * FR(I) + F(18) ) ) ) ) )
            IF ( FR(I) .GE. 20.0 ) FF(I) = 1.0
         ELSE
  
C           FORM FACTORS FOR SURFACES

CLVB        See Takahashi "Aircraft Performance and Sizing, Vol 1",
CLVB        eqn. 4.12 for alternate form factor for wings.
CLVB        The block below reflects the Takahashi method and
CLVB        is taken from the edet2013a source from ASU
CLVBC    REVISED PER TAKAHASHI, GERMAN, et. al. AIAA J. of Aircraft - Vol 49, No. 1
CLVB            SWEEP = COS(ATAN(TAN(SW25/CONSTRADTODEG)+(1-TAPER) /
CLVB     1               (2*AR*(1+TAPER))))   
CLVB            AR2 = AR / (AR+2)
CLVB            
CLVB            FF(I)= 1.09372 - 0.09225 * TAPER + 1.32556 * FR(I) 
CLVB     1          + .46411 * AR2 - 0.83752 * SWEEP 
CLVB     1          + 0.01553 * TAPER * FR(I) + 0.16884 * TAPER * AR2
CLVB     1          + 0.15554 * TAPER * SWEEP
CLVB     1          - 1.78902 * FR(I) * AR2 - 2.62139 * FR(I) * SWEEP
CLVB     1          + 0.03318 * AR2 * SWEEP
CLVB     1          - 0.88546 * TAPER * FR(I) * AR2
CLVB     1          + 0.49463 * TAPER * FR(I) * SWEEP
CLVB     1          - 0.18727 * TAPER * AR2 * SWEEP
CLVB     1          + 8.00303 * FR(I) * AR2 * SWEEP
CLVB     1          - 0.0828 * TAPER **2 + 0.04733 * TAPER ** 3
CLVB     1          + 0.02271 * FR(I) **2 + 7.98567 * FR(I) ** 3
CLVB     1          - 24.97043 * FR(I) **4
CLVB     1          - 0.71383 * AR2**2 + 0.29892 * AR2**3
CLVB     1          + 1.245155 * SWEEP**2 - 0.63223 * SWEEP**3

CLVB        NOTE: the following is the orginal EDET/sfaero 
CLVB        form factor for surfaces
            FF1 = 1. + FR(I) *(F(19) + FR(I) *(F(20) + FR(I) * (F(21) +
     1        FR(I) * (F(22) + FR(I) * (F(23) + FR(I) * F(24) ) ) ) ) )
            FF2 = 1.0 + FR(I) * F(25)
            FF(I) = FF1 * (2.0 - AITEK) + FF2 * (AITEK - 1.0)
         ENDIF
  
         CDF(I)   = SWET(I) * CF(I) * FF(I) / SREF
         ANS      = ANS + SWET(I) * CF(I) / SREF
         CDF(42)  = CDF(42)  + CDF(I)
         SWET(41) = SWET(41) + SWET(I)
   20 CONTINUE

CLVB - Not sure why this is here to circumvent
CLVB   execution. Cannot be activated without 
CLVB   changing code    
CLVB      IF ( IPRINT .GT. 10 ) RETURN

CLVB - Change to reflect EDET CR-151971
CLVB   Not sure why 6% was used in sfaero.f.
CLVB   CR-151971 applies a factor of 1.284 for crud
CLVB     CDF(41) = 0.06 * CDF(42)
      CDF(41) = CRUDF * CDF(42)
      IF ( NCRUD .EQ. 0 ) GO TO 40
  
C     ADD DRAG FOR EXCRESCENCES
  
      DO 30 I = 1,NCRUD
         CDF(42) = CDF(42) + CDFCRD(I)
   30 CONTINUE
  
   40 ANS = CDF(42) + CDF(41)
      CFAVG = ANS * SREF / SWET(41)

C     PRINT RESULTS

      IF ( IPRINT .EQ. 0 ) RETURN
      
      WRITE(7, 80)
      IF (AITEK.EQ.1) THEN
         IF (LAM.EQ.1) THEN
            WRITE(7, 90) AMCH, ALT, SREF, 'LAMINAR-FLOW'
         ELSE
            WRITE(7, 90) AMCH, ALT, SREF, 'CONVENTIONAL'
         END IF
      ELSE
         WRITE (7,90) AMCH, ALT, SREF, 'SUPERCRITICAL'
      END IF

      WRITE(7, 100)
      DO 50 I = 1,N
         TEMP = RN(I) * 1.0D-6
         WRITE(7, 110) NAME(I), SWET(I), EL(I), FR(I),
     1                   FF(I), TEMP, CF(I), CDF(I)
   50 CONTINUE
      IF ( NCRUD .EQ. 0 ) GO TO 70
      DO 60 I = 1,NCRUD
         WRITE(7, 120) NAME(I),CDFCRD(I)
   60 CONTINUE
   70 CONTINUE
      WRITE(7, 130) CDF(41)
      WRITE(7, 140) SWET(41), CFAVG, ANS
      RETURN
  
   80 FORMAT (/' *   MACH          ALTITUDE     REFERENCE_AREA',5X,
     1         'TECHNOLOGY LEVEL')
   90 FORMAT ( F9.2,F15.0,' FT',F13.2,' SQ_FT',8X,A13)
  100 FORMAT (' * '/,' * COMPONENT',15X,'SWET',5X,'LENGTH   FINENESS',
     1         7X,'FORM',7X,'RN',9X,'CF        CDF'/,26X,'SQ_FT',
     2         9X,'FT',6X,'RATIO',5X,'FACTOR MILLIONS' )
  110 FORMAT ( 1X,A16,F14.2,F11.2,F11.4,F11.3,F9.1,2F11.5 )
  120 FORMAT ( ' DELTA ',A16,62X,F10.5 )
  130 FORMAT ( ' MISC/EXCRESENCE  ',67X,F10.5 )
  140 FORMAT ( /' TOTAL',11X,F14.2,42X,2F11.5 )
      END

CCCCCCCCCCCCCCCCCCCCCCC  SUBROUTINE SEPARATOR  CCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CDPP (AR, TC, DELCL, DELM, CAM, DCDP)

C      *************************************************************
C      *                                                           *
C      *  PRESSURE DRAG COEFFICIENT SUBROUTINE                     *
C      *                                                           *
C      *************************************************************
C      AR      WING ASPECT RATIO
C      TC      WING THICKNESS-CHORD RATIO 
C      DELCL   DELTA ABOVE DESIGN LIFT COEFFICIENT
C      DELM    DELTA ABOVE DESIGN MACH NUMBER 
C      CAM     WING CAMBER
C      DCDP    PRESSURE DRAG COEFFICIENT
  
      IMPLICIT DOUBLE PRECISION ( A-H, O-Z )
      COMMON /AEROTB/ AR05 (132),AR1  (132),AR2  (132),AR4  (132),
     1                AR6  (120),ARS07(110),ARS08(110),ARS10(110),
     2                ARS12(110),ARS14(110),ARS16(110),ARS18(110),
     3                ARS20(110),AMDES (36),CMDES (32),HSMDES(28),
     4                BINT (154),BSUB  (90),BSUP (105),BUFT  (99),
     5                PCAR (190),PCW  (112)

      DIMENSION B(5), C(5)
  
      A = AR * TC ** (1./3.)
C     FIND PROPER SET OF TABLES 
  
      IF (DELM .GT. 0.075) GO TO 10
      IF (A .LT. 0.5) GO TO 50
      IF (A .GT. 6.) GO TO 60
  
      C(1) = TRP2 ( AR05, DELM, DELCL )
      C(2) = TRP2 (  AR1, DELM, DELCL )
      C(3) = TRP2 (  AR2, DELM, DELCL )
      C(4) = TRP2 (  AR4, DELM, DELCL )
      C(5) = TRP2 (  AR6, DELM, DELCL )
      B(1) = .5
      B(2) = 1.
      B(3) = 2.
      B(4) = 4.
      B(5) = 6.
      GO TO 30
  
   10 IF (A .GT. 1.4) GO TO 20
      IF (A .LT. 0.7) GO TO 70
      B(1) = .7
      B(2) = .8
      B(3) = 1.0
      B(4) = 1.2
      B(5) = 1.4
      C(1) = TRP2 ( ARS07, DELM, DELCL )
      C(2) = TRP2 ( ARS08, DELM, DELCL )
      C(3) = TRP2 ( ARS10, DELM, DELCL )
      C(4) = TRP2 ( ARS12, DELM, DELCL )
      C(5) = TRP2 ( ARS14, DELM, DELCL )
      GO TO 30
  
   20 IF (A .GT. 2.) GO TO 80
      B(1) = 1.2
      B(2) = 1.4
      B(3) = 1.6
      B(4) = 1.8
      B(5) = 2.
      C(1) = TRP2 ( ARS12, DELM, DELCL )
      C(2) = TRP2 ( ARS14, DELM, DELCL )
      C(3) = TRP2 ( ARS16, DELM, DELCL )
      C(4) = TRP2 ( ARS18, DELM, DELCL )
      C(5) = TRP2 ( ARS20, DELM, DELCL )
  
   30 CALL XTERP (B, C, A, FCDP)
  
C     CALCULATE PRESSURE DRAG COEFFICIENT 
  
   40 DCDP = FCDP * (1. + CAM / 10.) * A / AR 
      IF (DCDP .LT. 0.) DCDP = 0.0
      RETURN
  
   50 SL1 = TRP2 ( AR05, DELM, DELCL )
      SL2 = TRP2 (  AR1, DELM, DELCL )
      FCDP = SL1 + (A - .5) * (SL2 - SL1) / .5
      GO TO 40
  
   60 SL1 = TRP2 (  AR4, DELM, DELCL )
      SL2 = TRP2 (  AR6, DELM, DELCL )
      FCDP = 2. * SL1 * SL2 / ((A - 4.) * SL1 - (A - 6.) * SL2)
      GO TO 40
  
   70 SL1 = TRP2 ( ARS07, DELM, DELCL )
      SL2 = TRP2 ( ARS08, DELM, DELCL )
      FCDP = SL1 + (A - .7) * (SL2 - SL1) / .1
      GO TO 40
  
   80 SL1 = TRP2 ( ARS18, DELM, DELCL )
      SL2 = TRP2 ( ARS20, DELM, DELCL )
      FCDP = SL2 + (A - 2.) * (SL2 - SL1) / .2
      GO TO 40
  
      END

CCCCCCCCCCCCCCCCCCCCCCC  SUBROUTINE SEPARATOR  CCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CFF ( H, EL, EM, CF, R ) 
  
C SOMMER AND SHORT T PRIME METHOD FOR SKIN FRICTION CALCULATIONS
C     H     GEOMETRIC ALTITUDE IN FEET
C     EL    CHARACTERISTIC LENGTH 
C     EM    MACH NUMBER 
C     CF    SKIN FRICTION COEFFICIENT 
C     R     REYNOLDS NUMBER 

      IMPLICIT DOUBLE PRECISION ( A-H, O-Z )
      DATA CONLOG /2.302585/
      DTC  = 0.
      CF   = 0.
      R    = 0.
      IF ( EM * EL .LE. 0. ) RETURN
C     GIVEN H, FIND TEMPERATURE AND RE/FT/M, IGNORE THE REST  
      CALL ATMO(H,DTC,DELTA,THETA,ASTAR,T,RE,HFT) 
C     CONVERT TO RANKINE
      T    = T * 1.8  
C     SUTHERLAND'S CONSTANT IS 198.72 DEG R FROM 1962 ON
      T216 = T + 198.72
      E    = 0.80
C     COMBINED CONSTANT INCLUDING 1/RHO 
      ESH  = 4.593153D-6 * E * T216 / (RE * EM * T ** 1.5)
C     REYNOLDS NUMBER 
      R    = RE * EM * EL 
C     WALL TEMPERATURE
      TAW  = (1.0 + 0.176 * EM * EM) * T
      TW   = TAW
C     INITIAL GUESS AT SKIN FRICTION COEFFICIENT
      CFPC = (0.242 / (LOG(R*0.0015)/CONLOG) ) ** 2
C     WALL TEMPERATURE RATIO
      TPT  = 1.0 + 0.035 * EM * EM + 0.45 * (TW / T - 1.0)
C     THIS ITERATIVE LOOP WILL CONVERGE THROUGH MACH 25 
      DO 20 I = 1,5
         CFL = CFPC / (1.0 + 3.59 * SQRT( CFPC )) / TPT
         TW  = (TAW / (1.0 + ESH * TW ** 3 / CFL) + TW) * 0.5
         TW  = (TAW / (1.0 + ESH * TW ** 3 / CFL) + TW) * 0.5
         TPT = 1.0 + 0.035 * EM * EM + 0.45 * (TW / T - 1.0)
         RP = R * (TPT * T + 198.72) / (T216 * TPT * TPT * SQRT( TPT ))
         DO 10 K = 1,3
            CFPC = (0.242 / (LOG(RP*CFPC)/CONLOG) ) ** 2
   10    CONTINUE
         CF = CFPC / TPT
   20 CONTINUE
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCC  SUBROUTINE SEPARATOR  CCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE RNCORRLVB(WMAC, REREF)

C      *************************************************************
C      *                                                           *
C      *  GENERATE CDPA ARRAY WHICH CONTAINS THE CHANGE OF SKIN    *
C      *  FRICTION DRAG DUE TO CHANGES IN ALTITUDE AND MACH NUMBER *
C      *                                                           *
C      *************************************************************
  
      IMPLICIT DOUBLE PRECISION ( A-H, O-Z )
CLVB      COMMON /UNITS / IU5, IU6, IU7, IU8, IU9, IU16, IU17, IU18
      DIMENSION HTEMP(60), RECHK(60)
CLVB      COMMON / AERO / CDTAB(1200),CDPA(1000)
      COMMON / MISSA/ AITEK ,AR    ,CAM,CH,DB ,BODYLD,SBASE, 
     1                SPI   ,SREF  ,SW25  ,TAPER,TC,VCMIN,CRUDF,
     2                CDCTAB(40)   ,CDF(42)      ,CDFCRD(10)   ,
     3                CDFTAB(40)   ,CDITAB(40,40),CDPTAB(40,40),
     4                CDREF(40)    ,CF(41)       ,CL(40)       ,
     5                CLBUF(40)    ,FF(40)       ,FR(40)       ,
     6                EM(40)       ,RN(40)       ,SWET(41)     ,
     7                TRU          ,TRL          ,EL(40)       ,
     8                ITAB  ,LAM   ,MNO   ,N     ,NCRUD  
  
C     WRITE HEADERS
      WRITE(7,50)
  
C     REGENERATE BASELINE RN SKIN FRICTION DRAG 
  
      DO 10 I = 1,MNO
         IF (ABS(EM(I)-VCMIN).LT..00001) THEN
            HRECHK = CH
            GO TO 8
         END IF
		 DO 7 K = 1,54
            HTEMP(K) = 2000D0 * (K - 11)
            DTC = 0D0
            CALL ATMO(HTEMP(K),DTC,DELTA,THETA,ASTAR,T,RETEMP,HFT)
            RECHK(K) = RETEMP*EM(I)*WMAC
            IF (K.GT.1) THEN
               IF (RECHK(K).LE.REREF) THEN
                  HRECHK  = (HTEMP(K) - HTEMP(K-1)) / 
     1            (RECHK(K) - RECHK(K-1)) * (REREF - RECHK(K-1)) + 
     2            HTEMP(K-1)    
                  GO TO 8
                  END IF
               END IF
   7    CONTINUE  
   8    CALL CDFF (HRECHK,EM(I),CDREF(I),0)
   10 CONTINUE
  
C     GENERATE SKIN FRICTION DRAG COEFFICIENT DELTAS - LOOP ON ALTITUDE 
      DO I = 1,18
         HCDP = 5000. * (I - 1)			
C        THE FIRST ELEMENT IN EACH ROW IS THE ALTITUDE          
C        LOOP ON MACH NUMBER 
         DO J = 1,MNO
            CALL CDFF (HCDP,EM(J),CDRN,0) 
            CDPALT = CDRN - CDREF(J)    
            WRITE(7, 60) HCDP,EM(J),CDPALT
         END DO
      END DO
      RETURN
   50 FORMAT (' *'/' * CHANGE IN DRAG COEFFICIENT FROM REF ALT & MACH',
     1        /' * ALTITUDE     MACH  DELTA_CD')
   60 FORMAT (1X,F9.0,1X,F9.3,1X,F9.5)
      END

CCCCCCCCCCCCCCCCCCCCCCC  SUBROUTINE SEPARATOR  CCCCCCCCCCCCCCCCCCCCCCCCC

      FUNCTION TRP2 ( T, X, Y ) 

C     **************************************************************
C     *                                                            *
C     *    BIVARIANT PARABOLIC OR LINEAR INTERPOLATION FUNCTION    *
C     *                                                            *
C     **************************************************************

C     T IS A SPECIAL FORMAT ARRAY CONTAINING ALL TABULAR DATA
C     T(1) DEFINES ARRAY SIZES = NX (NUMBER OF X POINTS) * 1000 +
C                                NY (NUMBER OF Y POINTS).
C     T(2) TO T(1 + NY) ARE Y VALUES
C     T(2 + NY) IS THE FIRST X VALUE
C     T(3 + NY) TO T(2 + NY * 2) ARE THE FIRST Z VALUES
C     THE X - Z PATTERN IS REPEATED NX TIMES
C
C               NX0NY   Y1   Y2   Y3   Y4   Y5  . . .
C                  X1  Z11  Z12  Z13  Z14  Z15  . . .
C                  X2  Z21  Z22  Z23  Z24  Z25  . . .
C                  X3  Z31  Z32  Z33  Z34  Z35  . . .
C                   .    .    .    .    .    .  . . .
C
C     X, Y = POINTS AT WHICH Z IS TO BE INTERPOLATED
C     USES EVERY POINT FOR BRACKETING AND INTERPOLATION
C     TRP2 = RESULTING INTERPOLATED Z VALUE
C     LVB 6/12/2017: REMOVED OPTION ("M=2") TO USE EVERY OTHER  
C                    POINT FOR BRACKETING (PARABOLIC ONLY)
  
      IMPLICIT DOUBLE PRECISION ( A-H, O-Z )
      COMMON /INTPPA/ ITPAER
  
      DIMENSION  T(*), Z(3)
      NDIM = INT ( T(1) )
      NX = NDIM / 1000
      NY = NDIM - 1000 * NX

C     ARRAY DIMENSIONS

      I = NX + 1
      J = NY + 1

C     LINEAR INTERPOLATION

   60 J1 = J * 2 + 1
      L1 = J * (I-2) + 1
      DO 70 K = J1,L1,J
         IF ( T(K) .GE. X ) GO TO 80
   70 CONTINUE
      K = L1 + J
   80 DELX = (T(K) - X) / (T(K) - T(K-J))
      J2 = J - 1
      DO 90 L = 3,J2
         IF ( T(L) .GE. Y ) GO TO 100
   90 CONTINUE
      L = J
  100 DELY = (T(L) - Y) / (T(L) - T(L-1))
      N = K + L - 1
      TRP2 = DELX      * (DELY * T(N-J-1) + (1.-DELY) * T(N-J))
     1     + (1.-DELX) * (DELY * T(N-1)   + (1.-DELY) * T(N))

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCC  SUBROUTINE SEPARATOR  CCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE XTERP ( X, Y, XO, YO )

C     *************************************************************
C     *                                                           *
C     *           MONOVARIANT INTERPOLATION SUBROUTINE            *
C     *                                                           *
C     *************************************************************
  
C     X IS A SET OF 5 ABSCISSA POINTS
C     Y IS A SET OF 5 ORDINATE POINTS
C     XO IS THE QUESTION....YO IS THE ANSWER
  
      IMPLICIT DOUBLE PRECISION ( A-H, O-Z )
      
      DIMENSION X(5), Y(5), T(5), EM(5)
  
      DO 10 I = 1,4
         DX    = X(I + 1) - X(I)
         DY    = Y(I + 1) - Y(I)
         EM(I) = DY / DX
   10 CONTINUE
  
      XE1 = X(4) + X(5) - X(3)
      XE2 = 2. * X(5) - X(3)
  
      D1  = Y(5) - Y(4)
      D2  = X(5) - X(4)
      D3  = Y(4) - Y(3)
      D4  = X(4) - X(3)
      D5  = XE1 - X(5)
      D7  = XE2 - XE1
      YE1 = (2. * D1 / D2 - D3 / D4) * D5 + Y(5)
      D6  = YE1 - Y(5)
      YE2 = (2. * D6 / D5 - D1 / D2) * D7 + YE1
  
      XS2 = X(2) - X(3) + X(1)
      XS1 = 2. * X(1) - X(3)
  
      D1  = Y(2) - Y(1)
      D2  = X(2) - X(1)
      D3  = Y(3) - Y(2)
      D4  = X(3) - X(2)
      D5  = X(1) - XS2
      D7  = X(1) - XS2
      D8  = XS2 - XS1
      YS2 = -(2. * D1 / D2 - D3 / D4) * D5 + Y(1)
      D6  = Y(1) - YS2
      YS1 = -(2. * D6 / D7 - D1 / D2) * D8 + YS2
  
      EM(5) = (YE1 - Y(5)) / (XE1 - X(5))
      EME1  = (YE2 - YE1) / (XE2 - XE1)
      EMS2  = D6 / D7
      EMS1  = (YS2 - YS1) / D8
  
      D1  = ABS(EM(2) - EM(1))
      D2  = ABS(EMS2 - EMS1)
      IF ( (D1 + D2) .EQ. 0. ) THEN
         T(1) = (EMS2 + EM(1)) / 2.
      ELSE
         T(1) = (D1 * EMS2 + D2 * EM(1)) / (D1 + D2)
      ENDIF

      D3  = ABS(EM(3) - EM(2))
      D4  = ABS(EM(1) - EMS2)
      IF ( (D3 + D4) .EQ. 0. ) THEN
         T(2) = (EM(1) + EM(2)) / 2.
      ELSE
         T(2) = (D3 * EM(1) + D4 * EM(2)) / (D3 + D4)
      ENDIF

      D5  = ABS(EM(4) - EM(3))
      D6  = ABS(EM(2) - EM(1))
      IF ( (D5 + D6) .EQ. 0. ) THEN
         T(3) = (EM(2) + EM(3)) / 2.
      ELSE
         T(3) = (D5 * EM(2) + D6 * EM(3)) / (D5 + D6)
      ENDIF

      D7  = ABS(EM(5) - EM(4))
      D8  = ABS(EM(3) - EM(2))
      IF ( (D7 + D8) .EQ. 0. ) THEN
         T(4) = (EM(3) + EM(4)) / 2.
      ELSE
         T(4) = (D7 * EM(3) + D8 * EM(4)) / (D7 + D8)
      ENDIF

      D9  = ABS(EME1 - EM(5))
      D10 = ABS(EM(4) - EM(3))
      IF ( (D9 + D10) .EQ. 0. ) THEN
         T(5) = (EM(4) + EM(5)) / 2.
      ELSE
         T(5) = (D9 * EM(4) + D10 * EM(5)) / (D9 + D10)
      ENDIF

      IF (XO .LT. X(1) .AND. XO .GE. XS2) GO TO 40
      IF (XO .LT. XS2  .AND. XO .GE. XS1) GO TO 50
  
      DO 20 J = 2,5
         IF (XO .GT. X(J)) GO TO 20
         JJ = J - 1
         GO TO 30
   20 CONTINUE
      GO TO 60
   30 P0  = Y(JJ)
      P1  = T(JJ)
      DY  = Y(JJ + 1) - Y(JJ)
      DX  = X(JJ + 1) - X(JJ)
      P2  = (3. * DY / DX - 2. * T(JJ) - T(JJ + 1)) / DX
      P3  = (T(JJ) + T(JJ + 1) - 2. * DY / DX) / (DX * DX)
      DEL = XO - X(JJ)
      YO  = P0 + DEL * (P1 + DEL * (P2 + DEL * P3))
      GO TO 100
   40 YO  = (Y(1) - YS2) / (X(1) - XS2) * (XO - XS2) + YS2
      GO TO 90
   50 YO  = (YS2 - YS1) / (XS2 - XS1) * (XO - XS1) + YS1
      GO TO 90
   60 IF (XO .LE. XE1 .AND. XO .GT. X(5)) GO TO 70
      IF (XO .LE. XE2 .AND. XO .GT. XE1 ) GO TO 80
      YO  = 999999999.D0
      WRITE(7, 120) XO
      WRITE(7, 110) (X(I), I = 1,5)
      WRITE(7, 110) (Y(I), I = 1,5)
      GO TO 100
   70 YO  = (YE1 - Y(5)) / (XE1 - X(5)) * (XO - X(5)) + Y(5)
      GO TO 90
   80 YO  = (YE2 - YE1) / (XE2 - XE1) * (XO - XE1) + YE1
   90 WRITE(7, 130)
  100 CONTINUE
  
      RETURN
  
  110    FORMAT (' B   ',5D13.5)
  120    FORMAT (/' XO=',D13.5,' OUTSIDE RANGE EXTRAP DATA')
  130    FORMAT (/' OUTPUT DETERMINED FROM EXTRAPOLATED DATA')
      END

CCCCCCCCCCCCCCCCCCCCCCC  SUBROUTINE SEPARATOR  CCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CLDESN (TC, AR, SW25, CAM, CLDES, CL, NUMCL) 

C      *************************************************************
C      *                                                           *
C      *  DESIGN CL SUBROUTINE                                     *
C      *                                                           *
C      *************************************************************
C      TC      WING THICKNESS-CHORD RATIO 
C      AR      WING ASPECT RATIO
C      SW25    WING QUARTER-CHORD SWEEP 
C      CAM     WING CAMBER
C      CLDES   DESIGN LIFT COEFFICIENT
C      CL      ARRAY OF LIFT COEFFICIENTS FOR TABULAR DATA
C      NUMCL   NUMBER OF VALID ELEMENTS IN CL ARRAY 

      IMPLICIT DOUBLE PRECISION ( A-H, O-Z )
	  PARAMETER (CONSTRADTODEG = 57.29577951)
      DIMENSION CL(40)
  
      IF (TC .LE. 0.065) GO TO 10 
  
C     SUBSONIC - IF T/C > .065, WING IS ASSUMED TO BE SUBSONIC
  
      CLDES = (.029 + .1843*AR) * COS(SW25/CONSTRADTODEG) * 
     1        (1. + CAM/10.) / SQRT(AR) 
      GO TO 20
  
C     SUPERSONIC
  
   10 FAR = AR * TC ** (1. / 3.)
      CLDES = -.06416 + FAR * (.530389 + FAR * (.0376684*FAR -.214493))
  
C     ROUND TO NEAREST .05
  
   20 CLLOP = AINT(CLDES * 20. +.5) / 20. 
  
C     ZERO ARRAY
      DO 30 I=1,40
         CL(I) = 0.0D0
   30 CONTINUE
  
C     SET UP CL ARRAY 
      CL(1) = CLLOP - .4
      IF (CL(1) .LT. 0.) CL(1) = 0.0
      DO 40 I = 2,15
         NUMCL = I
         CL(I) = CL(I - 1) + .05
         IF ((CL(I) - CLDES) .GT. 0.3) GO TO 50 
   40 CONTINUE
   50 RETURN
  
      END

CCCCCCCCCCCCCCCCCCCCCCC  SUBROUTINE SEPARATOR  CCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MDESN(TC,AR,SW25,AITEK,CLDES,DESM,EM,MNO,TAPER)

C      *************************************************************
C      *                                                           *
C      *  DESIGN MACH NUMBER ROUTINE                               *
C      *                                                           *
C      *************************************************************
C      TC      WING THICKNESS-CHORD RATIO 
C      AR      WING ASPECT RATIO
C      SW25    WING QUARTER-CHORD SWEEP 
C      AITEK   WING TECHNOLOGY FACTOR 
C      CLDES   DESIGN LIFT COEFFICIENT
C      DESM    DESIGN MACH NUMBER 
C      EM      ARRAY OF MACH NUMBERS FOR TABULAR DATA 
C      MNO     NUMBER OF VALID ELEMENTS IN EM ARRAY 
C      TAPER   WING TAPER RATIO
  
      IMPLICIT DOUBLE PRECISION ( A-H, O-Z )
      COMMON /AEROTB/ AR05 (132),AR1  (132),AR2  (132),AR4  (132),
     1                AR6  (120),ARS07(110),ARS08(110),ARS10(110),
     2                ARS12(110),ARS14(110),ARS16(110),ARS18(110),
     3                ARS20(110),AMDES (36),CMDES (32),HSMDES(28),
     4                BINT (154),BSUB  (90),BSUP (105),BUFT  (99),
     5                PCAR (190),PCW  (112)
      COMMON /VLIMIT/ VMAX  ,VMMO
	  PARAMETER (CONSTRADTODEG = 57.29577951)
      DIMENSION EM(40)
  
      IF ( DESM .GT. 0. ) GO TO 30
      IF ( TC .GT. .065 .OR. VMAX .LT. 1. ) GO TO 10
  
C     SUPERSONIC
      DESM2D = TRP2(HSMDES,CLDES,TC)
      GO TO 20
  
C     SUBSONIC
   10 TC23 = TC ** (2. / 3.)
      ANS1 = TRP2(CMDES,CLDES,TC23) 
      ANS2 = TRP2(AMDES,CLDES,TC23) 
      ANS = ANS1 * (2D0 - AITEK) + ANS2 * (AITEK - 1.)
      DESM2D = SQRT(ANS + 1D0)
  
   20 DMDSWP = 0.32D0 * (1D0 - COS(SW25 / CONSTRADTODEG)) 
      DMDAR = 0.144D0 / AR
      DESM = DESM2D + DMDSWP + DMDAR
      IF (SW25 .GE. 0.) GO TO 30
  
C     FORWARD SWEEP MODIFICATION
  
      TH = TAN(SW25/CONSTRADTODEG) - (1D0 - TAPER)/(1D0 + TAPER)/AR 
      S2A = 2D0 * TH / (1D0 + TH * TH)
      DESM = DESM * (1D0 + (0.01D0 + 0.04D0 / AR) * (1D0 - TAPER) * S2A)

   30 IF (TC .GT. 0.065) VELLMT = DESM + 0.075D0
      IF (TC .LE. 0.065) VELLMT = 2D0

C     ZERO OUT ARRAY
      DO 40 I = 1,40
         EM(I) = 0.0D0
   40 CONTINUE
  
C     SET UP MACH NUMBER ARRAY
  
      EM(1) = 0.2D0
      MF = 1
      IF (VELLMT .GT. 1.6) MF = 2 
      DM = 0.1D0 * MF
      IF ( DESM .LT. 0.5D0 ) THEN
         EM(1) = 0.1D0
         DM = 0.05D0
      ENDIF
      IF ( DESM .LT. 0.3D0 ) DM = 0.025D0
      DO 50 I = 2,20
         MNO = I - 1
         EM(I) = EM(I - 1) + DM
         IF (EM(I) .GE. VELLMT) GO TO 60
         IF ((EM(I) - DESM) .GT. -.25) DM = 0.10D0
         IF ((EM(I) - DESM) .GT. -.15) DM = 0.05D0
         IF ((EM(I) - DESM) .GE. -.075) DM = 0.025D0
         IF (EM(I) .GT. (DESM + .05)) DM = 0.05D0
         IF (EM(I) .GT. 1.15D0) THEN
			IF(EM(I).LE.1.3D0) THEN
				EM(I) = 1.3D0
			ENDIF
			DM = 0.1D0 * MF
			IF(EM(I)+DM.GT.VELLMT) THEN
				EM(I+1) = VELLMT
				MNO = I+1
				GO TO 60
			ENDIF
		 ENDIF
   50 CONTINUE
   60 RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCC  SUBROUTINE SEPARATOR  CCCCCCCCCCCCCCCCCCCCCCCCC

      BLOCK DATA AROTAB 
  
C  INITIALIZE ALL AERODYNAMIC TABLES AND COMPONENT NAMES 
C  DRAG POLARS SMOOTHED BY PHIL ARCARA 10/4/90
  
      IMPLICIT DOUBLE PRECISION ( A-H, O-Z )
      COMMON / AERO / CDTAB(1200),CDPA(1000)
      COMMON /WAVDAT/ TREF  ,SREF  ,ELTOT ,VOLTOT,AWETN ,CEXP  ,
     1                DCDNAC (40)  ,ELW   (40)   ,
     2                AWETT  (40)  ,AWETW (40)   ,
     3                FORM  (40)  
      COMMON /WAVPLT/ PCDW(40), PMAW(40), NCDW
      COMMON /AEROTB/ AR05 (132),AR1  (132),AR2  (132),AR4  (132),
     1                AR6  (120),ARS07(110),ARS08(110),ARS10(110),
     2                ARS12(110),ARS14(110),ARS16(110),ARS18(110),
     3                ARS20(110),AMDES (36),CMDES (32),HSMDES(28),
     4                BINT (154),BSUB  (90),BSUP (105),BUFT  (99),
     5                PCAR (190),PCW  (112)
      COMMON /RFHDAT/ CK(30), CLB(30),
     1                REFAS, REFBS, REFAT, REFBT, EXPS, EXPT,
     2                MMACH
      COMMON /CONPON/ NAME(10)
      CHARACTER*16 NAME
  
C     ZERO OUT POTENTIAL INPUT ARRAYS 
  
      DATA CDTAB /1200*0./
      DATA CDPA  /1000*0./
      
C     COMPONENT NAMES TO BE USED IN OUTPUT
  
      DATA NAME / 'WING            ',
     1            'FUSELAGE        ',
     2            'HORIZONTAL_TAIL ',
     3            'VERTICAL___TAIL ',
     4            'NACELLES        ',
     5            'MISC_1          ',
     6            'MISC_2          ',
     7            'MISC_3          ',
     8            'MISC_4          ',
     9            'MISC_5          ' /
     
  
C      *************************************************************
C      *                                                           *
C      *  TABLES OF CDP/(T/C)**(1/3)/(1+%CAM/10)                   *
C      *  DELTA MACH ON THE VERTICAL, DELTA CL ON THE HORIZONTAL   *
C      *                                                           *
C      *************************************************************
  
C                               AR * (T/C)**(1/3) = 0.5 
  
      DATA AR05/011010.,
     *       -.40,  -.30, -.20, -.10, -.05, 0.0,  .05, .10,  .20, .30,
     *-.80, .00150,.0004,.0015,.0024,.0035,.0060,.014,.028,.0825,.1500, 
     *-.20, .00150,.0004,.0015,.0024,.0035,.0060,.014,.028,.0825,.1500, 
     *-.16, .00150,.0004,.0015,.0024,.0035,.0060,.014,.028,.0825,.1500, 
     *-.12, .00150,.0004,.0015,.0024,.0035,.0060,.014,.028,.0825,.1500, 
     *-.08, .00150,.0004,.0015,.0024,.0035,.0060,.014,.028,.0825,.1500, 
     *-.04, .00150,.0004,.0015,.0026,.0035,.0075,.014,.028,.0825,.1525, 
     *-.02, .00160,.0004,.0016,.0028,.0035,.0085,.014,.029,.0840,.1560, 
     *0.00, .00180,.0004,.0018,.0030,.0036,.0100,.014,.031,.0860,.1620, 
     * .02, .00205,.0004,.0021,.0031,.0039,.0100,.016,.033,.0895,.1680, 
     * .04, .00230,.0004,.0023,.0032,.0045,.0100,.018,.034,.0955,.1725, 
     * .05, .00240,.0004,.0024,.0032,.0049,.0100,.019,.034,.1000,.1750/ 
  
C                               AR * (T/C)**(1/3) = 1.0 
      DATA AR1/011010., 
     *      -.40, -.30, -.20,  -.10, -.05, 0.0,  .05,   .10,  .20,  .30,
     *-.80,.0016,.0005,.0016,.00290,.0051,.0065,.0130,.0240,.0600,.1200,
     *-.20,.0016,.0005,.0016,.00290,.0051,.0065,.0130,.0240,.0600,.1200,
     *-.16,.0016,.0005,.0016,.00295,.0051,.0065,.0130,.0240,.0600,.1200,
     *-.12,.0016,.0005,.0016,.00305,.0051,.0065,.0130,.0240,.0600,.1200,
     *-.08,.0016,.0005,.0016,.00315,.0051,.0065,.0130,.0240,.0600,.1200,
     *-.04,.0017,.0005,.0017,.00350,.0053,.0065,.0135,.0245,.0600,.1200,
     *-.02,.0018,.0005,.0018,.00390,.0055,.0075,.0150,.0260,.0620,.1235,
     *0.00,.0019,.0005,.0019,.00425,.0060,.0095,.0165,.0275,.0645,.1275,
     * .02,.0022,.0005,.0022,.00450,.0069,.0115,.0190,.0300,.0670,.1325,
     * .04,.0026,.0005,.0026,.00465,.0084,.0140,.0230,.0340,.0710,.1380,
     * .05,.0030,.0005,.0030,.00475,.0093,.0160,.0260,.0375,.0740,.1410/
  
C                               AR * (T/C)**(1/3) = 2.0 
      DATA AR2/011010., 
     *      -.40, -.30, -.20, -.10, -.05,  0.0,  .05,  .10,  .20,  .30, 
     *-.80,.0015,.0005,.0015,.0028,.0047,.0075,.0110,.0180,.0350,.0730, 
     *-.20,.0015,.0005,.0015,.0028,.0047,.0075,.0110,.0180,.0350,.0730, 
     *-.16,.0015,.0005,.0015,.0029,.0047,.0075,.0110,.0180,.0350,.0730, 
     *-.12,.0016,.0005,.0016,.0031,.0047,.0075,.0110,.0180,.0350,.0730, 
     *-.08,.0016,.0005,.0016,.0034,.0047,.0075,.0110,.0180,.0350,.0730, 
     *-.04,.0018,.0005,.0018,.0037,.0049,.0075,.0120,.0185,.0350,.0730, 
     *-.02,.0019,.0005,.0019,.0041,.0053,.0087,.0140,.0205,.0365,.0735, 
     *0.00,.0021,.0005,.0021,.0047,.0065,.0106,.0165,.0235,.0400,.0750, 
     * .02,.0022,.0006,.0026,.0057,.0084,.0140,.0195,.0280,.0460,.0780, 
     * .04,.0029,.0006,.0033,.0075,.0110,.0170,.0245,.0325,.0550,.0840, 
     * .05,.0037,.0007,.0037,.0088,.0135,.0210,.0290,.0370,.0600,.0900/ 
  
C                               AR * (T/C)**(1/3) = 4.0 
      DATA AR4/011010., 
     *      -.40, -.30, -.20,  -.10,  -.05,  0.0,  .05,  .10, .20, .30, 
     *-.80,.0011,.0004,.0011,.00280,.00340,.0055,.0080,.0115,.020,.036, 
     *-.20,.0011,.0004,.0011,.00280,.00340,.0055,.0080,.0115,.020,.036, 
     *-.16,.0011,.0004,.0011,.00280,.00345,.0055,.0080,.0115,.020,.036, 
     *-.12,.0012,.0004,.0012,.00285,.00355,.0055,.0080,.0115,.020,.036, 
     *-.08,.0013,.0004,.0013,.00290,.00365,.0055,.0080,.0115,.020,.036, 
     *-.04,.0014,.0004,.0014,.00305,.00390,.0055,.0080,.0115,.020,.036, 
     *-.02,.0016,.0004,.0016,.00320,.00410,.0058,.0081,.0123,.021,.037, 
     *0.00,.0019,.0004,.0019,.00355,.00460,.0075,.0112,.0158,.026,.040, 
     * .02,.0024,.0005,.0024,.00450,.00600,.0100,.0140,.0195,.031,.046, 
     * .04,.0032,.0006,.0035,.00680,.01220,.0180,.0240,.0300,.043,.056, 
     * .05,.0044,.0006,.0044,.01130,.01810,.0240,.0310,.0380,.051,.064/ 
  
C                               AR * (T/C)**(1/3) = 6.0 
      DATA AR6/011009., 
     *       -.40,  -.30,  -.20,  -.10,  -.05,  0.0,  .05,  .10,  .20,
     *-.80,.00085,.00020,.00085,.00200,.00270,.0041,.0058,.0071,.0107,
     *-.20,.00085,.00020,.00085,.00200,.00270,.0041,.0058,.0071,.0107,
     *-.16,.00089,.00020,.00089,.00200,.00270,.0041,.0058,.0071,.0107,
     *-.12,.00096,.00020,.00096,.00200,.00270,.0041,.0058,.0071,.0107,
     *-.08,.00108,.00020,.00108,.00200,.00270,.0041,.0058,.0071,.0107,
     *-.04,.00120,.00026,.00120,.00200,.00270,.0041,.0058,.0071,.0117,
     *-.02,.00130,.00030,.00130,.00200,.00270,.0041,.0058,.0077,.0167,
     *0.00,.00148,.00034,.00148,.00230,.00290,.0043,.0061,.0112,.0240,
     * .02,.00185,.00037,.00185,.00320,.00450,.0072,.0130,.0185,.0395,
     * .04,.00290,.00045,.00290,.00700,.01050,.0145,.0240,.0330,.0545,
     * .05,.00390,.00052,.00390,.01150,.01600,.0270,.0370,.0470,.0680/
  
C                               AR * (T/C)**(1/3) =  .7 
      DATA ARS07/009010., 
     *       -.40, -.30, -.20, -.10, -.05,  0.0,  .05,  .10, .20, .30,
     * .05, .0015,.0000,.0015,.0045,.0080,.0135,.0200,.0360,.085,.165,
     * .10, .0017,.0000,.0017,.0045,.0090,.0150,.0235,.0415,.091,.200,
     * .15, .0018,.0000,.0018,.0047,.0105,.0170,.0265,.0480,.100,.230,
     * .20, .0020,.0000,.0020,.0050,.0120,.0190,.0300,.0550,.112,.260,
     * .30, .0030,.0000,.0030,.0060,.0155,.0240,.0395,.0720,.145,.320,
     * .50, .0040,.0000,.0040,.0083,.0225,.0360,.0675,.1120,.210,.440,
     * .70, .0040,.0005,.0040,.0120,.0295,.0550,.0900,.1460,.273,.550,
     * .90, .0036,.0007,.0036,.0160,.0365,.0650,.1100,.1740,.330,.630,
     *1.10, .0034,.0008,.0034,.0200,.0425,.0740,.1300,.2000,.380,.700/
  
C                               AR * (T/C)**(1/3) =  .8 
      DATA ARS08/009010., 
     *    -.40,-.30, -.20, -.10, -.05,  0.0, .05, .10, .20, .30,
     * .05,0.0,.001,.0020,.0040,.0050,.0100,.015,.025,.080,.140,
     * .10,0.0,.001,.0025,.0050,.0075,.0140,.018,.029,.086,.158,
     * .15,0.0,.001,.0030,.0062,.0090,.0170,.026,.037,.094,.171,
     * .20,0.0,.001,.0035,.0075,.0115,.0200,.032,.051,.103,.185,
     * .30,0.0,.001,.0040,.0090,.0160,.0275,.045,.072,.130,.236,
     * .50,0.0,.001,.0050,.0130,.0265,.0440,.069,.106,.190,.335,
     * .70,0.0,.001,.0060,.0160,.0350,.0590,.092,.143,.245,.420,
     * .90,0.0,.001,.0065,.0210,.0430,.0710,.113,.175,.290,.480,
     *1.10,0.0,.001,.0070,.0260,.0510,.0800,.135,.200,.320,.520/
  
C                               AR * (T/C)**(1/3) = 1.0 
      DATA ARS10/009010., 
     *    -.40, -.30, -.20, -.10, -.05,  0.0, .05,  .10, .20, .30,
     * .05,0.0,.0001,.0030,.0060,.0080,.0155,.020,.0270,.072,.126,
     * .10,0.0,.0002,.0037,.0075,.0115,.0187,.026,.0365,.076,.135,
     * .15,0.0,.0003,.0042,.0095,.0135,.0220,.033,.0450,.082,.148,
     * .20,0.0,.0004,.0048,.0115,.0160,.0255,.040,.0530,.089,.160,
     * .30,0.0,.0006,.0060,.0140,.0205,.0325,.051,.0670,.110,.203,
     * .50,0.0,.0010,.0075,.0185,.0310,.0480,.070,.0980,.160,.280,
     * .70,0.0,.0014,.0090,.0245,.0425,.0650,.090,.1270,.204,.350,
     * .90,0.0,.0018,.0110,.0315,.0540,.0820,.112,.1520,.240,.405,
     *1.10,0.0,.0022,.0123,.0380,.0655,.1000,.136,.1760,.270,.450/
  
C                               AR * (T/C)**(1/3) = 1.2 
      DATA ARS12/009010., 
     *    -.40, -.30, -.20, -.10, -.05,  0.0,  .05, .10, .20, .30,
     * .05,0.0,.0001,.0030,.0080,.0110,.0175,.0275,.039,.064,.115,
     * .10,0.0,.0002,.0040,.0100,.0145,.0220,.0325,.045,.068,.120,
     * .15,0.0,.0003,.0050,.0120,.0165,.0250,.0375,.050,.072,.129,
     * .20,0.0,.0004,.0060,.0135,.0180,.0320,.0435,.055,.077,.138,
     * .30,0.0,.0006,.0070,.0165,.0225,.0355,.0495,.065,.094,.165,
     * .50,0.0,.0010,.0090,.0235,.0330,.0480,.0670,.088,.140,.250,
     * .70,0.0,.0014,.0150,.0300,.0465,.0650,.0855,.114,.190,.350,
     * .90,0.0,.0018,.0140,.0375,.0580,.0830,.1085,.140,.240,.460,
     *1.10,0.0,.0022,.0170,.0450,.0730,.1010,.1335,.165,.300,.600/
  
C                               AR * (T/C)**(1/3) = 1.4 
      DATA ARS14/009010., 
     *    -.40, -.30, -.20, -.10, -.05,  0.0,  .05,  .10, .20, .30, 
     * .05,0.0,.0010,.0030,.0100,.0140,.0200,.0280,.0355,.060,.100, 
     * .10,0.0,.0010,.0040,.0115,.0145,.0215,.0300,.0380,.062,.105, 
     * .15,0.0,.0011,.0050,.0130,.0180,.0250,.0330,.0440,.066,.111, 
     * .20,0.0,.0012,.0060,.0140,.0190,.0250,.0350,.0480,.071,.122, 
     * .30,0.0,.0014,.0070,.0170,.0235,.0335,.0440,.0560,.082,.145, 
     * .50,0.0,.0017,.0095,.0235,.0345,.0480,.0630,.0810,.116,.235, 
     * .70,0.0,.0020,.0125,.0315,.0475,.0650,.0850,.1140,.180,.350, 
     * .90,0.0,.0022,.0150,.0395,.0590,.0810,.1080,.1350,.240,.450, 
     *1.10,0.0,.0025,.0185,.0475,.0740,.1010,.1290,.1550,.300,.550/ 
  
C                               AR * (T/C)**(1/3) = 1.6 
      DATA ARS16/009010., 
     *    -.40, -.30, -.20, -.10, -.05,  0.0,  .05,  .10, .20, .30, 
     * .05,0.0,.0010,.0035,.0095,.0130,.0200,.0280,.0370,.055,.088, 
     * .10,0.0,.0010,.0040,.0120,.0160,.0225,.0300,.0400,.061,.092, 
     * .15,0.0,.0010,.0050,.0135,.0180,.0260,.0350,.0445,.065,.097, 
     * .20,0.0,.0010,.0060,.0150,.0200,.0290,.0390,.0485,.072,.104, 
     * .30,0.0,.0010,.0070,.0180,.0245,.0360,.0460,.0575,.081,.125, 
     * .50,0.0,.0015,.0095,.0240,.0350,.0505,.0635,.0780,.108,.190, 
     * .70,0.0,.0020,.0125,.0320,.0470,.0645,.0870,.1080,.170,.280, 
     * .90,0.0,.0025,.0170,.0400,.0625,.0830,.1060,.1300,.240,.380, 
     *1.10,0.0,.0030,.0220,.0480,.0760,.1020,.1290,.1540,.300,.490/ 
  
C                               AR * (T/C)**(1/3) = 1.8 
      DATA ARS18/009010., 
     *    -.40, -.30, -.20, -.10, -.05,  0.0,  .05,  .10,  .20, .30,
     * .05,0.0,.0007,.0035,.0110,.0145,.0190,.0260,.0340,.0500,.080,
     * .10,0.0,.0008,.0042,.0120,.0165,.0245,.0315,.0400,.0600,.082,
     * .15,0.0,.0009,.0052,.0135,.0180,.0265,.0365,.0465,.0655,.085,
     * .20,0.0,.0010,.0060,.0150,.0200,.0300,.0395,.0495,.0705,.090,
     * .30,0.0,.0012,.0070,.0180,.0245,.0355,.0465,.0585,.0790,.105,
     * .50,0.0,.0016,.0095,.0235,.0350,.0490,.0635,.0795,.1080,.150,
     * .70,0.0,.0020,.0125,.0315,.0475,.0665,.0855,.1100,.1670,.230,
     * .90,0.0,.0025,.0164,.0400,.0630,.0860,.1085,.1310,.2320,.330,
     *1.10,0.0,.0030,.0220,.0485,.0765,.1040,.1300,.1550,.3020,.440/
  
C                               AR * (T/C)**(1/3) = 2.0 
      DATA ARS20/009010., 
     *    -.40, -.30, -.20, -.10, -.05, 0.0, .05, .10,  .20,  .30,
     * .05,0.0,.0007,.0030,.0110,.0140,.020,.027,.034,.0510,.0680,
     * .10,0.0,.0008,.0045,.0120,.0160,.024,.030,.039,.0590,.0805,
     * .15,0.0,.0009,.0050,.0135,.0180,.026,.035,.046,.0650,.0870,
     * .20,0.0,.0010,.0057,.0150,.0200,.030,.039,.049,.0695,.0915,
     * .30,0.0,.0012,.0070,.0180,.0245,.033,.044,.054,.0760,.1000,
     * .50,0.0,.0016,.0095,.0240,.0350,.047,.058,.069,.0930,.1340,
     * .70,0.0,.0020,.0126,.0315,.0480,.066,.084,.105,.1460,.2000,
     * .90,0.0,.0028,.0160,.0400,.0615,.084,.108,.131,.2100,.2900,
     *1.10,0.0,.0036,.0220,.0480,.0750,.102,.128,.155,.2690,.3750/
  
  
C      THESE ARE TABLES OF 2-DIMENSIONAL (MACH**2 - 1). 
C      DESIGN CL ON THE VERTICAL...THICKNESS RATIO FUNCTION ON
C      THE HORIZONTAL 
  
      DATA AMDES/008003.,   .18,   .24,   .30,
     C             .1,     -.208, -.333, -.459, 
     C             .2,     -.218, -.343, -.468, 
     C             .3,     -.229, -.353, -.478, 
     C             .4,     -.242, -.365, -.488, 
     C             .5,     -.258, -.377, -.495, 
     C             .6,     -.271, -.388, -.507, 
     C             .7,     -.294, -.413, -.530, 
     C             .8,     -.317, -.431, -.546/ 
  
      DATA CMDES/007003.,   .18,   .24,   .30,
     C             .1,     -.374, -.445, -.513, 
     C             .2,     -.385, -.461, -.537, 
     C             .3,     -.401, -.478, -.556, 
     C             .4,     -.416, -.490, -.564, 
     C             .5,     -.441, -.509, -.578, 
     C             .6,     -.474, -.532, -.591, 
     C             .7,     -.518, -.571, -.621/ 
  
C      HERE, THE WING THICKNESS RATIO IS ON THE HORIZONTAL
  
      DATA HSMDES/006003.,  .02,   .04,   .06,
     C              0,      .844,  .822,  .801, 
     C             .1,      .836,  .815,  .793, 
     C             .2,      .829,  .807,  .786, 
     C             .3,      .820,  .799,  .778, 
     C             .4,      .811,  .791,  .770, 
     C             .5,      .802,  .781,  .759/ 
  
  
C                 BODY COMPRESSIBILITY DRAG TABLES
C     MACH NUMBER ON THE VERTICAL..BASE AREA FUNCTION ON THE HORIZONTAL 
  
      DATA BSUB/017004.,    1.0,    1.2,    1.4,    1.5,
     *              .200,   0.0,    0.0,    0.0,    0.0,
     *              .500,   0.0,    0.0,    0.0,    0.0,
     *              .700,   0.0,    0.0,    0.0,    0.0,
     *              .780,   0.0,    0.0,    0.0,    0.0,
     *              .820,   0.0,    0.0,    0.15,   0.21, 
     *              .840,   0.0,    0.15,   0.20,   0.35, 
     *              .860,   0.09,   0.22,   0.40,   0.52, 
     *              .880,   0.20,   0.38,   0.61,   0.78, 
     *              .900,   0.38,   0.58,   0.91,   1.10, 
     *              .910,   0.53,   0.75,   1.10,   1.33, 
     *              .920,   0.73,   0.95,   1.30,   1.60, 
     *              .930,   0.95,   1.20,   1.65,   1.93, 
     *              .940,   1.30,   1.55,   2.05,   2.49, 
     *              .950,   1.75,   2.20,   2.90,   3.65, 
     *              .960,   2.45,   3.25,   4.50,   6.40, 
     *              .965,   3.00,   4.22,   6.30,   8.45, 
     *              .970,   3.90,   5.60,   9.50,  11.50/ 
  
      DATA BSUP/014006.,   1.0,   1.1,   1.2,   1.3,   1.4,   1.5,
     *             1.00,  24.5,  20.0,  16.2,  13.4,  11.1,   9.5,
     *             1.05,  30.7,  23.6,  20.0,  16.0,  12.9,  10.5,
     *             1.10,  33.0,  26.2,  21.5,  17.4,  14.0,  11.1,
     *             1.15,  34.3,  27.3,  22.3,  18.2,  14.8,  11.6,
     *             1.20,  34.7,  27.7,  22.5,  18.5,  15.0,  11.9,
     *             1.25,  34.5,  27.5,  22.4,  18.2,  14.9,  11.9,
     *             1.30,  33.8,  27.0,  22.0,  17.6,  14.5,  11.7,
     *             1.35,  32.9,  26.4,  21.7,  17.3,  14.2,  11.4,
     *             1.40,  32.4,  25.9,  21.4,  17.2,  14.1,  11.0,
     *             1.50,  32.0,  25.6,  21.1,  17.0,  14.1,  10.9,
     *             1.60,  32.0,  25.6,  21.0,  17.0,  14.1,  10.9,
     *             1.80,  32.0,  25.6,  21.0,  17.0,  14.2,  11.4,
     *             2.00,  32.0,  25.6,  21.0,  17.1,  14.4,  11.8,
     *             2.20,  32.0,  25.6,  21.0,  17.3,  14.6,  12.0/                
  
  
C                 WING COMPRESSIBILITY DRAG TABLES
C     DELTA MACH ON THE VERTICAL...WING T/C FUNCTION ON THE HORIZONTAL
  
      DATA PCW/013007., .10, .12, .14, .16,  .18,  .22,  .30, 
     *           -.80, .0,   .0,  .0,  .0,   .0,   .0,   .0,
     *           -.20, .060, .04, .02, .020, .010, .008, .002,
     *           -.16, .072, .05, .03, .026, .017, .016, .006,
     *           -.12, .100, .06, .04, .038, .025, .024, .012,
     *           -.08, .125, .08, .05, .049, .035, .033, .019,
     *           -.04, .160, .12, .08, .068, .054, .047, .030,
     *           -.02, .200, .16, .12, .110, .070, .059, .039,
     *           0.00, .280, .22, .16, .120, .093, .077, .052,
     *            .01, .340, .27, .20, .152, .118, .093, .061,
     *            .02, .440, .33, .24, .197, .153, .117, .073,
     *            .03, .640, .45, .31, .255, .203, .148, .087,
     *            .04,1.100, .66, .41, .325, .270, .187, .103,
     *            .05,1.900,1.02, .56, .400, .350, .235, .127/
  
C     DELTA MACH ON THE VERTICAL...WING AR FUNCTION ON THE HORIZONTAL
C      LAST 2 COLUMNS ADDED 4/8/82-LAM
      DATA PCAR/018009.,1.0,  1.5,  2.0,  2.5,  3.0,  3.5, 4.0, 5.0,6.0,
     *            .05,  2.4,  1.7,  1.17,  .85,  .73,  .67, .60,.54,.52,
     *            .07,  3.1,  2.25, 1.58, 1.10,  .89,  .77, .70,.62,.60,
     *            .09,  3.55, 2.61, 1.88, 1.24,  .99,  .87, .75,.67,.65,
     *            .11,  3.85, 2.88, 2.03, 1.33, 1.07,  .92, .80,.71,.68,
     *            .13,  3.97, 3.05, 2.14, 1.41, 1.12,  .96, .84,.74,.71,
     *            .15,  4.00, 3.10, 2.17, 1.48, 1.16,  .99, .86,.75,.72,
     *            .20,  3.90, 3.00, 2.20, 1.55, 1.20, 1.00, .86,.74,.70,
     *            .25,  3.68, 2.85, 2.16, 1.57, 1.20, 1.00, .83,.70,.65,
     *            .30,  3.43, 2.70, 2.10, 1.55, 1.17,  .92, .77,.63,.58,
     *            .40,  3.03, 2.45, 1.90, 1.47, 1.10,  .88, .73,.59,.53,
     *            .50,  2.75, 2.22, 1.71, 1.37, 1.02,  .84, .73,.57,.52,
     *            .60,  2.49, 2.00, 1.55, 1.26,  .97,  .81, .74,.56,.51,
     *            .70,  2.25, 1.80, 1.41, 1.17,  .91,  .79, .71,.55,.51,
     *            .80,  1.99, 1.62, 1.30, 1.10,  .88,  .75, .70,.55,.50,
     *            .90,  1.80, 1.50, 1.20, 1.00,  .84,  .70, .66,.54,.50,
     *           1.00,  1.65, 1.40, 1.10,  .95,  .80,  .70, .66,.54,.50,
     *           1.50,  0.80, 0.80, 0.70,  .70,  .60,  .60, .58,.53,.50,
     *           2.00,  0.60, 0.60, 0.60,  .58,  .56,  .54, .52,.51,.50/  

C                 WING-FUSELAGE INTERFERENCE DRAG TABLES
C     MACH ON THE VERTICAL...FUSE DIAMETER/WING SPAN ON THE HORIZONTAL

       DATA BINT/013010., 
     *     .1,.12, .14,   .15,   .16,   .17,   .18,   .19,  .20,  .22,
     * 1.0, 0.,0.,  0.,    0.,    0.,    0.,    0.,    0.,   0.,   0.,
     * 1.05,0.,0.,.0004,-.0003,-.0008,-.0011,-.0010,-.0004,.0003,.0018, 
     * 1.10,0.,0.,.0006,-.0006,-.0014,-.0018,-.0014,-.0006,.0004,.0026, 
     * 1.15,0.,0.,.0003,-.0008,-.0017,-.0020,-.0015,-.0006,.0004,.0024, 
     * 1.20,0.,0.,.0002,-.0008,-.0017,-.0018,-.0014,-.0006,.0003,.0020, 
     * 1.30,0.,0.,.0002,-.0006,-.0010,-.0010,-.0008,-.0005,.0001,.0012, 
     * 1.40,0.,0.,.0001,-.0003,-.0003,-.0003,-.0002,-.0001,.0003,.0009, 
     * 1.50,0.,0.,.0001,0.0000, .0003, .0003, .0004, .0004,.0005,.0007, 
     * 1.60,0.,0.,.0000, .0004, .0005, .0009, .0009, .0008,.0007,.0005, 
     * 1.70,0.,0.,.0000, .0005, .0007, .0012, .0011, .0010,.0008,.0005, 
     * 1.80,0.,0.,.0000, .0006, .0009, .0012, .0011, .0010,.0008,.0005, 
     * 1.90,0.,0.,.0000, .0006, .0009, .0010, .0010, .0009,.0008,.0005, 
     * 2.00,0.,0.,.0000, .0005, .0009, .0011, .0010, .0009,.0007,.0005/ 
  
  
C                 BUFFET TABLE DATA 
C     THICKNESS FUNCTION ON THE VERTICAL...DELTA MACH ON THE HORIZONTAL 
  
      DATA BUFT/8010.,
     *   -.8,  -.6,  -.4,  -.3,  -.2,  -.1,  0.,     .05,  .1,   .15, 
     *.1, .078, .076, .061, .050, .046, .051, .070,  .09,  .12,  .156,
     *.12,.078, .076, .061, .050, .043, .036, .039,  .051, .073, .104,
     *.14,.078, .076, .061, .050, .042, .034, .030,  .031, .040, .057,
     *.16,.078, .076, .061, .050, .041, .031, .023,  .020, .016, .010,
     *.18,.078, .076, .061, .050, .040, .030, .018,  .010, .002,-.009,
     *.20,.078, .076, .061, .050, .040, .029, .015,  .005,-.007,-.020,
     *.24,.078, .076, .061, .050, .040, .028, .009, -.005,-.020,-.036,
     *.30,.078, .076, .061, .050, .040, .028, .004, -.015,-.037,-.060/
      END

CCCCCCCCCCCCCCCCCCCCCCC  SUBROUTINE SEPARATOR  CCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE ATMO ( ZFT, DTC, DELTA, THETA, ASTAR, TM, RE, HFT )

C  1962 STANDARD ATMOSPHERIC PROPERTIES GOOD UP TO 88743 METERS
C      GEOPOTENTIAL ALTITUDE (90 KM GEOMETRIC ALTITUDE OR 291152 FEET)
C      ALSO SAME AS 1976 STD ATMOSPHERE TO 51 KM (167323 FEET)
C  INPUT/OUTPUT IN ENGLISH UNITS, CALCULATIONS IN SI UNITS
C  BASE PRESSURES AND EXPONENTS FOR EACH LAYER ARE RECOMPUTED TO ASSURE
C  CONTINUITY AT THE LAYER BOUNDARIES REGARDLESS OF THE COMPUTER USED

C      ZFT       INPUT ALTITUDE - FEET
C      DTC       DELTA TEMPERATURE FROM STD - DEG C
C      DELTA     PRESSURE RATIO
C      THETA     TEMPERATURE RATIO
C      ASTAR     SPEED OF SOUND - KNOTS
C      TM        MOLECULAR-SCALE TEMPERATURE - DEG KELVIN
C      RE        REYNOLDS NUMBER PER FOOT AT MACH 1.
C      HFT       GEOPOTENTIAL ALTITUDE - FEET

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DIMENSION P(9), E(9)
      SAVE P, E, DDLTA, DHETA, DSTAR, DTM, DRE, DFT, SFT, STC, IFIR
      DATA REARTH/6367533.D0/, GR/9.80665D0/, GNS/9.823693D0/,
     1     CM1/.9985D0/, OC2/26.76566D-10/, IFIR/1/, SFT,STC/2*-1000.D0/

C     PRECALCULATE EXPONENTS AND BASE PRESSURE RATIOS

      IF ( IFIR .NE. 1 ) GO TO 5
      P(1) = 1.D0
      GMOR = 9.80665D0 * 1.225D0 * 288.15D0 / 101.325D0
      E(1) = GMOR / 6.5D0
      P(2) = (216.65D0/288.15D0)**E(1)
      E(2) = -GMOR / 216.65D0
      P(3) = P(2) * EXP(E(2) * 9.D0)
      E(3) = GMOR
      P(4) = P(3) * (216.65D0/228.65D0)**GMOR
      E(4) = GMOR / 2.8D0
      P(5) = P(4) * (228.65D0/270.65D0)**E(4)
      E(5) = -GMOR / 270.65D0
      P(6) = P(5) * EXP(E(5) * 5.D0)
      E(6) = GMOR / 2.D0
      P(7) = P(6) * (252.65D0/270.65D0)**E(6)
      E(7) = GMOR / 4.D0
      P(8) = P(7) * (180.65D0/252.65D0)**E(7)
      E(8) = -GMOR / 180.65D0
      Z90  = 90000.D0
      R    = REARTH + Z90
      GN   = GNS * (REARTH / R)**(CM1 + 1.D0)
      H90  = (R * GN * ( (R/REARTH)**CM1 - 1.D0) / CM1
     1       - Z90 * (R - Z90/2.D0) * OC2) / GR
      DH   = H90/1000.D0 - 79.D0
      P(9) = P(8) * EXP(E(8) * DH)
      E(9) = 11.056D0
      IFIR = 0

C CONVERT INPUT FEET TO METERS

    5 IF ( ZFT .EQ. SFT .AND. DTC .EQ. STC ) GO TO 110
      SFT  = ZFT
      STC  = DTC
      Z    = ZFT * .3048D0
CLVB      IF ( Z .LT. -1000.D0 ) Z = -1000.D0

C CALCULATE GEOPOTENTIAL ALTITUDE

      R    = REARTH + Z
      RPOW = (REARTH / R)**CM1
      GN   = GNS * (REARTH / R) * RPOW
      H    = (R * GN * ( 1.D0/RPOW - 1.D0) / CM1
     1       - Z * (R - Z/2.D0) * OC2) / GR
      DFT  = H / .3048D0

C CONVERT H TO KILOMETERS

      H    = H/1000.D0

C SEA LEVEL TO 11 KM

      IF ( H .GT. 11.D0 ) GO TO 11
      DTM   = 288.15D0 - 6.5D0 * H
      DDLTA = (DTM/288.15D0)**E(1)
      GO TO 100

C 11 KM TO 20 KM

   11 IF ( H .GT. 20.D0 ) GO TO 20
      DH    = H - 11.D0
      DTM   = 216.65D0
      DDLTA = P(2) * EXP(E(2) * DH)
      GO TO 100

C 20 KM TO 32 KM

   20 IF ( H .GT. 32.D0 ) GO TO 32
      DH    = H - 20.D0
      DTM   = 216.65D0 + DH
      DDLTA = P(3) * (216.65D0/DTM)**E(3)
      GO TO 100

C 32 KM TO 47 KM

   32 IF ( H .GT. 47.D0 ) GO TO 47
      DH    = H - 32.D0
      DTM   = 228.65D0 + 2.8D0 * DH
      DDLTA = P(4) * (228.65D0/DTM)**E(4)
      GO TO 100

C 47 KM TO 52 KM

   47 IF ( H .GT. 52.D0 ) GO TO 52
      DH    = H - 47.D0
      DTM   = 270.65D0
      DDLTA = P(5) * EXP(E(5) * DH)
      GO TO 100

C 52 KM TO 61 KM

   52 IF ( H .GT. 61.D0 ) GO TO 61
      DH    = H - 52.D0
      DTM   = 270.65D0 - 2.0D0 * DH
      DDLTA = P(6) * (DTM/270.65D0)**E(6)
      GO TO 100

C 61 KM TO 79 KM

   61 IF ( H .GT. 79.D0 ) GO TO 79
      DH    = H - 61.D0
      DTM   = 252.65D0 - 4.0D0 * DH
      DDLTA = P(7) * (DTM/252.65D0)**E(7)
      GO TO 100

C 79 KM TO 88743 METERS

   79 IF ( Z .GT. 90000.D0 ) GO TO 90
      DH    = H - 79.D0
      DTM   = 180.65D0
      DDLTA = P(8) * EXP(E(8) * DH)
      GO TO 100

C ABOVE 88743 M, 1962 STD ATMOSPHERE SWITCHES TO GEOMETRIC ALTITUDE
C THE EQUATIONS BELOW ARE CLOSE UP TO 100 KM AND DIVERGE AFTER THAT

   90 DH    = Z/1000.D0 - 90.D0
      DTM   = 180.65D0 + 3.0D0 * DH
      DDLTA = P(9) * (180.65D0/DTM)**E(9)

C CALCULATE TEMPERATURE RATIO, SPEED OF SOUND, AND REYNOLDS NUMBER

  100 DHETA = (DTM + DTC) / 288.15D0
      DSTAR = 661.479D0 * SQRT(DHETA)
      DRE   = 1.479301D+9 * DDLTA * (DTM + 110.4D0) / DTM**2

  110 DELTA = DDLTA
      THETA = DHETA
      ASTAR = DSTAR
      TM    = DTM
      RE    = DRE
      HFT   = DFT
      RETURN

      END
