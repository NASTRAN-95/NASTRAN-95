!*==optp1c.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE optp1c(Elt,Elop,Pr)
   IMPLICIT NONE
   USE C_BLANK
   USE C_GPTA1
   USE C_NAMES
   USE C_OPTPW1
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Elt
   INTEGER , DIMENSION(2,2) :: Elop
   INTEGER , DIMENSION(1) :: Pr
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: card
   INTEGER , DIMENSION(21) , SAVE :: dtyp
   INTEGER :: i , icpr , icpt , idp , idpe , idps , idx , ietyp , j1 , j2 , jetyp , m1 , npr
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , SAVE :: rm1
   EXTERNAL fread , locate , mesage , page2 , read
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
   !>>>>EQUIVALENCE (m1,rm1)
   DATA name/4H OPT , 4HP1C / , rm1/ - 1.0/
!
!      PROPERTY CORRELATOR TO EST DESIGN VARIABLE (100*EST LOCATION).
!      THIS VALUE ADDS/SUBTRACTS FROM EST ENTRY TO GET EPT LOCATION.
!      ENTRY IS MADE BY THE ELT ARRAY (SEQUENTIAL LIST OF NUMBERS WITH
!      ZEROS FOR ELEMENTS NOT USED).
!
!              BR  EB   IS  QM  M1  M2  QP  Q1  Q2  RD
!              SH  TB   T1  T2  T6  TM  TP  TU  Q4  T3
   DATA dtyp/ - 14 , -6 , -10 , -5 , -5 , -5 , -5 , -5 , -5 , -2 , -4 , -4 , -4 , -4 , -7 , -4 , -4 , -2 , -5 , -5 , 0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         jetyp = 1
         idps = Elop(2,1)
         idpe = Elop(2,2) - 1
!
         SPAG_Loop_1_2: DO ietyp = 1 , Ntypes
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  IF ( Elt(ietyp)<=0 ) CYCLE
                  npr = (idpe+1-idps)/Nwdsp
                  IF ( npr<0 ) THEN
                  ELSEIF ( npr==0 ) THEN
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
                  ELSE
!
                     idx = Entry(jetyp)
                     idx = Incr*(idx-1)
                     idp = idx + 7
                     card(1) = Ne(idp)
                     card(2) = Ne(idp+1)
                     IF ( Ne(idp+2)>Prcor ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
!
                     CALL locate(*20,X(B1p1),card,i)
                     icpr = Pr(idps)
                     icpt = idps
                     SPAG_Loop_2_1: DO
!
                        CALL read(*40,*60,Ept,Prc,Ne(idp+2),Noeor,i)
!
!     SEQUENTIAL PROPERTY SEARCH.  PROPERTIES THAT ARE UNSORTED ON EPT
!     WILL FAIL.  THIS MAY OCCUR FOR 2 PID/CARD (E.G., QDMEM, QUAD2,
!     SHEAR, TRIA2, TRMEM).
!
                        IF ( Prc(1)<icpr ) THEN
                        ELSEIF ( Prc(1)==icpr ) THEN
!
!     PROPERTY IN CORE LOCATED.
!
                           npr = npr - 1
                           Pr(icpt+5) = 0
                           Pr(icpt+4) = m1
!
!     LOCATE VARIABLE AS SET BY OPTP1A
!
                           j1 = Pr(icpt+1)/100
                           j2 = j1 + dtyp(jetyp)
                           Pr(icpt+3) = Prc(j2)
                           Pr(icpt+2) = Prc(j2)
!
!     ICPT+0, +1 SET BY OPTP1A
!
                           icpt = icpt + Nwdsp
                           IF ( icpt>idpe ) THEN
!
!     NEW ELEMENT TYPE COMING
!
                              IF ( npr>0 ) EXIT SPAG_Loop_2_1
                              CALL fread(Ept,0,0,Nweor)
                              spag_nextblock_2 = 2
                              CYCLE SPAG_DispatchLoop_2
                           ELSE
                              icpr = Pr(icpt)
                           ENDIF
                        ELSE
                           EXIT SPAG_Loop_2_1
                        ENDIF
                     ENDDO SPAG_Loop_2_1
                  ENDIF
!
!     LOGIC OR UNSORTED FILE ERROR
!
                  CALL page2(-2)
                  WRITE (Outtap,99001) Sfm , ietyp , Prc(1) , name
99001             FORMAT (A25,' 2299, INCORRECT LOGIC FOR ELEMENT TYPE',I4,', PROPERTY',I9,2H (,2A4,2H).)
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               CASE (2)
                  idps = idpe + 1
                  jetyp = jetyp + 1
                  IF ( jetyp>Npow ) EXIT SPAG_Loop_1_2
                  idpe = Elop(2,jetyp+1) - 1
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO SPAG_Loop_1_2
         spag_nextblock_1 = 2
      CASE (2)
!
!
         RETURN
      CASE (3)
!
!     ERRORS
!
         Count = -1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     UNABLE TO LOCATE SORTED PID
!
 20      WRITE (Outtap,99002) Sfm , name , Prc(1)
99002    FORMAT (A25,' 2300, ',2A4,'UNABLE TO LOCATE PROPERTY',I10,' ON EPT OR IN CORE.')
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
!
!     INSUFFICIENT CORE /OPTPW1/
!
         CALL page2(-2)
         WRITE (Outtap,99003) Ufm , name , Prcor , ietyp
99003    FORMAT (A23,' 2296. INSUFFICIENT CORE ',2A4,1H(,I10,' ), ELEMENT',I9)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     ILLEGAL EOF
!
 40      CALL mesage(-2,Ept,name)
!
!     ILLEGAL EOR
!
 60      CALL mesage(-3,Ept,name)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE optp1c
