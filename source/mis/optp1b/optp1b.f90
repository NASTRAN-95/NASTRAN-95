!*==optp1b.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE optp1b(Elt,Elop,Ele,Pr)
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
   INTEGER , DIMENSION(2,1) :: Elop
   INTEGER , DIMENSION(1) :: Ele
   INTEGER , DIMENSION(1) :: Pr
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: card
   INTEGER :: elcr , elpt , i , idee , ides , idp , idx , ieop , j , k , kid , l , m , nele , npr , nx , pid , prpt , prpt1
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL bishel , bisloc , fread , locate , mesage , page2 , read
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
   DATA name/4H OPT , 4HP1B /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
         ieop = 1
         ides = Elop(1,ieop)
         idee = Elop(1,ieop+1)
         prpt = 1
         prpt1 = 1
         Elop(2,1) = 1
!
!     IN CASE OF ERROR SET PRC(1)
!
         Prc(1) = -1
!
         SPAG_Loop_1_2: DO k = 1 , Numelm
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  nele = (idee-ides)/Nwdse
                  IF ( nele<0 ) GOTO 2
                  IF ( nele==0 ) THEN
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
!
                  idx = Incr*(Itype(k)-1)
                  idp = idx + 4
                  card(1) = Ne(idp)
                  card(2) = Ne(idp+1)
                  IF ( Ne(idp+2)>Prcor ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  CALL locate(*20,X(B1p1),card(1),i)
!
!     SEQUENTIAL ELEMENT SEARCH
!
                  npr = 0
                  elpt = ides
                  elcr = Ele(elpt)
                  spag_nextblock_2 = 2
               CASE (2)
                  SPAG_Loop_2_1: DO
!
                     CALL read(*20,*20,Ect,Prc,Ne(idp+2),Noeor,i)
                     IF ( Prc(1)<elcr ) THEN
                     ELSEIF ( Prc(1)==elcr ) THEN
!
!     ELEMENT ID IN CORE .EQ. ECT ID - ELEMENT TO BE OPTIMIZED
!
                        pid = Prc(2)
                        card(1) = pid
                        card(2) = Ele(elpt+4)
!
!     TEST FOR CORE NEEDED AFTER EXPANDING TO NWDSP WORDS
!
                        IF ( prpt1+Nwdsp*(npr/2+1)>Ycor ) THEN
                           spag_nextblock_1 = 5
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        CALL bishel(*4,card,npr,2,Pr(prpt1))
                        GOTO 4
                     ELSE
                        EXIT SPAG_Loop_2_1
                     ENDIF
                  ENDDO SPAG_Loop_2_1
!
!     LOGIC OR FILE FAILURE
!
 2                CALL page2(-2)
                  WRITE (Outtap,99001) Sfm , Itype(k) , Prc(1) , name
99001             FORMAT (A25,' 2297, INCORRECT LOGIC FOR ELEMENT TYPE',I4,', ELEMENT',I8,2H (,2A4,2H).)
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
 4                Ele(elpt+4) = pid
                  elpt = elpt + Nwdse
                  IF ( elpt>=idee ) THEN
!
!     NEW ELEMENT TYPE COMING
!
                     CALL fread(Ect,0,0,Nweor)
!
!     EXPAND PROPERTIES TO NWDSP WORDS/PROPERTY
!
                     nx = npr/2
                     IF ( nx<1 ) GOTO 2
                     IF ( nx/=1 ) THEN
                        DO i = 1 , nx
                           j = nx - i
                           l = prpt1 + j*Nwdsp
                           m = prpt1 + j*2
                           Pr(l) = Pr(m)
                           Pr(l+1) = Pr(m+1)
                        ENDDO
                     ENDIF
!
                     prpt = prpt1 + nx*Nwdsp
!
!     PLACE POINTERS IN ELEMENT ARRAY
!
                     l = idee - 1
                     DO i = ides , l , Nwdse
                        kid = Ele(i+4)
                        CALL bisloc(*2,kid,Pr(prpt1),Nwdsp,nx,j)
                        Ele(i+4) = j
                     ENDDO
                  ELSE
                     elcr = Ele(elpt)
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  spag_nextblock_2 = 3
               CASE (3)
!
!     SETUP FOR NEXT ELEMENT
!
                  ieop = ieop + 1
                  Elop(2,ieop) = prpt
                  prpt1 = prpt
                  ides = idee
                  IF ( ieop>Npow ) EXIT SPAG_Loop_1_2
                  idee = Elop(1,ieop+1)
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO SPAG_Loop_1_2
         spag_nextblock_1 = 2
      CASE (2)
!
!
         Nprw = prpt - 1
         RETURN
      CASE (3)
!
!     ERRORS
!
!     INSUFFICIENT CORE IN /OPTPW1/ OR /XXOPT1/
!
         Count = -1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     FILE ERRORS
!
 20      CALL mesage(-7,Ect,name)
         spag_nextblock_1 = 4
      CASE (4)
         prpt = 1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      CASE (5)
!
!     INSUFFICIENT CORE
!
         CALL page2(-2)
         WRITE (Outtap,99002) Ufm , name , B1p1 , pid
99002    FORMAT (A23,' 2298, INSUFFICIENT CORE ',2A4,1H(,I10,'), PROPERTY',I9)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE optp1b
