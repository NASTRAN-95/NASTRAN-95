!*==optp1b.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE optp1b(Elt,Elop,Ele,Pr)
   USE c_blank
   USE c_gpta1
   USE c_names
   USE c_optpw1
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
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
         prc(1) = -1
!
         SPAG_Loop_1_2: DO k = 1 , numelm
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  nele = (idee-ides)/nwdse
                  IF ( nele<0 ) GOTO 2
                  IF ( nele==0 ) THEN
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
!
                  idx = incr*(itype(k)-1)
                  idp = idx + 4
                  card(1) = ne(idp)
                  card(2) = ne(idp+1)
                  IF ( ne(idp+2)>prcor ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  CALL locate(*20,x(b1p1),card(1),i)
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
                     CALL read(*20,*20,ect,prc,ne(idp+2),noeor,i)
                     IF ( prc(1)<elcr ) THEN
                     ELSEIF ( prc(1)==elcr ) THEN
!
!     ELEMENT ID IN CORE .EQ. ECT ID - ELEMENT TO BE OPTIMIZED
!
                        pid = prc(2)
                        card(1) = pid
                        card(2) = Ele(elpt+4)
!
!     TEST FOR CORE NEEDED AFTER EXPANDING TO NWDSP WORDS
!
                        IF ( prpt1+nwdsp*(npr/2+1)>ycor ) THEN
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
                  WRITE (outtap,99001) sfm , itype(k) , prc(1) , name
99001             FORMAT (A25,' 2297, INCORRECT LOGIC FOR ELEMENT TYPE',I4,', ELEMENT',I8,2H (,2A4,2H).)
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
 4                Ele(elpt+4) = pid
                  elpt = elpt + nwdse
                  IF ( elpt>=idee ) THEN
!
!     NEW ELEMENT TYPE COMING
!
                     CALL fread(ect,0,0,nweor)
!
!     EXPAND PROPERTIES TO NWDSP WORDS/PROPERTY
!
                     nx = npr/2
                     IF ( nx<1 ) GOTO 2
                     IF ( nx/=1 ) THEN
                        DO i = 1 , nx
                           j = nx - i
                           l = prpt1 + j*nwdsp
                           m = prpt1 + j*2
                           Pr(l) = Pr(m)
                           Pr(l+1) = Pr(m+1)
                        ENDDO
                     ENDIF
!
                     prpt = prpt1 + nx*nwdsp
!
!     PLACE POINTERS IN ELEMENT ARRAY
!
                     l = idee - 1
                     DO i = ides , l , nwdse
                        kid = Ele(i+4)
                        CALL bisloc(*2,kid,Pr(prpt1),nwdsp,nx,j)
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
                  IF ( ieop>npow ) EXIT SPAG_Loop_1_2
                  idee = Elop(1,ieop+1)
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO SPAG_Loop_1_2
         spag_nextblock_1 = 2
      CASE (2)
!
!
         nprw = prpt - 1
         RETURN
      CASE (3)
!
!     ERRORS
!
!     INSUFFICIENT CORE IN /OPTPW1/ OR /XXOPT1/
!
         count = -1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     FILE ERRORS
!
 20      CALL mesage(-7,ect,name)
         spag_nextblock_1 = 4
      CASE (4)
         prpt = 1
         spag_nextblock_1 = 2
      CASE (5)
!
!     INSUFFICIENT CORE
!
         CALL page2(-2)
         WRITE (outtap,99002) ufm , name , b1p1 , pid
99002    FORMAT (A23,' 2298, INSUFFICIENT CORE ',2A4,1H(,I10,'), PROPERTY',I9)
         spag_nextblock_1 = 3
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE optp1b
