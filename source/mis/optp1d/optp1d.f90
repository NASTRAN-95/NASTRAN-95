!*==optp1d.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE optp1d(Elop,Pr,Pl)
   IMPLICIT NONE
   USE C_BLANK
   USE C_NAMES
   USE C_OPTPW1
   USE C_SYSTEM
   USE C_XMSSG
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2,1) :: Elop
   INTEGER , DIMENSION(1) :: Pr
   REAL , DIMENSION(1) :: Pl
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ill , ip1 , ip2 , ipr , itp , l , loc , lpl , nogo , npl , npr , pid , plp
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(2) :: nkl
   EXTERNAL fread , mesage , page2 , read
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     PROPERTY OPTIMIZER   SET POINTERS TO PLIMIT
!
   !>>>>EQUIVALENCE (Nkl(1),Kl(1))
   DATA name/4H OPT , 4HPID /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         nogo = 0
         plp = 1
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_1: DO
            l = 0
            npl = 0
            CALL read(*20,*40,Scrth1,itp,1,Noeor,i)
            IF ( itp>Npow ) EXIT SPAG_Loop_1_1
!
            ip1 = Elop(2,itp)
            ip2 = Elop(2,itp+1) - 1
            npr = ip2 - ip1
            IF ( npr<=0 ) THEN
!
!     READ A NEW ELEMENT TYPE
!
               CALL fread(Scrth1,0,0,Nweor)
            ELSE
               CALL fread(Scrth1,l,1,Noeor)
               IF ( l<=0 ) EXIT SPAG_Loop_1_1
!
               CALL fread(Scrth1,nkl(1),4,Noeor)
               l = l - 1
!
!     SEQUENTIAL SEARCH ON PLIMIT AND PROPERTY DATA
!     LPL -- LAST PLIMIT POINTED TO (BY ILL).
!     NPL -- NUMBER OF PLIMIT FOR THIS ELEMENT TYPE IN CORE.
!     PLP -- POINTER FIRST PLIMIT  --    --     -- .
!
               lpl = -9877
!
               SPAG_Loop_2_3: DO ipr = ip1 , ip2 , Nwdsp
                  spag_nextblock_2 = 1
                  SPAG_DispatchLoop_2: DO
                     SELECT CASE (spag_nextblock_2)
                     CASE (1)
                        pid = Pr(ipr)
                        SPAG_Loop_3_2: DO
!
                           IF ( pid<nkl(1) ) THEN
                           ELSEIF ( pid==nkl(1) ) THEN
                              EXIT SPAG_Loop_3_2
!
!     CHECK UPPER RANGE PLIMIT
!
                           ELSEIF ( pid<=nkl(2) ) THEN
                              EXIT SPAG_Loop_3_2
                           ENDIF
!
!     READ NEXT PLIMIT INTO CORE
!
                           IF ( l<=0 ) EXIT SPAG_Loop_2_3
                           CALL fread(Scrth1,nkl(1),4,Noeor)
                           l = l - 1
                        ENDDO SPAG_Loop_3_2
!
!     PLIMIT EXISTS - SEE IF MATCHES LAST
!
                        IF ( lpl/=l ) THEN
!
!     DOESNOT - CHECK IF PREVIOUS ENTRY
!
                           IF ( npl/=0 ) THEN
                              DO lpl = plp , loc , 2
                                 IF ( Pl(lpl)==Kl(3) ) THEN
                                    IF ( Pl(lpl+1)==Kl(4) ) GOTO 2
                                 ENDIF
                              ENDDO
                           ENDIF
!
!     NEW PLIMIT
!
                           IF ( npl+plp+1>Ycor ) THEN
                              spag_nextblock_1 = 5
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           npl = npl + 2
                           loc = npl + plp - 2
                           Pl(loc) = Kl(3)
                           Pl(loc+1) = Kl(4)
                           lpl = l
                           ill = loc
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
!
!     PREVIOUS MATCH
!
 2                         ill = lpl
                           lpl = l
                        ENDIF
                        spag_nextblock_2 = 2
                     CASE (2)
!
!     LOAD POINTER
!
                        Pr(ipr+5) = ill
                        EXIT SPAG_DispatchLoop_2
                     END SELECT
                  ENDDO SPAG_DispatchLoop_2
!
               ENDDO SPAG_Loop_2_3
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO SPAG_Loop_1_1
!
         CALL page2(-2)
         WRITE (Outtap,99001) Sfm , name , itp , l
99001    FORMAT (A25,' 2301,',2A4,' FILE OPTIMIZATION PARAMETER INCORRECT',' AS',2I8)
         nogo = nogo + 1
         spag_nextblock_1 = 3
      CASE (3)
!
         plp = plp + npl
         CALL fread(Scrth1,0,0,Nweor)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     END-OF-FILE
!
 20      Nklw = plp + npl - 1
         spag_nextblock_1 = 4
      CASE (4)
         IF ( nogo>0 ) Count = -1
         RETURN
!
!     ILLEGAL EOR
!
 40      CALL mesage(-3,Scrth1,name)
         spag_nextblock_1 = 5
      CASE (5)
!
!     INSUFFICIENT COREINTERNAL ELEMENT NUMBER PRINTED
!
         CALL page2(-2)
         WRITE (Outtap,99002) Ufm , name , B1p1 , itp
99002    FORMAT (A23,' 2298, INSUFFICIENT CORE ',2A4,1H(,I10,' ), PROPERTY',I9)
         Nklw = -plp
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE optp1d
