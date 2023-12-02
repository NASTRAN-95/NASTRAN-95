!*==combo.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE combo(Cdata,Nx,Extra,Nnam,Name,Nn,Var,Ier)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(5) :: Cdata
   INTEGER :: Nx
   INTEGER , DIMENSION(3,1) :: Extra
   INTEGER :: Nnam
   INTEGER , DIMENSION(2,1) :: Name
   INTEGER :: Nn
   INTEGER , DIMENSION(3,2) :: Var
   INTEGER :: Ier
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: eqsn , lprn , manu , nams , ncno , nnc , nno , nopt , nsort
   INTEGER :: i , ic , ix , iy , j , jnam , k , kn , lword , spag_nextblock_1 , spag_nextblock_2
   INTEGER , DIMENSION(7) , SAVE :: inum , nai
   INTEGER , DIMENSION(3) , SAVE :: mopt , msort
   INTEGER , DIMENSION(7) :: numbs
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE  PROCESSES THE  COMBINE INPUT.
!        THE  INPUT/ OUTPUTS  ARE
!
!                  CDATA  -  XRCARD  IMAGE OF  COMBINE CARD  (IN)
!                  NX     -  NUMBER OF EXTRAS                (IN)
!                  EXTRA  -  3 BY NX ARRAY OF EXTRAS         (IN)
!                  NNAM   -  NUMBER OF CURRENT SUBS NAMES (IN/OUT)
!                  NAMES  -  ARRAY OF  CURRENT SUBS NAMES (IN/OUT)
!                  NN     -  NUMBER OF SUBS TO BE COMBINED  (OUT)
!                  VAR    -  3 BY NVAR ARRAY OF VARIABLES   (OUT)
!                            ARRANGED AS- KEY WORD + 2 DATA WORDS
!
!
!
!
!
!
!
   DATA inum/4HN1   , 4HN2   , 4HN3   , 4HN4   , 4HN5   , 4HN6   , 4HN7  /
   DATA lprn , nopt , nsort , mopt , msort/4H(    , 4HOPTS , 4HSORT , 4HAUTO , 4HMAN  , 4HREST , 4HX    , 4HY    , 4HZ   /
   DATA manu/4HMANU/
   DATA nno/4HNAME/ , nnc/4HNAMC/ , nams/4HNAMS/
   DATA nai/4HNA1  , 4HNA2  , 4HNA3  , 4HNA4  , 4HNA5  , 4HNA6  , 4HNA7 /
   DATA ncno/4HNCNO/
   DATA eqsn/4H=   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
         lword = rshift(complf(0),1)
         Ier = 0
!     COMBINE OPERATION
!         PROCESS PRIMARY CARD -COMBINE( OPTS,SORT) = NAME1,NAME2, ETC
!     SET DEFAULTS
         DO i = 1 , 150
            Var(i,1) = 0
         ENDDO
         jnam = 6
         Var(1,1) = nopt
         Var(2,1) = mopt(1)
         Var(1,2) = nsort
         Var(2,2) = msort(1)
         IF ( Cdata(5)/=lprn ) THEN
!
!     NO  OPTION
            k = 4
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSE
            k = 6
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     PROCESS  AUTO/MAN  OR XYZ
!
         DO i = 1 , 3
            IF ( Cdata(k)==mopt(i) ) THEN
               Var(2,1) = mopt(i)
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Cdata(k)==manu ) THEN
               Var(2,1) = mopt(2)
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Cdata(k)==msort(i) ) THEN
               Var(2,2) = msort(i)
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!     NOT VALID    ASSUME EQ SIGN OR NAME
!
         ENDDO
         spag_nextblock_1 = 4
      CASE (3)
         k = k + 2
         spag_nextblock_1 = 2
      CASE (4)
!
!     CHECK FOR  EQ SIGN
         IF ( Cdata(k+1)==eqsn ) k = k + 2
!
!     PROCESS NAMES
         Nn = 0
         SPAG_Loop_1_1: DO i = 1 , 7
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  kn = k + 2*i - 2
                  IF ( Cdata(kn)==lword ) EXIT SPAG_Loop_1_1
!
                  Var(1,i+2) = nams
                  Var(2,i+2) = Cdata(kn)
                  Var(3,i+2) = Cdata(kn+1)
!
!     FIND STRUCTURE NUMBER
                  IF ( Nnam/=0 ) THEN
                     DO j = 1 , Nnam
                        IF ( Cdata(kn)==Name(1,j) .AND. Cdata(kn+1)==Name(2,j) ) THEN
                           numbs(i) = j
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDDO
                  ENDIF
!
!     NEW NAME
!
                  Nnam = Nnam + 1
                  numbs(i) = Nnam
                  Name(1,Nnam) = Cdata(kn)
                  Name(2,Nnam) = Cdata(kn+1)
                  spag_nextblock_2 = 2
               CASE (2)
                  Nn = Nn + 1
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO SPAG_Loop_1_1
!
!
!     MOVE  EXTRAS INTO PLACE  CHANGE NAME TO NAMC
         ic = 0
         DO j = 1 , Nx
            ix = j + 3*Nn + 2
            IF ( Extra(1,j)==nno ) THEN
               Extra(1,j) = nnc
               ic = ix
            ENDIF
            DO k = 1 , 3
               Var(k,ix) = Extra(k,j)
            ENDDO
         ENDDO
!
!     SET  STRUCTURE NUMBER KEYS
!
         IF ( Nn==0 ) THEN
            Ier = 1
         ELSE
!
            DO i = 1 , Nn
!
               ix = i + Nn + 2
               Var(1,ix) = inum(i)
               Var(2,ix) = -1
               Var(3,ix) = numbs(i)
               iy = ix + Nn
               Var(1,iy) = nai(i)
               Var(2,iy) = Var(2,i+2)
               Var(3,iy) = Var(3,i+2)
            ENDDO
         ENDIF
!
!     CHECK  FOR NAMC AS A PREVIOUS NAME  OR MISSING
         IF ( ic/=0 ) THEN
            DO j = 1 , Nnam
               IF ( Var(2,ic)==Name(1,j) .AND. Var(3,ic)==Name(2,j) ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
!
!     OK -NEW NAME , ADD TO LIST
!
            Nnam = Nnam + 1
            Name(1,Nnam) = Var(2,ic)
            Name(2,Nnam) = Var(3,ic)
            ix = Nx + 3*Nn + 3
            Var(1,ix) = ncno
            Var(2,ix) = -1
            Var(3,ix) = Nnam
            RETURN
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         Ier = Ier + 2
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE combo
