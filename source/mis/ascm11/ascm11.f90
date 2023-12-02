!*==ascm11.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ascm11(Name,Iphase,Isol,Nogo)
   USE c_asdbd
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Name
   INTEGER :: Iphase
   INTEGER :: Isol
   INTEGER :: Nogo
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(6,7) , SAVE :: comnd
   INTEGER :: i , icomnd , j , k , l
   INTEGER , DIMENSION(21) , SAVE :: isave
   INTEGER , DIMENSION(7,12) , SAVE :: ptbs
   INTEGER , DIMENSION(18,2) , SAVE :: rdmap
   REAL , SAVE :: slash
   INTEGER , DIMENSION(2) , SAVE :: subnam
   INTEGER , DIMENSION(4) , SAVE :: xtra
   EXTERNAL khrfn1 , mesage
!
! End of declarations rewritten by SPAG
!
!
!     EXIO COMMANDS DMAP DATA
!
   DATA comnd/4HSOFI , 2 , 4 , 0 , 12 , 0 , 4HSOFO , 2 , 4 , 0 , 12 , 0 , 4HREST , 2 , 4 , 0 , 12 , 0 , 4HDUMP , 2 , 4 , 0 , 12 ,   &
      & 0 , 4HCHEC , 2 , 4 , 0 , 12 , 0 , 4HCOMP , 2 , 4 , 0 , 12 , 0 , 4HAPPE , 2 , 4 , 0 , 12 , 0/
   DATA slash/1H//
   DATA isave/1 , 7 , 1 , 1 , 11 , 3 , 1 , 13 , 2 , 1 , 15 , 1 , 2 , 6 , 1 , 2 , 11 , 3 , 2 , 14 , 2/
   DATA rdmap/4HEXIO , 4H     , 4H  // , 4HS,N, , 4HDRY/ , 4HMACH , 4H!*DE , 4HVI*/ , 4H*UNI , 4HTNAM , 4HE*!* , 4HFORM , 4H*!*M ,  &
       &4HODE* , 4H!*PO , 4HSI*/ , 4H*ITE , 4HM*/  , 4H     , 4H     , 4H  *N , 4HAME0 , 4H001* , 4H!*NA , 4HME00 , 4H02*/ ,        &
      & 4H*NAM , 4HE000 , 4H3*!* , 4HNAME , 4H0004 , 4H*!*N , 4HAME0 , 4H005* , 4H $   , 4H    /
   DATA xtra/4HMACH , 4HPOSI , 4HITEM , 4HNAME/
   DATA ptbs/1 , 21 , 21 , 4 , 101 , 0 , 0 , 1 , 27 , 27 , 4 , 102 , 0 , 0 , 1 , 34 , 34 , 8 , 103 , 0 , 0 , 1 , 45 , 45 , 4 , 104 ,&
      & 0 , 0 , 1 , 52 , 52 , 4 , 105 , 0 , 0 , 1 , 59 , 59 , 4 , 106 , 0 , 0 , 1 , 66 , 66 , 4 , 107 , 0 , 0 , 2 , 12 , 12 , 8 ,   &
      & 108 , 0 , 0 , 2 , 23 , 23 , 8 , 109 , 0 , 0 , 2 , 34 , 34 , 8 , 110 , 0 , 0 , 2 , 45 , 45 , 8 , 111 , 0 , 0 , 2 , 56 , 56 , &
      & 8 , 112 , 0 , 0/
   DATA subnam/4HASCM , 2H11/
!
!     RESTORE TO ORIGINAL DATA BY REPLACEING ! BY / IN RDMAP ARRAY
!     (SEE ASCM01 FOR EXPLANATION))
!
   DO l = 1 , 21 , 3
      i = isave(l+1)
      j = isave(l)
      k = isave(l+2)
      rdmap(i,j) = khrfn1(rdmap(i,j),k,slash,1)
   ENDDO
!
!     VALIDATE COMMAND AND SET POINTERS
!
   DO i = 1 , 7
      IF ( Name==comnd(1,i) ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
   ENDDO
!
!     INPUT ERROR
!
   CALL mesage(7,0,subnam)
   Nogo = 1
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
      Icomnd = I
      irdm = 1
      nrdm = Comnd(2,Icomnd)
      ixtra = irdm + 18*nrdm
      nxtra = Comnd(3,Icomnd)
      ioct = ixtra + nxtra
      noct = Comnd(4,Icomnd)
      iptbs = ioct + 3*noct
      nptbs = Comnd(5,Icomnd)
      iph = iptbs + 7*nptbs
      nph = Comnd(6,Icomnd)
!
!     MOVE RDMAP DATA
!
      K = 0
      IF ( nrdm/=0 ) THEN
         DO J = 1 , nrdm
            DO I = 1 , 18
               K = K + 1
               idat(K) = Rdmap(I,J)
            ENDDO
         ENDDO
      ENDIF
!
!     MOVE XTRA DATA
!
      IF ( nxtra/=0 ) THEN
         DO I = 1 , nxtra
            K = K + 1
            idat(K) = Xtra(I)
         ENDDO
      ENDIF
!
!     MOVE PTBS DATA
!
      IF ( nptbs/=0 ) THEN
         DO J = 1 , nptbs
            DO I = 1 , 7
               K = K + 1
               idat(K) = Ptbs(I,J)
            ENDDO
         ENDDO
      ENDIF
!
   END SUBROUTINE spag_block_1
!
END SUBROUTINE ascm11
