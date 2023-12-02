!*==initl2.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE initl2(Offset,Deltt)
USE C_DCOMPX
USE C_NAMES
USE C_SADDX
USE C_SFACT
USE C_TRDXX
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Offset
   REAL :: Deltt
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(2) :: alpha , beta
   INTEGER :: file , i , ip1 , iprec , itypal , itypbt
   INTEGER , DIMENSION(7) :: ifila , ifilb , ifilc
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL decomp , korsz , mesage , sadd , sdcomp , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     INITL2 WILL COMPUTE THE STARTING VALUES FOR THE INTEGRATION
!     ROUTINE
!
!     THIS ROUTINE IS SUITABLE FOR DOUBLE PRECISION OPERATION
!
   !>>>>EQUIVALENCE (Mcbs(1),Ifila(1)) , (Mcbs(8),Itypal) , (Mcbs(9),Alpha(1)) , (Mcbs(13),Ifilb(1)) , (Mcbs(20),Itypbt) ,               &
!>>>>    & (Mcbs(21),Beta(1)) , (Mcbs(61),Ifilc(1))
   DATA name/4HINIT , 4HL2  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         Nomat = 2
         iprec = Rdp
         alpha(2) = 0.0D0
         beta(2) = 0.0D0
         Nx = korsz(Z) - Offset
         Nz = Nx
!
!     FORM AND DECOMPOSE THE LEFT HAND MATRIX
!
         itypal = Rdp
         itypbt = Rdp
         alpha(1) = 1.0D0/Deltt**2
         beta(1) = 0.5D0/Deltt
         ifilc(4) = 6
         DO i = 1 , 7
            ifila(i) = Filem(i)
            ifilb(i) = Fileb(i)
         ENDDO
         ifilc(2) = Filek(2)
         ifilc(1) = Iscr2
         IF ( Filek(1)<=0 ) ifilc(1) = Iscr1
         ifilc(3) = Filek(2)
         IF ( ifila(1)/=0 .AND. ifila(4)/=6 ) ifilc(4) = Sqr
         IF ( ifilb(1)/=0 .AND. ifilb(4)/=6 ) ifilc(4) = Sqr
         ifilc(5) = iprec
         IF ( Filem(1)<=0 .AND. Fileb(1)<=0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL sadd(Z,Z)
         IF ( Filek(1)<=0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         DO i = 1 , 7
            ifila(i) = ifilc(i)
            ifilb(i) = Filek(i)
         ENDDO
         IF ( ifilb(4)/=6 ) ifilc(4) = Sqr
         ifilc(1) = Iscr1
         alpha(1) = 1.0D0
         beta(1) = 1.0D0/3.0D0
         CALL sadd(Z,Z)
         spag_nextblock_1 = 3
      CASE (3)
         CALL wrttrl(ifilc)
         IF ( ifilc(4)/=6 ) THEN
!
!     SET UP FOR UNSYMMETRIC DECOMPOSITION
!
            Isym = 1
            DO i = 1 , 7
               Ia(i) = ifilc(i)
            ENDDO
            Il(1) = Iscr2
            Iu(1) = Iscr3
            Iscr10 = Iscr4
            Iscr20 = Iscr5
            Iscr30 = Iscr6
            Il(5) = iprec
            file = Ia(1)
            CALL decomp(*20,Z(1),Z(1),Z(1))
            CALL wrttrl(Il)
            CALL wrttrl(Iu)
         ELSE
!
!     SET UP FOR SYMMETRIC DECOMPOSITION
!
            DO i = 1 , 7
               Ifa(i) = ifilc(i)
            ENDDO
            Ifl(1) = Iscr2
            Ifu(1) = Iscr3
            Isc1 = Iscr4
            Isc2 = Iscr5
            Isc3 = Iscr6
            Ifl(5) = iprec
            Ichl = 0
            Nxx = Nx
            file = Ifa(1)
            CALL sdcomp(*20,Z,Z,Z)
            CALL wrttrl(Ifl)
            Isym = 0
         ENDIF
!
!     FORM FIRST RIGHT HAND MATRIX
!
         DO i = 1 , 7
            ifila(i) = Filem(i)
         ENDDO
         alpha(1) = 2.0D0/Deltt**2
         beta(1) = -1.0D0/3.0D0
         ifilc(1) = Iscr1
         CALL sadd(Z,Z)
!
!     FORM SECOND RIGHT HAND MATRIX
!
         alpha(1) = -1.0D0/Deltt**2
         ifilc(1) = Iscr5
         CALL sadd(Z,Z)
         DO i = 1 , 7
            ifila(i) = ifilc(i)
            ifilb(i) = Fileb(i)
         ENDDO
         alpha(1) = 1.0D0
         beta(1) = 0.5D0/Deltt
         ifilc(1) = Iscr4
         CALL sadd(Z,Z)
         RETURN
!
!     ERRORS
!
 20      ip1 = -5
         spag_nextblock_1 = 4
      CASE (4)
         CALL mesage(ip1,file,name(1))
         spag_nextblock_1 = 5
      CASE (5)
!
!     NO BDD OR MDD
!
         IF ( Filek(1)<=0 ) THEN
!
!     ILLEGAL INPUT.   NO MATRICES
!
            ip1 = -7
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSE
            ifilc(1) = 0
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE initl2
