!*==scaled.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE scaled(Type,Emord)
USE C_EMGDIC
USE C_EMGEST
USE C_EMGPRM
USE C_SYSTEM
USE C_XMSSG
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Type
   INTEGER :: Emord
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: code , eid , gsube , i , ioutpt , ip , ipg , ncol , ngrids , nterms
   INTEGER , DIMENSION(2) , SAVE :: cpt
   INTEGER , DIMENSION(7) :: dict
   REAL(REAL64) , DIMENSION(16) :: dz
   INTEGER , DIMENSION(4) , SAVE :: gpt , gspt , kpt
   INTEGER , DIMENSION(2) :: icomp , isil
   INTEGER , DIMENSION(1) :: iest
   REAL , DIMENSION(16) :: z
   EXTERNAL emgout
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE PROCESSES CELAS, CDAMP, AND CMASS ELEMENTS.
!
!     TYPE  - DENOTES FORM OF EST DATA. IE CELAS1,CELAS2,ETC.
!     EMORD - DENOTES MATRIX  1 = CELAS = STIFFNESS MATRIX,
!                             2 = CMASS = MASS MATRIX,
!                             3 = CDAMP = DAMPING MATRIX
!
!     EST FOR ELAS ELEMENTS
!
!                     TYPE           TYPE           TYPE           TYPE
!             CELAS1         CELAS2         CELAS3         CELAS4
!             ------  ----   ------  ----   ------  ----   ------  ----
!     ECPT(1) IELID     I    IELID     I    IELID     I    IELID     I
!     ECPT(2) IGP1      I    K         R    IS1       I    K         R
!     ECPT(3) IGP2      I    IGP1      I    IS2       I    IS1       I
!     ECPT(4) IC1       I    IGP2      I    K         R    IS2       I
!     ECPT(5) IC2       I    IC1       I    GSUBE     R
!     ECPT(6) K         R    IC2       I    S         R
!     ECPT(7) GSUBE     R    GSUBE     R
!     ECPT(8) S         R    S         R
!
   !>>>>EQUIVALENCE (Ksystm(2),Ioutpt) , (z(1),dz(1)) , (Iest(1),Est(1))
   DATA gpt/2 , 3 , 2 , 3/ , cpt/4 , 5/ , kpt/6 , 2 , 4 , 2/
   DATA gspt/7 , 7 , 5 , 0/
!
!     TEST IF MATRIX TO BE PRODUCED IS REQUESTED
!
   IF ( Imat(Emord)==0 ) RETURN
!
!     MOVE EST DATA TO LOCAL ARRAYS.  LOCATIONS ARE GIVEN BY DATA //
!
   eid = iest(1)
   ip = kpt(Type)
   z(1) = Est(ip)
   gsube = 0
   icomp(1) = 0
   icomp(2) = 0
   dict(2) = 1
   ngrids = 2
   ip = gpt(Type)
   isil(1) = iest(ip)
   isil(2) = iest(ip+1)
   IF ( Type<3 ) THEN
      ip = cpt(Type)
      IF ( iest(ip)/=0 ) icomp(1) = iest(ip) - 1
      IF ( iest(ip+1)/=0 ) icomp(2) = iest(ip+1) - 1
   ENDIF
!
!     IF ONE SIL IS ZERO INSURE THAT IT IS THE SECOND.
!     IF BOTH SILS ARE NON-ZERO MAKE SURE HIGHER OF TWO IS SECOND.
!
   IF ( isil(2)/=0 ) THEN
      IF ( isil(1)/=0 ) THEN
         IF ( isil(1)<=isil(2) ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
      ENDIF
!
!     SWITCH SILS AND COMPS
!
      ip = isil(2)
      isil(2) = isil(1)
      isil(1) = ip
      ip = icomp(2)
      icomp(2) = icomp(1)
      icomp(1) = ip
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      IF ( isil(2)>0 ) THEN
!
         IF ( isil(2)/=isil(1) ) THEN
            IF ( icomp(1)==icomp(2) ) THEN
!
!     COMPONENTS ARE THE SAME FOR BOTH POINTS
!
               nterms = 4
               ncol = 2
               code = 2**icomp(1)
               z(2) = -z(1)
               z(3) = -z(1)
               z(4) = z(1)
               CALL spag_block_2
               RETURN
            ENDIF
!
!     IF THE ELEMENT CONNECTS TWO COMPONENTS OF THE SAME POINT IT
!     MUST HAVE SPECIAL TREATMENT
!
         ELSEIF ( icomp(2)==icomp(1) ) THEN
!
            WRITE (ioutpt,99001) Uwm , eid
99001       FORMAT (A25,' 3120, IMPROPER CONNECTION ON CELAS ELEMENT',I9)
            RETURN
!
!     IN THE GENERAL CASE, THE CONNECTED COMPONENTS MAY BE THE SAME
!     AND THE MATRIX IS A 2 BY 2.  IF THE COMPONENTS ARE DIFFERENT
!     THE MATRIX WILL BE A 4 BY 4 WITH ADDITIONAL ZEROS.
!
         ENDIF
!
         nterms = 16
         code = 2**icomp(1) + 2**icomp(2)
         ncol = 4
         DO i = 2 , 16
            z(i) = 0.0
         ENDDO
         IF ( icomp(2)<icomp(1) ) THEN
            z(6) = z(1)
            z(7) = -z(1)
            z(10) = -z(1)
            z(11) = z(1)
            z(1) = 0.0
            IF ( isil(1)==isil(2) ) THEN
               z(1) = z(11)
               z(2) = z(10)
               z(5) = z(7)
               z(7) = 0.0
               z(10) = 0.0
               z(11) = 0.0
            ENDIF
         ELSE
            z(4) = -z(1)
            z(13) = -z(1)
            z(16) = z(1)
            IF ( isil(1)==isil(2) ) THEN
               z(2) = z(4)
               z(5) = z(13)
               z(6) = z(16)
               z(4) = 0.0
               z(13) = 0.0
               z(16) = 0.0
            ENDIF
         ENDIF
      ELSE
!
!     IF THE SECOND SIL EQUALS ZERO THE ELEMENT IS GROUNDED
!     ONLY A SINGLE MATRIX TERM IS PRODUCED
!
         ngrids = 1
         dict(2) = 1
         nterms = 1
         code = 2**icomp(1)
         ncol = 1
      ENDIF
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
!
!     OUTPUT THE MATRIX HERE
!
      dict(1) = Estid
      dict(3) = ncol
      dict(4) = code
      dict(5) = 0
      ipg = gspt(Type)
!
!     STRUCTURAL DAMPING FOR  STIIFNESS MATRICES IS INSERTED IN DICT
!
      IF ( Emord==1 .AND. Type<=3 ) dict(5) = iest(ipg)
      IF ( Iprec/=1 ) THEN
         i = nterms
         SPAG_Loop_1_1: DO
            dz(i) = z(i)
            i = i - 1
            IF ( i<=0 ) EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
      ENDIF
      CALL emgout(z,dz,nterms,1,dict,Emord,Iprec)
      RETURN
   END SUBROUTINE spag_block_2
END SUBROUTINE scaled
