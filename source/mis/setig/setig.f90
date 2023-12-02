!*==setig.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE setig(Kg1,Kg2,Ig,Norig)
   IMPLICIT NONE
   USE C_BANDS
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Kg1
   INTEGER :: Kg2
   INTEGER , DIMENSION(1) :: Ig
   INTEGER , DIMENSION(1) :: Norig
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: is , k , l , loop , m
   REAL , DIMENSION(2) , SAVE :: sub
   EXTERNAL bpack , bunpk , mesage
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!     THIS ROUTINE SETS IG(KG1,-)=KG2 AND IG(KG2,-)=KG1 IF THIS
!     CONNECTION HAS NOT ALREADY BEEN SET.
!     NEDGE = NUMBER OF UNIQUE EDGES.
!
   DATA sub/4HSETI , 4HG   /
!
   IF ( Kg1/=0 .AND. Kg2/=0 .AND. Kg1/=Kg2 ) THEN
      l = Kg1
      k = Kg2
      SPAG_Loop_1_2: DO loop = 1 , 2
         IF ( loop/=1 ) THEN
            l = Kg2
            k = Kg1
         ENDIF
         m = 0
         SPAG_Loop_2_1: DO
            m = m + 1
            IF ( m>Maxdeg ) THEN
               CALL spag_block_1
               RETURN
            ENDIF
            is = bunpk(Ig,l,m)
            IF ( is==0 ) THEN
               CALL bpack(Ig,l,m,k)
               Mm = max0(Mm,m)
               IF ( loop==1 ) Nedge = Nedge + 1
               EXIT SPAG_Loop_2_1
            ELSEIF ( is==k ) THEN
               EXIT SPAG_Loop_1_2
            ENDIF
         ENDDO SPAG_Loop_2_1
      ENDDO SPAG_Loop_1_2
   ENDIF
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
!
      WRITE (Nout,99001) Norig(l) , Maxdeg
99001 FORMAT (34H0***  FATAL ERROR - - - GRID POINT,I10,48H  HAS DEGREE EXCEEDING THE NODAL DEGREE LIMIT OF,I8)
      CALL mesage(-8,0,sub)
   END SUBROUTINE spag_block_1
END SUBROUTINE setig
