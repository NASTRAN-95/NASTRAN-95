!*==qloadl.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE qloadl(Iopt)
   IMPLICIT NONE
   USE C_LOADX
   USE C_QVECT
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iopt
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(19) :: card
   REAL , DIMENSION(4) :: coef
   REAL :: dot , dot2 , flag
   REAL , DIMENSION(3) :: e , v1 , v2
   INTEGER :: i , id , int , isil , n , nwords , type
   INTEGER , DIMENSION(3) :: ie
   INTEGER , DIMENSION(6) , SAVE :: igrids
   INTEGER , SAVE :: iold , itran1
   INTEGER , DIMENSION(2) , SAVE :: minus , subr
   LOGICAL :: nogo , transt
   INTEGER , DIMENSION(4) :: sils
   EXTERNAL mesage , numtyp , read , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE CALCULATES THERMAL LOADS FROM QBDY1, QBDY2, OR
!     QVECT DATA. THE INPUT DATA, READ FROM FILE SLT, IS -
!
!     ENTRY       QBDY1         QBDY2          QVECT
!     -----       -----         -----          -----
!       1          TYPE         EL.ID.          SIL1
!       2         EL.ID.         TYPE           SIL2
!       3          SIL1          SIL1           SIL3
!       4          SIL2          SIL2           SIL4
!       5          SIL3          SIL3          EL.ID.
!       6          SIL4          SIL4           TYPE
!      7-10      C1,C2,C3,C4    -SAME          -SAME
!     11-13       E VECTOR       NONE           NONE
!     14-16      V1 VECTOR         *             *
!     17-19      V2 VECTOR         *             *
!
   !>>>>EQUIVALENCE (type,card(1)) , (id,card(2)) , (sils(1),card(3)) , (coef(1),card(7)) , (e(1),ie(1),card(11)) , (v1(1),card(14)) ,   &
!>>>>    & (v2(1),card(17))
   DATA igrids/1 , 2 , 2 , 3 , 4 , 2/
   DATA subr/4HQLOA , 4HDL  /
   DATA itran1 , iold , minus/4HTRAN , 0 , -1 , -1/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         transt = .FALSE.
         IF ( Itran==itran1 ) transt = .TRUE.
         nwords = 10
         IF ( Iopt==3 ) nwords = 19
!
         CALL read(*20,*40,Slt,card(1),nwords,0,flag)
!
!     REARRANGE CARD ARRAY FOR UNIFORMITY.
!
         IF ( Iopt==1 ) THEN
         ELSEIF ( Iopt==3 ) THEN
!
!     QVECT LOADS
!
            dot = card(5)
            dot2 = card(6)
            card(6) = card(4)
            card(5) = card(3)
            card(4) = card(2)
            card(3) = card(1)
            card(2) = dot
            card(1) = dot2
            n = igrids(type)
            dot = 0.0
            int = 0
            IF ( type==6 ) THEN
!
!     QVECT ON ELCYL ELEMENT
!
               dot2 = 0.0
               DO i = 1 , 3
                  IF ( numtyp(ie(i))==1 ) THEN
                     int = int + 1
                  ELSE
                     dot = dot + e(i)*v1(i)
                     dot2 = dot2 + e(i)*v2(i)
                  ENDIF
               ENDDO
               IF ( int>0 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               coef(1) = coef(1)*sqrt(dot**2+dot2**2)
               coef(2) = coef(1)
               isil = sils(1)
               Core(isil) = Core(isil) + coef(1)
               isil = sils(2)
               Core(isil) = Core(isil) + coef(2)
               RETURN
            ELSE
               DO i = 1 , 3
                  IF ( numtyp(ie(i))==1 ) THEN
                     int = int + 1
                  ELSE
                     dot = dot + e(i)*v1(i)
                  ENDIF
               ENDDO
               IF ( int>0 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( dot>=0.0 ) RETURN
               DO i = 1 , n
                  isil = sils(i)
                  Core(isil) = Core(isil) - dot*coef(i)
               ENDDO
               RETURN
            ENDIF
         ELSE
            dot = card(1)
            card(1) = card(2)
            card(2) = dot
         ENDIF
         n = igrids(type)
!
!     QBDY1 OR QBDY2
!
         DO i = 1 , n
            isil = sils(i)
            Core(isil) = Core(isil) + coef(i)
         ENDDO
         RETURN
      CASE (2)
!
!     GOES HERE IF INTEGERS ARE FOUND IN E VECTOR
!
         IF ( .NOT.transt ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     BUILD QVECT RECORDS FOR TRANSIENT
!
         IF ( Ilid/=iold ) THEN
!
!     TERMINATE OLD RECORD
!
            IF ( iold/=0 ) CALL write(Iqvect,minus,2,0)
            iold = Ilid
            CALL write(Iqvect,Ilid,1,0)
         ENDIF
!
!     DUMP DATA ON IQVECT
!
         CALL write(Iqvect,n,1,0)
         DO i = 1 , n
            CALL write(Iqvect,sils(i),1,0)
            CALL write(Iqvect,coef(i),1,0)
         ENDDO
         CALL write(Iqvect,ie,3,0)
         CALL write(Iqvect,v1,6,0)
         RETURN
!
 20      CALL mesage(-2,Slt,subr)
 40      CALL mesage(-3,Slt,subr)
         spag_nextblock_1 = 3
      CASE (3)
         nogo = .TRUE.
         WRITE (Iout,99001) Ufm , id
99001    FORMAT (A23,' 3080, ERROR IN QVECT DATA, INTEGER VALUES SPECIFIED',' FOR THERMAL FLUX VECTOR COMPONENTS',/30X,             &
                &'IN A NON-TRANSIENT ANALYSIS.',/30X,'ELEMENT ID = ',I9)
         CALL mesage(-61,0,subr)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE qloadl
