!*==border.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE border(Gplst,X,U,Istore,Deform,B1,Opcor)
   IMPLICIT NONE
   USE C_BLANK
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Gplst
   REAL , DIMENSION(3,1) :: X
   REAL , DIMENSION(2,1) :: U
   INTEGER , DIMENSION(2) :: Istore
   INTEGER :: Deform
   INTEGER :: B1
   INTEGER :: Opcor
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: elid , i , ie , ie1 , iflag , ig , igdpt , ione , ip1 , itwo , j , lcor , m , nelmt
   REAL , DIMENSION(2,3) :: pt
   INTEGER , DIMENSION(2) :: words
   EXTERNAL close , fread , fwdrec , line , open , read
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
   !>>>>EQUIVALENCE (words(1),nelmt) , (words(2),igdpt)
!
         lcor = Opcor/5 - 1
         CALL open(*99999,Scr2,Gplst(B1),0)
         CALL line(0.,0.,0.,0.,1,-1)
 20      CALL fwdrec(*60,Scr2)
         spag_nextblock_1 = 2
      CASE (2)
         CALL read(*60,*20,Scr2,iflag,1,0,m)
         IF ( iflag==0 ) GOTO 60
         IF ( iflag==-1 ) GOTO 20
         CALL fread(Scr2,words,2,0)
         ie = -1
         DO
            ie = ie + 2
            CALL read(*60,*40,Scr2,elid,1,0,m)
            CALL fread(Scr2,Istore(ie),2,0)
         ENDDO
 40      ione = Istore(1)
         itwo = Istore(2)
         IF ( nelmt/=1 ) THEN
            ie = 2*nelmt
            ie1 = ie - 1
            DO i = 1 , ie1
               IF ( Istore(i)/=0 ) THEN
                  ip1 = i + 1
                  SPAG_Loop_2_1: DO j = ip1 , ie
                     IF ( Istore(i)==Istore(j) ) THEN
                        Istore(i) = 0
                        Istore(j) = 0
                        EXIT SPAG_Loop_2_1
                     ENDIF
                  ENDDO SPAG_Loop_2_1
               ENDIF
            ENDDO
            j = 0
            DO i = 1 , ie
               IF ( Istore(i)/=0 ) THEN
                  j = j + 1
                  IF ( j<=1 ) THEN
                     ione = Istore(i)
                  ELSE
                     itwo = Istore(i)
                  ENDIF
               ENDIF
            ENDDO
            IF ( j==0 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         ig = iabs(Gplst(igdpt))
         IF ( Deform/=0 ) THEN
            pt(1,3) = U(1,ig)
            pt(2,3) = U(2,ig)
         ELSE
            pt(1,3) = X(2,ig)
            pt(2,3) = X(3,ig)
         ENDIF
         ig = ione
         DO i = 1 , 2
            ig = iabs(Gplst(ig))
            IF ( Deform/=0 ) THEN
               pt(1,i) = U(1,ig)
               pt(2,i) = U(2,ig)
            ELSE
               pt(1,i) = X(2,ig)
               pt(2,i) = X(3,ig)
            ENDIF
            CALL line(pt(1,i),pt(2,i),pt(1,3),pt(2,3),1,0)
            ig = itwo
         ENDDO
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 60      CALL line(0.,0.,0.,0.,1,+1)
         CALL close(Scr2,1)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99999 END SUBROUTINE border
