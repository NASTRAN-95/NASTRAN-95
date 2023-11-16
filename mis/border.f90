
SUBROUTINE border(Gplst,X,U,Istore,Deform,B1,Opcor)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Scr2 , Scr4
   REAL Scr3 , Skip(25)
   COMMON /blank / Skip , Scr2 , Scr3 , Scr4
!
! Dummy argument declarations
!
   INTEGER B1 , Deform , Opcor
   INTEGER Gplst(1) , Istore(2)
   REAL U(2,1) , X(3,1)
!
! Local variable declarations
!
   INTEGER elid , i , ie , ie1 , iflag , ig , igdpt , ione , ip1 , itwo , j , lcor , m , nelmt , words(2)
   REAL pt(2,3)
!
! End of declarations
!
!
   EQUIVALENCE (words(1),nelmt) , (words(2),igdpt)
!
   lcor = Opcor/5 - 1
   CALL open(*99999,Scr2,Gplst(B1),0)
   CALL line(0.,0.,0.,0.,1,-1)
 100  CALL fwdrec(*400,Scr2)
 200  CALL read(*400,*100,Scr2,iflag,1,0,m)
   IF ( iflag==0 ) GOTO 400
   IF ( iflag==-1 ) GOTO 100
   CALL fread(Scr2,words,2,0)
   ie = -1
   DO
      ie = ie + 2
      CALL read(*400,*300,Scr2,elid,1,0,m)
      CALL fread(Scr2,Istore(ie),2,0)
   ENDDO
 300  ione = Istore(1)
   itwo = Istore(2)
   IF ( nelmt/=1 ) THEN
      ie = 2*nelmt
      ie1 = ie - 1
      DO i = 1 , ie1
         IF ( Istore(i)/=0 ) THEN
            ip1 = i + 1
            DO j = ip1 , ie
               IF ( Istore(i)==Istore(j) ) THEN
                  Istore(i) = 0
                  Istore(j) = 0
                  EXIT
               ENDIF
            ENDDO
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
      IF ( j==0 ) GOTO 200
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
   GOTO 200
 400  CALL line(0.,0.,0.,0.,1,+1)
   CALL close(Scr2,1)
99999 RETURN
END SUBROUTINE border
