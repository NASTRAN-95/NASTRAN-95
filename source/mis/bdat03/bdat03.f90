!*==bdat03.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE bdat03
   USE c_blank
   USE c_cmb001
   USE c_cmb002
   USE c_cmb003
   USE c_cmb004
   USE c_output
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: aaa , trans
   REAL :: flag , xmag , ymag , zmag
   INTEGER :: i , id , ierr , ifile , imsg , inum , it , j , jdh , kk , ll , n , ngtrn , ngtrn1 , nnn , npm1
   INTEGER , SAVE :: iblnk
   INTEGER , DIMENSION(10) , SAVE :: ihd
   REAL , DIMENSION(9) :: out , temp
   REAL , DIMENSION(3) :: v2 , xax , yax , zax
   EXTERNAL andf , close , eof , locate , mesage , page , read , rshift , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE PROCESSES TRANS BULK DATA, GENERATES THE
!     TRANSFORMATION MATRIX, AND WRITES TO SCBDAT.
!
   DATA ihd/4H  SU , 4HMMAR , 4HY OF , 4H PRO , 4HCESS , 4HED T , 4HRANS , 4H BUL , 4HK DA , 4HTA  /
   DATA trans/310 , 3/ , aaa/4HBDAT , 4H03  / , iblnk/4H    /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         ngtrn = z(buf4)
         inum = 1
         ierr = 0
         DO i = 1 , 7
            DO j = 1 , 3
               origin(i,j) = 0.0
            ENDDO
         ENDDO
         DO i = 1 , 96
            ihead(i) = iblnk
         ENDDO
         j = 1
         DO i = 76 , 85
            ihead(i) = ihd(j)
            j = j + 1
         ENDDO
         CALL locate(*40,z(buf1),trans(1),flag)
         ifile = geom4
         spag_nextblock_1 = 2
      CASE (2)
         SPAG_Loop_1_1: DO
            CALL read(*60,*20,geom4,id,1,0,n)
            DO i = 1 , npsub
               it = 1
               IF ( id==combo(i,3) ) EXIT SPAG_Loop_1_1
            ENDDO
            IF ( ngtrn/=0 ) THEN
               DO i = 1 , ngtrn
                  it = 2
                  IF ( id==z(buf4+i) ) EXIT SPAG_Loop_1_1
               ENDDO
            ENDIF
            CALL read(*60,*80,geom4,temp,-9,0,nnn)
         ENDDO SPAG_Loop_1_1
         tdat(3) = .TRUE.
         IF ( it==1 ) combo(i,3) = -combo(i,3)
         IF ( it==2 ) z(buf4+i) = -z(buf4+i)
         CALL read(*60,*80,geom4,temp,9,0,nnn)
         IF ( it==1 ) THEN
            DO ll = 1 , 3
               origin(i,ll) = temp(ll)
            ENDDO
         ENDIF
!
!     DEFINE Z-AXIS
!
         zax(1) = temp(4) - temp(1)
         zax(2) = temp(5) - temp(2)
         zax(3) = temp(6) - temp(3)
!
!     DEFINE Y-AXIS
!
         v2(1) = temp(7) - temp(1)
         v2(2) = temp(8) - temp(2)
         v2(3) = temp(9) - temp(3)
         yax(1) = zax(2)*v2(3) - zax(3)*v2(2)
         yax(2) = zax(3)*v2(1) - zax(1)*v2(3)
         yax(3) = zax(1)*v2(2) - zax(2)*v2(1)
!
!     DEFINE X-AXIS
!
         xax(1) = yax(2)*zax(3) - zax(2)*yax(3)
         xax(2) = yax(3)*zax(1) - zax(3)*yax(1)
         xax(3) = yax(1)*zax(2) - zax(1)*yax(2)
!
!     CHANGE TO UNIT VECTORS
!
         zmag = sqrt(zax(1)**2+zax(2)**2+zax(3)**2)
         ymag = sqrt(yax(1)**2+yax(2)**2+yax(3)**2)
         xmag = sqrt(xax(1)**2+xax(2)**2+xax(3)**2)
         DO i = 1 , 3
            zax(i) = zax(i)/zmag
            yax(i) = yax(i)/ymag
            xax(i) = xax(i)/xmag
         ENDDO
         CALL write(scbdat,id,1,0)
         CALL write(scbdat,1,1,0)
         CALL write(scbdat,temp(1),3,0)
         out(1) = xax(1)
         out(2) = yax(1)
         out(3) = zax(1)
         out(4) = xax(2)
         out(5) = yax(2)
         out(6) = zax(2)
         out(7) = xax(3)
         out(8) = yax(3)
         out(9) = zax(3)
         CALL write(scbdat,out,9,0)
         IF ( andf(rshift(iprint,6),1)==1 ) THEN
            inum = inum + 1
            IF ( mod(inum,2)==0 ) CALL page
            WRITE (outt,99001) id
99001       FORMAT (//48X,34HTRANS SET IDENTIFICATION NUMBER = ,I8)
            WRITE (outt,99002) (temp(i),i=1,3)
99002       FORMAT (/50X,37HCOORDINATES OF ORIGIN IN BASIC SYSTEM,/45X,3E15.6,//58X,21HTRANSFORMATION MATRIX/)
            WRITE (outt,99003) (out(i),i=1,9)
99003       FORMAT (43X,5H*****,42X,5H*****,/3(43X,1H*,50X,1H*,/43X,1H*,1X,3E15.6,4X,1H*,/),43X,1H*,50X,1H*,/43X,5H*****,42X,       &
                  & 5H*****)
         ENDIF
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     PROCESS REPEATED GTRAN IDS
!
 20      IF ( ngtrn>=2 ) THEN
            ngtrn1 = ngtrn - 1
            DO i = 1 , ngtrn1
               IF ( z(buf4+i)<0 ) THEN
                  kk = i + 1
                  DO j = kk , ngtrn
                     IF ( iabs(z(buf4+i))==z(buf4+j) ) z(buf4+j) = -z(buf4+j)
                  ENDDO
               ENDIF
            ENDDO
         ENDIF
         npm1 = npsub - 1
         DO i = 1 , npm1
            IF ( combo(i,3)<0 ) THEN
               kk = i + 1
               DO j = kk , npsub
                  IF ( iabs(combo(i,3))==combo(j,3) ) THEN
                     combo(j,3) = -combo(j,3)
                     DO jdh = 1 , 3
                        origin(j,jdh) = origin(i,jdh)
                     ENDDO
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
!
!     TEST TO SEE THAT ALL TRANS HAVE BEEN FOUND
!
         DO i = 1 , npsub
            IF ( combo(i,3)>0 ) THEN
               ierr = 1
               WRITE (outt,99004) ufm , combo(i,3)
!
99004          FORMAT (A23,' 6511, THE REQUESTED TRANS SET ID',I9,' HAS NOT BEEN DEFINED BY BULK DATA.')
            ENDIF
         ENDDO
         IF ( ngtrn/=0 ) THEN
            DO i = 1 , ngtrn
               IF ( z(buf4+i)>0 ) THEN
                  ierr = 1
                  WRITE (outt,99005) ufm , z(buf4+i)
99005             FORMAT (A23,' 6513, THE TRANS SET ID',I9,' REQUESTED BY A GTRAN ','BULK DATA CARD HAS NOT BEEN DEFINED.')
               ENDIF
            ENDDO
         ENDIF
 40      CALL eof(scbdat)
         CALL write(scbdat,id,1,1)
         CALL close(scbdat,1)
         DO i = 1 , npsub
            combo(i,3) = iabs(combo(i,3))
         ENDDO
         IF ( ierr==1 ) idry = -2
         RETURN
!
 60      imsg = -2
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 80      imsg = -3
         spag_nextblock_1 = 3
      CASE (3)
         CALL mesage(imsg,ifile,aaa)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE bdat03
