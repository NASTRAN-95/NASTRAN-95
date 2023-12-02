!*==wrtmsg.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE wrtmsg(Filex)
   USE c_machin
   USE c_output
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Filex
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: blank , formax , lstmax , rew
   INTEGER :: count , file , i , j , k1 , k2 , maxlin , mo , n , n2cpw , n2cpw1 , nbpc , nbpc2 , nbpw , ncpw , nf , pos , ret
   INTEGER , DIMENSION(100) :: for
   CHARACTER(1) , DIMENSION(400) :: formt
   INTEGER , DIMENSION(50) :: lst
   INTEGER , DIMENSION(5) :: mask1 , mask2
   INTEGER , DIMENSION(32,6) :: ttlsav
   REAL :: xlst
   EXTERNAL andf , close , complf , eject , forwrt , fread , lshift , orf , read , rshift
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!WKBI
!WKBI
   !>>>>EQUIVALENCE (formt,for)
   !>>>>EQUIVALENCE (xlst,lst)
   !>>>>EQUIVALENCE (Sysx(2),Mo) , (Sysx(9),Maxlin) , (Sysx(12),Count) , (Sysx(39),Nbpc) , (Sysx(40),Nbpw) , (Sysx(41),Ncpw)
   DATA lstmax , rew , formax , blank/50 , 1 , 100 , 4H    /
!
   n2cpw = ncpw/2
   n2cpw1 = n2cpw - 1
   nbpc2 = 2*nbpc
   mask1(1) = rshift(complf(0),nbpc2)
   mask2(1) = complf(mask1(1))
   DO i = 2 , n2cpw
      mask1(i) = orf(mask2(1),rshift(mask1(i-1),nbpc2))
      mask2(i) = complf(mask1(i))
   ENDDO
   file = Filex
!
   DO j = 1 , 6
      DO i = 1 , 32
         ttlsav(i,j) = title(i,j)
      ENDDO
   ENDDO
!
 100  count = maxlin
 200  SPAG_Loop_1_1: DO
      CALL read(*300,*100,file,n,1,0,nf)
      IF ( n<0 ) THEN
!
!     A TITLE OR SUBTITLE FOLLOWS.
!
         n = -n
         IF ( n<=6 ) CALL fread(file,title(1,n),32,0)
         IF ( n>6 ) CALL fread(file,0,-32,0)
         GOTO 100
      ELSEIF ( n/=0 ) THEN
!
!     A MESSAGE FOLLOWS...N = NUMBER OF LIST ITEMS.
!
         IF ( n<=lstmax ) THEN
            IF ( n/=0 ) CALL fread(file,lst,n,0)
         ELSE
            CALL fread(file,0,-n,0)
         ENDIF
      ENDIF
      DO
!
!     READ THE CORRESPONDING FORMAT...NF = SIZE OF THE FORMAT.
!
         CALL fread(file,nf,1,0)
         IF ( nf<0 ) THEN
            count = count - nf
         ELSEIF ( nf==0 ) THEN
            count = maxlin
         ELSEIF ( nf<=formax ) THEN
            CALL fread(file,for,nf,0)
!
!     CONDENSE FOR ARRAY TO ACQUIRE CONTIGUOUS HOLLERITH STRINGS.
!
            IF ( ncpw/=4 ) THEN
               DO i = 2 , nf
                  spag_nextblock_1 = 1
                  SPAG_DispatchLoop_1: DO
                     SELECT CASE (spag_nextblock_1)
                     CASE (1)
                        k1 = 1
                        pos = 2*i - 1
                        j = (pos+n2cpw1)/n2cpw
                        k2 = pos - n2cpw*(j-1)
                        ASSIGN 202 TO ret
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
 202                    k1 = 2
                        IF ( k2+1<=n2cpw ) THEN
                           k2 = k2 + 1
                        ELSE
                           k2 = 1
                           j = j + 1
                        ENDIF
                        ASSIGN 205 TO ret
                        spag_nextblock_1 = 2
                     CASE (2)
                        IF ( k2<k1 ) THEN
                           for(j) = orf(andf(for(j),mask1(k2)),lshift(andf(for(i),mask2(k1)),(nbpc2*(k1-k2))))
                        ELSEIF ( k2==k1 ) THEN
                           for(j) = orf(andf(for(j),mask1(k2)),andf(for(i),mask2(k1)))
                        ELSE
                           for(j) = orf(andf(for(j),mask1(k2)),rshift(andf(for(i),mask2(k1)),(nbpc2*(k2-k1))))
                        ENDIF
                        GOTO ret
                     END SELECT
                  ENDDO SPAG_DispatchLoop_1
 205           ENDDO
            ENDIF
!
!     PRINT THE LINE
!
            IF ( eject(1)/=0 ) THEN
               DO j = 4 , 6
                  spag_nextblock_2 = 1
                  SPAG_DispatchLoop_2: DO
                     SELECT CASE (spag_nextblock_2)
                     CASE (1)
                        DO i = 1 , 32
                           IF ( title(i,j)/=blank ) THEN
                              spag_nextblock_2 = 2
                              CYCLE SPAG_DispatchLoop_2
                           ENDIF
                        ENDDO
                        count = count - 1
                     CASE (2)
                        WRITE (mo,99001) (title(i,j),i=1,32)
                        EXIT SPAG_DispatchLoop_2
                     END SELECT
                  ENDDO SPAG_DispatchLoop_2
               ENDDO
               WRITE (mo,99001)
               count = count + 1
            ENDIF
!
            IF ( n==0 .AND. (mach==5 .OR. mach==12) ) THEN
               WRITE (mo,for)
            ELSEIF ( mach==5 .OR. mach==12 ) THEN
               WRITE (mo,for,ERR=200) (lst(j),j=1,n)
            ELSE
               CALL forwrt(formt,lst,n)
            ENDIF
            CYCLE SPAG_Loop_1_1
         ELSE
            CALL fread(file,0,-nf,0)
            GOTO 100
         ENDIF
      ENDDO
      EXIT SPAG_Loop_1_1
   ENDDO SPAG_Loop_1_1
!
!     END OF MESSAGE FILE
!
 300  CALL close(file,rew)
   DO j = 1 , 6
      DO i = 1 , 32
         title(i,j) = ttlsav(i,j)
      ENDDO
   ENDDO
99001 FORMAT (2X,32A4)
END SUBROUTINE wrtmsg
