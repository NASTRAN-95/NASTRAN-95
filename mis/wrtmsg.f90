
SUBROUTINE wrtmsg(Filex)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Count , Mach , Maxlin , Mo , Nbpc , Nbpw , Ncpw , Sysx(41) , Title(32,6)
   COMMON /machin/ Mach
   COMMON /output/ Title
   COMMON /system/ Sysx
!
! Dummy argument declarations
!
   INTEGER Filex
!
! Local variable declarations
!
   INTEGER andf , complf , eject , lshift , orf , rshift
   INTEGER blank , file , for(100) , formax , i , j , k1 , k2 , lst(50) , lstmax , mask1(5) , mask2(5) , n , n2cpw , n2cpw1 ,       &
         & nbpc2 , nf , pos , ret , rew , ttlsav(32,6)
   CHARACTER*1 formt(400)
   REAL xlst
   EXTERNAL andf , complf , lshift , orf , rshift
!
! End of declarations
!
!
!WKBI
!WKBI
   EQUIVALENCE (formt,for)
   EQUIVALENCE (xlst,lst)
   EQUIVALENCE (Sysx(2),Mo) , (Sysx(9),Maxlin) , (Sysx(12),Count) , (Sysx(39),Nbpc) , (Sysx(40),Nbpw) , (Sysx(41),Ncpw)
   DATA lstmax , rew , formax , blank/50 , 1 , 100 , 4H    /
!
   n2cpw = Ncpw/2
   n2cpw1 = n2cpw - 1
   nbpc2 = 2*Nbpc
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
         ttlsav(i,j) = Title(i,j)
      ENDDO
   ENDDO
!
 100  Count = Maxlin
 200  CALL read(*300,*100,file,n,1,0,nf)
   IF ( n<0 ) THEN
!
!     A TITLE OR SUBTITLE FOLLOWS.
!
      n = -n
      IF ( n<=6 ) CALL fread(file,Title(1,n),32,0)
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
         Count = Count - nf
      ELSEIF ( nf==0 ) THEN
         Count = Maxlin
      ELSEIF ( nf<=formax ) THEN
         CALL fread(file,for,nf,0)
!
!     CONDENSE FOR ARRAY TO ACQUIRE CONTIGUOUS HOLLERITH STRINGS.
!
         IF ( Ncpw/=4 ) THEN
            DO i = 2 , nf
               k1 = 1
               pos = 2*i - 1
               j = (pos+n2cpw1)/n2cpw
               k2 = pos - n2cpw*(j-1)
               ASSIGN 205 TO ret
               GOTO 210
 205           k1 = 2
               IF ( k2+1<=n2cpw ) THEN
                  k2 = k2 + 1
               ELSE
                  k2 = 1
                  j = j + 1
               ENDIF
               ASSIGN 220 TO ret
 210           IF ( k2<k1 ) THEN
                  for(j) = orf(andf(for(j),mask1(k2)),lshift(andf(for(i),mask2(k1)),(nbpc2*(k1-k2))))
               ELSEIF ( k2==k1 ) THEN
                  for(j) = orf(andf(for(j),mask1(k2)),andf(for(i),mask2(k1)))
               ELSE
                  for(j) = orf(andf(for(j),mask1(k2)),rshift(andf(for(i),mask2(k1)),(nbpc2*(k2-k1))))
               ENDIF
               GOTO ret
 220        ENDDO
         ENDIF
!
!     PRINT THE LINE
!
         IF ( eject(1)/=0 ) THEN
            DO j = 4 , 6
               DO i = 1 , 32
                  IF ( Title(i,j)/=blank ) GOTO 225
               ENDDO
               Count = Count - 1
               CYCLE
 225           WRITE (Mo,99001) (Title(i,j),i=1,32)
            ENDDO
            WRITE (Mo,99001)
            Count = Count + 1
         ENDIF
!
         IF ( n==0 .AND. (Mach==5 .OR. Mach==12) ) THEN
            WRITE (Mo,for)
         ELSEIF ( Mach==5 .OR. Mach==12 ) THEN
            WRITE (Mo,for,ERR=200) (lst(j),j=1,n)
         ELSE
            CALL forwrt(formt,lst,n)
         ENDIF
         GOTO 200
      ELSE
         CALL fread(file,0,-nf,0)
         GOTO 100
      ENDIF
   ENDDO
!
!     END OF MESSAGE FILE
!
 300  CALL close(file,rew)
   DO j = 1 , 6
      DO i = 1 , 32
         Title(i,j) = ttlsav(i,j)
      ENDDO
   ENDDO
99001 FORMAT (2X,32A4)
END SUBROUTINE wrtmsg
