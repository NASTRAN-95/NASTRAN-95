
SUBROUTINE sofint(Ib1,Ib2,Numb,Ibl1)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Buf(1) , Filnam(10) , Filsiz(10) , Hiblk , Ifrst , Ihalf , Item(7,1) , Mac , Nbpc , Nbpw , Nbuff , Ncpw , Nfiles ,       &
         & Nitem , Nout , Nsbuff , Psswrd(2) , Status
   LOGICAL First
   CHARACTER*25 Sfm , Uwm
   CHARACTER*31 Sim
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   REAL X1(36) , X4(3)
   COMMON /itemdt/ Nitem , Item
   COMMON /machin/ Mac , Ihalf
   COMMON /sofcom/ Nfiles , Filnam , Filsiz , Status , Psswrd , First
   COMMON /sys   / Nsbuff , X4 , Hiblk , Ifrst
   COMMON /system/ Nbuff , Nout , X1 , Nbpc , Nbpw , Ncpw
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm , Sim
   COMMON /zzzzzz/ Buf
!
! Dummy argument declarations
!
   INTEGER Ib1 , Ib2 , Ibl1 , Numb
!
! Local variable declarations
!
   INTEGER file , i , ibl , idiff , iempty , ihere1 , ird , iwrt , j , k , last , lastsz , lstsiz , max , maxnxt , maxold , min ,   &
         & n , name(2)
   INTEGER lshift , orf , rshift
   EXTERNAL lshift , orf , rshift
!
! End of declarations
!
!
!     CALLED ONCE BY EVERY RUN USING THE SOF UTILITY SUBROUTINES.
!     SHOULD BE CALLED BEFORE ANY OF THEM IS CALLED.  IF THE SOF IS
!     NOT EMPTY, SOME SECURITY CHECKS WILL BE TAKEN CARE OF, AND THE
!     SOF COMMON BLOCKS WILL BE UPDATED AND WRITTEN OUT ON THE FIRST
!     BLOCK OF EACH OF THE SOF FILES.  IF THE SOF IS EMPTY, THE DIT
!     MDI, AND ARRAY NXT WILL BE INITIALIZED AND WRITTEN OUT ON THE
!     THIRD, FOURTH, AND SECOND BLOCKS OF THE FIRST FILE OF THE SOF,
!     AND THE SOF COMMON BLOCKS WILL BE INITIALIZED AND WRITTEN OUT
!     ON THE FIRST BLOCK OF EACH OF THE SOF FILES.
!
!     THE FIRST BLOCK OF EACH OF THE SOF FILES CONTAINS THE FOLLOWING
!     INFORMATION
!       WORD                   WORD                   WORD
!      NUMBER  CONTENTS       NUMBER  CONTENTS       NUMBER  CONTENTS
!      ------  --------       ------  --------       ------  --------
!       1- 2   PASSWORD          26   DIRSIZ            32   MDIBL
!          3   FILE NUMBER       27   SUPSIZ            33   NXTTSZ
!          4   NFILES            28   AVBLKS         34-43   NXTFSZ
!       5-14   FILNAM            29   DITSIZ            44   NXTCUR
!      15-24   FILSIZ            30   DITNSB            45   NXTRST
!         25   BLKSIZ            31   DITBL             46   HIBLK
!                                                       47   IFRST
!
!     STARTING AT LOCATION 100 THE CONTENTS OF THE ITEMDT COMMON BLOCK
!     ARE STORED
!
!
   DATA ird , iwrt/1 , 2/
   DATA iempty , name/4H     , 4HSOFI , 4HNT  /
!
   IF ( Ncpw>4 ) THEN
      n = Nbpw - Nbpc*4
      DO i = 1 , 10
         Filnam(i) = lshift(rshift(Filnam(i),n),n)
      ENDDO
   ENDIF
   IF ( Nfiles<=0 ) THEN
!
!     ERROR MESSAGES.
!
      WRITE (Nout,99001) Sfm
99001 FORMAT (A25,' 6202.  THE REQUESTED NO. OF FILES IS NON POSITIVE.')
      CALL mesage(-37,0,name(1))
      RETURN
   ELSE
      IF ( Status==0 ) THEN
!
!     THE SOF IS EMPTY.  INITIALIZE THE SOF COMMON BLOCKS WHICH ARE
!     STORED IN THE ARRAY A.
!     CHECK IF THE NASTRAN BUFFER SIZE IS LARGE ENOUGH
!
         min = 100 + 7*Nitem + (Nbuff-Nsbuff)
         IF ( Nbuff<min ) GOTO 600
         last = Nbuff - 4
         Hiblk = 0
         Ifrst = 3
         DO i = 1 , last
            Buf(Ib1+i) = 0
         ENDDO
         Buf(Ib1+1) = Psswrd(1)
         Buf(Ib1+2) = Psswrd(2)
         Buf(Ib1+25) = Nsbuff
         Buf(Ib1+26) = Nitem + Ifrst - 1
         Buf(Ib1+27) = 2*(Buf(Ib1+25)-1)
         Buf(Ib1+28) = -4
         DO i = 1 , Nfiles
            Buf(Ib1+28) = Buf(Ib1+28) + Filsiz(i)
         ENDDO
         Buf(Ib1+29) = 0
         Buf(Ib1+30) = 0
         Buf(Ib1+31) = 3
         Buf(Ib1+32) = 4
         Buf(Ib1+33) = 1
         Buf(Ib1+44) = 1
         Buf(Ib1+45) = 0
         Buf(Ib1+46) = 4
         Buf(Ib1+47) = Ifrst
!
         Buf(Ib1+100) = Nitem
         k = 100
         DO i = 1 , Nitem
            DO j = 1 , 7
               Buf(Ib1+k+j) = Item(j,i)
            ENDDO
            k = k + 7
         ENDDO
!
!     INITIALIZE THE ARRAY NXT AND WRITE IT ON THE SECOND BLOCK OF THE
!     FIRST SOF FILE.
!
         DO i = 1 , last
            Buf(Ib2+i) = 0
         ENDDO
         IF ( Buf(Ib1+27)+1>Filsiz(1) ) THEN
            IF ( mod(Filsiz(1),2)==1 ) THEN
               max = (Filsiz(1)-1)/2
               Buf(Ib2+max+1) = lshift(Filsiz(1),Ihalf)
            ELSE
               max = Filsiz(1)/2
            ENDIF
            Buf(Ib2+1) = Filsiz(1)
         ELSE
            max = Buf(Ib1+25) - 1
            Buf(Ib2+max+1) = lshift(Buf(Ib1+27)+1,Ihalf)
            Buf(Ib2+1) = Buf(Ib1+27) + 1
         ENDIF
         Buf(Ib2+1) = orf(Buf(Ib2+1),lshift(5,Ihalf))
         Buf(Ib2+2) = 0
         Buf(Ib2+3) = 6
         DO i = 4 , max
            Buf(Ib2+i) = 2*i
            Buf(Ib2+i) = orf(Buf(Ib2+i),lshift(2*i-1,Ihalf))
         ENDDO
         CALL sofio(iwrt,1,Buf(Ib2-2))
         CALL sofio(iwrt,2,Buf(Ib2-2))
!
!     INITIALIZE THE DIT AND WRITE IT ON THE THIRD BLOCK OF THE FIRST
!     SOF FILE.
!
         DO i = 1 , last
            Buf(Ib2+i) = iempty
         ENDDO
         CALL sofio(iwrt,3,Buf(Ib2-2))
!
!     INITIALIZE THE MDI AND WRITE IT ON THE FOURTH BLOCK OF THE FIRST
!     SOF FILE.
!
         DO i = 1 , last
            Buf(Ib2+i) = 0
         ENDDO
         CALL sofio(iwrt,4,Buf(Ib2-2))
         Numb = 0
      ELSE
!
!     THE SOF IS NOT EMPTY.  READ THE FIRST BLOCK OF THE FIRST SOF FILE
!     AND VERIFY THE SECURITY VARIABLES.
!
         file = Filnam(1)
         CALL sofio(ird,1,Buf(Ib1-2))
         IF ( (Buf(Ib1+1)/=Psswrd(1)) .OR. (Buf(Ib1+2)/=Psswrd(2)) ) GOTO 100
         IF ( Buf(Ib1+3)/=1 ) GOTO 200
         IF ( Buf(Ib1+25)/=Nsbuff ) THEN
!
            i = (Nbuff-Nsbuff) + Buf(Ib1+25)
            WRITE (Nout,99002) Ufm , i
99002       FORMAT (A23,' 6205, SUBROUTINE SOFINT - THE BUFFER SIZE HAS BEEN',' MODIFIED.',/30X,                                    &
                   &'THE CORRECT NASTRAN PARAMETER IS BUFFSIZE = ',I6)
            GOTO 500
         ELSE
!
!     CHECK IF THE SPECIFIED NUMBER OF FILES AND THEIR SIZES IS ADEQUATE
!
            IF ( Buf(Ib1+4)>=Nfiles ) THEN
               max = Nfiles - 1
            ELSE
               max = Buf(Ib1+4) - 1
            ENDIF
            IF ( max>=1 ) THEN
               DO i = 1 , max
                  IF ( Buf(Ib1+14+i)/=Filsiz(i) ) THEN
                     file = Filnam(i)
                     GOTO 300
                  ENDIF
               ENDDO
!
!     CHECK IF ALL SOF FILES HAVE THE CORRECT PASSWORD AND SEQUENCE
!     NUMBER
!
               max = max + 1
               ibl = 1
               DO i = 2 , max
                  file = Filnam(i)
                  ibl = ibl + Filsiz(i-1)
                  CALL sofio(ird,ibl,Buf(Ib1-2))
                  IF ( (Buf(Ib1+1)/=Psswrd(1)) .OR. (Buf(Ib1+2)/=Psswrd(2)) ) GOTO 100
                  IF ( Buf(Ib1+3)/=i ) GOTO 200
               ENDDO
               CALL sofio(ird,1,Buf(Ib1-2))
               max = max - 1
            ENDIF
            IF ( Buf(Ib1+14+max+1)==Filsiz(max+1) ) THEN
               Numb = 0
            ELSE
               maxnxt = 0
               IF ( max>=1 ) THEN
                  DO i = 1 , max
                     maxnxt = maxnxt + Buf(Ib1+33+i)
                  ENDDO
               ENDIF
               lastsz = (Filsiz(max+1)-1)/Buf(Ib1+27)
               IF ( Filsiz(max+1)-1/=lastsz*Buf(Ib1+27) ) lastsz = lastsz + 1
               maxnxt = maxnxt + lastsz
               IF ( Buf(Ib1+33)>maxnxt ) GOTO 400
               maxold = maxnxt - lastsz + Buf(Ib1+33+max+1)
               IF ( Buf(Ib1+33)/=maxold ) THEN
                  Numb = 0
               ELSE
                  IF ( Buf(Ib1+14+max+1)>Filsiz(max+1) ) GOTO 400
                  lstsiz = mod(Buf(Ib1+14+max+1)-2,Buf(Ib1+27)) + 1
                  IF ( lstsiz==Buf(Ib1+27) ) THEN
                     Numb = 0
                  ELSE
!
!     THE SIZE OF THE LAST SUPERBLOCK THAT WAS USED ON FILE (MAX+1)
!     SHOULD BE INCREASED.
!
                     IF ( Filsiz(max+1)-Buf(Ib1+14+max+1)>=Buf(Ib1+27)-lstsiz ) THEN
                        Numb = Buf(Ib1+27) - lstsiz
                     ELSE
                        Numb = Filsiz(max+1) - Buf(Ib1+14+max+1)
                     ENDIF
                     Ibl1 = 0
                     IF ( max>=1 ) THEN
                        DO i = 1 , max
                           Ibl1 = Ibl1 + Filsiz(i)
                        ENDDO
                     ENDIF
                     Ibl1 = Ibl1 + Buf(Ib1+14+max+1) + 1
                  ENDIF
               ENDIF
            ENDIF
!
!     UPDATE THE VARIABLE WHICH INDICATES THE NUMBER OF FREE BLOCKS ON
!     THE SOF.
!
            IF ( Nfiles<Buf(Ib1+4) ) THEN
               idiff = Buf(Ib1+14+Nfiles) - Filsiz(Nfiles)
               min = Nfiles + 1
               last = Buf(Ib1+4)
               DO i = min , last
                  idiff = idiff + Buf(Ib1+14+i)
               ENDDO
            ELSEIF ( Nfiles==Buf(Ib1+4) ) THEN
               idiff = Buf(Ib1+14+Nfiles) - Filsiz(Nfiles)
            ELSE
               ihere1 = Buf(Ib1+4)
               idiff = Buf(Ib1+14+ihere1) - Filsiz(ihere1)
               min = Buf(Ib1+4) + 1
               DO i = min , Nfiles
                  idiff = idiff - Filsiz(i)
               ENDDO
            ENDIF
            Buf(Ib1+28) = Buf(Ib1+28) - idiff
!
!     IF NO ITEM STRUCTURE IS ON THE SOF (THE SOF WAS CREATED BEFORE
!     LEVEL 17.0) THEN USE THE LEVEL 16.0 ITEM STRUCTURE.
!
            IF ( Buf(Ib1+100)<=0 .OR. Buf(Ib1+100)>100 ) THEN
               WRITE (Nout,99003) Uwm
!
99003          FORMAT (A25,'6235, THE OLD SOF CONTAINS NO ITEM STRUCTURE ','INFORMATION.',/27X,                                     &
                     & 'THE LEVEL 16.0 ITEM STRUCTURE WILL ','BE USED.')
               Buf(Ib1+47) = 3
               Buf(Ib1+100) = 18
               k = 100
               DO i = 1 , 18
                  DO j = 1 , 7
                     Buf(Ib1+k+j) = Item(j,i)
                  ENDDO
                  k = k + 7
               ENDDO
!
!     CHECK IF THE DIRECTORY SIZE HAS BEEN CHANGED
!
            ELSEIF ( Nitem/=Buf(Ib1+100) ) THEN
               WRITE (Nout,99004) Uwm
!
99004          FORMAT (A25,' 6233, THE ITEM STRUCTURE HAS BEEN CHANGED FOR THE ','SOF.',/32X,                                       &
                      &'NEW CAPABILITIES USING THESE ITEMS MAY NOT ','BE USED WITH THIS SOF.')
            ENDIF
         ENDIF
      ENDIF
!
!     UPDATE THE COMMON BLOCKS USED BY THE SOF UTILITY SUBROUTINES.
!
      Buf(Ib1+4) = Nfiles
      DO i = 1 , Nfiles
         Buf(Ib1+4+i) = Filnam(i)
         Buf(Ib1+14+i) = Filsiz(i)
         Buf(Ib1+33+i) = (Filsiz(i)-1)/Buf(Ib1+27)
         IF ( Filsiz(i)-1/=Buf(Ib1+33+i)*Buf(Ib1+27) ) Buf(Ib1+33+i) = Buf(Ib1+33+i) + 1
      ENDDO
!
!     WRITE THE UPDATED ARRAY A ON THE FIRST BLOCK OF EACH OF THE SOF
!     FILES.
!
      ibl = 1
      DO i = 1 , Nfiles
         Buf(Ib1+3) = i
         CALL sofio(iwrt,ibl,Buf(Ib1-2))
         ibl = ibl + Filsiz(i)
      ENDDO
!
!     PRINT MESSAGE INDICATING THE STATUS OF THE CURRENT SOF FILES.
!
      WRITE (Nout,99005) Sim , Nfiles
99005 FORMAT (A31,' 6201,',I3,' FILES HAVE BEEN ALLOCATED TO THE SOF ','WHERE --')
      DO i = 1 , Nfiles
         WRITE (Nout,99006) i , Filsiz(i)
99006    FORMAT (18H     SIZE OF FILE ,I2,3H = ,I10,7H BLOCKS)
      ENDDO
      WRITE (Nout,99007) Buf(Ib1+25)
99007 FORMAT (32H     AND WHERE A BLOCK CONTAINS ,I4,6H WORDS)
      RETURN
   ENDIF
!
 100  WRITE (Nout,99008) Ufm , file
99008 FORMAT (A23,' 6206, SUBROUTINE SOFINT - WRONG PASSWORD ON SOF ','FILE ',A4,1H.)
   GOTO 500
!
 200  WRITE (Nout,99009) Ufm , file
99009 FORMAT (A23,' 6207, SUBROUTINE SOFINT - THE SOF FILE ',A4,' IS OUT OF SEQUENCE.')
   GOTO 500
!
 300  WRITE (Nout,99010) Ufm , file
99010 FORMAT (A23,' 6208, SUBROUTINE SOFINT - THE SIZE OF THE SOF FILE ',A4,' HAS BEEN MODIFIED.')
   GOTO 500
!
 400  WRITE (Nout,99011) Ufm , file
99011 FORMAT (A23,' 6209, SUBROUTINE SOFINT - THE NEW SIZE OF FILE ',A4,' IS TOO SMALL.')
 500  CALL mesage(-61,0,0)
!
 600  WRITE (Nout,99012) Ufm , min
99012 FORMAT (A23,' 6234, THE NASTRAN BUFFER SIZE IS TO SMALL FOR THE',' SOF FILE.',/30X,'MINIMUM BUFFER SIZE IS ',I10)
   GOTO 500
!
END SUBROUTINE sofint
