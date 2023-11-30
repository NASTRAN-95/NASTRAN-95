
SUBROUTINE fndpar(Np2,Index)
   IMPLICIT NONE
   INTEGER Mask , Mask2 , Mask3 , Nout , Oscar(7)
   REAL Sysbuf
   CHARACTER*23 Ufm
   COMMON /oscent/ Oscar
   COMMON /sem   / Mask , Mask2 , Mask3
   COMMON /system/ Sysbuf , Nout
   COMMON /xmssg / Ufm
   INTEGER Index , Np2
   INTEGER andf
   INTEGER i , itype , k , m , name(2) , nip , nop , np , np1
   EXTERNAL andf
!
!     FNDPAR FINDS THE INDEX INTO THE  VPS FOR PARAMETER NUMBER NP
!     IN THE CURRENT OSCAR (THIS PARAMETER MUST BE VARIABLE)
!
   DATA name/4HFNDP , 4HAR  /
!
   nip = Oscar(7)
   itype = andf(Oscar(3),7)
   i = 8 + 3*nip
   IF ( itype/=2 ) THEN
      nop = Oscar(i)
      i = i + 3*nop + 1
   ENDIF
   i = i + 1
   np1 = Oscar(i)
   np = iabs(Np2)
   IF ( np>np1 ) THEN
      IF ( Np2<=0 ) THEN
!
!     PARAMETER SPORT NOT SUPPLIES
!
         Index = -1
         GOTO 99999
      ELSE
         WRITE (Nout,99001) Ufm , np
99001    FORMAT (A23,' 3123, PARAMETER NUMBER',I6,' NOT IN DMAP CALL.')
         CALL mesage(-61,0,name)
      ENDIF
   ENDIF
   np1 = np - 1
   k = i + 1
   IF ( np1/=0 ) THEN
      DO i = 1 , np1
         m = Oscar(k)
         IF ( m<0 ) THEN
!
!     VARTABLE
!
            k = k + 1
         ELSE
!
!     CONSTANT
!
            k = k + 1 + m
         ENDIF
      ENDDO
   ENDIF
!
!     K POINTS  TO WANTED OSCAR WORD
!
   IF ( Oscar(k)>=0 ) THEN
      IF ( Np2<=0 ) THEN
         Index = -1
         GOTO 99999
      ELSE
         WRITE (Nout,99002) Ufm , np
99002    FORMAT (A23,' 3124, PARAMETER NUMBER',I6,' IS NOT A VARIABLE.')
         CALL mesage(-61,0,name)
      ENDIF
   ENDIF
   Index = andf(Oscar(k),Mask3)
   RETURN
99999 RETURN
END SUBROUTINE fndpar
