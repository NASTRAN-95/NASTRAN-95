
SUBROUTINE push(In,Bcd,Icol,Nchar,Flag)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Idum(35) , Iout , Isys , Nbpc , Nbpw , Ncpw , Nogo
   CHARACTER*23 Ufm
   COMMON /system/ Isys , Iout , Nogo , Idum , Nbpc , Nbpw , Ncpw
   COMMON /xmssg / Ufm
!
! Dummy argument declarations
!
   INTEGER Flag , Icol , Nchar
   INTEGER Bcd(1) , In(1)
!
! Local variable declarations
!
   INTEGER blank , cperwd , digit , i , iadd , iadd1 , ib1 , ibl , ic , icl , ii(18) , ij , isave , ish , iwrd , ix , ixtra , k ,   &
         & lcol , lwrd , m , m1 , m2 , m3 , minus , nin , numbs(10) , nx
   LOGICAL first
   INTEGER klshft , krshft , orf
   EXTERNAL orf
!
! End of declarations
!
!
!     THIS ROUTINE IS USED TO PLACE BCD CHARACTERS OR INTEGERS FROM II
!     ARRAY INTO THE BCD STRING . IF FLAG = 1 AN INTEGER IS INPUT
!
   DATA numbs/1H0 , 1H1 , 1H2 , 1H3 , 1H4 , 1H5 , 1H6 , 1H7 , 1H8 , 1H9/
   DATA cperwd/4/ , first/.TRUE./ , blank/1H / , minus/4H-   /
!
   IF ( first ) THEN
      first = .FALSE.
!
!     REMOVE BLANKS FROM NUMBERS, AND ZERO FILL
!
      ish = Ncpw - 1
      DO i = 1 , 10
         isave = krshft(numbs(i),ish)
         numbs(i) = klshft(isave,ish)
      ENDDO
      isave = krshft(minus,ish)
      minus = klshft(isave,ish)
      nx = Ncpw - cperwd
      ixtra = nx*Nbpc
      ibl = 0
      IF ( nx/=0 ) THEN
         ib1 = krshft(blank,ish)
         DO i = 1 , nx
            ibl = orf(ibl,klshft(ib1,i-1))
         ENDDO
      ENDIF
   ENDIF
!
   IF ( Nchar<=0 ) RETURN
   IF ( Nchar+Icol>128 ) THEN
!
      WRITE (Iout,99001) Ufm , Nchar , In
99001 FORMAT (A23,' 6015. TOO MANY CHARACTERS TO BE INSERTED IN A DMAP',' LINE',/6X,4H N =,I8,6X,6HWORD =,A4)
      GOTO 300
   ELSE
      nin = (Nchar-1)/cperwd + 1
      DO i = 1 , nin
         ii(i) = In(i)
      ENDDO
      IF ( Flag/=1 ) GOTO 100
!
!     INTEGER HAS BEEN INPUT - 1 WORD ONLY
!
!     FIND POWER OF 10 = NUMBER OF CHARACTERS
!
      ix = iabs(In(1))
      DO i = 1 , 8
         ix = ix/10
         IF ( ix==0 ) GOTO 50
      ENDDO
      GOTO 200
 50   ic = i
      IF ( In(1)<0 ) ic = ic + 1
      IF ( ic>Nchar ) GOTO 200
      ii(2) = blank
      ix = iabs(In(1))
      IF ( ic>cperwd ) THEN
!
!     NUMBER TAKES TWO WORDS
!
         m = ic - cperwd
         ii(2) = krshft(blank,m)
         DO i = 1 , m
            ij = ix/10
            digit = iabs(ix-10*ij) + 1
            ix = ij
            iadd = numbs(digit)
            ii(2) = orf(ii(2),krshft(iadd,m-i))
         ENDDO
!
         ic = cperwd
      ENDIF
!
!     FIRST WORD SET HERE FOR BOTH CASES
!
      ii(1) = krshft(blank,ic)
      DO i = 1 , ic
         IF ( i/=ic .OR. In(1)>=0 ) THEN
            ij = ix/10
            digit = iabs(ix-10*ij) + 1
            ix = ij
            iadd = numbs(digit)
            ii(1) = orf(ii(1),krshft(iadd,ic-i))
         ENDIF
      ENDDO
      IF ( In(1)<0 ) ii(1) = orf(ii(1),minus)
   ENDIF
!
 100  iwrd = (Icol-1)/cperwd + 1
   icl = Icol - (iwrd-1)*cperwd
   lwrd = (Icol+Nchar-2)/cperwd + 1
   lcol = Icol + Nchar - (lwrd-1)*cperwd - 1
   m1 = (icl-1)*Nbpc
   m2 = cperwd*Nbpc - m1
   m3 = m2 + (Ncpw-cperwd)*Nbpc
!
!     M1 IS THE NUMBER OF BITS FOR THE  FIRST SET OF CHARACTERS
!     M2 IS THE NUMBER OF BITS FOR THE SECOND SET OF CHARACTERS
!     M3 IS THE NUMBER OF BITS FOR THE RIGHT HALF OF THE WORD
!
!     IADD IS THE CURRENT WORKING WORD, IADD1 IS THE SPILL
!
   isave = krshft(Bcd(iwrd),m3/Nbpc)
   iadd1 = klshft(isave,m3/Nbpc)
   k = 0
   DO i = iwrd , lwrd
      k = k + 1
!
!     SPLIT INPUT WORD INTO TWO SETS
!
!     MOVE LEFT HALF TO RIGHT SIDE OF IADD AND ADD IADD1
!
      isave = krshft(ii(k),(m1+ixtra)/Nbpc)
      iadd = orf(klshft(isave,ixtra/Nbpc),iadd1)
!
!     IF THIS ISNT THE LAST WORD MOVE THE RIGHT HALF TO IADD1 AND INSERT
!
      IF ( i<lwrd ) THEN
         isave = krshft(ii(k),ixtra/Nbpc)
         iadd1 = klshft(isave,m3/Nbpc)
!
         Bcd(i) = orf(iadd,ibl)
      ENDIF
!
   ENDDO
!
!     LAST WORD PROCESSED HERE, REMOVE EXTRA CHARACTERS
!
   ish = Ncpw - lcol
   isave = krshft(iadd,ish)
   iadd = klshft(isave,ish)
   isave = klshft(Bcd(lwrd),lcol)
   Bcd(lwrd) = orf(iadd,krshft(isave,lcol))
   RETURN
 200  WRITE (Iout,99002) Ufm , In
99002 FORMAT (A23,' 6016. TOO MANY DIGITS TO BE INSERTED IN DMAP.',2X,'VALUE =',I12)
!
 300  Nogo = 1
END SUBROUTINE push
