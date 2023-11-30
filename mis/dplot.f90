
SUBROUTINE dplot
!
   IMPLICIT NONE
   INTEGER Bgpdt , Bufsiz , Casecc , Ecpt , Elsets , Eqexin , Gpsets , Iused , Lsil , Ngp , Ngpset , Nodef , Nout , Nsets , Oes1 ,  &
         & Pdef1 , Pdef2 , Plotx , Pltflg , Pltnum , Pltpar , S2 , Scr1 , Scr2 , Scr3 , Scr4 , Setd , Sil , Skp1(3) , X(1)
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /blank / Ngp , Lsil , Nsets , Pltflg , Pltnum , Ngpset , Nodef , Skp1 , Pltpar , Gpsets , Elsets , Casecc , Bgpdt ,       &
                 & Eqexin , Sil , Pdef1 , Pdef2 , S2 , Plotx , Setd , Ecpt , Oes1 , Scr1 , Scr2 , Scr3 , Scr4
   COMMON /plothd/ Iused
   COMMON /system/ Bufsiz , Nout
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ X
   INTEGER buf , i , i1 , i2 , inprew , name(2) , oes1l , onrgy1 , rew , tit(32)
   INTEGER korsz
!
!     NOTE THAT NSETS IS DMAP PARAMETER JUMPPLOT
!     IUSED IS USED IN PLOT AND HDPLOT
!
   DATA inprew , rew/0 , 1/ , tit/12*1H  , 4HMESS , 4HAGES , 4H FRO , 4HM TH , 4HE PL , 4HOT M , 4HODUL , 1HE , 12*1H /
   DATA name/4HDPLO , 4HT   /
!
!     FILE NAMES FOR UNDEFORMED PLOTS MAY BE
!     108  = USET (GPTLBL - SPC DEGREES OF FREEDOM)
!     109  = ECT  (ELELBL - PROPERTY IDS)
!     110  = ECPT
!          = EPT (UNDEFORMED PLOT ONLY, DMAP NUMBER 25 OR LESS)
!            EPT IS NEEDED FOR PSHELL CARDS IN ORDER TO PICK UP ANY
!            OFFSET FOR CTRIA3 AND CQUAD4 (IN COMECT)
!
   Pltpar = 101
   Gpsets = 102
   Elsets = 103
   Casecc = 104
   Bgpdt = 105
   Eqexin = 106
   Sil = 107
   Pdef1 = 108
   Pdef2 = 109
   Ecpt = 110
   Oes1 = 111
   oes1l = 112
   onrgy1 = 113
   Plotx = 201
   Scr1 = 301
   Scr2 = 302
   Scr3 = 303
   Scr4 = 304
   Nodef = 0
   IF ( Ngp<=0 .OR. Lsil<=0 ) GOTO 300
   CALL totape(2,X(1))
!
!     OUTPUT THE TITLE FOR MESSAGE FILE
!     THE LAST BUFFER IS BUFSIZ+1 FOR SUBROUTINE ELELBL
!
   buf = korsz(X) - 4*Bufsiz
   IF ( buf-4*Bufsiz<10 ) THEN
!
!     INSUFFICIENT CORE
!
      CALL mesage(-8,buf,name)
      Nsets = -1
      Pltflg = -1
      GOTO 300
   ELSEIF ( Nsets>0 ) THEN
      CALL gopen(Plotx,X(buf),rew)
!
!     COMMENTS FROM G.CHAN/UNISYS       11/90
!     NEXT 2 LINES ADD TIT HEADING TO THE 4TH LINE OF NASTRAN HEADERS
!     WHEN THE PLOTX FILE IS READ AND PRINTED BY PRTMSG MODULE.
!     THIS SHORTCUT TECHNIQUE IS NO WHERE DISCUSSED IN THE USER'S NOR
!     PROGRAMMER'S MAUNALS
!
      CALL write(Plotx,-4,1,0)
      CALL write(Plotx,tit,32,0)
!
!     READ THE SETID-S FROM -GPSETS- FILE.  SET NEGATIVE SETID-S THAT
!     HAVE NO ASSOCIATED GRIDS.  FIND FIRST DEFINED SET OR EXIT IF NONE
!
      buf = buf - Bufsiz
      CALL gopen(Gpsets,X(buf),inprew)
      CALL fread(Gpsets,X,Nsets,1)
      Setd = 0
      X(Nsets+1) = 1
!
      DO i = 1 , Nsets
         CALL read(*20,*100,Gpsets,X(Nsets+2),1,1,i1)
         IF ( X(Nsets+2)>0 ) THEN
            IF ( Setd==0 ) Setd = i
            CYCLE
         ENDIF
 20      WRITE (Nout,99001) Uwm , X(Nsets+1)
99001    FORMAT (A25,' 697, SET',I9,' NOT DEFINED.  FIRST SET DEFINED WILL BE USED.')
         X(i) = -X(i)
      ENDDO
      CALL close(Gpsets,rew)
      IF ( Setd/=0 ) GOTO 200
   ENDIF
 100  WRITE (Nout,99002) Ufm
99002 FORMAT (A23,' 698, NO SETS DEFINED FOR PLOTS')
   CALL mesage(-61,0,0)
!
!     PROCESS PLOT REQUESTS
!
 200  CALL gopen(Pltpar,X(buf),inprew)
   i1 = 1
   i2 = i1 + Nsets
   buf = buf - Bufsiz
   CALL param(X(i1),X(i2),buf-Nsets)
   CALL close(Pltpar,rew)
!
!     SET JUMPPLOT NEGATIVE IF NO FUTHER REQUESTS
!
   IF ( Pltflg>=0 .AND. Nodef==0 ) Nsets = -1
   CALL clstab(Plotx,rew)
   CALL close(Gpsets,rew)
   Pltflg = -1
 300  RETURN
END SUBROUTINE dplot