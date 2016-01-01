      program intrat
!
!***********************************************************************
!
!          Reads emissivities for all transition with n.le.ncut from
!          the input files supplied with the program and computes
!          INTensity RATios of HYDROGENIC recombination lines for
!          specified transitions at temperatures and densities in input
!          files. The parameter 'ncut' is specified in the tables and
!          is currently set to 25. Extensive facilities are provided
!          for interactive two-dimensional interpolation to specified
!          temperatures and densities.  Interpolations are made to
!          four orders (currently 2,3,4 5) to give some impression
!          of the reliability of the interpolation, and can be made in
!          either the intensity ratios (or emissivities), or in the
!          logarithms of these quantities.  The interpolates are output
!          together following the symbol "r(i)=" or "r(l)=", which
!          indicates linear or logarithmic interpolation. Experience to
!          date has shown that the logarithmic interpolations are
!          almost always more reliable, and is essential for emissivitie
!
!          Total recombination coefficients are also available and
!          can be accessed by responding to the cue concerning "alpha-to
!          or by specifying "a" from the menu after the table display.
!          Interpolation in this table is identical to that for other
!          tables; for low temperatures and high densities logarithmic
!          interpolation is necessary.
!
!          Input files are named e1a.d, e1b.d, e2a.d, e2b.d,...,
!          e8a.d, e8b.d, and contain emissivities for cases A nd B
!          of all hydrogenic ions through oxygen. (at present, only
!          'b'-files, but the remainder are being computed).
!
!          Output appears also in file intrat_zion_case.d.
!
!
!          *********************************************************
!          *                                                       *
!          *     !!!!!  PRELIMINARY VERSION 1.0  !!!!!             *
!          *                                                       *
!          *                 19 february 1993 (0.0)                *
!          *                                                       *
!          *                   15 july 1993
!          *                                                       *
!          *     please send error reports and suggestions for     *
!          *          improvements to D.G.Hummer at                *
!          *                                                       *
!          *               dgh@usm.uni-muenchen.de                 *
!          *                                                       *
!          *                 (49)-(0)89-9220-9441                  *
!          *                                                       *
!          *********************************************************
!
!
!          respond to prompts for the following input:
!               ZION  -  charge of recombining ion (<9)
!               CASE  -  A or B
!               DMIN  -  minimum vlue of electron density; data for six
!                        values will appear in output tables
!               N_upper, N_lower, N_upper_ref, N_lower_ref - upper and
!                        lower principal quantum numbers for transition
!                        of interest and of reference transition. For
!                        the emissivity of the transition, set
!                            N_upper_ref = N_lower_ref = 0
!                        note:  N_upper = -1 allows minimum density to b
!                        and setting all four N's = 0 ends run
!
!
!          after desired portion of table is displayed, respond to promp
!               - 2-d interpolation of intensity ratio or emissivity (i)
!               - 2-d logarithmic interpolation of table (l)
!               - new transition(s) (n)
!               - alpha-tot (a)
!               - set new value of minimum density displayed in table (d
!               - end run (e)
!
!          P.J.Storey and D.G.Hummer, January 1993 ; see MNRAS,xxxxx,199
!
!***********************************************************************

      implicit none
      integer, parameter :: dp = kind(1.d0)
      character*1 zion,case,ques
      character*2 name
      real(kind=dp), dimension(15) :: dens,temp,x,y
      real(kind=dp), dimension(5) :: cx,cy,ri
      real(kind=dp), dimension(15,15) :: r,a
      real(kind=dp), dimension(300,15,15) :: e
      real(kind=dp), dimension(2,15) :: f
      real(kind=dp) :: dmin, rint, rrr, xd, xp, xt, yp
      integer :: i, i0, ia, ib, id, int, ip, is, it, j, j0, jp, js, k, kp, ks, kx, ky, m0, m1, max, ncase, ncut, ndens, ne, nint, nint1, nl, nls, nof, ns, nt, ntemp, nu, nus
      integer, dimension(5) :: ni
                                     ! interpolation parameters
      max=4
      ni=(/2,3,4,5,6/)

!          identify ion and case
!
    1 write(*,5)
    5 format(//'           ****** WELCOME TO INTRAT ******'//           &
     &'  please type ZION and CASE, separated by one space'//)
      read(*,10) zion,case
   10 format(a1,1x,a1)
      if(case.eq.'a'.or.case.eq.'b') case=char(ichar(case)-32)
      if(case.eq.'A') then
           ncase=0
      else
           ncase=1
      endif
!
!          open i/o files
!
      name=zion//case
      open(unit=15,file='/usr/share/intrat/e'//name//'.d',status='old')
      open(unit=16,file='intrat'//name//'.d',status='replace')
!
!          output ion, case and range specifications
!
      write(*,20) zion,case
      write(16,20) zion,case
   20 format('  zion= ',a1,'  case= ',a1/)
      write(*,21) 'e'//name//'.d'
   21 format('  please wait - data file ',a5,' is being loaded')
!
!          read secondary file for this ion and case
!
      read(15,*) ntemp,ndens
      do ia=1,ntemp
           do ib=1,ndens
                read(15,25) dens(ib),temp(ia),ncut
   25           format(1x,e10.3,5x,e10.3,13x,i2)
                ne=ncut*(ncut-1)/2
                read(15,30) (e(j,ia,ib),j=1,ne)
   30           format((8e10.3))
           enddo
      enddo
      read(15,*) ((a(i,j),i=1,ndens),j=1,ntemp)
      write(*,31)
   31 format('  data input complete'//)
      write(*,32) (temp(i),i=1,ntemp)
   32 format(' temperatures:'/(1p7e10.3))
      write(*,33) (dens(i),i=1,ndens)
   33 format(/' densities:'/(1p7e10.3))
      write(*,34) (ni(i),i=1,max)
      write(16,34) (ni(i),i=1,max)
   34 format(/' interpolation orders=',5i3)

!
!          interpolation variables
!
      do i=1,ndens
           x(i)=log10(dens(i))
      enddo
      do  i=1,ntemp
           y(i)=sqrt(temp(i))
                               ! f is emissivity smoothing function in t
           f(1,i)=1.0
           f(2,i)=y(i)
      enddo
!
!          limit output to desired minimum density plus six higher value
!
  38  print *,"  please type minimum density to be considered"
      read(*,*,err=38) dmin
      m0=1
      do i=1,ndens
           if(dens(i).le.dmin) then
                m0=i
           endif
      enddo
      m1=m0+6
      if(m1.gt.ndens) then
           m1=ndens
      endif
!
!          direct access to alpha-tot
!
      print *, " start with line-ratios/emiss. (l) or alphas (a)?"
      read(*,"(A1)") ques
!
!     load alpha-tot table in table array
!
   42 if(ques.eq.'a'.or.ques.eq.'A') then
           r=transpose(a)
           ns=1
           goto 60
      endif
!
!          choose transition of interest (and standard transition if wan
!
   45 print *," n_upper, n_lower, n_upper_ref, n_lower_ref"
      read(*,*,err=45) nu,nl,nus,nls
                                         ! change minimum density
      if(nu.lt.0) then
           go to 38
      endif
                                         ! end program
      if(nu.eq.0.or.nl.eq.0) then
           stop
      endif
                                         ! check order of levels
      if(nu.le.nl.or.nu.gt.ncut) then
           goto 45
      endif
                                         ! check order of levels
      if(nus.lt.nls.or.nus.gt.ncut) then
           goto 45
      endif
      if((nu.gt.2.and.nl.eq.1.and.ncase.eq.1).or.(nus.gt.2.and.nls.eq.1.&
     &and.ncase.eq.1)) then
                             ! Ly beta,..
           go to 45
      endif
      write(16,52) nu,nl,nus,nls
   52 format(//' nu=',i3,' nl=',i3,' nus=',i3,' nls=',i3/)
!
!          set keys to locate transitions of interest
!
      if((nus+nls).eq.0) then
           ns=2
           ks=999
      else
           ns=1
           ks=(((ncut-nus)*(ncut+nus-1))/2)+nls
      endif
      k=(((ncut-nu)*(ncut+nu-1))/2)+nl
!
!          calculate desired intensity ratio (or emissivity if nus=nls=0
!
      if (ns.eq.1) then
           r(:,:) = e(k,:,:)/e(ks,:,:)
      else
           r(:,:) = e(k,:,:)
      endif
!
!          output table of line intensity (ratios) to screen and file
!
   60 write(*,70) (dens(i),i=m0,m1)
      write(16,70) (dens(i),i=m0,m1)
   70 format(' dens:     ',1p7e9.2)
      write(*,71)
      write(16,71)
   71 format(' temp      ')
      do i=1,ntemp
           write(*,72) temp(i),(r(i,j),j=m0,m1)
           write(16,72) temp(i),(r(i,j),j=m0,m1)
   72      format(1pe9.2,2x,7e9.2)
      enddo
!
!          interpolate in r-table
!
   78 print *," interpolate(i), interpolate log(l), new n(n), alpha-tot(a), new min density(d)"
      print *," exit(e)?"
      read(*,"(A1)",err=78) ques
      if(ques.eq.'e'.or.ques.eq.'E') then
           stop
      endif
      if(ques.eq.'a'.or.ques.eq.'A') then
           goto 42
      endif
      if(ques.eq.'d'.or.ques.eq.'D') then
           goto 38
      endif
      if(ques.eq.'n'.or.ques.eq.'N') then
           goto 45
      endif
      if(ques.eq.'i'.or.ques.eq.'I'.or.ques.eq.'l'.or.ques.eq.'L') then
           if(ques.eq.'i'.or.ques.eq.'I') then
                nt=0
           else
                nt=1
           endif
   83      print *," please type desired temperature and density"
           read(*,*,err=83) xt,xd
           if(xt.lt.temp(1).or.xt.gt.temp(ntemp).or.xd.lt.dens(1).or.xd.gt.dens(ndens)) then
                print *," requested temp/dens not in table"
                goto 78
           endif
                                   ! interpolate in log(dens)
           xp=log10(xd)
                                   ! interpolate in temp**0.5
           yp=sqrt(xt)
!
!          find interpolation box
!
           i=1
   86      if(xp.ge.x(i).and.xp.le.x(i+1)) then
                goto 88
           else
                i=i+1
                if(i.eq.ndens) then
                     stop 'dens overflow'
                endif
                goto 86
           endif
   88      i0=i
           j=1
   90      if(yp.ge.y(j).and.yp.le.y(j+1)) then
                goto 92
           else
                j=j+1
                if(j.eq.ntemp) then
                     stop 'temp overflow'
                endif
                goto 90
           endif
   92      j0=j
!
!          interpolate to orders 2,3,4,5 in both directions
!
           do int=1,max
                                         ! interpolation order
                nint=ni(int)
                nint1=nint-1
                nof=nint1/2
!
!          shift i0 to nearest box boundary in each direction if nint is
!
                                                              ! note ODD
                if(nint.eq.3.or.nint.eq.5.or.nint.eq.7) then
                     if((xp-x(i0)).gt.(x(i0+1)-xp)) then
                           is=i0+1-nof
                     else
                           is=i0-nof
                     endif
                     if((yp-y(j0)).gt.(y(j0+1)-yp)) then
                          js=j0+1-nof
                     else
                          js=j0-nof
                     endif
                else
                     is=i0-nof
                     js=j0-nof
                endif
!
!          ensure that interpolation box lies in table
!
                if(is.lt.1) then
                     is=1
                endif
                if((is+nint1).gt.ndens) then
                     is=ndens-nint1
                endif
                if(js.lt.1) then
                     js=1
                endif
                if((js+nint1).gt.ntemp) then
                     js=ntemp-nint1
                endif
!
!          nint**2-point interpolation
!
                do k=1,nint
                     i=is+k-1
                     cx(k)=1.0
                          do kp=1,nint
                               if(kp.ne.k) then
                                    ip=is+kp-1
                                    cx(k)=cx(k)*(xp-x(ip))/(x(i)-x(ip))
                               endif
                          enddo
                enddo
                do k=1,nint
                     j=js+k-1
                     cy(k)=1.0
                          do kp=1,nint
                               if(kp.ne.k) then
                                    jp=js+kp-1
                                    cy(k)=cy(k)*(yp-y(jp))/(y(j)-y(jp))
                               endif
                          enddo
                enddo
                rint=0.0
                do kx=1,nint
                     do ky=1,nint
                          if((js+ky-1).gt.ntemp.or.(is+kx-1).gt.ndens)  &
     &                         then
                               stop 'final loop error'
                          endif
                                                               ! smoothi
                          rrr=r(js+ky-1,is+kx-1)*f(ns,js+ky-1)
                          if(nt.ne.0) then
                               rrr=log(rrr)
                          endif
                          rint=rint+cx(kx)*cy(ky)*rrr
                     enddo
                enddo
                ri(int)=rint
                if(nt.ne.0) then
                     ri(int)=exp(ri(int))
                endif
                if(ns.eq.2) then
                                        ! remove smoothing function = te
                     ri(int)=ri(int)/yp
                endif
                                                 ! end nint-loop
           enddo
           write(*,94) xt,xd,ques,(ri(i),i=1,max)
           write(16,94) xt,xd,ques,(ri(i),i=1,max)
   94      format(/' Te=',1pe10.3,' Ne=',e10.3,'  r(',a1,')=',4e9.2)
           goto 78
      endif
!
!          meaningless character - try again
!
      goto 78
!
      END
