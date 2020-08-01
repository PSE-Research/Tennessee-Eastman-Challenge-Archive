!
!               Tennessee Eastman Process Control Test Problem
!
!                    James J. Downs and Ernest F. Vogel
!
!                  Process and Control Systems Engineering
!                        Tennessee Eastman Company
!                              P.O. Box 511
!                          Kingsport,TN  37662
!
!  Reference:
!    "A Plant-Wide Industrial Process Control Problem"
!    Presented at the AIChE 1990 Annual Meeting
!    Industrial Challenge Problems in Process Control,Paper #24a
!    Chicago,Illinois,November 14,1990
!
!  Subroutines:
!
!    TEFUNC - Function evaluator to be called by integrator
!    TEINIT - Initialization
!    TESUBi - Utility subroutines, i=1,2,..,8
!
!
!  The process simulation has 50 states (NN=50).  If the user wishes to
!  integrate additional states, NN must be increased accordingly in the
!  calling program.  The additional states should be appended to the end
!  of the YY vector, e.g. YY(51),...  The additional derivatives should
!  be appended to the end of the YP vector, e.g. YP(51),...  To initialize
!  the new states and to calculate derivatives for them, we suggest
!  creating new function evaluator and initialization routines as follows.
!
!          C-----------------------------------------------
!          C
!                SUBROUTINE FUNC(NN,TIME,YY,YP)
!          C
!                INTEGER NN
!                DOUBLE PRECISION TIME, YY(NN), YP(NN)
!          C
!          C  Call the function evaluator for the process
!          C
!                CALL TEFUNC(NN,TIME,YY,YP)
!          C
!          C  Calculate derivatives for additional states
!          C
!                YP(51) = ....
!                YP(52) = ....
!                   .
!                   .
!                   .
!                YP(NN) = ....
!          C
!                RETURN
!                END
!          C
!          C-----------------------------------------------
!          C
!                SUBROUTINE INIT(NN,TIME,YY,YP)
!          C
!                INTEGER NN
!                DOUBLE PRECISION TIME, YY(NN), YP(NN)
!          C
!          C  Call the initialization for the process
!          C
!                CALL TEINIT(NN,TIME,YY,YP)
!          C
!          C  Initialize additional states
!          C
!                YY(51) = ....
!                YY(52) = ....
!                   .
!                   .
!                   .
!                YY(NN) = ....
!          C
!                RETURN
!                END
!          C
!          C-----------------------------------------------
!
!  Differences between the code and its description in the paper:
!
!  1.  Subroutine TEINIT has TIME in the argument list.  TEINIT sets TIME
!      to zero.
!
!  2.  There are 8 utility subroutines (TESUBi) rather than 5.
!
!  3.  Process disturbances 14 through 20 do NOT need to be used in
!      conjunction with another disturbance as stated in the paper.  All
!      disturbances can be used alone or in any combination.
!
!
!  Manipulated Variables
!
!    XMV(1)     A Feed Flow (stream 1)
!    XMV(2)     D Feed Flow (stream 2)
!    XMV(3)     E Feed Flow (stream 3)
!    XMV(4)     A and C Feed Flow (stream 4)
!    XMV(5)     Compressor Recycle Valve
!    XMV(6)     Purge Valve (stream 9)
!    XMV(7)     Separator Pot Liquid Flow (stream 10)
!    XMV(8)     Stripper Liquid Product Flow (stream 11)
!    XMV(9)     Stripper Steam Valve
!    XMV(10)    Reactor Cooling Water Flow
!    XMV(11)    Condenser Cooling Water Flow
!    XMV(12)    Agitator Speed
!
!  Continuous Process Measurements
!
!    XMEAS(1)   A Feed  (stream 1)                    kscmh
!    XMEAS(2)   D Feed  (stream 2)                    kg/hr
!    XMEAS(3)   E Feed  (stream 3)                    kg/hr
!    XMEAS(4)   A and C Feed  (stream 4)              kscmh
!    XMEAS(5)   Recycle Flow  (stream 8)              kscmh
!    XMEAS(6)   Reactor Feed Rate  (stream 6)         kscmh
!    XMEAS(7)   Reactor Pressure                      kPa gauge
!    XMEAS(8)   Reactor Level                         %
!    XMEAS(9)   Reactor Temperature                   Deg C
!    XMEAS(10)  Purge Rate (stream 9)                 kscmh
!    XMEAS(11)  Product Sep Temp                      Deg C
!    XMEAS(12)  Product Sep Level                     %
!    XMEAS(13)  Prod Sep Pressure                     kPa gauge
!    XMEAS(14)  Prod Sep Underflow (stream 10)        m3/hr
!    XMEAS(15)  Stripper Level                        %
!    XMEAS(16)  Stripper Pressure                     kPa gauge
!    XMEAS(17)  Stripper Underflow (stream 11)        m3/hr
!    XMEAS(18)  Stripper Temperature                  Deg C
!    XMEAS(19)  Stripper Steam Flow                   kg/hr
!    XMEAS(20)  Compressor Work                       kW
!    XMEAS(21)  Reactor Cooling Water Outlet Temp     Deg C
!    XMEAS(22)  Separator Cooling Water Outlet Temp   Deg C
!
!  Sampled Process Measurements
!
!    Reactor Feed Analysis (Stream 6)
!        Sampling Frequency = 0.1 hr
!        Dead Time = 0.1 hr
!        Mole %
!    XMEAS(23)   Component A
!    XMEAS(24)   Component B
!    XMEAS(25)   Component C
!    XMEAS(26)   Component D
!    XMEAS(27)   Component E
!    XMEAS(28)   Component F
!
!    Purge Gas Analysis (Stream 9)
!        Sampling Frequency = 0.1 hr
!        Dead Time = 0.1 hr
!        Mole %
!    XMEAS(29)   Component A
!    XMEAS(30)   Component B
!    XMEAS(31)   Component C
!    XMEAS(32)   Component D
!    XMEAS(33)   Component E
!    XMEAS(34)   Component F
!    XMEAS(35)   Component G
!    XMEAS(36)   Component H
!
!    Product Analysis (Stream 11)
!        Sampling Frequency = 0.25 hr
!        Dead Time = 0.25 hr
!        Mole %
!    XMEAS(37)   Component D
!    XMEAS(38)   Component E
!    XMEAS(39)   Component F
!    XMEAS(40)   Component G
!    XMEAS(41)   Component H
!
!  Process Disturbances
!
!    IDV(1)   A/C Feed Ratio, B Composition Constant (Stream 4)          Step
!    IDV(2)   B Composition, A/C Ratio Constant (Stream 4)               Step
!    IDV(3)   D Feed Temperature (Stream 2)                              Step
!    IDV(4)   Reactor Cooling Water Inlet Temperature                    Step
!    IDV(5)   Condenser Cooling Water Inlet Temperature                  Step
!    IDV(6)   A Feed Loss (Stream 1)                                     Step
!    IDV(7)   C Header Pressure Loss - Reduced Availability (Stream 4)   Step
!    IDV(8)   A, B, C Feed Composition (Stream 4)            Random Variation
!    IDV(9)   D Feed Temperature (Stream 2)                  Random Variation
!    IDV(10)  C Feed Temperature (Stream 4)                  Random Variation
!    IDV(11)  Reactor Cooling Water Inlet Temperature        Random Variation
!    IDV(12)  Condenser Cooling Water Inlet Temperature      Random Variation
!    IDV(13)  Reaction Kinetics                                    Slow Drift
!    IDV(14)  Reactor Cooling Water Valve                            Sticking
!    IDV(15)  Condenser Cooling Water Valve                          Sticking
!    IDV(16)  Unknown
!    IDV(17)  Unknown
!    IDV(18)  Unknown
!    IDV(19)  Unknown
!    IDV(20)  Unknown
!
!
!=============================================================================
!

Subroutine tefunc(nn, time, yy, yp)
!
!       Function Evaluator
!
!         Inputs:
!
!           NN   = Number of differential equations
!           Time = Current time(hrs)
!           YY   = Current state values
!
!         Outputs:
!
!           YP   = Current derivative values
!
    Double Precision yy(nn), yp(nn)
!
!  MEASUREMENT AND VALVE COMMON BLOCK
!
    Double Precision xmeas, xmv
    Integer isd
    Common /pv/xmeas(41), xmv(12), isd
!
!   DISTURBANCE VECTOR COMMON BLOCK
!
    Integer idv
    Common /dvec/idv(20)
    Double Precision uclr, ucvr, utlr, utvr, xlr, xvr, etr, esr, tcr, tkr, dlr, vlr, vvr, vtr, ptr, ppr, crxr, rr, rh, fwr, twr, qur, hwr, uar, ucls, ucvs, utls, utvs, xls, xvs, ets, ess, tcs, tks, dls, vls, vvs, vts, pts, pps, fws, tws, qus, hws, uclc, utlc, xlc, etc, esc, tcc, dlc, vlc, vtc, quc, ucvv, utvv, xvv, etv, esv, tcv, tkv, vtv, ptv, vcv, vrng, vtau, ftm, fcm, xst, xmws, hst, tst, sfr, cpflmx, cpprmx, cpdh, tcwr, tcws, htr, agsp, xdel, xns, tgas, tprod, vst
    Integer ivst
    Common /teproc/uclr(8), ucvr(8), utlr, utvr, xlr(8), xvr(8), etr, esr, tcr, tkr, dlr, vlr, vvr, vtr, ptr, ppr(8), crxr(8), rr(4), rh, fwr, twr, qur, hwr, uar, ucls(8), ucvs(8), utls, utvs, xls(8), xvs(8), ets, ess, tcs, tks, dls, vls, vvs, vts, pts, pps(8), fws, tws, qus, hws, uclc(8), utlc, xlc(8), etc, esc, tcc, dlc, vlc, vtc, quc, ucvv(8), utvv, xvv(8), etv, esv, tcv, tkv, vtv, ptv, vcv(12), vrng(12), vtau(12), ftm(13), fcm(8, 13), xst(8, 13), xmws(13), hst(13), tst(13), sfr(8), cpflmx, cpprmx, cpdh, tcwr, tcws, htr(3), agsp, xdel(41), xns(41), tgas, tprod, vst(12), ivst(12)
    Integer idvwlk
    Double Precision adist, bdist, cdist, ddist, tlast, tnext, hspan, hzero, sspan, szero, spspan
    Common /wlk/adist(12), bdist(12), cdist(12), ddist(12), tlast(12), tnext(12), hspan(12), hzero(12), sspan(12), szero(12), spspan(12), idvwlk(12)
    Double Precision avp, bvp, cvp, ah, bh, ch, ag, bg, cg, av, ad, bd, cd, xmw
    Common /const/avp(8), bvp(8), cvp(8), ah(8), bh(8), ch(8), ag(8), bg(8), cg(8), av(8), ad(8), bd(8), cd(8), xmw(8)
    Integer nn, i
    Double Precision rg, vpr, fin(8), time, flms, dlp, pr, flcoef, uas, uac, vovrl, uarlev, vpos(12), xmns, xcmp(41), tmpfac, r1f, r2f, hwlk, swlk, spwlk, tesub7, tesub8
    Do i = 1, 20
        If (idv(i)>0) Then
            idv(i) = 1
        Else
            idv(i) = 0
        End If
    End Do
    idvwlk(1) = idv(8)
    idvwlk(2) = idv(8)
    idvwlk(3) = idv(9)
    idvwlk(4) = idv(10)
    idvwlk(5) = idv(11)
    idvwlk(6) = idv(12)
    idvwlk(7) = idv(13)
    idvwlk(8) = idv(13)
    idvwlk(9) = idv(16)
    idvwlk(10) = idv(17)
    idvwlk(11) = idv(18)
    idvwlk(12) = idv(20)
    Do i = 1, 9
        If (time>=tnext(i)) Then
            hwlk = tnext(i) - tlast(i)
            swlk = adist(i) + hwlk*(bdist(i)+hwlk*(cdist(i)+hwlk*ddist(i)))
            spwlk = bdist(i) + hwlk*(2.D0*cdist(i)+3.D0*hwlk*ddist(i))
            tlast(i) = tnext(i)
            Call tesub5(swlk, spwlk, adist(i), bdist(i), cdist(i), ddist(i), tlast(i), tnext(i), hspan(i), hzero(i), sspan(i), szero(i), spspan(i), idvwlk(i))
        End If
    End Do
    Do i = 10, 12
        If (time>=tnext(i)) Then
            hwlk = tnext(i) - tlast(i)
            swlk = adist(i) + hwlk*(bdist(i)+hwlk*(cdist(i)+hwlk*ddist(i)))
            spwlk = bdist(i) + hwlk*(2.D0*cdist(i)+3.D0*hwlk*ddist(i))
            tlast(i) = tnext(i)
            If (swlk>0.1D0) Then
                adist(i) = swlk
                bdist(i) = spwlk
                cdist(i) = -(3.D0*swlk+0.2D0*spwlk)/0.01D0
                ddist(i) = (2.D0*swlk+0.1D0*spwlk)/0.001D0
                tnext(i) = tlast(i) + 0.1D0
            Else
                isd = -1
                hwlk = hspan(i)*tesub7(isd) + hzero(i)
                adist(i) = 0.D0
                bdist(i) = 0.D0
                cdist(i) = dble(idvwlk(i))/hwlk**2
                ddist(i) = 0.D0
                tnext(i) = tlast(i) + hwlk
            End If
        End If
    End Do
    If (time==0.D0) Then
        Do i = 1, 12
            adist(i) = szero(i)
            bdist(i) = 0.D0
            cdist(i) = 0.D0
            ddist(i) = 0.D0
            tlast(i) = 0.0D0
            tnext(i) = 0.1D0
        End Do
    End If
    xst(1, 4) = tesub8(1, time) - idv(1)*0.03D0 - idv(2)*2.43719D-3
    xst(2, 4) = tesub8(2, time) + idv(2)*0.005D0
    xst(3, 4) = 1.D0 - xst(1, 4) - xst(2, 4)
    tst(1) = tesub8(3, time) + idv(3)*5.D0
    tst(4) = tesub8(4, time)
    tcwr = tesub8(5, time) + idv(4)*5.D0
    tcws = tesub8(6, time) + idv(5)*5.D0
    r1f = tesub8(7, time)
    r2f = tesub8(8, time)
    Do i = 1, 3
        ucvr(i) = yy(i)
        ucvs(i) = yy(i+9)
        uclr(i) = 0.0
        ucls(i) = 0.0
    End Do
    Do i = 4, 8
        uclr(i) = yy(i)
        ucls(i) = yy(i+9)
    End Do
    Do i = 1, 8
        uclc(i) = yy(i+18)
        ucvv(i) = yy(i+27)
    End Do
    etr = yy(9)
    ets = yy(18)
    etc = yy(27)
    etv = yy(36)
    twr = yy(37)
    tws = yy(38)
    Do i = 1, 12
        vpos(i) = yy(i+38)
    End Do
    utlr = 0.0
    utls = 0.0
    utlc = 0.0
    utvv = 0.0
    Do i = 1, 8
        utlr = utlr + uclr(i)
        utls = utls + ucls(i)
        utlc = utlc + uclc(i)
        utvv = utvv + ucvv(i)
    End Do
    Do i = 1, 8
        xlr(i) = uclr(i)/utlr
        xls(i) = ucls(i)/utls
        xlc(i) = uclc(i)/utlc
        xvv(i) = ucvv(i)/utvv
    End Do
    esr = etr/utlr
    ess = ets/utls
    esc = etc/utlc
    esv = etv/utvv
    Call tesub2(xlr, tcr, esr, 0)
    tkr = tcr + 273.15
    Call tesub2(xls, tcs, ess, 0)
    tks = tcs + 273.15
    Call tesub2(xlc, tcc, esc, 0)
    Call tesub2(xvv, tcv, esv, 2)
    tkv = tcv + 273.15
    Call tesub4(xlr, tcr, dlr)
    Call tesub4(xls, tcs, dls)
    Call tesub4(xlc, tcc, dlc)
    vlr = utlr/dlr
    vls = utls/dls
    vlc = utlc/dlc
    vvr = vtr - vlr
    vvs = vts - vls
    rg = 998.9
    ptr = 0.0
    pts = 0.0
    Do i = 1, 3
        ppr(i) = ucvr(i)*rg*tkr/vvr
        ptr = ptr + ppr(i)
        pps(i) = ucvs(i)*rg*tks/vvs
        pts = pts + pps(i)
    End Do
    Do i = 4, 8
        vpr = dexp(avp(i)+bvp(i)/(tcr+cvp(i)))
        ppr(i) = vpr*xlr(i)
        ptr = ptr + ppr(i)
        vpr = dexp(avp(i)+bvp(i)/(tcs+cvp(i)))
        pps(i) = vpr*xls(i)
        pts = pts + pps(i)
    End Do
    ptv = utvv*rg*tkv/vtv
    Do i = 1, 8
        xvr(i) = ppr(i)/ptr
        xvs(i) = pps(i)/pts
    End Do
    utvr = ptr*vvr/rg/tkr
    utvs = pts*vvs/rg/tks
    Do i = 4, 8
        ucvr(i) = utvr*xvr(i)
        ucvs(i) = utvs*xvs(i)
    End Do
    rr(1) = dexp(31.5859536-40000.0/1.987/tkr)*r1f
    rr(2) = dexp(3.00094014-20000.0/1.987/tkr)*r2f
    rr(3) = dexp(53.4060443-60000.0/1.987/tkr)
    rr(4) = rr(3)*0.767488334D0
    If (ppr(1)>0.0 .And. ppr(3)>0.0) Then
        r1f = ppr(1)**1.1544
        r2f = ppr(3)**0.3735
        rr(1) = rr(1)*r1f*r2f*ppr(4)
        rr(2) = rr(2)*r1f*r2f*ppr(5)
    Else
        rr(1) = 0.0
        rr(2) = 0.0
    End If
    rr(3) = rr(3)*ppr(1)*ppr(5)
    rr(4) = rr(4)*ppr(1)*ppr(4)
    Do i = 1, 4
        rr(i) = rr(i)*vvr
    End Do
    crxr(1) = -rr(1) - rr(2) - rr(3)
    crxr(3) = -rr(1) - rr(2)
    crxr(4) = -rr(1) - 1.5D0*rr(4)
    crxr(5) = -rr(2) - rr(3)
    crxr(6) = rr(3) + rr(4)
    crxr(7) = rr(1)
    crxr(8) = rr(2)
    rh = rr(1)*htr(1) + rr(2)*htr(2)
    xmws(1) = 0.0
    xmws(2) = 0.0
    xmws(6) = 0.0
    xmws(8) = 0.0
    xmws(9) = 0.0
    xmws(10) = 0.0
    Do i = 1, 8
        xst(i, 6) = xvv(i)
        xst(i, 8) = xvr(i)
        xst(i, 9) = xvs(i)
        xst(i, 10) = xvs(i)
        xst(i, 11) = xls(i)
        xst(i, 13) = xlc(i)
        xmws(1) = xmws(1) + xst(i, 1)*xmw(i)
        xmws(2) = xmws(2) + xst(i, 2)*xmw(i)
        xmws(6) = xmws(6) + xst(i, 6)*xmw(i)
        xmws(8) = xmws(8) + xst(i, 8)*xmw(i)
        xmws(9) = xmws(9) + xst(i, 9)*xmw(i)
        xmws(10) = xmws(10) + xst(i, 10)*xmw(i)
    End Do
    tst(6) = tcv
    tst(8) = tcr
    tst(9) = tcs
    tst(10) = tcs
    tst(11) = tcs
    tst(13) = tcc
    Call tesub1(xst(1,1), tst(1), hst(1), 1)
    Call tesub1(xst(1,2), tst(2), hst(2), 1)
    Call tesub1(xst(1,3), tst(3), hst(3), 1)
    Call tesub1(xst(1,4), tst(4), hst(4), 1)
    Call tesub1(xst(1,6), tst(6), hst(6), 1)
    Call tesub1(xst(1,8), tst(8), hst(8), 1)
    Call tesub1(xst(1,9), tst(9), hst(9), 1)
    hst(10) = hst(9)
    Call tesub1(xst(1,11), tst(11), hst(11), 0)
    Call tesub1(xst(1,13), tst(13), hst(13), 0)
    ftm(1) = vpos(1)*vrng(1)/100.0
    ftm(2) = vpos(2)*vrng(2)/100.0
    ftm(3) = vpos(3)*(1.D0-idv(6))*vrng(3)/100.0
    ftm(4) = vpos(4)*(1.D0-idv(7)*0.2D0)*vrng(4)/100.0 + 1.D-10
    ftm(11) = vpos(7)*vrng(7)/100.0
    ftm(13) = vpos(8)*vrng(8)/100.0
    uac = vpos(9)*vrng(9)*(1.D0+tesub8(9,time))/100.0
    fwr = vpos(10)*vrng(10)/100.0
    fws = vpos(11)*vrng(11)/100.0
    agsp = (vpos(12)+150.0)/100.0
    dlp = ptv - ptr
    If (dlp<0.0) dlp = 0.0
    flms = 1937.6D0*dsqrt(dlp)
    ftm(6) = flms/xmws(6)
    dlp = ptr - pts
    If (dlp<0.0) dlp = 0.0
    flms = 4574.21D0*dsqrt(dlp)*(1.D0-0.25D0*tesub8(12,time))
    ftm(8) = flms/xmws(8)
    dlp = pts - 760.0
    If (dlp<0.0) dlp = 0.0
    flms = vpos(6)*0.151169D0*dsqrt(dlp)
    ftm(10) = flms/xmws(10)
    pr = ptv/pts
    If (pr<1.0) pr = 1.0
    If (pr>cpprmx) pr = cpprmx
    flcoef = cpflmx/1.197D0
    flms = cpflmx + flcoef*(1.0-pr**3)
    cpdh = flms*(tcs+273.15D0)*1.8D-6*1.9872D0*(ptv-pts)/(xmws(9)*pts)
    dlp = ptv - pts
    If (dlp<0.0) dlp = 0.0
    flms = flms - vpos(5)*53.349D0*dsqrt(dlp)
    If (flms<1.D-3) flms = 1.D-3
    ftm(9) = flms/xmws(9)
    hst(9) = hst(9) + cpdh/ftm(9)
    Do i = 1, 8
        fcm(i, 1) = xst(i, 1)*ftm(1)
        fcm(i, 2) = xst(i, 2)*ftm(2)
        fcm(i, 3) = xst(i, 3)*ftm(3)
        fcm(i, 4) = xst(i, 4)*ftm(4)
        fcm(i, 6) = xst(i, 6)*ftm(6)
        fcm(i, 8) = xst(i, 8)*ftm(8)
        fcm(i, 9) = xst(i, 9)*ftm(9)
        fcm(i, 10) = xst(i, 10)*ftm(10)
        fcm(i, 11) = xst(i, 11)*ftm(11)
        fcm(i, 13) = xst(i, 13)*ftm(13)
    End Do
    If (ftm(11)>0.1) Then
        If (tcc>170.) Then
            tmpfac = tcc - 120.262
        Else If (tcc<5.292) Then
            tmpfac = 0.1
        Else
            tmpfac = 363.744/(177.-tcc) - 2.22579488
        End If
        vovrl = ftm(4)/ftm(11)*tmpfac
        sfr(4) = 8.5010*vovrl/(1.0+8.5010*vovrl)
        sfr(5) = 11.402*vovrl/(1.0+11.402*vovrl)
        sfr(6) = 11.795*vovrl/(1.0+11.795*vovrl)
        sfr(7) = 0.0480*vovrl/(1.0+0.0480*vovrl)
        sfr(8) = 0.0242*vovrl/(1.0+0.0242*vovrl)
    Else
        sfr(4) = 0.9999
        sfr(5) = 0.999
        sfr(6) = 0.999
        sfr(7) = 0.99
        sfr(8) = 0.98
    End If
    Do i = 1, 8
        fin(i) = 0.0
        fin(i) = fin(i) + fcm(i, 4)
        fin(i) = fin(i) + fcm(i, 11)
    End Do
    ftm(5) = 0.0
    ftm(12) = 0.0
    Do i = 1, 8
        fcm(i, 5) = sfr(i)*fin(i)
        fcm(i, 12) = fin(i) - fcm(i, 5)
        ftm(5) = ftm(5) + fcm(i, 5)
        ftm(12) = ftm(12) + fcm(i, 12)
    End Do
    Do i = 1, 8
        xst(i, 5) = fcm(i, 5)/ftm(5)
        xst(i, 12) = fcm(i, 12)/ftm(12)
    End Do
    tst(5) = tcc
    tst(12) = tcc
    Call tesub1(xst(1,5), tst(5), hst(5), 1)
    Call tesub1(xst(1,12), tst(12), hst(12), 0)
    ftm(7) = ftm(6)
    hst(7) = hst(6)
    tst(7) = tst(6)
    Do i = 1, 8
        xst(i, 7) = xst(i, 6)
        fcm(i, 7) = fcm(i, 6)
    End Do
    If (vlr/7.8>50.0) Then
        uarlev = 1.0
    Else If (vlr/7.8<10.0) Then
        uarlev = 0.0
    Else
        uarlev = 0.025*vlr/7.8 - 0.25
    End If
    uar = uarlev*(-0.5*agsp**2+2.75*agsp-2.5)*855490.D-6
    qur = uar*(twr-tcr)*(1.D0-0.35D0*tesub8(10,time))
    uas = 0.404655*(1.0-1.0/(1.0+(ftm(8)/3528.73)**4))
    qus = uas*(tws-tst(8))*(1.D0-0.25D0*tesub8(11,time))
    quc = 0.D0
    If (tcc<100.) quc = uac*(100.0-tcc)
    xmeas(1) = ftm(3)*0.359/35.3145
    xmeas(2) = ftm(1)*xmws(1)*0.454
    xmeas(3) = ftm(2)*xmws(2)*0.454
    xmeas(4) = ftm(4)*0.359/35.3145
    xmeas(5) = ftm(9)*0.359/35.3145
    xmeas(6) = ftm(6)*0.359/35.3145
    xmeas(7) = (ptr-760.0)/760.0*101.325
    xmeas(8) = (vlr-84.6)/666.7*100.0
    xmeas(9) = tcr
    xmeas(10) = ftm(10)*0.359/35.3145
    xmeas(11) = tcs
    xmeas(12) = (vls-27.5)/290.0*100.0
    xmeas(13) = (pts-760.0)/760.0*101.325
    xmeas(14) = ftm(11)/dls/35.3145
    xmeas(15) = (vlc-78.25)/vtc*100.0
    xmeas(16) = (ptv-760.0)/760.0*101.325
    xmeas(17) = ftm(13)/dlc/35.3145
    xmeas(18) = tcc
    xmeas(19) = quc*1.04D3*0.454
    xmeas(20) = cpdh*0.0003927D6
    xmeas(20) = cpdh*0.29307D3
    xmeas(21) = twr
    xmeas(22) = tws
    isd = 0
    If (xmeas(7)>3000.0) isd = 1
    If (vlr/35.3145>24.0) isd = 1
    If (vlr/35.3145<2.0) isd = 1
    If (xmeas(9)>175.0) isd = 1
    If (vls/35.3145>12.0) isd = 1
    If (vls/35.3145<1.0) isd = 1
    If (vlc/35.3145>8.0) isd = 1
    If (vlc/35.3145<1.0) isd = 1
    If (time>0.0 .And. isd==0) Then
        Do i = 1, 22
            Call tesub6(xns(i), xmns)
            xmeas(i) = xmeas(i) + xmns
        End Do
    End If
    xcmp(23) = xst(1, 7)*100.0
    xcmp(24) = xst(2, 7)*100.0
    xcmp(25) = xst(3, 7)*100.0
    xcmp(26) = xst(4, 7)*100.0
    xcmp(27) = xst(5, 7)*100.0
    xcmp(28) = xst(6, 7)*100.0
    xcmp(29) = xst(1, 10)*100.0
    xcmp(30) = xst(2, 10)*100.0
    xcmp(31) = xst(3, 10)*100.0
    xcmp(32) = xst(4, 10)*100.0
    xcmp(33) = xst(5, 10)*100.0
    xcmp(34) = xst(6, 10)*100.0
    xcmp(35) = xst(7, 10)*100.0
    xcmp(36) = xst(8, 10)*100.0
    xcmp(37) = xst(4, 13)*100.0
    xcmp(38) = xst(5, 13)*100.0
    xcmp(39) = xst(6, 13)*100.0
    xcmp(40) = xst(7, 13)*100.0
    xcmp(41) = xst(8, 13)*100.0
    If (time==0.D0) Then
        Do i = 23, 41
            xdel(i) = xcmp(i)
            xmeas(i) = xcmp(i)
        End Do
        tgas = 0.1
        tprod = 0.25
    End If
    If (time>=tgas) Then
        Do i = 23, 36
            xmeas(i) = xdel(i)
            Call tesub6(xns(i), xmns)
            xmeas(i) = xmeas(i) + xmns
            xdel(i) = xcmp(i)
        End Do
        tgas = tgas + 0.1
    End If
    If (time>=tprod) Then
        Do i = 37, 41
            xmeas(i) = xdel(i)
            Call tesub6(xns(i), xmns)
            xmeas(i) = xmeas(i) + xmns
            xdel(i) = xcmp(i)
        End Do
        tprod = tprod + 0.25
    End If
    Do i = 1, 8
        yp(i) = fcm(i, 7) - fcm(i, 8) + crxr(i)
        yp(i+9) = fcm(i, 8) - fcm(i, 9) - fcm(i, 10) - fcm(i, 11)
        yp(i+18) = fcm(i, 12) - fcm(i, 13)
        yp(i+27) = fcm(i, 1) + fcm(i, 2) + fcm(i, 3) + fcm(i, 5) + fcm(i, 9) - fcm(i, 6)
    End Do
    yp(9) = hst(7)*ftm(7) - hst(8)*ftm(8) + rh + qur

!		Here is the "correct" version of the separator energy balance:

!	YP(18)=HST(8)*FTM(8)-
!    .(HST(9)*FTM(9)-cpdh)-
!    .HST(10)*FTM(10)-
!    .HST(11)*FTM(11)+
!    .QUS

!		Here is the original version

    yp(18) = hst(8)*ftm(8) - hst(9)*ftm(9) - hst(10)*ftm(10) - hst(11)*ftm(11) + qus

    yp(27) = hst(4)*ftm(4) + hst(11)*ftm(11) - hst(5)*ftm(5) - hst(13)*ftm(13) + quc
    yp(36) = hst(1)*ftm(1) + hst(2)*ftm(2) + hst(3)*ftm(3) + hst(5)*ftm(5) + hst(9)*ftm(9) - hst(6)*ftm(6)
    yp(37) = (fwr*500.53*(tcwr-twr)-qur*1.D6/1.8)/hwr
    yp(38) = (fws*500.53*(tcws-tws)-qus*1.D6/1.8)/hws
    ivst(10) = idv(14)
    ivst(11) = idv(15)
    ivst(5) = idv(19)
    ivst(7) = idv(19)
    ivst(8) = idv(19)
    ivst(9) = idv(19)
    Do i = 1, 12
        If (time==0.D0 .Or. dabs(vcv(i)-xmv(i))>vst(i)*ivst(i)) vcv(i) = xmv(i)
        If (vcv(i)<0.0) vcv(i) = 0.0
        If (vcv(i)>100.0) vcv(i) = 100.0
        yp(i+38) = (vcv(i)-vpos(i))/vtau(i)
    End Do
    If (time>0.0 .And. isd/=0) Then
        Do i = 1, nn
            yp(i) = 0.0
        End Do
    End If
    Return
End Subroutine tefunc
!     END SUBROUTINE TEFUNC
!
!=============================================================================
!
Subroutine teinit(nn, time, yy, yp)
!
!       Initialization
!
!         Inputs:
!
!           NN   = Number of differential equations
!
!         Outputs:
!
!           Time = Current time(hrs)
!           YY   = Current state values
!           YP   = Current derivative values
!
    Double Precision xmeas, xmv
    Common /pv/xmeas(41), xmv(12)
    Integer idv
    Common /dvec/idv(20)
    Double Precision g
    Common /randsd/g
    Double Precision uclr, ucvr, utlr, utvr, xlr, xvr, etr, esr, tcr, tkr, dlr, vlr, vvr, vtr, ptr, ppr, crxr, rr, rh, fwr, twr, qur, hwr, uar, ucls, ucvs, utls, utvs, xls, xvs, ets, ess, tcs, tks, dls, vls, vvs, vts, pts, pps, fws, tws, qus, hws, uclc, utlc, xlc, etc, esc, tcc, dlc, vlc, vtc, quc, ucvv, utvv, xvv, etv, esv, tcv, tkv, vtv, ptv, vcv, vrng, vtau, ftm, fcm, xst, xmws, hst, tst, sfr, cpflmx, cpprmx, cpdh, tcwr, tcws, htr, agsp, xdel, xns, tgas, tprod, vst
    Integer ivst
    Common /teproc/uclr(8), ucvr(8), utlr, utvr, xlr(8), xvr(8), etr, esr, tcr, tkr, dlr, vlr, vvr, vtr, ptr, ppr(8), crxr(8), rr(4), rh, fwr, twr, qur, hwr, uar, ucls(8), ucvs(8), utls, utvs, xls(8), xvs(8), ets, ess, tcs, tks, dls, vls, vvs, vts, pts, pps(8), fws, tws, qus, hws, uclc(8), utlc, xlc(8), etc, esc, tcc, dlc, vlc, vtc, quc, ucvv(8), utvv, xvv(8), etv, esv, tcv, tkv, vtv, ptv, vcv(12), vrng(12), vtau(12), ftm(13), fcm(8, 13), xst(8, 13), xmws(13), hst(13), tst(13), sfr(8), cpflmx, cpprmx, cpdh, tcwr, tcws, htr(3), agsp, xdel(41), xns(41), tgas, tprod, vst(12), ivst(12)
    Integer idvwlk
    Double Precision adist, bdist, cdist, ddist, tlast, tnext, hspan, hzero, sspan, szero, spspan
    Common /wlk/adist(12), bdist(12), cdist(12), ddist(12), tlast(12), tnext(12), hspan(12), hzero(12), sspan(12), szero(12), spspan(12), idvwlk(12)
    Double Precision avp, bvp, cvp, ah, bh, ch, ag, bg, cg, av, ad, bd, cd, xmw
    Common /const/avp(8), bvp(8), cvp(8), ah(8), bh(8), ch(8), ag(8), bg(8), cg(8), av(8), ad(8), bd(8), cd(8), xmw(8)
    Integer i, nn
    Double Precision yy(nn), yp(nn), time
    xmw(1) = 2.0
    xmw(2) = 25.4
    xmw(3) = 28.0
    xmw(4) = 32.0
    xmw(5) = 46.0
    xmw(6) = 48.0
    xmw(7) = 62.0
    xmw(8) = 76.0
    avp(1) = 0.0
    avp(2) = 0.0
    avp(3) = 0.0
    avp(4) = 15.92
    avp(5) = 16.35
    avp(6) = 16.35
    avp(7) = 16.43
    avp(8) = 17.21
    bvp(1) = 0.0
    bvp(2) = 0.0
    bvp(3) = 0.0
    bvp(4) = -1444.0
    bvp(5) = -2114.0
    bvp(6) = -2114.0
    bvp(7) = -2748.0
    bvp(8) = -3318.0
    cvp(1) = 0.0
    cvp(2) = 0.0
    cvp(3) = 0.0
    cvp(4) = 259.0
    cvp(5) = 265.5
    cvp(6) = 265.5
    cvp(7) = 232.9
    cvp(8) = 249.6
    ad(1) = 1.0
    ad(2) = 1.0
    ad(3) = 1.0
    ad(4) = 23.3
    ad(5) = 33.9
    ad(6) = 32.8
    ad(7) = 49.9
    ad(8) = 50.5
    bd(1) = 0.0
    bd(2) = 0.0
    bd(3) = 0.0
    bd(4) = -0.0700
    bd(5) = -0.0957
    bd(6) = -0.0995
    bd(7) = -0.0191
    bd(8) = -0.0541
    cd(1) = 0.0
    cd(2) = 0.0
    cd(3) = 0.0
    cd(4) = -0.0002
    cd(5) = -0.000152
    cd(6) = -0.000233
    cd(7) = -0.000425
    cd(8) = -0.000150
    ah(1) = 1.0D-6
    ah(2) = 1.0D-6
    ah(3) = 1.0D-6
    ah(4) = 0.960D-6
    ah(5) = 0.573D-6
    ah(6) = 0.652D-6
    ah(7) = 0.515D-6
    ah(8) = 0.471D-6
    bh(1) = 0.0
    bh(2) = 0.0
    bh(3) = 0.0
    bh(4) = 8.70D-9
    bh(5) = 2.41D-9
    bh(6) = 2.18D-9
    bh(7) = 5.65D-10
    bh(8) = 8.70D-10
    ch(1) = 0.0
    ch(2) = 0.0
    ch(3) = 0.0
    ch(4) = 4.81D-11
    ch(5) = 1.82D-11
    ch(6) = 1.94D-11
    ch(7) = 3.82D-12
    ch(8) = 2.62D-12
    av(1) = 1.0D-6
    av(2) = 1.0D-6
    av(3) = 1.0D-6
    av(4) = 86.7D-6
    av(5) = 160.D-6
    av(6) = 160.D-6
    av(7) = 225.D-6
    av(8) = 209.D-6
    ag(1) = 3.411D-6
    ag(2) = 0.3799D-6
    ag(3) = 0.2491D-6
    ag(4) = 0.3567D-6
    ag(5) = 0.3463D-6
    ag(6) = 0.3930D-6
    ag(7) = 0.170D-6
    ag(8) = 0.150D-6
    bg(1) = 7.18D-10
    bg(2) = 1.08D-9
    bg(3) = 1.36D-11
    bg(4) = 8.51D-10
    bg(5) = 8.96D-10
    bg(6) = 1.02D-9
    bg(7) = 0.D0
    bg(8) = 0.D0
    cg(1) = 6.0D-13
    cg(2) = -3.98D-13
    cg(3) = -3.93D-14
    cg(4) = -3.12D-13
    cg(5) = -3.27D-13
    cg(6) = -3.12D-13
    cg(7) = 0.D0
    cg(8) = 0.D0
    yy(1) = 10.40491389
    yy(2) = 4.363996017
    yy(3) = 7.570059737
    yy(4) = 0.4230042431
    yy(5) = 24.15513437
    yy(6) = 2.942597645
    yy(7) = 154.3770655
    yy(8) = 159.1865960
    yy(9) = 2.808522723
    yy(10) = 63.75581199
    yy(11) = 26.74026066
    yy(12) = 46.38532432
    yy(13) = 0.2464521543
    yy(14) = 15.20484404
    yy(15) = 1.852266172
    yy(16) = 52.44639459
    yy(17) = 41.20394008
    yy(18) = 0.5699317760
    yy(19) = 0.4306056376
    yy(20) = 7.9906200783D-03
    yy(21) = 0.9056036089
    yy(22) = 1.6054258216D-02
    yy(23) = 0.7509759687
    yy(24) = 8.8582855955D-02
    yy(25) = 48.27726193
    yy(26) = 39.38459028
    yy(27) = 0.3755297257
    yy(28) = 107.7562698
    yy(29) = 29.77250546
    yy(30) = 88.32481135
    yy(31) = 23.03929507
    yy(32) = 62.85848794
    yy(33) = 5.546318688
    yy(34) = 11.92244772
    yy(35) = 5.555448243
    yy(36) = 0.9218489762
    yy(37) = 94.59927549
    yy(38) = 77.29698353
    yy(39) = 63.05263039
    yy(40) = 53.97970677
    yy(41) = 24.64355755
    yy(42) = 61.30192144
    yy(43) = 22.21000000
    yy(44) = 40.06374673
    yy(45) = 38.10034370
    yy(46) = 46.53415582
    yy(47) = 47.44573456
    yy(48) = 41.10581288
    yy(49) = 18.11349055
    yy(50) = 50.00000000
    Do i = 1, 12
        xmv(i) = yy(i+38)
        vcv(i) = xmv(i)
        vst(i) = 2.0D0
        ivst(i) = 0
    End Do
    vrng(1) = 400.00
    vrng(2) = 400.00
    vrng(3) = 100.00
    vrng(4) = 1500.00
    vrng(7) = 1500.00
    vrng(8) = 1000.00
    vrng(9) = 0.03
    vrng(10) = 1000.
    vrng(11) = 1200.0
    vtr = 1300.0
    vts = 3500.0
    vtc = 156.5
    vtv = 5000.0
    htr(1) = 0.06899381054D0
    htr(2) = 0.05D0
    hwr = 7060.
    hws = 11138.
    sfr(1) = 0.99500
    sfr(2) = 0.99100
    sfr(3) = 0.99000
    sfr(4) = 0.91600
    sfr(5) = 0.93600
    sfr(6) = 0.93800
    sfr(7) = 5.80000D-02
    sfr(8) = 3.01000D-02
    xst(1, 1) = 0.0
    xst(2, 1) = 0.0001
    xst(3, 1) = 0.0
    xst(4, 1) = 0.9999
    xst(5, 1) = 0.0
    xst(6, 1) = 0.0
    xst(7, 1) = 0.0
    xst(8, 1) = 0.0
    tst(1) = 45.
    xst(1, 2) = 0.0
    xst(2, 2) = 0.0
    xst(3, 2) = 0.0
    xst(4, 2) = 0.0
    xst(5, 2) = 0.9999
    xst(6, 2) = 0.0001
    xst(7, 2) = 0.0
    xst(8, 2) = 0.0
    tst(2) = 45.
    xst(1, 3) = 0.9999
    xst(2, 3) = 0.0001
    xst(3, 3) = 0.0
    xst(4, 3) = 0.0
    xst(5, 3) = 0.0
    xst(6, 3) = 0.0
    xst(7, 3) = 0.0
    xst(8, 3) = 0.0
    tst(3) = 45.
    xst(1, 4) = 0.4850
    xst(2, 4) = 0.0050
    xst(3, 4) = 0.5100
    xst(4, 4) = 0.0
    xst(5, 4) = 0.0
    xst(6, 4) = 0.0
    xst(7, 4) = 0.0
    xst(8, 4) = 0.0
    tst(4) = 45.
    cpflmx = 280275.
    cpprmx = 1.3
    vtau(1) = 8.
    vtau(2) = 8.
    vtau(3) = 6.
    vtau(4) = 9.
    vtau(5) = 7.
    vtau(6) = 5.
    vtau(7) = 5.
    vtau(8) = 5.
    vtau(9) = 120.
    vtau(10) = 5.
    vtau(11) = 5.
    vtau(12) = 5.
    Do i = 1, 12
        vtau(i) = vtau(i)/3600.
    End Do
    g = 1431655765.D0
    xns(1) = 0.0012D0
    xns(2) = 18.000D0
    xns(3) = 22.000D0
    xns(4) = 0.0500D0
    xns(5) = 0.2000D0
    xns(6) = 0.2100D0
    xns(7) = 0.3000D0
    xns(8) = 0.5000D0
    xns(9) = 0.0100D0
    xns(10) = 0.0017D0
    xns(11) = 0.0100D0
    xns(12) = 1.0000D0
    xns(13) = 0.3000D0
    xns(14) = 0.1250D0
    xns(15) = 1.0000D0
    xns(16) = 0.3000D0
    xns(17) = 0.1150D0
    xns(18) = 0.0100D0
    xns(19) = 1.1500D0
    xns(20) = 0.2000D0
    xns(21) = 0.0100D0
    xns(22) = 0.0100D0
    xns(23) = 0.250D0
    xns(24) = 0.100D0
    xns(25) = 0.250D0
    xns(26) = 0.100D0
    xns(27) = 0.250D0
    xns(28) = 0.025D0
    xns(29) = 0.250D0
    xns(30) = 0.100D0
    xns(31) = 0.250D0
    xns(32) = 0.100D0
    xns(33) = 0.250D0
    xns(34) = 0.025D0
    xns(35) = 0.050D0
    xns(36) = 0.050D0
    xns(37) = 0.010D0
    xns(38) = 0.010D0
    xns(39) = 0.010D0
    xns(40) = 0.500D0
    xns(41) = 0.500D0
    Do i = 1, 20
        idv(i) = 0
    End Do
    hspan(1) = 0.2D0
    hzero(1) = 0.5D0
    sspan(1) = 0.03D0
    szero(1) = 0.485D0
    spspan(1) = 0.D0
    hspan(2) = 0.7D0
    hzero(2) = 1.0D0
    sspan(2) = .003D0
    szero(2) = .005D0
    spspan(2) = 0.D0
    hspan(3) = 0.25D0
    hzero(3) = 0.5D0
    sspan(3) = 10.D0
    szero(3) = 45.D0
    spspan(3) = 0.D0
    hspan(4) = 0.7D0
    hzero(4) = 1.0D0
    sspan(4) = 10.D0
    szero(4) = 45.D0
    spspan(4) = 0.D0
    hspan(5) = 0.15D0
    hzero(5) = 0.25D0
    sspan(5) = 10.D0
    szero(5) = 35.D0
    spspan(5) = 0.D0
    hspan(6) = 0.15D0
    hzero(6) = 0.25D0
    sspan(6) = 10.D0
    szero(6) = 40.D0
    spspan(6) = 0.D0
    hspan(7) = 1.D0
    hzero(7) = 2.D0
    sspan(7) = 0.25D0
    szero(7) = 1.0D0
    spspan(7) = 0.D0
    hspan(8) = 1.D0
    hzero(8) = 2.D0
    sspan(8) = 0.25D0
    szero(8) = 1.0D0
    spspan(8) = 0.D0
    hspan(9) = 0.4D0
    hzero(9) = 0.5D0
    sspan(9) = 0.25D0
    szero(9) = 0.0D0
    spspan(9) = 0.D0
    hspan(10) = 1.5D0
    hzero(10) = 2.0D0
    sspan(10) = 0.0D0
    szero(10) = 0.0D0
    spspan(10) = 0.D0
    hspan(11) = 2.0D0
    hzero(11) = 3.0D0
    sspan(11) = 0.0D0
    szero(11) = 0.0D0
    spspan(11) = 0.D0
    hspan(12) = 1.5D0
    hzero(12) = 2.0D0
    sspan(12) = 0.0D0
    szero(12) = 0.0D0
    spspan(12) = 0.D0
    Do i = 1, 12
        tlast(i) = 0.D0
        tnext(i) = 0.1D0
        adist(i) = szero(i)
        bdist(i) = 0.D0
        cdist(i) = 0.D0
        ddist(i) = 0.D0
    End Do
    time = 0.0
    Call tefunc(nn, time, yy, yp)
    Return
End Subroutine teinit
!     END SUBROUTINE TEINIT
!
!=============================================================================
!     TESUBi - Utility subroutines, i=1,2,..,8
!
!-----------------------------------------------------------------------
!     TESUB1(Z,T,H,ITY)
!
Subroutine tesub1(z, t, h, ity)
    Double Precision avp, bvp, cvp, ah, bh, ch, ag, bg, cg, av, ad, bd, cd, xmw
    Common /const/avp(8), bvp(8), cvp(8), ah(8), bh(8), ch(8), ag(8), bg(8), cg(8), av(8), ad(8), bd(8), cd(8), xmw(8)
    Double Precision z(8), r, t, h, hi
    Integer ity, i
    If (ity==0) Then
        h = 0.0D0
        Do i = 1, 8
            hi = t*(ah(i)+bh(i)*t/2.D0+ch(i)*t**2/3.D0)
            hi = 1.8D0*hi
            h = h + z(i)*xmw(i)*hi
        End Do
    Else
        h = 0.0D0
        Do i = 1, 8
            hi = t*(ag(i)+bg(i)*t/2.D0+cg(i)*t**2/3.D0)
            hi = 1.8D0*hi
            hi = hi + av(i)
            h = h + z(i)*xmw(i)*hi
        End Do
    End If
    If (ity==2) Then
        r = 3.57696D0/1.D6
        h = h - r*(t+273.15)
    End If
    Return
End Subroutine tesub1
!     END SUBROUTINE TESUB1
!
!-----------------------------------------------------------------------
!     TESUB2(Z,T,H,ITY)
!
Subroutine tesub2(z, t, h, ity)
    Double Precision avp, bvp, cvp, ah, bh, ch, ag, bg, cg, av, ad, bd, cd, xmw
    Common /const/avp(8), bvp(8), cvp(8), ah(8), bh(8), ch(8), ag(8), bg(8), cg(8), av(8), ad(8), bd(8), cd(8), xmw(8)
    Integer ity, j
    Double Precision z(8), t, h, tin, htest, err, dh, dt
    tin = t
    Do j = 1, 100
        Call tesub1(z, t, htest, ity)
        err = htest - h
        Call tesub3(z, t, dh, ity)
        dt = -err/dh
        t = t + dt
        If (dabs(dt)<1.D-12) Goto 300
    End Do
    t = tin
    300 Return
End Subroutine tesub2
!     END SUBROUTINE TESUB2
!
!-----------------------------------------------------------------------
!     TESUB3(Z,T,DH,ITY)
!
Subroutine tesub3(z, t, dh, ity)
    Double Precision avp, bvp, cvp, ah, bh, ch, ag, bg, cg, av, ad, bd, cd, xmw
    Common /const/avp(8), bvp(8), cvp(8), ah(8), bh(8), ch(8), ag(8), bg(8), cg(8), av(8), ad(8), bd(8), cd(8), xmw(8)
    Integer ity, i
    Double Precision z(8), r, t, dh, dhi
    If (ity==0) Then
        dh = 0.0D0
        Do i = 1, 8
            dhi = ah(i) + bh(i)*t + ch(i)*t**2
            dhi = 1.8D0*dhi
            dh = dh + z(i)*xmw(i)*dhi
        End Do
    Else
        dh = 0.0D0
        Do i = 1, 8
            dhi = ag(i) + bg(i)*t + cg(i)*t**2
            dhi = 1.8D0*dhi
            dh = dh + z(i)*xmw(i)*dhi
        End Do
    End If
    If (ity==2) Then
        r = 3.57696D0/1.D6
        dh = dh - r
    End If
    Return
End Subroutine tesub3
!     END SUBROUTINE TESUB3
!
!-----------------------------------------------------------------------
!     TESUB4(X,T,R)
!
Subroutine tesub4(x, t, r)
    Double Precision avp, bvp, cvp, ah, bh, ch, ag, bg, cg, av, ad, bd, cd, xmw
    Common /const/avp(8), bvp(8), cvp(8), ah(8), bh(8), ch(8), ag(8), bg(8), cg(8), av(8), ad(8), bd(8), cd(8), xmw(8)
    Double Precision v, r, x(8), t
    Integer i
    v = 0.0
    Do i = 1, 8
        v = v + x(i)*xmw(i)/(ad(i)+(bd(i)+cd(i)*t)*t)
    End Do
    r = 1.0/v
    Return
End Subroutine tesub4
!     END SUBROUTINE TESUB4
!
!-----------------------------------------------------------------------
!     TESUB5(S,SP,ADIST,BDIST,CDIST,DDIST,TLAST,
!    .TNEXT,HSPAN,HZERO,SSPAN,SZERO,SPSPAN,IDVFLAG)
!
Subroutine tesub5(s, sp, adist, bdist, cdist, ddist, tlast, tnext, hspan, hzero, sspan, szero, spspan, idvflag)
    Double Precision s, sp, h, s1, s1p, adist, bdist, cdist, ddist, tlast, tnext, hspan, hzero, sspan, szero, spspan, tesub7
    Integer i, idvflag
    i = -1
    h = hspan*tesub7(i) + hzero
    s1 = sspan*tesub7(i)*idvflag + szero
    s1p = spspan*tesub7(i)*idvflag
    adist = s
    bdist = sp
    cdist = (3.D0*(s1-s)-h*(s1p+2.D0*sp))/h**2
    ddist = (2.D0*(s-s1)+h*(s1p+sp))/h**3
    tnext = tlast + h
    Return
End Subroutine tesub5
!     END SUBROUTINE TESUB5
!
!-----------------------------------------------------------------------
!     TESUB6(STD,X)
!
Subroutine tesub6(std, x)
    Integer i
    Double Precision std, x, tesub7
    x = 0.D0
    Do i = 1, 12
        x = x + tesub7(i)
    End Do
    x = (x-6.D0)*std
    Return
End Subroutine tesub6
!     END SUBROUTINE TESUB6
!
!-----------------------------------------------------------------------
!     TESUB7(I)
!
Double Precision Function tesub7(i)
    Integer i
    Double Precision g, dmod
    Common /randsd/g
    g = dmod(g*9228907.D0, 4294967296.D0)
    If (i>=0) tesub7 = g/4294967296.D0
    If (i<0) tesub7 = 2.D0*g/4294967296.D0 - 1.D0
    Return
End Function tesub7
!     END FUNCTION TESUB7
!
!-----------------------------------------------------------------------
!     TESUB8(I,T)
!
Double Precision Function tesub8(i, t)
    Integer i
    Double Precision h, t
    Integer idvwlk
    Double Precision adist, bdist, cdist, ddist, tlast, tnext, hspan, hzero, sspan, szero, spspan
    Common /wlk/adist(12), bdist(12), cdist(12), ddist(12), tlast(12), tnext(12), hspan(12), hzero(12), sspan(12), szero(12), spspan(12), idvwlk(12)
    h = t - tlast(i)
    tesub8 = adist(i) + h*(bdist(i)+h*(cdist(i)+h*ddist(i)))
    Return
End Function tesub8

