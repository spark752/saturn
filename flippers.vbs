' Code based on "nFozzy roth Physics.pdf" and the VPX Medieval Madness
' implementation by Skitso, rothbauerw, bord, ninuzzu, Tom Tower, JPSalas,
' Dozer, Pinball Ken, Jamin, Macho, Joker, PacDude, and others.
' Reworked for readability, consistancy, and global namespace reduction.

' CheckLiveCatch
' **************
' Flipper dampener possibly used by CheckLiveCatch. Adjust these values
' to increase  or lessen the elasticity... somehow.
'dim FlippersD : Set FlippersD = new Dampener
'FlippersD.name = "Flippers"
'FlippersD.debugOn = False
'FlippersD.Print = False
'FlippersD.addpoint 0, 0, 1.1
'FlippersD.addpoint 1, 3.77, 0.99
'FlippersD.addpoint 2, 6, 0.99

' Called by flipper collide events. If will improve catches by reducing or
' eliminating the ball velocity and angular momentum. This relies on parameters
' that are set by FlipperTricks, which is called by a timer at 1 ms intervals.
' That seems like a lot of overhead since a catch is only considered to be
' within the first 16 ms of the flipper coming up: it almost never happens.
' And the ball seems relatively easy to control without it.
'
' Parameters:
'   ball = A VPX Ball object, normally the VPX global "ActiveBall"
'   Flipper = A VPX Flipper object, usually "LeftFlipper" or "RightFlipper"
'   FCount = A variable specific to the flipper. It is actually a timestamp
'       and not a count. It is set in the FlipperTricks sub but not here.
'   parm = The parameter received by the collide event. I can't find any
'       VPX documentation of this parameter or the event itself.
Sub CheckLiveCatch(ball, Flipper, FCount, parm)
    Const LIVE_DISTANCE_MIN = 30 ' Min vp units from base for dampening
    Const LIVE_DISTANCE_MAX = 114 'Max " (tip protection)
    Const LIVE_CATCH_THR = 16 ' Threshold for live catch time

    Dim LiveCatchBounce ' If live catch is not perfect don't freeze ball

    Dim Dir
    Dir = Flipper.StartAngle / Abs(Flipper.StartAngle) ' -1 for Right Flipper
    Dim CatchTime
    CatchTime = GameTime - FCount ' GameTime = VPX global game time in ms
    Dim DistX
    DistX = Abs(Flipper.X - ball.X)

    debug.print "CheckLiveCatch CatchTime=" & CatchTime & " Threshold=" & LIVE_CATCH_THR
    If CatchTime <= LIVE_CATCH_THR and parm > 6 and DistX > LIVE_DISTANCE_MIN and DistX < LIVE_DISTANCE_MAX Then
        If CatchTime <= LIVE_CATCH_THR * 0.5 Then
            'Perfect catch at start of the window
            debug.print "CheckLiveCatch Perfect Catch"
            LiveCatchBounce = 0
        Else
            'Partial catch when a bit late
            debug.print "CheckLiveCatch Partial Catch"
            LiveCatchBounce = Abs((LIVE_CATCH_THR * 0.5) - CatchTime)
        End If
        If LiveCatchBounce = 0 and ball.velX * Dir > 0 Then
            ball.velX = 0
        End If
        ' Multiplier for inaccuracy bounce
        ball.velY = LiveCatchBounce * (32 / LIVE_CATCH_THR)
        ball.AngMomX = 0
        ball.AngMomY = 0
        ball.AngMomZ = 0
    Else
        ' FIXME: This is not in the physics paper
        If Abs(Flipper.CurrentAngle) <= Abs(Flipper.EndAngle) + 1 Then
            'FlippersD.Dampenf ActiveBall, parm
        End If
    End If
End Sub

' Polarity & Velocity Correction
' ******************************
' From nFozzy flipper physics. Apparently these are corrections for the
' velocity X component and magnitude relative to the ball contact point on
' the flipper, but only right after the flipper has been fired. These are
' not needed for EM era flippers or short flippers.
'
' The area surrounding the flipper needs a shaped trigger and the tip of the
' flipper needs an invisible primitive for tracking.
' The flipper objects must be named "LeftFlipper" and "RightFlipper", the
' triggers must be named "TriggerLF" and "TriggerRF", and the tip objects must
' be named "EndPointLp" and "EndPointRp".
'
' This code requires two global objects of "FlipperPolarity" to have been
' created with the not so descriptive names of "LF" and "RF". These are used
' for corrections to "LeftFlipper" and "RightFlipper" respectively. The objects
' should be created at system initialization prior to calling the "InitPolarity"
' sub below.

' This should be called at script start to create the correction curves.
' Curves for both modern and one older style are shown here. For older curves,
' consult the physics document.
Sub InitPolarity()
    Dim x, a
    a = Array(LF, RF)
    For Each x in a
        x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1 ' Disabled
        x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
        x.enabled = True
        x.TimeDelay = 60
    Next

    If True Then ' Change this as needed
        ' 90's and newer
        AddPt "Polarity", 0, 0, 0
        AddPt "Polarity", 1, 0.05, -5.5
        AddPt "Polarity", 2, 0.4, -5.5
        AddPt "Polarity", 3, 0.6, -5.0
        AddPt "Polarity", 4, 0.65, -4.5
        AddPt "Polarity", 5, 0.7, -4.0
        AddPt "Polarity", 6, 0.75, -3.5
        AddPt "Polarity", 7, 0.8, -3.0
        AddPt "Polarity", 8, 0.85, -2.5
        AddPt "Polarity", 9, 0.9,-2.0
        AddPt "Polarity", 10, 0.95, -1.5
        AddPt "Polarity", 11, 1, -1.0
        AddPt "Polarity", 12, 1.05, -0.5
        AddPt "Polarity", 13, 1.1, 0
        AddPt "Polarity", 14, 1.3, 0
    Else
        ' Late 80's and early 90's
        AddPt "Polarity", 0, 0, 0
        AddPt "Polarity", 1, 0.05, -5
        AddPt "Polarity", 2, 0.4, -5
        AddPt "Polarity", 3, 0.6, -4.5
        AddPt "Polarity", 4, 0.65, -4.0
        AddPt "Polarity", 5, 0.7, -3.5
        AddPt "Polarity", 6, 0.75, -3.0
        AddPt "Polarity", 7, 0.8, -2.5
        AddPt "Polarity", 8, 0.85, -2.0
        AddPt "Polarity", 9, 0.9,-1.5
        AddPt "Polarity", 10, 0.95, -1.0
        AddPt "Polarity", 11, 1, -0.5
        AddPt "Polarity", 12, 1.1, 0
        AddPt "Polarity", 13, 1.3, 0
    End If

    AddPt "Velocity", 0, 0, 1
    AddPt "Velocity", 1, 0.16, 1.06
    AddPt "Velocity", 2, 0.41, 1.05
    AddPt "Velocity", 3, 0.53, 1'0.982
    AddPt "Velocity", 4, 0.702, 0.968
    AddPt "Velocity", 5, 0.95, 0.968
    AddPt "Velocity", 6, 1.03, 0.945

    LF.Object = LeftFlipper
    'FIXME LF.EndPoint = EndPointLp
    RF.Object = RightFlipper
    RF.EndPoint = EndPointRp
End Sub

' Helper for "InitPolarity" to add a point to curves for both flippers
Sub AddPt(aStr, idx, aX, aY)
    Dim a
    a = Array(LF, RF)
    Dim x
    For Each x in a
        x.AddPoint aStr, idx, aX, aY
    Next
End Sub

' Flipper trigger hit subs. Depends on the "LF" and "RF" objects described
' above.
Sub TriggerLF_Hit()
    LF.Addball activeball
End Sub
Sub TriggerLF_UnHit()
    LF.PolarityCorrect activeball
End Sub
Sub TriggerRF_Hit()
    RF.Addball activeball
End Sub
Sub TriggerRF_UnHit()
    RF.PolarityCorrect activeball
End Sub

' This big class avoids a lot of namespace pollution. Code should create an
' object of this type for each flipper, as detailed in "InitPolarity" above.
' Provides:
'   Fire = The main interface which should be used to fire the flipper
'       instead of calling "RotateToEnd" directly on the flipper object.
'   AddBall = Called by the area triggers on a hit to signal that a ball is
'       nearby and should be added for processing. (This should not be confused
'       with non flipper related routines which may exist to create balls etc.)
'   PolarityCorrect = Called by the area triggers to signal that a ball is
'       leaving the flipper area and the corrections should be preformed.
'   AddPoint = Called by "InitPolarity" via "AddPt" to add a point to a
'       correction curve.
Class FlipperPolarity
    Public DebugOn, Enabled
    Private FlipAt ' Timer variable (IE 'flip at 723,530ms...)
    Public TimeDelay ' Delay before trigger turns off and polarity is disabled TODO set time!
    Private Flipper, FlipperStart,FlipperEnd, FlipperEndY, LR, PartialFlipCoef
    Private Balls(20), balldata(20)

    Dim PolarityIn, PolarityOut
    Dim VelocityIn, VelocityOut
    Dim YcoefIn, YcoefOut

    Public Sub Class_Initialize
        ReDim PolarityIn(0)
        ReDim PolarityOut(0)
        ReDim VelocityIn(0)
        ReDim VelocityOut(0)
        ReDim YcoefIn(0)
        ReDim YcoefOut(0)
        Enabled = True
        TimeDelay = 50
        LR = 1
        Dim x
        For x = 0 To UBound(balls)
            balls(x) = Empty
            set Balldata(x) = new SpoofBall
        Next
     End Sub

    Public Property Let Object(aInput)
        Set Flipper = aInput
        StartPoint = Flipper.x
    End Property
    Public Property Let StartPoint(aInput)
        If IsObject(aInput) Then
            FlipperStart = aInput.x
        Else
            FlipperStart = aInput
        End If
    End Property
    Public Property Get StartPoint
        StartPoint = FlipperStart
    End Property
    Public Property Let EndPoint(aInput)
        FlipperEnd = aInput.x
        FlipperEndY = aInput.y
    End Property
    Public Property Get EndPoint
        EndPoint = FlipperEnd
    End Property
    Public Property Get EndPointY
        EndPointY = FlipperEndY
    End Property

    Public Sub AddPoint(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out)
        Select Case aChooseArray
            Case "Polarity"
                ShuffleArrays PolarityIn, PolarityOut, 1
                PolarityIn(aIDX) = aX
                PolarityOut(aIDX) = aY
                ShuffleArrays PolarityIn, PolarityOut, 0
            Case "Velocity"
                ShuffleArrays VelocityIn, VelocityOut, 1
                VelocityIn(aIDX) = aX
                VelocityOut(aIDX) = aY
                ShuffleArrays VelocityIn, VelocityOut, 0
            Case "Ycoef"
                ShuffleArrays YcoefIn, YcoefOut, 1
                YcoefIn(aIDX) = aX
                YcoefOut(aIDX) = aY
                ShuffleArrays YcoefIn, YcoefOut, 0
        End Select
        If GameTime > 100 Then Report aChooseArray
    End Sub

    Public Sub Report(aChooseArray) 'debug, reports all coords in tbPL.text
        If Not DebugOn Then Exit Sub
        Dim a1, a2
        Select Case aChooseArray
            case "Polarity"
                a1 = PolarityIn
                a2 = PolarityOut
            Case "Velocity"
                a1 = VelocityIn
                a2 = VelocityOut
            Case "Ycoef"
                a1 = YcoefIn
                a2 = YcoefOut
            Case Else
                tbpl.text = "wrong string"
                Exit Sub
        End Select
        Dim str, x
        For x = 0 To UBound(a1)
            str = str & aChooseArray & " x: " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline
        Next
        tbpl.text = str
    End Sub

    Public Sub AddBall(aBall)
        Dim x
        For x = 0 To UBound(balls)
            If IsEmpty(balls(x)) Then
                Set balls(x) = aBall
                Exit Sub
            End If
        Next
    End Sub

    Private Sub RemoveBall(aBall)
        Dim x
        For x = 0 To UBound(balls)
            If TypeName(balls(x)) = "IBall" Then
                If aBall.ID = Balls(x).ID Then
                    balls(x) = Empty
                    Balldata(x).Reset
                End If
            End If
        Next
    End Sub

    Public Sub Fire()
        debug.print "FlipperPolarity Fire"
        Flipper.RotateToEnd
        processballs
    End Sub

    Public Property Get Pos 'returns % position a ball. For debug stuff.
        Dim x
        For x = 0 To UBound(balls)
            If Not IsEmpty(balls(x)) Then
                pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
            End If
        Next
    End Property

    Public Sub ProcessBalls() 'save data of balls in flipper range
        FlipAt = GameTime
        Dim x
        For x = 0 To UBound(balls)
            If Not IsEmpty(balls(x)) Then
                balldata(x).Data = balls(x)
            End If
        Next
        PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
        PartialFlipCoef = abs(PartialFlipCoef-1)
    End Sub

    Private Function FlipperOn()
        If gameTime < FlipAt + TimeDelay Then
            FlipperOn = True
        End If
    End Function

    Public Sub PolarityCorrect(aBall)
        If FlipperOn() Then
            Dim tmp, BallPos, x, IDX, Ycoef
            Ycoef = 1

            'y safety Exit
            If aBall.VelY > -8 Then 'ball going down
                RemoveBall aBall
                Exit Sub
            End If

            'Find balldata. BallPos = % on Flipper
            For x = 0 To UBound(Balls)
                If aBall.id = BallData(x).id And Not IsEmpty(BallData(x).id) Then
                    idx = x
                    BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
                    if BallPos > 0.65 then
                        Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)                                'find safety coefficient 'ycoef' data
                    End If
                End If
            Next

            If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
                BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
                If BallPos > 0.65 Then
                    Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)                                                'find safety coefficient 'ycoef' data
                End If
            End If

            'Velocity correction
            If Not IsEmpty(VelocityIn(0)) Then
                Dim VelCoef
                VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

                If partialflipcoef < 1 Then
                    VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
                End If

                If Enabled Then aBall.VelX = aBall.VelX * VelCoef
                If Enabled Then aBall.VelY = aBall.VelY * VelCoef
            End If

            'Polarity Correction (optional now)
            If Not IsEmpty(PolarityIn(0)) Then
                If StartPoint > EndPoint Then LR = -1 'Reverse polarity if left flipper
                Dim AddX
                AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
                If Enabled Then
                    aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
                End If
            End If
        End If
        RemoveBall aBall
    End Sub
End Class

' Used by the "FlipperPolarity" class, somehow
Class Spoofball
    Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius
    Public Property Let Data(aBall)
        With aBall
            x = .x : y = .y : z = .z
            velx = .velx : vely = .vely : velz = .velz
            id = .ID : mass = .mass : radius = .radius
        End With
    End Property
    Public Sub Reset()
        x = Empty : y = Empty : z = Empty
        velx = Empty : vely = Empty : velz = Empty
        id = Empty : mass = Empty : radius = Empty
    End Sub
End Class

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, ByVal offset)
    Dim x, aCount
    aCount = 0
    ReDim a(UBound(aArray))
    For x = 0 To UBound(aArray)        ' Shuffle objects in a temp array
        If Not IsEmpty(aArray(x)) Then
            If IsObject(aArray(x)) Then
                Set a(aCount) = aArray(x)
            Else
                a(aCount) = aArray(x)
            End If
        aCount = aCount + 1
        End If
    Next
    If offset < 0 Then offset = 0
    ReDim aArray(aCount -1 + offset)    ' Resize original array
    For x = 0 To aCount -1              ' Set objects back into original array
        If IsObject(a(x)) Then
            Set aArray(x) = a(x)
        Else
            aArray(x) = a(x)
        End If
    Next
End Sub

' Used for flipper correction and rubber dampeners
Sub ShuffleArrays(aArray1, aArray2, offset)
    ShuffleArray aArray1, offset
    ShuffleArray aArray2, offset
End Sub

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2) 'Set up line via two points, no clamping. Input X, output Y
    Dim x, y, b, m
    x = input
    m = (Y2 - Y1) / (X2 - X1)
    b = Y2 - m * X2
    Y = M * x + b
    PSlope = Y
End Function

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
    Dim y 'Y output
    Dim L 'Line
    Dim i
    For i = 1 To UBound(xKeyFrame)        'find active line
        If xInput <= xKeyFrame(i) Then
            L = i
            Exit For
        End If
    Next
    If xInput > xKeyFrame(uBound(xKeyFrame)) Then L = uBound(xKeyFrame)        'catch line overrun
    Y = PSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L))

    If xInput <= xKeyFrame(lBound(xKeyFrame)) Then
        Y = yLvl(lBound(xKeyFrame))         'Clamp lower
    End If
    If xInput >= xKeyFrame(uBound(xKeyFrame)) Then
        Y = yLvl(uBound(xKeyFrame))        'Clamp upper
    End If
    LinearEnvelope = Y
End Function

' ****

Dim PI
PI = 4 * Atn(1)

Function Radians(Degrees)
    Radians = Degrees * PI / 180
End Function

' Called by a 1 ms timer vis FlipperControl (the HardwareFlipper object)
Sub FlipperNudge(Flipper1, EndAngle1, ByRef EOSNudge1, Flipper2, EndAngle2)
    Dim b, BOT ' Only initalized in the branch where they are used

    If Flipper1.CurrentAngle = EndAngle1 and EOSNudge1 <> 1 Then
        EOSNudge1 = 1
        'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
        If Flipper2.CurrentAngle = EndAngle2 Then
            BOT = GetBalls ' VPX API which returns ball objects
            For b = 0 to Ubound(BOT)
                If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
                    'Debug.Print "ball in flip1. exit"
                    Exit Sub
                End If
            Next
            For b = 0 to Ubound(BOT)
                If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
                    debug.print "FlipperNudge triggered"
                    ' These values are different in the paper
                    BOT(b).velx = BOT(b).velx / 1.3
                    BOT(b).vely = BOT(b).vely - 0.5
                End If
            Next
        End If
    Else
        ' This code is different from the paper
        If Abs(Flipper1.CurrentAngle) > Abs(EndAngle1) + 30 Then
            ' This gets called a lot
            EOSNudge1 = 0
        End If
    End If
End Sub

' Used by FlipperNudge
Function FlipperTrigger(ballx, bally, Flipper)
    Dim DiffAngle
    DiffAngle  = Abs(Flipper.CurrentAngle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
    If DiffAngle > 180 Then DiffAngle = DiffAngle - 360

    If DistanceFromFlipper(ballx, bally, Flipper) < 48 And DiffAngle <= 90 And Distance(ballx, bally, Flipper.x, Flipper.y) < Flipper.Length Then
        FlipperTrigger = True
    Else
        FlipperTrigger = False
    End If
End Function

' Used by FlipperTrigger
Function AnglePP(ax, ay, bx, by)
    AnglePP = Atn2((by - ay), (bx - ax)) * 180 / PI
End Function

' Used by AnglePP
Function Atn2(dy, dx)
    If dx > 0 Then
        Atn2 = Atn(dy / dx)
    ElseIf dx < 0 Then
        If dy = 0 Then
            Atn2 = PI
        Else
            Atn2 = Sgn(dy) * (PI - Atn(Abs(dy / dx)))
        End If
    ElseIf dx = 0 Then
        If dy = 0 Then
            Atn2 = 0
        Else
            Atn2 = Sgn(dy) * PI / 2
        End If
    End If
End Function

' Used by FlipperTrigger
Function DistanceFromFlipper(ballx, bally, Flipper)
    DistanceFromFlipper = DistancePL(ballx, bally, Flipper.X, Flipper.Y, Cos(Radians(Flipper.CurrentAngle + 90)) + Flipper.X, Sin(Radians(Flipper.CurrentAngle + 90)) + Flipper.Y)
End Function

' Distance between a point (px, py) and a line
' Used by DistanceFromFlipper
Function DistancePL(px, py, ax, ay, bx, by)
    DistancePL = Abs((by - ay) * px - (bx - ax) * py + bx * ay - by * ax) / Distance(ax, ay, bx, by)
End Function