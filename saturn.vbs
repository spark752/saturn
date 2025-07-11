' Originally based on Big Deal by Klodo81 2022 which is based on JPSalas Basic
' EM script. However all dependencies on "core.vbs" and "controller.vbs" have
' been removed (after vpmtimer mysteriously broke) and gradually everything
' else is being rewritten too.
'
' Trying to make something kind of generic for a basic 2 flipper game
'
' The intent of the design is to separate the hardware simulation from the
' software simulation. There are a number of things that we have to do here in
' software that real machines don't. For instance, when a mechanical plunger is
' pulled back it makes a sound. We need to have software that does that, but
' it should be part of the hardware simulation and completely separate from the
' software simulation. When the plunger is released, it makes a different sound
' when it is driving a ball then when it isn't. To handle that in the hardware
' simulation we may need a trigger (switch) on the table in the plunger lane.
' This might be a wire switch as used on some machines or it might be invisible
' and not accessible to the software simulation at all. In that case it doesn't
' represent a real switch and  is just there to help us simulate something that
' happens automatically in real life.
'
' Because VPX is doing a lot of simulation itself, we may be able to simplify
' some things. For instance, a slingshot on a real machine has a switch routed
' to the CPU. Game software is usually responsible for detecting this and
' setting an output to fire the coil to kick the ball away. But VPX slingshot
' objects will do this for us automatically. We get a hit event that can be
' used in the hardware simulation to play a sound effect, and that we can turn
' into an event that the software simulation can use for scoring. But we do
' not have to put anything in either simulation to fire the coil. (Note that
' early Williams System 11 games did not use CPU control of slingshots and pop
' bumpers because of a concern that it would be too slow. They wired the
' switches directly to the coil as all EM games had. The downside was that a
' stuck switch would result in a blown fuse or burnt out coil, something we
' don't have to worry about.)
'
' Table Requirements (OUT OF DATE FIXME)
'   Trough1, Trough2, Trough3 = Kickers for a 3 ball trough
'   Drain = A kicker at the lowest point on the table to
'   LightShootAgain = A light to indicate the ball saver state
'   StateTimer = A 1000ms timer which is disabled in the editor
'   DMDTimer = A 16ms timer which is enabled in the editor
'   HardwareTimer = A 50ms timer which is disabled in the editor
'   TroughTimer = A 200ms timer which is disabled in the editor
'   GR1 = A trigger the detects the ball leaving the plunger lane. (A gate
'       near the same location is required by physics.vbs so this requirement
'       may change in the future.
'   Plus all of the requirements listed in physics.vbs

Option Explicit
Randomize

Const TableName = "Hack"
Const cGameName = "Hack" ' Used by FlexDMD (and others not currently supported)

On Error Resume Next
ExecuteGlobal GetTextFile("hardware.vbs")
If Err Then MsgBox "hardware failure"
ExecuteGlobal GetTextFile("flippers.vbs")
If Err Then MsgBox "flippers failure"
ExecuteGlobal GetTextFile("display.vbs")
If Err Then MsgBox "display failure"
On Error GoTo 0
ExecuteGlobal GetTextFile("physics.vbs")

' Platform global variables
Dim Credits             ' Always 1 credit per play unless in free play
Dim Bonus               ' Bonus added at end of ball
Dim BonusMultiplier     ' Multiplier used on bonus when it is added
Dim BallNumber          ' 0 when game is not in play
Dim ExtraBallsAwards    ' Increases on extra ball award, decrease when used
Dim Score               ' Current score
Dim ScoreMultiplier     ' Scales scoring events for different machines or modes
Dim Tilted              ' Boolean set when machine is in tilt mode
Dim BallsOnPlayfield    ' Allows for multiball support
Dim bFreePlay           ' True if credits aren't needed
Dim bGameInPlay         ' True when playing
Dim bBallInPlungerLane  ' Mostly for selecting the right plunger sound
Dim bBallSaverActive    ' Ball will be saved for "Shoot Again" if True
Dim bShowShootAgain     ' Show the user that the ball has been saved
Dim bMultiballActive    ' Multiball not just awarded but in progress
Dim StateTimerState     ' Slow timer based state machine for various things
Dim BallSaveCountdown   ' Keeps ball save active for some seconds after start
Dim LastSwitchHit       ' String containing the switch name

' Display
Dim FlexDMD
Const DMD_GREY_4 = 1
Const DMD_RGB = 2
Const ALIGN_TOP_LEFT = 0
Const ALIGN_TOP = 1
Const ALIGN_TOP_RIGHT = 2
Const ALIGN_LEFT = 3
Const ALIGN_CENTER = 4
Const ALIGN_RIGHT = 5
Const ALIGN_BOTTOM_LEFT = 6
Const ALIGN_BOTTOM = 7
Const ALIGN_BOTTOM_RIGHT = 8

' StateTimerState
Const STATE_NEW_BALL = 0
Const STATE_BALL_RELEASE = 1
Const STATE_SHOOT_AGAIN = 2
Const STATE_SAVE_COUNTDOWN = 3
Const STATE_SAVE_COOLDOWN = 4
Const STATE_END_OF_BALL = 5
Const STATE_BALL_OVER = 6
Const STATE_GAME_OVER = 7

' Possibly table specific constants
Const BALL_SAVE_SECONDS = 10
Const BALLS_PER_GAME = 3

' Custom hardware objects
Dim Trough
Dim TiltMech
Dim FlipperControl
Dim Coils

' Flipper physics
Dim LF ' Correction object for LeftFlipper
Dim RF ' Correction object for RightFlipper

' Sets or clears the tilted status. This may be called by the (simulated) tilt
' hardware sensor which doesn't know or care if a game is in progress or already
' tilted, so check those things here if necessary.
Sub SetTilted(Enabled)
    If Enabled Then
        debug.print "SetTilted True"
        Tilted = True
        bBallSaverActive = False
        bShowShootAgain = False
        Coils.bCoilPower = False
    Else
        debug.print "SetTilted False"
        Tilted = False
        TiltMech.Reset
        Coils.bCoilPower = True
    End If
End Sub

' A 50ms timer that is used for hardware logic
Sub HardwareTimer_Timer()
    ' Time decay of tilt value
    TiltMech.Tick
End Sub

' Table events called by VPX
Sub Table1_Init()
    ' Init ALL of the global variables
    bBallInPlungerLane = False

    ' Custom hardware objects
    Set Trough = New HardwareTrough
    Set TiltMech = New HardwareTilt
    Set FlipperControl = New HardwareFlippers
    Set Coils = New HardwareCoils

    ' Flipper physics
    Set LF = New FlipperPolarity
    Set RF = New FlipperPolarity
    InitPolarity

    ' Make sure flippers are off and timers are set properly
    FlipperControl.BothDeactivate
    StateTimer.Enabled = False
    HardwareTimer.Enabled = True

    ' Display
    Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
    If FlexDMD is Nothing Then
        MsgBox "No FlexDMD Found"
        Exit Sub
    End If
    With FlexDMD
        .GameName = cGameName
        .TableFile = Table1.Filename & ".vpx"
        .Color = RGB(255, 88, 32)
        .RenderMode = DMD_RGB
        .Width = 128
        .Height = 32
        .Clear = True
        .Run = True
    End With
    CreateScoreScene

    Software_Init
End Sub

Sub Table1_Exit()
    If Not FlexDMD is Nothing Then
        FlexDMD.Show = False
        FlexDMD.Run = False
        FlexDMD = NULL
    End If
End Sub

' PlungerTrigger is an invisible virtual switch located just above the plunger.
' It is only used to control which sound the plunger makes.
Sub PlungerTrigger_Hit()
    bBallInPlungerLane = True
End Sub

Sub PlungerTrigger_UnHit()
    bBallInPlungerLane = False
End Sub

' Immediately add to score
Sub AddScore(Points)
    If Tilted Then Exit Sub
    Score = Score + Points
End Sub

' Add to end of ball bonus score which can be effected by multipliers
Sub AddBonus(Points)
    If Tilted Then Exit Sub
    Bonus = Bonus + Points
End Sub

' This could happen for a replay award etc. so does not imply a coin sound
Sub AddCredits(Value)
    Credits = Credits + Value
    'PlaySound "fx_relay"
End Sub

' Called at start of new game
Sub Game_Init()
End Sub

Sub swLRampMade_Hit
    AddScore 10000
End Sub

' Launch Detect via trigger near end of lane
Sub GR1_Hit
    If bBallSaverActive Then
       BallSaveCountdown = BALL_SAVE_SECONDS
       StateTimerState = STATE_SAVE_COUNTDOWN
       StateTimer.Enabled = True
    End If
End Sub

Dim Cor
Set Cor = New CorTracker

' FIXME: Update called every 10ms but results don't seem to be used anywhere yet
Class CorTracker
    Public ballvel, ballvelx, ballvely

    Private Sub Class_Initialize()
        ReDim ballvel(0)
        ReDim ballvelx(0)
        ReDim ballvely(0)
    End Sub

    ' Call from a ~10ms timer to track ball velocity
    Public Sub Update()
        Dim b, AllBalls, HighestID
        AllBalls = GetBalls ' GetBalls = VPX API that returns Ball objects

        For Each b in AllBalls
            If b.ID > HighestID then HighestID = b.ID
        Next

        If UBound(ballvel) < HighestID Then ReDim ballvel(HighestID)
        if UBound(ballvelx) < HighestID Then ReDim ballvelx(HighestID)
        if UBound(ballvely) < HighestID Then ReDim ballvely(HighestID)

        For Each b in AllBalls
            ballvel(b.id) = BallSpeed(b)
            ballvelx(b.id) = b.VelX
            ballvely(b.id) = b.VelY
        Next
    End Sub
End Class

Function BallSpeed(ball)
    BallSpeed = Sqr(ball.VelX ^ 2 + ball.VelY ^ 2 + ball.VelZ ^ 2)
End Function

Function Distance(ax, ay, bx, by)
    Distance = Sqr((ax - bx) ^ 2 + (ay - by) ^ 2)
End Function

'****************
' Flipper Tricks
'****************
' Uses RightFlipper built in timer at 1 ms interval CAUTION!
RightFlipper.TimerInterval = 1
Rightflipper.TimerEnabled = True

' A very fast (~1 ms) timer that updates flipper stuff
Sub RightFlipper_timer()
    FlipperControl.Tricks
End Sub

' Class for FlipperController
Class HardwareFlippers
    Private LCount, RCount, LPress, RPress, LState, RState ' Used for tricks
    Private LEndAngle, REndAngle, LEOSNudge, REOSNudge ' Used for tricks
    Private EOST, EOSA, FReturn, FRampUp, FElasticity ' Compensation
    Private REFLIP_ANGLE ' For sound. Class level Const if those worked.

    Private Sub Class_Initialize()
        LCount = 0
        RCount = 0
        LPress = 0
        RPress = 0
        LState = 1
        RState = 1
        LEndAngle = LeftFlipper.EndAngle
        REndAngle = RightFlipper.EndAngle
        LEOSNudge = 0
        REOSNudge = 0
        REFLIP_ANGLE = 20

        ' These could be constants but this lets the values be set in the
        ' editor. It does assumes that the user is going to set both flippers
        ' the same since it only uses the left values.
        EOST = LeftFlipper.EOSTorque
        EOSA = LeftFlipper.EOSTorqueAngle
        FReturn = LeftFlipper.Return
        FRampUp = LeftFlipper.RampUp
        FElasticity = LeftFlipper.Elasticity
    End Sub

    Public Sub LeftCollide(parm)
        CheckLiveCatch ActiveBall, LeftFlipper, LCount, parm
        PlaySound "fx_rubber_flipper", Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
    End Sub
    Public Sub RightCollide(parm)
        debug.print "RightCollide RCount=" & RCount
        CheckLiveCatch ActiveBall, RightFlipper, RCount, parm
        PlaySound "fx_rubber_flipper", Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
    End Sub

    Public Sub LeftActivate()
        LF.Fire
        LeftSound LeftFlipper, True
        LPress = 1
        LeftFlipper.Elasticity = FElasticity
        LeftFlipper.EOSTorque = EOST
        LeftFlipper.EOSTorqueAngle = EOSA
    End Sub
    Public Sub RightActivate()
        RF.Fire
        RightSound RightFlipper, True
        RPress = 1
        RightFlipper.Elasticity = FElasticity
        RightFlipper.EOSTorque = EOST
        RightFlipper.EOSTorqueAngle = EOSA
    End Sub

    Private Sub Deactivate(Flipper, FlipperPress)
        Const EOS_RETURN = 0.035 ' Mid 80's to early 90's
        'Const EOS_RETURN = 0.025 ' Mid 90's and later

        FlipperPress = 0
        Flipper.EOSTorqueAngle = EOSA
        Flipper.EOSTorque = EOST * EOS_RETURN / FReturn
        If Abs(Flipper.CurrentAngle) <= Abs(Flipper.EndAngle) + 0.1 Then
            Dim b, BOT
            BOT = GetBalls ' VPX API
            For b = 0 to UBound(BOT)
                If Distance(BOT(b).X, BOT(b).Y, Flipper.X, Flipper.Y) < 55 Then
                    ' Check for cradle
                    If BOT(b).VelY >= -0.4 Then
                        debug.print "FlipperDeactivate Cradle Y Boost"
                        BOT(b).VelY = -0.4
                    End If
                End If
            Next
        End If
    End Sub
    Public Sub LeftDeactivate()
        LeftFlipper.RotateToStart
        LeftSound LeftFlipper, False
        Deactivate LeftFlipper, LPress
    End Sub
    Public Sub RightDeactivate()
        RightFlipper.RotateToStart
        RightSound RightFlipper, False
        Deactivate RightFlipper, RPress
    End Sub
    Public Sub BothDeactivate()
        LeftDeactivate
        RightDeactivate
    End Sub

    Private Sub FlipperTricks(Flipper, FlipperPress, ByRef FCount, FEndAngle, FState)
        Const SOS_RAMP_UP = 2.5 ' Fast. Use 6 for medium or 8.5 for slow.
        Const EOS_RAMP_UP = 0 ' From paper
        Const SOSEM = 0.815 ' From paper
        Const EOSA_NEW = 1 ' From paper
        Const EOST_NEW = 0.8 ' Possibly use 1 for machines earlier than 1990
        Dim Dir
        Dir = Flipper.StartAngle / Abs(Flipper.StartAngle) ' -1 for Right Flipper

        If Abs(Flipper.CurrentAngle) > Abs(Flipper.StartAngle) - 0.05 Then
            If FState <> 1 Then
                'debug.print "FlipperTricks FState -> 1"
                Flipper.RampUp = SOS_RAMP_UP
                Flipper.EndAngle = FEndAngle - 3 * Dir
                Flipper.Elasticity = FElasticity * SOSEM
                FCount = 0
                FState = 1
            End If
        ElseIf Abs(Flipper.CurrentAngle) <= Abs(Flipper.EndAngle) And FlipperPress = 1 Then
            If FCount = 0 Then FCount = GameTime ' VPX API
            If FState <> 2 Then
                'debug.print "FlipperTricks FState -> 2 FCount=" & FCount
                Flipper.EOSTorqueAngle = EOSA_NEW
                Flipper.EOSTorque = EOST_NEW
                Flipper.RampUp = EOS_RAMP_UP
                Flipper.EndAngle = FEndAngle
                FState = 2
            End If
        Elseif Abs(Flipper.CurrentAngle) > Abs(Flipper.EndAngle) + 0.01 And FlipperPress = 1 Then
            If FState <> 3 Then
                'debug.print "FlipperTricks FState -> 3"
                Flipper.EOSTorque = EOST
                Flipper.EOSTorqueAngle = EOSA
                Flipper.RampUp = FRampUp
                Flipper.Elasticity = FElasticity
                FState = 3
            End If
        End If
    End Sub
    Public Sub Tricks()
        FlipperTricks LeftFlipper, LPress, LCount, LEndAngle, LState
        FlipperTricks RightFlipper, RPress, RCount, REndAngle, RState
        FlipperNudge RightFlipper, REndAngle, REOSNudge, LeftFlipper, LEndAngle
        FlipperNudge LeftFlipper, LEndAngle, LEOSNudge,  RightFlipper, REndAngle
    End Sub

    Private Sub LeftSound(Flipper, Enabled)
        If Enabled Then
            If Flipper.CurrentAngle > Flipper.EndAngle - REFLIP_ANGLE Then
                PlaySoundAt "Flipper_ReFlip_L", Flipper
            Else
                PlaySoundAt "Flipper_Attack_L", Flipper
                PlaySoundAt "Flipper_L", Flipper
            End If
        Else
            If Flipper.CurrentAngle > Flipper.StartAngle + 5 Then
                PlaySoundAt "Flipper_Down_L", Flipper
            End If
            'FlipperLeftHitParm = FlipperUpSoundLevel
        End If
    End Sub
    Private Sub RightSound(Flipper, Enabled)
        If Enabled Then
            If Flipper.CurrentAngle > Flipper.EndAngle - REFLIP_ANGLE Then
                PlaySoundAt "Flipper_ReFlip_R", Flipper
            Else
                PlaySoundAt "Flipper_Attack_R", Flipper
                PlaySoundAt "Flipper_R", Flipper
            End If
        Else
            If Flipper.CurrentAngle > Flipper.StartAngle + 5 Then
                PlaySoundAt "Flipper_Down_R", Flipper
            End If
            'FlipperRightHitParm = FlipperUpSoundLevel
        End If
    End Sub
End Class

Sub RightFlipper_Collide(parm)
    FlipperControl.RightCollide parm
End Sub

Sub LeftFlipper_Collide(parm)
    FlipperControl.LeftCollide parm
End Sub

' Start button
Sub swStart_Hit()
    debug.print "swStart_Hit " & BallsOnPlayfield & " " & bGameInPlay
    If BallsOnPlayfield < 1 And Not bGameInPlay Then
        If bFreePlay Then
            StartGame
        ElseIf Credits > 0 Then
            Credits = Credits - 1
            StartGame
        End If
    End If
End Sub

' Coin accepted switch
Sub swCredit_Hit()
    If Not bFreePlay Then AddCredits 1
End Sub

' Flipper buttons. Hardware drives the flippers so these can be used for
' secondary things like lane change, high score initials, etc.
Sub swLeftFlipper_Hit()
End Sub
Sub swRightFlipper_Hit()
End Sub

' Slingshot and bumper scoring switches
Sub swSlingshot_Hit()
    LastSwitchHit = "Slingshot"
    AddScore 10 * ScoreMultiplier
End Sub
Sub swBumper_Hit()
    LastSwitchHit = "Bumper"
    AddScore 100 * ScoreMultiplier
End Sub

' Lower lanes
Sub swLeftInLane()
    LastSwitchHit = "LeftInLane"
End Sub
Sub swLeftOutLane()
    LastSwitchHit = "LeftOutLane"
End Sub
Sub swRightInLane()
    LastSwitchHit = "RightInLane"
End Sub
Sub swRightOutLane()
    LastSwitchHit = "RightOutLane"
End Sub

' Drain switch
Sub swDrain_Hit()
    debug.print "swDrain_Hit"
    LastSwitchHit = "Drain"
    BallsOnPlayfield = BallsOnPlayfield - 1
    If bGameInPLay Then
        If BallsOnPlayfield < 1 Then
            'If bBallSaverActive Then
            '   bBallSaverActive = False
            '    StateTimerState = STATE_SHOOT_AGAIN
            '    StateTimer.Enabled = True
            'Else
                StateTimerState = STATE_END_OF_BALL
                StateTimer.Enabled = True
            'End If
        ElseIf BallsOnPlayfield = 1 Then
            bMultiballActive = False
        End If
    End If
End Sub

Sub Software_Init()
    ' Should initialize all software globals, even if also set in StartGame
    Credits = 0
    Bonus = 0
    BonusMultiplier = 1
    BallNumber = 0
    ExtraBallsAwards = 0
    Score = 0
    ScoreMultiplier = 1
    BallsOnPlayfield = 0
    bGameInPlay = False
    bBallSaverActive = False
    bShowShootAgain = False
    bMultiballActive = False
    StateTimerState = STATE_NEW_BALL
    BallSaveCountdown = 15
    LastSwitchHit = ""
End Sub

Sub StartGame()
    debug.print "StartGame"

    ' Only game start things need to be set here since the others will be
    ' set by ResetForNewBall
    Score = 0
    bGameInPlay = True
    BallsOnPlayfield = 0
    TiltMech.Reset
    SetTilted False ' Resets tilt mech and turns on bCoilPower
    Game_Init() ' Table specific hook

    BallNumber = 1
    StateTimerState = STATE_NEW_BALL
    StateTimer.Interval = 500
    StateTimer.Enabled = True
End Sub

Sub ResetForNewBall()
    debug.print "ResetForNewBall"
    Bonus = 0
    BonusMultiplier = 1
    ExtraBallsAwards = 0
    BallsOnPlayfield = 0
    bBallInPlungerLane = False
    bBallSaverActive = False
End Sub

' Slow timer that is used for some basic state machine things
Sub StateTimer_Timer()
    Select Case StateTimerState
        Case STATE_NEW_BALL
            debug.print "STATE_NEW_BALL"
            ResetForNewBall()
            StateTimerState = STATE_BALL_RELEASE

        Case STATE_BALL_RELEASE
            debug.print "STATE_BALL_RELEASE"
            Trough1.Kick 90, 4
            PlaySoundAt "fx_Ballrel", Plunger
            bShowShootAgain = False
            StateTimer.Enabled = False
            ' Timer will be restarted once a switch detects the ball in
            ' the playfield

        Case STATE_END_OF_BALL
            debug.print "STATE_END_OF_BALL"
            If Not Tilted Then
                ' Add bonus here with countup, animation, etc. in the future
                AddScore Bonus * BonusMultiplier
            End If
            StateTimerState = STATE_BALL_OVER

        Case STATE_BALL_OVER
            debug.print "STATE_BALL_OVER"
            If Tilted Then
                SetTilted False
            End If
            If ExtraBallsAwards > 0 Then
                ExtraBallsAwards = ExtraBallsAwards - 1 ' Same ball number
            Else
                BallNumber = BallNumber + 1
            End If
            If BallNumber > BALLS_PER_GAME Then
                BallNumber = 0
                StateTimerState = STATE_GAME_OVER
            Else
                StateTimerState = STATE_NEW_BALL
            End If

        Case STATE_GAME_OVER
            debug.print "STATE_GAME_OVER"
            bGameInPlay = False
            Coils.bCoilPower = False
            'TextBall.Text = "GAME OVER"
            StateTimer.Enabled = False

        Case Else
            debug.print "StateTimer unknown state"
            StateTimer.Enabled = False
    End Select
End Sub