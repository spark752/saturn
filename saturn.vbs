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
' GameTime = VPX global which gives an internal time in milliseconds. This is
' probably from table start which means it is not actually a game time. It is
' an int and should be good enough for most timing.
' PreciseGameTime = VPX global which gives an internal time in seconds as a
' double. More precise then GameTime but may not be available. There is no
' mention of that in the documentation, but some versions of core.vbs check
' for it and fall back to "GameTime / 1000.0" if not available.

Option Explicit
Randomize

' Name used for FlexDMD and NV (non volatile) storage of high score, etc.
Const TableName = "Saturn"

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
Dim ExtraBallAwards     ' Increases on extra ball award, decrease when used
Dim Score               ' Current score
Dim ScoreMultiplier     ' Scales scoring events for different machines or modes
Dim IsTilted            ' Boolean set when machine is in tilt mode
Dim BallsOnPlayfield    ' Allows for multiball support
Dim IsFreePlay          ' True if credits aren't needed
Dim IsGameInPlay        ' True when playing
Dim IsInPlungerLane     ' Mostly for selecting the right plunger sound
Dim IsSaverActive       ' Ball will be saved for "Shoot Again" if True
Dim IsMultiballActive   ' Multiball not just awarded but in progress
Dim StateTimerState     ' Slow timer based state machine for various things
Dim LastSwitchHit       ' String containing the switch name
Dim GameStartTime       ' Timestamp for start of current game in milliseconds

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
Dim NV
Dim Display

' Flipper physics
Dim LF ' Correction object for LeftFlipper
Dim RF ' Correction object for RightFlipper

' Sets or clears the tilted status. This may be called by the (simulated) tilt
' hardware sensor which doesn't know or care if a game is in progress or already
' tilted, so check those things here if necessary.
Sub SetTilted(Enabled)
    If Enabled Then
        debug.print "SetTilted True"
        IsTilted = True
        IsSaverActive = False
        Coils.IsPowerOn = False
    Else
        debug.print "SetTilted False"
        IsTilted = False
        TiltMech.Reset
        Coils.IsPowerOn = True
    End If
End Sub

' A 50ms timer that is used for hardware logic
Sub HardwareTimer_Timer()
    ' Time decay of tilt value
    TiltMech.Tick
End Sub

' Table events called by VPX
Sub Table1_Init()
    ' Init global variables
    IsInPlungerLane = False
    GameStartTime = GameTime ' GameTime = VPX API

    ' Custom hardware objects
    Set Trough = New hwTrough
    Set TiltMech = New hwTiltMech
    Set FlipperControl = New hwFlipperControl
    Set Coils = New hwCoils
    Set NV = New hwNV
    Set Display = New hwDisplay

    ' Flipper physics
    Set LF = New FlipperPolarity
    Set RF = New FlipperPolarity
    InitPolarity

    ' Make sure flippers are off and timers are set properly
    FlipperControl.BothDeactivate
    StateTimer.Enabled = False
    HardwareTimer.Enabled = True

    Software_Init
End Sub

Sub Table1_Exit()
    ' Delete all hardware objects to make sure their terminate methods run
    Trough = NULL
    TiltMech = NULL
    FlipperControl = NULL
    Coils = NULL
    NV = NULL
    Display = NULL
End Sub

' PlungerTrigger is an invisible virtual switch located just above the plunger.
' It is only used to control which sound the plunger makes.
Sub PlungerTrigger_Hit()
    IsInPlungerLane = True
End Sub

Sub PlungerTrigger_UnHit()
    IsInPlungerLane = False
End Sub

' Immediately add to score
Sub AddScore(Points)
    If IsTilted Then Exit Sub
    Score = Score + Points
End Sub

' Add to end of ball bonus score which can be effected by multipliers
Sub AddBonus(Points)
    If IsTilted Then Exit Sub
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

' Start button
Sub swStart_Hit()
    debug.print "swStart_Hit " & BallsOnPlayfield & " " & IsGameInPlay
    If BallsOnPlayfield < 1 And Not IsGameInPlay Then
        If IsFreePlay Then
            StartGame
        ElseIf Credits > 0 Then
            Credits = Credits - 1
            StartGame
        End If
    End If
End Sub

' Coin accepted switch
Sub swCredit_Hit()
    If Not IsFreePlay Then AddCredits 1
    Display.CoinIn
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
    If IsGameInPlay Then
        If BallsOnPlayfield < 1 Then
            'If IsSaverActive Then
            '   IsSaverActive = False
            '    StateTimerState = STATE_SHOOT_AGAIN
            '    StateTimer.Enabled = True
            'Else
                StateTimerState = STATE_END_OF_BALL
                StateTimer.Enabled = True
            'End If
        ElseIf BallsOnPlayfield = 1 Then
            IsMultiballActive = False
        End If
    End If
End Sub

Sub Software_Init()
    ' Should initialize all software globals, even if also set in StartGame
    Credits = 0
    Bonus = 0
    BonusMultiplier = 1
    BallNumber = 0
    ExtraBallAwards = 0
    Score = 0
    ScoreMultiplier = 1
    BallsOnPlayfield = 0
    IsGameInPlay = False
    IsSaverActive = False
    IsMultiballActive = False
    StateTimerState = STATE_NEW_BALL
    LastSwitchHit = ""
    Display.SetText "SYSTEM", "RESET"
    Display.SetScene DISP_TWO_ROWS
    Display.AttractMode True
End Sub

Sub StartGame()
    debug.print "StartGame"

    ' Only game start things need to be set here since the others will be
    ' set by ResetForNewBall
    Score = 0
    IsGameInPlay = True
    BallsOnPlayfield = 0
    TiltMech.Reset
    SetTilted False ' Resets tilt mech and turns on IsPowerOn
    Game_Init() ' Table specific hook
    GameStartTime = GameTime ' GameTime = VPX API
    BallNumber = 1
    Display.SetScene DISP_SCOREBOARD
    Display.AttractMode False
    StateTimerState = STATE_NEW_BALL
    StateTimer.Interval = 500
    StateTimer.Enabled = True
End Sub

Sub ResetForNewBall()
    debug.print "ResetForNewBall"
    Bonus = 0
    BonusMultiplier = 1
    ExtraBallAwards = 0
    BallsOnPlayfield = 0
    IsInPlungerLane = False
    IsSaverActive = False
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
            Trough.Release
            PlaySoundAt "fx_Ballrel", Plunger
            StateTimer.Enabled = False
            ' Timer will be restarted once a switch detects the ball in
            ' the playfield

        Case STATE_END_OF_BALL
            debug.print "STATE_END_OF_BALL"
            If Not IsTilted Then
                ' Add bonus here with countup, animation, etc. in the future
                AddScore Bonus * BonusMultiplier
            End If
            StateTimerState = STATE_BALL_OVER

        Case STATE_BALL_OVER
            debug.print "STATE_BALL_OVER"
            If IsTilted Then
                SetTilted False
            End If
            If ExtraBallAwards > 0 Then
                ExtraBallAwards = ExtraBallAwards - 1 ' Same ball number
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
            IsGameInPlay = False
            Coils.IsPowerOn = False
            NV.EndOfGame Score, ((GameTime - GameStartTime) * 0.001)
            Display.AttractMode True
            StateTimer.Enabled = False

        Case Else
            debug.print "StateTimer unknown state"
            StateTimer.Enabled = False
    End Select
End Sub

' Main 10 ms timer
Dim TickCount
TickCount = 0
HardwareTimer.Enabled = True
Sub HardwareTimer_Timer()
    TickCount = TickCount + 1

    ' 10 ms
    Rolling_Tick
    Physics_Tick

    ' 20 ms on even tick
    If TickCount Mod 2 = 0 Then Display.Tick

    ' 200 ms on odd tick
    If TickCount Mod 20 = 1 Then Trough.Tick
End Sub
