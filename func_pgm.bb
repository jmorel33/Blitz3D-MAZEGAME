Const  gameFPS 		= 50 
Global framePeriod	= 1000 / gameFPS 
Global frameTime 	= MilliSecs () - framePeriod 
Global frameTween#  = 1.0
Global frameTicks, frameElapsed
Global Framecounter_counter, Framecounter_framerate, Framecounter_time

; this is the frames per second timer which makes the system pause when we meet this rate
Global wait_fps = CreateTimer(gameFPS)

; This is the pointer for a screen copy texture space
Global ptr_copyscreen


; these are the game menu constants
Const start_menu             = 100
Const program_menu           = 101
Const game_setup             = 106
Const barcode                = 109

; these are the game constants
Const start_game             = 110
Const game_running           = 111
Const game_resume            = 112
Const game_pause             = 115
Const game_suspend           = 117
Const end_game               = 119

; these are the game title screen constants
Const start_title_screen_1st = 120
Const start_title_screen     = 121
Const title_screen           = 125

; misc
Const program_start          = 001
Const program_exit           = 255


; define the state of the game program

Global program_status = program_start, game_status




; These functions should be used in collaboration with conditions

Function jumping_into_menu()
   If program_status = start_menu Then Return 1 Else Return 0
End Function

Function in_menu()
   If program_status = program_menu Then Return 1 Else Return 0
End Function

Function in_setup()
   If program_status = game_setup Then Return 1 Else Return 0
End Function


Function in_game()
   If program_status = game_running Then Return 1 Else Return 0
End Function

Function game_paused()
   If program_status = game_pause Then Return 1 Else Return 0
End Function

Function in_program()
   If program_status = program_exit Then Return 0 Else Return 1
End Function

Function new_game()
   If program_status = start_game Then Return 1 Else Return 0
End Function

Function game_stopped()
   If program_status = end_game Then Return 1 Else Return 0
End Function

Function program_status$()
   Select program_status
   Case start_menu             Return "start_menu"
   Case program_menu           Return "program_menu"
   Case game_setup             Return "game_setup"
   Case barcode                Return "barcode"
   Case start_game             Return "start_game"
   Case game_running           Return "game_running"
   Case game_resume            Return "game_resume"
   Case game_pause             Return "game_pause"
   Case game_suspend           Return "game_suspend"
   Case end_game               Return "end_game"
   Case start_title_screen_1st Return "start_title_screen_1st"
   Case start_title_screen     Return "start_title_screen"
   Case title_screen           Return "title_screen"
   Case program_start          Return "program_start"
   Case program_exit           Return "program_exit"
   End Select
End Function


Function manage_program()

   If graphics_mode = new_graphics_mode Then

      ; Frame limiting
      Repeat 
         frameElapsed = MilliSecs () - frameTime 
      Until frameElapsed 
      frameTicks  = frameElapsed / framePeriod 
      frameTween# = Float (frameElapsed Mod framePeriod) / Float (framePeriod) 

      Flip
      HidePointer

      ; Update game and world state
      For frameLimit = 1 To frameTicks
         If frameLimit = frameTicks Then CaptureWorld 

         frameTime = frameTime + framePeriod 

         update_mouse_click()
         UpdateCounters()

         manage_flow()
         UpdateWorld
      Next

      RenderWorld frameTween#
      If program_status <> game_running And program_status <> game_pause Then
         CopyRect 0,0,GraphicsWidth(), GraphicsHeight(),-1,-1,BackBuffer(),TextureBuffer(ptr_copyscreen)
      EndIf

      ; FPS counter
      Framecounter_counter = Framecounter_counter + 1
      If Framecounter_time = 0 Then Framecounter_time = MilliSecs()
      If Framecounter_time + 1001 < MilliSecs() Then
         Framecounter_framerate = Framecounter_counter
         Framecounter_counter = 0
         Framecounter_time = MilliSecs()
      EndIf

      clear_unnused_sound_buffers()

      ; display program trace information
      If KeyDown (KEY_SCROLLLOCK) Then TraceProgram()

      ; screen capture and save as file with date/time tag
      If KeyHit (KEY_F12) Then DumpScreen()

      If KeyHit (KEY_F11) Then wireframe_flag = 1 - wireframe_flag : WireFrame wireframe_flag

      ;WaitTimer wait_fps

   Else
      manage_flow()
   EndIf

End Function


Function manage_flow()
      Select program_status
         ; title screen
         Case start_title_screen_1st   program_status = init_title_screen()
         Case start_title_screen       program_status = init_title_screen()
         Case title_screen             program_status = IntitleLoop()

         ; menu management
         Case start_menu               program_status = init_menu()          ; initialize menu screen
         Case game_setup               program_status = InconfigLoop()
         Case barcode                  program_status = InbarcodeLoop()
         Case program_menu             program_status = InmenuLoop()

         ; game management
         Case start_game               program_status = SetupWorld(1, 4)
         Case game_resume              program_status = SetupWorld(3, CurrentGameLevel)
         Case game_running             program_status = IngameLoop()         ; game running loop
         Case game_pause               program_status = GamePause()          ; freezes any action within the game (should be allowed in multiplayer)

         Case end_game                 program_status = SetupWorld(2, CurrentGameLevel) ; exit game
                                       program_status = init_menu()          ; initialize menu screen

         Case game_suspend             program_status = SetupWorld(2, CurrentGameLevel) ; exit game
                                       program_status = init_menu()          ; initialize menu screen

      End Select

End Function
