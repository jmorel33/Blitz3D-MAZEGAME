AppTitle "MAZEGAME"

Include "func_pgm.bb"

Include "func_mesh.bb"

Include "func_controls.bb"
Include "func_data.bb"

Include "game_data.bb"

;----------------------------------------------------------------------------------------
; Define Global things
;----------------------------------------------------------------------------------------

Const test_layout = 0                     ; set to 1 to test a maze pattern

; general purpose physics constants
Const GRAVITY# = .00981 * (100 / gameFPS) ; adjusted with the frames per seconds
Const AIR_FRICTION# = 0.01
Const GROUND_FRICTION# = 0.0030

; for collision
Const projectile_type = 1
Const world_type      = 2
Const habitant_type   = 4

Global MusicChannel

Global wireframe_flag

Global nb_graphics_drivers, graphics_driver, new_graphics_driver
Global nb_graphics_mode, graphics_mode, new_graphics_mode
Global antialias_mode = 0, windowed_mode = 0, dithering_mode = 0
Global ptr_fnt, ptr_fnt2, ptr_fnt3
Global fontsize20#, fontsize40#

Global work_directory$ = "C:"

Global smooth_mouse_x#, smooth_mouse_y#

Global menu_pointer.menu_mouse_info = New menu_mouse_info

; this is a counter for when there is no activity going on around (no user action)
; currently used in the menus
Global doing_nothing_fps

; menu_values
Global ptr_menutex1, ptr_menutex2
Dim ptr_menucube(3), menu_cycle(5)

; Dummy Globals
Dim ptr_texture(0),sin_tb#(1079),cos_tb#(1079),sin_tb10#(7200),cos_tb10#(7200)
Dim maze(0,0), maze_lview(0,0)

; general purpose sine / cosine table
For i=0 To 1079: sin_tb#(i)=Sin(i): cos_tb#(i)=Cos(i): Next
For i=0 To 7200: j# = i:sin_tb10#(i) = Sin(j#/10): cos_tb10#(i) = Cos(j#/10): Next

; general purpose matrix operator
Dim getcos(3), getsin(3): getcos(0)=1: getcos(2)=-1: getsin(1)=1: getsin(3)=-1

; player's current level basic data
Global CurrentLevel.level_info = New level_info, CurrentGameLevel

; value that counts the number of maze walkable space
Global maze_boxes

; this value determines the scale in which the maze is managed from real coordinates and
; down To the maze grid.
Global maze_scale# = 20

; general purpose pointers
Global ptr_ground, ptr_light, ptr_mirror

Global cam_viewport_pitch#, cam_viewport_yaw#

;this is a per frame count of the number of projectiles managed
Global projectilecount

Global scrolltext$, cuberoom, newscrollitem, main_counter360, sc_offset

Global Half_Framerate   = 1   ;switch for only rendering texture every second loop

Global mouse_speed# = 2

Global fx_direction#, fx_u#, fx_v#

; These are the wall unit and sphere variables
Global sphere1, sphere2, sphere3, sphere4, huge_sphere, rotating_sphere, ingame_counter360
Dim wall_texture_number(5), wall_rotate(5), wall_angle(5)
Dim sphere_direction#(5), sphere_height#(5)
Dim walleffect(8), wallfxzoom(8), walldisplace#(8)

; most important
Global ptr_camera, ptr_microphone

Global number_of_players = 0
Global local_player = 1
Const max_number_of_players = 32

; this is the table that will store all pointers to player data, 32 being the max amount
; of simultaneous players
Dim player.habitant_info (max_number_of_players)


main()
End

;----------------------------------------------------------------------------------------
; Main Loop
;----------------------------------------------------------------------------------------
Function main()

   program_status = StartProgram()

   While in_program()
      manage_program()
   Wend

End Function


Function UpdateCounters()
   Half_Framerate = 1 - Half_Framerate     ;Half_Framerate switches between 1 and 0 (1-1=0 -> 1-0=1)
   main_counter360 = (main_counter360 + 1) Mod 360
End Function


Function TraceProgram()
   SetFont ptr_fnt2
   Color 255, 64, 64
   font_halfsize# = fontsize20# / 2
   Text 0, font_halfsize# * 25, program_status$()

   Select program_status
   Case game_running
      Text 0,0,                 "X = " + Int(player(1)\x#)
      Text font_halfsize# * 4,0,"Y = " + Int(player(1)\y#)
      Text font_halfsize# * 9,0,"Z = " + Int(player(1)\z#)
      Text font_halfsize# * 13,0,cam_viewport_yaw#

    ;  Text 0,font_halfsize#,                 "vX= " + player(1)\vx#
    ;  Text font_halfsize# * 4,font_halfsize#,"vY= " + player(1)\vy#
    ;  Text font_halfsize# * 9,font_halfsize#,"vZ= " + player(1)\vz#

      Text 0,font_halfsize# * 2,    "Maze coordinates   (" + player(1)\grid_xpos + "," + player(1)\grid_ypos + ")"
      Text 0,font_halfsize# * 3,"Projectiles active: " + projectilecount

   Case title_screen
      Text 0,0, doing_nothing_fps

   Case program_menu
      Text 0,0, doing_nothing_fps

   End Select

   Text 0,font_halfsize# * 4,   "Triangles Rendered: " + TrisRendered()
   Text 0,font_halfsize# * 5,   "Frames Per Second : " + Framecounter_framerate
   Text 0,font_halfsize# * 6,   "video memory used : " + ((TotalVidMem() - AvailVidMem()) / 1024)

End Function


Function DumpScreen()
   fn$ = "MAZEGAME - " + CurrentDate$() + " - " + Str MilliSecs() + ".BMP"
   SetFont ptr_fnt2
   Color 0,0,0
   Text GfxModeWidth(graphics_mode) / 2 - 2, GfxModeHeight(graphics_mode) - fontsize20# / 2 - 2, fn$
   Text GfxModeWidth(graphics_mode) / 2 + 2, GfxModeHeight(graphics_mode) - fontsize20# / 2 + 2, fn$
   Color 255, 255, 255
   Text GfxModeWidth(graphics_mode) / 2, GfxModeHeight(graphics_mode) - fontsize20# / 2, fn$
   SaveBuffer(BackBuffer(),work_directory$ + fn$)
   FlushKeys

End Function


;----------------------------------------------------------------------------------------
; Game world management
;----------------------------------------------------------------------------------------
Function SetupWorld(option, lvl)

   Select option

      Case 1         ; start a new game
      game_status = game_running
      InitNewGame(lvl, local_player)
      InitWorld(local_player)
      insert_local_player_in_world(local_player)
      Return game_running

      Case 2         ; stop gameplay
      game_status = game_suspend
      remove_player_from_world(local_player)
      DeleteWorld()
      Return start_menu

      Case 3         ; resume gameplay
      game_status = game_running
      InitWorld(local_player)
      insert_local_player_in_world(local_player)
      Return game_running

   End Select

End Function


;----------------------------------------------------------------------------------------
Function StartProgram()
   new_graphics_driver = 1
   nb_graphics_drivers = CountGfxDrivers()
   If nb_graphics_drivers = 0 Then Print "No graphics drivers!":WaitKey:End

   cnt = init_modetable()
   If cnt = 0 Print "No 3D Graphics modes detected, exiting...":WaitKey:End

   new_graphics_mode = find_graphicsmode(800,600,32)
   If new_graphics_mode = 0 Then
      new_graphics_mode = find_graphicsmode(640,480,32)
      If new_graphics_mode = 0 Then
         new_graphics_mode = find_graphicsmode(320,240,32)
         If new_graphics_mode = 0 Then Print "800 x 600, 640 x 480 and 320 x 240 32 bit not supported..." : WaitKey : End
      EndIf
   EndIf
   InitLevelTable()
   InitMazewallTable()

   If test_layout Then
      Graphics 1600,1200,0,0
      Text 1150,0,"boxes="
      init_maze(1)

      WaitKey
      End

   EndIf

   init_sound()

   Return start_title_screen_1st
End Function


Function set_graphics_mode()
   If program_status <> start_title_screen_1st Then
      FreeFont ptr_fnt2
      FreeFont ptr_fnt3
      FreeTexture ptr_copyscreen
      ClearWorld() 
   EndIf

   set_graphics_driver()

   If new_graphics_mode <> graphics_mode Then
      graphics_mode = new_graphics_mode
      Graphics3D GfxModeWidth(graphics_mode), GfxModeHeight(graphics_mode), GfxModeDepth(graphics_mode), windowed_mode + 1

      fontsize20# = GfxModeWidth(graphics_mode) / 20
      fontsize40# = GfxModeWidth(graphics_mode) / 40
   EndIf
   HidePointer

   If GfxModeDepth(graphics_mode) = 32 Then dithering_mode = 0
   Dither dithering_mode
   AntiAlias antialias_mode
   ptr_copyscreen = CreateTexture(GraphicsWidth(), GraphicsHeight(), 256)

   ; load the needed fonts as they get removed every screen resolution change
   ptr_fnt2 = LoadFont("Courrier", fontsize20#/2, True, False, False)
   ptr_fnt3 = unpack_loadfont("ATASCII", fontsize20#, False, False, False)

   SetBuffer BackBuffer()

End Function 


Function set_graphics_driver()
   If new_graphics_driver <> graphics_driver Then
      graphics_driver = new_graphics_driver
      SetGfxDriver graphics_driver
      cnt = init_modetable()
      If cnt < graphics_mode Then new_graphics_mode = cnt
      If Windowed3D() = False And windowed_mode = 1 Then windowed_mode = 0
   EndIf

End Function


Function unpack_loadfont(fontname$, fs#, f1, f2, f3)
   Local file_ptr, fi$
   ;If fontname$ = "ATASCII" Then Restore ATASCII
   ;ExportDataToBinary(fi$)

   font_ptr = LoadFont(fontname$, fs#, f1, f2, f3)
   Return font_ptr
End Function

Function test_font(fn$)
   font_ptr = LoadFont(fn$)
   FreeFont (font_ptr)
   Return font_ptr
End Function


; this is the soundspace data
Type sound_data
   Field ptr
   Field filename[20]
End Type


Function unpack_playmusic(songname$)
; unpack_playmusic("titlescreentrack")
   Select songname$
      Case "titlescreentrack"   Restore titlescreentrack
      Case "titlescreentrack"   Restore titlescreentrack
   End Select

   fi$ = work_directory$ + songname$ + ".XM"
   ExportDataToBinary(fi$)
   channel = PlayMusic(fi$)
   DeleteFile fi$

   Return channel
End Function


Function unpack_loadwav(wavname$,ext$)
   Select wavname$
      Case "RimShot"     Restore RimShot
      Case "TR909clap"   Restore TR909clap
      Case "chord1"      Restore chord1
      Case "pick1"       Restore pick1
      Case "shoot1"      Restore shoot1
      Case "shoot2"      Restore shoot2
      Case "shoot3"      Restore shoot3
      Case "tick1"       Restore tick1
      Case "tick2"       Restore tick2
      Case "rocket1"     Restore rocket1
      Case "lazer1"      Restore lazer1


   End Select

   fi$ = work_directory$ + wavname$ + ext$

   ExportDataToBinary(fi$)

   sound.sound_data = New sound_data
   sound\ptr = Load3DSound(fi$)
   If sound\ptr = 0 Then Print "boom... ": Print fi$:WaitKey:End
   For j = 1 To Len(wavname$)
      sound\filename[j] = Asc(Mid$(wavname$,j,1))
   Next

   DeleteFile fi$

End Function


Function init_sound()
   unpack_loadwav("TR909clap","MP3")
   unpack_loadwav("RimShot","MP3")
   unpack_loadwav("chord1","MP3")
   unpack_loadwav("pick1","MP3")
   unpack_loadwav("shoot1","MP3")
   unpack_loadwav("shoot2","MP3")
   unpack_loadwav("shoot3","MP3")
   unpack_loadwav("tick1","MP3")
   unpack_loadwav("tick2","MP3")
   unpack_loadwav("rocket1","MP3")
   unpack_loadwav("lazer1","MP3")

End Function


Type sound_buffer
   Field ptr
End Type

Function emit_sound(sn$)
   For sound.sound_data = Each sound_data
      n$ = ""
      For j = 1 To Len(sn$)
         n$ = n$ + Chr$(sound\filename[j])
      Next
      If n$ = sn$ Then
         chan_ptr = PlaySound (sound\ptr)
         sound_emitted.sound_buffer = New sound_buffer
         sound_emitted\ptr = chan_ptr
         Return
      EndIf
   Next
   Print "Charlie, " + sn$ + " was not found. bye."
   Print "check this out :" + n$
   WaitKey:End

End Function


Function emit_3D_sound(sn$, ptr_source)
   For sound.sound_data = Each sound_data
      n$ = ""
      For j = 1 To Len(sn$)
         n$ = n$ + Chr$(sound\filename[j])
      Next
      If n$ = sn$ Then
         chan_ptr = EmitSound (sound\ptr, ptr_source)
         sound_emitted.sound_buffer = New sound_buffer
         sound_emitted\ptr = chan_ptr
         Return
      EndIf
   Next
   Print "Charlie, " + sn$ + " was not found. bye."
   Print "check this out :" + n$
   WaitKey:End

End Function


Function clear_unnused_sound_buffers()
   For sound_emitted.sound_buffer = Each sound_buffer
      If Not ChannelPlaying(sound_emitted\ptr) Then
         StopChannel(sound_emitted\ptr)
         Delete sound_emitted.sound_buffer
      EndIf
   Next
End Function

;----------------------------------------------------------------------------------------
; title screen management
;----------------------------------------------------------------------------------------
Function init_title_screen()

   set_graphics_mode()
   doing_nothing_fps = 0

   ptr_camera = CreateCamera()
  ; ptr_microphone = CreateListener (ptr_camera, 1, 1, 1)

   ptr_light = CreateLight()
   LightColor ptr_light,0,0,0
   RotateEntity ptr_light,90,0,0
   AmbientLight 0,0,0

   t_obj.object_info = New object_info
   t_obj\number = 255
   t_obj\ptr = CreateTexture(fontsize20# * 8, fontsize20#, 256)
   t_obj\object_type = 2
   SetBuffer TextureBuffer(t_obj\ptr)
   ClsColor 255,255,0
   Cls
   Color 255, 0, 0
   SetFont ptr_fnt3
   Text fontsize20# * 2.5, fontsize20# * 0.30, "MAZEGAME", False, False
   t_tex = t_obj\ptr

   For z = 1 To 6
      For x = -z To z
         For y = -z To z
            t_obj.object_info = New object_info
            t_obj\ptr = CreateCylinder(36 - z * 4)
            t_obj\object_type = 1
            RotateEntity t_obj\ptr,15 * x,0,-15 * y
            ScaleMesh t_obj\ptr,8,2,8
            PositionEntity t_obj\ptr,32 * x, 32 * y,32 * z
            EntityTexture t_obj\ptr,t_tex,0,1 
         Next
      Next
   Next

   CameraFogMode ptr_camera,1
   CameraFogRange ptr_camera,64,224

   SetBuffer BackBuffer()

   MusicChannel = unpack_playmusic("titlescreentrack")

   main_counter360 = 345

   Return title_screen
End Function


Function IntitleLoop()
   If doing_nothing_fps <= 250 Then
      lc = doing_nothing_fps 
      AmbientLight lc,lc,lc
   ElseIf doing_nothing_fps >= 775 Then
      lc = (900 - doing_nothing_fps) * 2
      AmbientLight lc,lc,lc
   Else
      lc = 250
   EndIf

   lx = (cos_tb#(main_counter360 * 2 + 215) * 30)
   ly = (sin_tb#(main_counter360 * 2 + 215) * 30)
   LightColor ptr_light, lx * 8, ly * 8, ly * 8
   PositionEntity ptr_light,lx,ly,0

   If doing_nothing_fps < 500 Then
      PositionEntity ptr_camera,0,0,(cos_tb#(main_counter360 * 2 + 120) * 15)
      For t_obj.object_info = Each object_info
         If t_obj\object_type = 1 Then
            TurnEntity t_obj\ptr,0,sin_tb#(main_counter360 * 2 + 270) * 2, sin_tb#(main_counter360 * 2) * 2.5
         EndIf
      Next

   ElseIf doing_nothing_fps > 700 Then
      SetFont ptr_fnt2
      Color lc, lc, lc
      txt$ = "by Jacques Morel"
      Text GraphicsWidth() / 2, GraphicsHeight() - 30, txt$, True, False


   ElseIf doing_nothing_fps > 550 Then
      SetFont ptr_fnt2
      Color lc, lc, lc
      txt$ = "=== LIMITED DEMO VERSION ==="
      Text GraphicsWidth() / 2, GraphicsHeight() - 30, txt$, True, False

   EndIf

   doing_nothing_fps = doing_nothing_fps + 1
   If GetKey() Then doing_nothing_fps = 900
   If doing_nothing_fps = 900 Then
      close_title_screen()
      Return start_menu
   Else
      Return title_screen
   EndIf
End Function


Function close_title_screen()
   For t_obj.object_info = Each object_info
      If t_obj\object_type = 1 Then
         FreeEntity t_obj\ptr
      ElseIf t_obj\object_type = 2 Then
         FreeTexture t_obj\ptr
      EndIf
      Delete t_obj.object_info
   Next

End Function

;----------------------------------------------------------------------------------------
; menu management
;----------------------------------------------------------------------------------------
Function init_menu()
   set_graphics_mode()

   ptr_menutex2 = CreateTexture(64, 64)

   ptr_camera = CreateCamera()
   ;ptr_microphone = CreateListener (ptr_camera, 1, 1, 1)

   AmbientLight 127,127,127

   ptr_light = CreateLight()
   LightColor ptr_light,128,192,255
   RotateEntity ptr_light,90,0,0

   ptr_menucube(1) = create_flat(10)
   ScaleMesh ptr_menucube(1),4,3,0.1
   EntityTexture ptr_menucube(1), ptr_menutex2

   ptr_menucube(2) = create_cube(10)
   ScaleMesh ptr_menucube(2), 10, 10, 10
   RotateEntity ptr_menucube(2),0,45,45
   PositionEntity ptr_menucube(2), 22, 0, 20
   EntityTexture ptr_menucube(2),ptr_copyscreen

   ptr_menucube(3) = create_cube(10)
   ScaleMesh ptr_menucube(3), 10, 10, 10
   RotateEntity ptr_menucube(3),0,-45,-45
   PositionEntity ptr_menucube(3), -22, 0, 20
   EntityTexture ptr_menucube(3),ptr_copyscreen

   ClsColor 0,0,0
   SetBuffer BackBuffer()

   menu_cycle(1) = 0
   menu_cycle(2) = 0
   menu_cycle(3) = 0
   menu_cycle(4) = 0
   menu_cycle(5) = 0

   main_counter360 = 233

   doing_nothing_fps = 0

   FlushKeys

   Return program_menu

End Function


Function InmenuLoop()
   lx = (cos_tb#(main_counter360 * 2) * 30)
   ly = (sin_tb#(main_counter360 * 2) * 30)
   LightColor ptr_light, lx * 8, ly * 8, ly * 8
   PositionEntity ptr_light,lx,ly,0

   color_scroll()
   menu_counter_inc()

   PositionEntity ptr_menucube(1),0,-0.8 + sin_tb#(main_counter360 * 2),11 + sin_tb#(main_counter360) * 8
   TurnEntity ptr_menucube(2), 0.5,0,0
   TurnEntity ptr_menucube(3), 0.5,0,0

   menu_mouse_pointer()

   menutext$(0,0,"<<<< MAIN  MENU >>>>",5)
   F1 = menutext$(1,5,"[ F1 ] SETUP",1)
   F2 = menutext$(1,7,"[ F2 ] NEW GAME",1)
   If game_status = game_suspend Then
      F3 = menutext$(1,8,"[ F3 ] RESUME",1)
      If KeyHit (KEY_F3) Or F3 Then emit_sound("TR909clap") : Return game_resume
   EndIf

   F9 = menutext$(1,10,"[ F9 ] EXIT",1)
   menutext$(0,13," SELECT OPTION :",0)

   If KeyHit (KEY_F1) Or F1 Then emit_sound("tick2") : Return game_setup
   If KeyHit (KEY_F2) Or F2 Then emit_sound("tick2") : Return start_game
   If KeyHit (KEY_F9) Or F9 Then emit_sound("TR909clap") : Return program_exit

   ; flip between title screen and main menu screen
   doing_nothing_fps = doing_nothing_fps + 1
   If GetKey() Or MouseXSpeed() Or MouseYSpeed() Then doing_nothing_fps = 0

   If doing_nothing_fps > 950 Then 
      Return start_title_screen
   Else
      Return program_menu
   EndIf

End Function


Function InconfigLoop()
Local txt$, dummy
   lx = (cos_tb#(main_counter360 * 2) * 30)
   ly = (sin_tb#(main_counter360 * 2) * 30)
   LightColor ptr_light, lx * 8, ly * 8, ly * 8
   PositionEntity ptr_light,lx,ly,0

   color_scroll()
   menu_counter_inc()

   PositionEntity ptr_menucube(1),sin_tb#(main_counter360 * 2) * 2,-0.8 + sin_tb#(main_counter360) * 4,11 + cos_tb#(main_counter360) * 8
   TurnEntity ptr_menucube(2), sin_tb#(main_counter360),0,0
   TurnEntity ptr_menucube(3), sin_tb#(main_counter360),0,0

   menu_mouse_pointer()

   menutext$(0,0," ** SCREEN SETUP **",5)

   txt$ = "[F1-F2] " + Left$(get_graphicsinfo$(new_graphics_mode),9)
   F1F2 = menutext$(0,3,txt$,2)
   menutext$(0,4,"        " + Right$(get_graphicsinfo$(new_graphics_mode),10),9)

   txt$ = "[F3-F4] " + Left$(GfxDriverName$(new_graphics_driver),12)
   F3F4 = menutext$(0,5,txt$,2)

   If dithering_mode = 0 Then txt$ = "dither OFF" Else txt$ = "dither ON"
   F5 = menutext$(1,6,"[ F5 ] " + txt$,1)

   If antialias_mode = 0 Then txt$ = "a-alias OFF" Else txt$ = "a-alias ON"
   F6 = menutext$(1,7,"[ F6 ] " + txt$,1)

   If windowed_mode = 0 Then txt$ = "fullscreen" Else txt$ = "windowed"
   F7 = menutext$(1,8,"[ F7 ] " + txt$,1)
   F8 = menutext$(1,9,"[ F8 ] test pattern",1)

   F9 = menutext$(1,11,"[ F9 ] RETURN",1)
   menutext$(1,13," SELECT OPTION :",0)

   If KeyHit (KEY_F1) Or F1F2 = 1 Then
      If new_graphics_mode > 1 Then
         emit_sound("tick2")
         new_graphics_mode = new_graphics_mode - 1
         dummy = init_menu()
      EndIf
   EndIf

   If KeyHit (KEY_F2) Or F1F2 = 2 Then
      If new_graphics_mode < nb_graphics_mode Then
         emit_sound("tick2")
         new_graphics_mode = new_graphics_mode + 1
         dummy = init_menu()
      EndIf
   EndIf

   If KeyHit (KEY_F3) Or F3F4 = 1 Then
      If new_graphics_driver > 1 Then
         emit_sound("tick2")
         new_graphics_driver = new_graphics_driver - 1
         dummy = init_menu()
      EndIf
   EndIf

   If KeyHit (KEY_F4) Or F3F4 = 2 Then
      If new_graphics_driver < nb_graphics_drivers Then
         emit_sound("tick2")
         new_graphics_driver = new_graphics_driver + 1
         dummy = init_menu()
      EndIf
   EndIf

   If KeyHit (KEY_F5) Or F5 Then
      If GfxModeDepth(new_graphics_mode) = 32 And dithering_mode = 0 Then
         Return game_setup
      Else
         emit_sound("tick2")
         dithering_mode = 1 - dithering_mode
         dummy = init_menu()
      EndIf
   EndIf

   If KeyHit (KEY_F6) Or F6 Then emit_sound("tick2") : antialias_mode = 1 - antialias_mode : dummy = init_menu()

   If KeyHit (KEY_F7) Or F7 Then
      emit_sound("tick2")
      If Windowed3D() = True Then
         windowed_mode = 1 - windowed_mode
         graphics_mode=0
         dummy = init_menu()
      EndIf
   EndIf

   If KeyHit (KEY_F8) Or F8 Then emit_sound("tick2") : Return barcode

   If KeyHit (KEY_F9) Or F9 Then
      emit_sound("TR909clap")
      If new_graphics_mode <> graphics_mode Then
         dummy = init_menu()
      EndIf
      Return program_menu
   EndIf

   Return game_setup

End Function


Function InbarcodeLoop()
   x# = GfxModeWidth(graphics_mode) - 1
   y# = GfxModeHeight(graphics_mode) - 1
   
   p1# = 0.66
   p2# = 0.09
   p3# = 0.25
   bw# = Int (x# / 7.0 + 0.5)
   
   menu_counter_inc()

   Color 192,192,192 : Rect 0                       , 0                , bw#                           , y# * p1#
   Color 255,255,0   : Rect bw#                     , 0                , bw#                           , y# * p1#
   Color 0,255,255   : Rect bw# * 2                 , 0                , bw#                           , y# * p1#
   Color 0,255,0     : Rect bw# * 3                 , 0                , bw#                           , y# * p1#
   Color 255,0,255   : Rect bw# * 4                 , 0                , bw#                           , y# * p1#
   Color 255,0,0     : Rect bw# * 5                 , 0                , bw#                           , y# * p1#
   Color 0,0,255     : Rect bw# * 6                 , 0                , bw#                           , y# * p1#

   Color 0,0,255     : Rect 0                       , y# * p1#         , bw#                           , y# * p2#
   Color 0,0,0       : Rect bw#                     , y# * p1#         , bw#                           , y# * p2#
   Color 255,0,255   : Rect bw# * 2                 , y# * p1#         , bw#                           , y# * p2#
   Color 0,0,0       : Rect bw# * 3                 , y# * p1#         , bw#                           , y# * p2#
   Color 0,255,255   : Rect bw# * 4                 , y# * p1#         , bw#                           , y# * p2#
   Color 0,0,0       : Rect bw# * 5                 , y# * p1#         , bw#                           , y# * p2#
   Color 192,192,192 : Rect bw# * 6                 , y# * p1#         , bw#                           , y# * p2#

   Color 32,64,96    : Rect 0                       , y# * (p1# + p2#) , x# * 0.18 + 1                 , y# * p3# + 1
   Color 255,255,255 : Rect x# * 0.18               , y# * (p1# + p2#) , x# * 0.18 + 1                 , y# * p3# + 1
   Color 64,0,128    : Rect x# * 0.18 * 2           , y# * (p1# + p2#) , x# * 0.18 + 1                 , y# * p3# + 1
   Color 0,0,0       : Rect x# * 0.18 * 3           , y# * (p1# + p2#) , bw# * 2                       , y# * p3# + 1
   Color 32,32,32    : Rect x# * 0.18 * 3 + 2 * bw# , y# * (p1# + p2#) , bw# * 0.25                    , y# * p3# + 1
   Color 0,0,0       : Rect bw# * 6                 , y# * (p1# + p2#) , bw#                           , y# * p3# + 1
 
 ;  goodvibes = Abs(Int(sin_tb#(main_counter360 * 2)))
 ;  If goodvibes = 1 Then
 ;     Color goodvibes * 255,goodvibes * 255,goodvibes * 255
 ;     Text 0,fontsize20# * 5," [ F9 ] RETURN"
 ;  EndIf

   menu_mouse_pointer()
   F9 = menutext$(1,0,"[ F9 ] RETURN",1)

   If KeyHit (KEY_F9) Or F9 Then emit_sound("TR909clap") : Return game_setup

   Return barcode

End Function


;----------------------------------------------------------------------------------------
Type menu_mouse_info
   Field mouse_x
   Field mouse_y
   Field char_x
   Field char_y
   Field OnNewOption
   Field option_xb
   Field option_xe
   Field option_y
End Type

Function menu_mouse_pointer()
   Local x# = Int (MouseX() / fontsize20#): If x > 19 Then x = 19
   Local yf = Int (GraphicsHeight() / fontsize20#) - 1
   Local y# = Int (MouseY() / fontsize20#): If y > yf Then y = yf
   menu_pointer\mouse_x = x# * fontsize20#
   menu_pointer\mouse_y = y# * fontsize20#
   menu_pointer\char_x = menu_pointer\mouse_x / fontsize20#
   menu_pointer\char_y = menu_pointer\mouse_y / fontsize20#

   If menu_pointer\char_x < menu_pointer\option_xb Or menu_pointer\char_x > menu_pointer\option_xe Or menu_pointer\char_y <> menu_pointer\option_y Then
      menu_pointer\OnNewOption = 0
   EndIf

   If menu_pointer\OnNewOption = 1 Then emit_sound("RimShot")

   c = menu_cycle(5)
   Color c,c,127
   Rect menu_pointer\mouse_x,menu_pointer\mouse_y,fontsize20#,fontsize20#
End Function


Function menutext$(x, y, txt$, click)
   Local ret_status = 0
   Local c = 255

   Select click
   Case 0
      Color c,c,c
      SetFont ptr_fnt3
      Text fontsize20# * x,fontsize20# * y,txt$, False, False

   Case 1,2,3
      If (menu_pointer\char_x >= x And menu_pointer\char_x <= (x + Len(txt$) - 1)) And menu_pointer\char_y = y Then 
         c = 255 - menu_cycle(5)
         If mouse_click(1) = 1 Then ret_status = 1 
         If mouse_click(2) = 1 And click = 2 Then ret_status = 2 
         If mouse_click(3) = 1 And click = 3 Then ret_status = 3
         If menu_pointer\OnNewOption = 0 Then
            menu_pointer\OnNewOption = 1
            menu_pointer\option_xb = x
            menu_pointer\option_xe = (x + Len(txt$) - 1)
            menu_pointer\option_y = y
         ElseIf menu_pointer\OnNewOption = 1 Then
            menu_pointer\OnNewOption = 2
         EndIf
      EndIf
      Color c,c,c
      SetFont ptr_fnt3
      Text fontsize20# * x,fontsize20# * y,txt$, False, False

   Case 5
      SetFont ptr_fnt3
      d = menu_cycle(5)
      For i = 0 To (Len(txt$) - 1)
         cc$ = Mid$(txt$,i + 1,1)
         d = (d + 12) Mod 255
         Color d,d,d
         Text fontsize20# * (i + x), fontsize20# * y, cc$, False, False
      Next

   Case 9
      SetFont ptr_fnt3
      Text fontsize20# * x,fontsize20# * y,txt$, False, False

   End Select

   Return ret_status

End Function
;----------------------------------------------------------------------------------------


Function color_scroll()
Local i,j,k,x,y

   SetBuffer TextureBuffer (ptr_menutex2)
   LockBuffer TextureBuffer (ptr_menutex2)

   For y = 0 To 63

      menu_cycle(3) = (menu_cycle(3) + 32) Mod 255
      i = (menu_cycle(3) + menu_cycle(1)) Mod 255
      j = (menu_cycle(3) + menu_cycle(2)) Mod 255

      For x = 0 To 63
         k = (menu_cycle(1) + x * 16) Mod 255
         WritePixelFast x, y, i + (k Shl 8) + (j Shl 16)
      Next
   Next

   UnlockBuffer TextureBuffer (ptr_menutex2)
   SetBuffer BackBuffer()

End Function


Function menu_counter_inc()
   If in_menu() Then 
      menu_cycle(1) = menu_cycle(1) + sin_tb#(main_counter360) * 15
      menu_cycle(2) = menu_cycle(2) + 10
      menu_cycle(4) = menu_cycle(2) + cos_tb#(main_counter360 * 1.5) * 30
   Else
      menu_cycle(1) = menu_cycle(1) + cos_tb#(main_counter360) * 10
      menu_cycle(2) = menu_cycle(2) + cos_tb#(main_counter360 * 1.5) * 20
      menu_cycle(4) = menu_cycle(2) + sin_tb#(main_counter360) * 60
   EndIf
   menu_cycle(5) = menu_cycle(5) + 12

   menu_cycle(1) = menu_cycle(1) Mod 255
   menu_cycle(2) = menu_cycle(2) Mod 255
   menu_cycle(4) = menu_cycle(4) Mod 255
   menu_cycle(5) = menu_cycle(5) Mod 255

End Function


Type Graphics_mode_item
   Field gmode
   Field width
   Field height
   Field depth
   Field fontsize20#
End Type


Function init_modetable()
   nb_graphics_mode = CountGfxModes3D()

   For mode.Graphics_mode_item = Each Graphics_mode_item
      Delete mode.Graphics_mode_item
   Next

   For k = 1 To nb_graphics_mode
	  mode.Graphics_mode_item = New Graphics_mode_item
	  mode\gmode    = k
	  mode\width    = GfxModeWidth(k)
	  mode\height   = GfxModeHeight(k)
	  mode\depth    = GfxModeDepth(k)
	  mode\fontsize20# = GfxModeHeight(k) / 20
   Next
	
   Return k
End Function


Function get_graphicsinfo$(gmode)
Local txt$

   For mode.Graphics_mode_item = Each Graphics_mode_item
      If mode\gmode = gmode Then 
         txt$ = LSet$(Str mode\width + "x" + Str mode\height,10) + LSet$(Str mode\depth + " bit",10)
         Return txt$

      EndIf
   Next
End Function


Function find_graphicsmode(width,height,depth)
   For mode.Graphics_mode_item = Each Graphics_mode_item
      If mode\width = width And mode\height = height And mode\depth = depth Then
         Return mode\gmode
      EndIf
   Next
   Return 0
End Function


;----------------------------------------------------------------------------------------
; this is the main gameplay routine
;----------------------------------------------------------------------------------------
Function IngameLoop()

   If Half_Framerate = 1 Then
      UpdateNoiseTexture(10)
   EndIf

   LocalControls(local_player)
   AnimatingStarfield(local_player)
 ;  MazeHandle(maze_scale#, local_player)

   RotatingSphere()
   SphereAnimate()
   CubeRoomHandle()
   object_handle()
   habitant_handle()
   WallUnit()

   CopyRect 0,0,GraphicsWidth(), GraphicsHeight(),-1,-1,BackBuffer(),TextureBuffer(ptr_copyscreen)

  ; player_ingame_info(local_player)

   ingame_counter360 = (ingame_counter360 + 1) Mod 360

   Select True
      Case KeyHit (KEY_ESCAPE)
         Return game_suspend
      Case KeyHit (KEY_P)
         FlushKeys
         FlushMouse
         Return game_pause
      Default
         Return game_running
   End Select

End Function


Function GamePause()
Local KeyAction, MouseAction
Local gh = GraphicsHeight()

   SetFont ptr_fnt3

   cc = (main_counter360 Mod 16) * 16
   cx = Int (main_counter360 Mod 20) / 4

   Color cc, 0, 0
   Select cx
   Case 0  Text 0, 0,">    >    >    >    " , False, False
   Case 1  Text 0, 0," >    >    >    >   " , False, False
   Case 2  Text 0, 0,"  >    >    >    >  " , False, False
   Case 3  Text 0, 0,"   >    >    >    > " , False, False
   Case 4  Text 0, 0,"    >    >    >    >" , False, False
   Case 5  Text 0, 0,">    >    >    >    " , False, False
   End Select    

   Select cx
   Case 5  Text 0, gh - fontsize20#,"<    <    <    <    " , False, False
   Case 4  Text 0, gh - fontsize20#," <    <    <    <   " , False, False
   Case 3  Text 0, gh - fontsize20#,"  <    <    <    <  " , False, False
   Case 2  Text 0, gh - fontsize20#,"   <    <    <    < " , False, False
   Case 1  Text 0, gh - fontsize20#,"    <    <    <    <" , False, False
   Case 0  Text 0, gh - fontsize20#,"<    <    <    <    " , False, False
   End Select    


   If main_counter360 > 180 Then
 
      Color cc, cc, 0
      Text 0, gh - fontsize20# * 7, "       PAUSED       " , False, False
   Else
      Color cc, 0, cc
      Text 0, gh - fontsize20# * 8, "    PRESS ANY KEY   " , False, False
      Text 0, gh - fontsize20# * 7, "  OR USE THE MOUSE  " , False, False
      Text 0, gh - fontsize20# * 6, "    TO CONTINUE     " , False, False
   EndIf

   KeyAction = GetKey()
   MouseAction = MouseXSpeed() + MouseYSpeed() + MouseZSpeed() + GetMouse()

   If KeyAction Or MouseAction Then Return game_running
   Return game_pause

End Function


;----------------------------------------------------------------------------------------
Function LocalControls(pl_number)

   smooth_mouse_X# = curvevalue#(MouseXSpeed(), smooth_mouse_X#, mouse_speed# ) 
   smooth_mouse_Y# = curvevalue#(MouseYSpeed(), smooth_mouse_Y#, mouse_speed# ) 
   MoveMouse GraphicsWidth()/2, GraphicsHeight()/2

   ; ******************************************* Mouse...
   cam_viewport_pitch# = cam_viewport_pitch# + smooth_mouse_Y#
   If cam_viewport_pitch# < -85 Then cam_viewport_pitch# = -85
   If cam_viewport_pitch# > +85 Then cam_viewport_pitch# = +85

   cam_viewport_yaw# = EntityYaw#(player(pl_number)\pivot)

   TurnEntity ptr_camera,smooth_mouse_Y#,0,0 
   TurnEntity player(pl_number)\pivot,0,-smooth_mouse_X#,0 
   RotateEntity ptr_camera,cam_viewport_pitch#,cam_viewport_yaw#,0 


   If MouseDown(1) Then fire_projectile(ptr_camera, pl_number)

   If mouse_click(2) = 1 Then drop_trace(pl_number)

   ; ******************************************* Keys...

   If KeyDown (KEY_LEFT_SHIFT) Or KeyDown(KEY_RIGHT_SHIFT) Then
      If player(pl_number)\moving_speed# < player(pl_number)\running_speed# Then
         player(pl_number)\moving_speed# = player(pl_number)\moving_speed# + 0.1
      Else
         player(pl_number)\moving_speed# = player(pl_number)\running_speed#
      EndIf

   Else
      If player(pl_number)\moving_speed# > player(pl_number)\walking_speed# Then
         player(pl_number)\moving_speed# = player(pl_number)\moving_speed# - 0.1
      Else
         player(pl_number)\moving_speed# = player(pl_number)\walking_speed#
      EndIf
   EndIf

   If KeyHit(KEY_V) Then player(pl_number)\visor = 1 - player(pl_number)\visor

   If KeyDown (KEY_LEFT_CONTROL) Or KeyDown (KEY_RIGHT_CONTROL) Then
      If KeyHit (KEY_1) Then  SetRotatingWall(1)
      If KeyHit (KEY_2) Then  SetRotatingWall(2)
      If KeyHit (KEY_3) Then  SetRotatingWall(3)
      If KeyHit (KEY_4) Then  SetRotatingWall(4)
      If KeyHit (KEY_5) Then  SetLiftingSphere(1)
      If KeyHit (KEY_6) Then  SetLiftingSphere(2)
      If KeyHit (KEY_7) Then  SetLiftingSphere(3)
      If KeyHit (KEY_8) Then  SetLiftingSphere(4)

   Else
      If KeyHit (KEY_1) Then  player(pl_number)\current_weapon = 1
      If KeyHit (KEY_2) Then  player(pl_number)\current_weapon = 2
      If KeyHit (KEY_3) Then  player(pl_number)\current_weapon = 3
      If KeyHit (KEY_4) Then  player(pl_number)\current_weapon = 4
      If KeyHit (KEY_5) Then  player(pl_number)\current_weapon = 5
      If KeyHit (KEY_6) Then  player(pl_number)\current_weapon = 6
      If KeyHit (KEY_7) Then  player(pl_number)\current_weapon = 7
      If KeyHit (KEY_8) Then  player(pl_number)\current_weapon = 8
      If KeyHit (KEY_9) Then  player(pl_number)\current_weapon = 9
      If KeyHit (KEY_0) Then  player(pl_number)\current_weapon = 0 : player(pl_number)\visor = 0

   EndIf

   If KeyHit (KEY_RETURN) Then
      SetRotatingWall(1)
      SetRotatingWall(2)
      SetRotatingWall(3)
      SetRotatingWall(4)
      SetRotatingWall(5)

   EndIf

   If KeyHit (KEY_F1) Then AOB_experience(pl_number, 1)
   If KeyHit (KEY_F2) Then AOB_experience(pl_number, 2)
   If KeyHit (KEY_F3) Then AOB_experience(pl_number, 3)
   If KeyHit (KEY_F4) Then AOB_experience(pl_number, 4)

   Select out_of_body
      Case 0
         body_movement(pl_number)
      Case 1
         PositionEntity ptr_camera, sin_tb#(ingame_counter360 + 90) * 475, 510 + sin_tb#(ingame_counter360) * 300, cos_tb#(ingame_counter360 + 90) * 475
      Case 2
         PositionEntity ptr_camera, cos_tb#(ingame_counter360 + 90) * 475, 510 + sin_tb#(ingame_counter360) * 300, sin_tb#(ingame_counter360 + 90) * 475
      Case 3
         PositionEntity ptr_camera, cos_tb#(ingame_counter360 + 270) * 475, 510 + cos_tb#(ingame_counter360) * 300, cos_tb#(ingame_counter360) * 475
      Case 4
         PositionEntity ptr_camera, sin_tb#(ingame_counter360 + 270) * 475, 510 + sin_tb#(ingame_counter360) * 300, cos_tb#(ingame_counter360) * 475

   End Select

   If player(pl_number)\shot_timer > 0 Then player(pl_number)\shot_timer = player(pl_number)\shot_timer - 1

End Function


Function curvevalue#(newvalue#,oldvalue#,increments# ) 
   If increments# > 1 Then oldvalue# = oldvalue# - (oldvalue# - newvalue#) / increments# 
   If increments# <= 1 Then oldvalue# = newvalue# 
   Return oldvalue#

End Function 


;----------------------------------------------------------------------------------------
Function player_ingame_info(pl_number)
Local c$
Local gh = GraphicsHeight()
Local gwh = GraphicsWidth() * 0.5
Local cw = player(pl_number)\current_weapon

Local cc = (main_counter360 Mod 16) * 16

   bw# = player(pl_number)\resistance_level# * 0.1 * fontsize20#
   bc = player(pl_number)\resistance_level# * 2.5
   If player(pl_number)\resistance_level# < 10 Then
      Color cc, 0, 0
   Else
      Color 255 - bc, bc, bc 
   EndIf
   Rect gwh, gh - fontsize20#, bw#, fontsize20#, True

   bw# = player(pl_number)\shield_level# * fontsize20#
   bc = player(pl_number)\shield_level# * 25
   If player(pl_number)\shield_level# < 3 Then
      Color cc, 0, 0
   Else
      Color 255 - bc, bc, bc 
   EndIf
   Rect gwh - bw#, gh - fontsize20#, bw#, fontsize20#, True

   SetFont ptr_fnt3

   If CurrentLevel\compass Then 
      compass = Int ((cam_viewport_yaw#) / 45)
      Select compass
         Case 0 c$ = "N"
         Case 1 c$ = "NE"
         Case 2 c$ = "E"
         Case 3 c$ = "SE"
         Case 4 c$ = "S"
         Case 5 c$ = "SW"
         Case 6 c$ = "W"
         Case 7 c$ = "NW"
         Case 8 c$ = "N"
      End Select

      cl# = (Len(c$)) * 0.5 * fontsize20#
      If CurrentLevel\compass = 1 Then
         Color 255,255,255
         Text gwh - cl#, gh - fontsize20#, c$, False, False
      ElseIf CurrentLevel\compass = 2 Then
         Color cc,cc,cc
         Text gwh - cl#, gh - fontsize20#, c$, False, False
      EndIf

   EndIf


   Color 0,127,127
   Text 0, gh - fontsize20# * 2, weapon_name$(cw) + "     marks", False, False

   If cw > 0 Then
      cnt = player(pl_number)\bullets[cw]
      If cnt > 5 Then
         Color 0, 127, 0
      ElseIf cnt > 0 Then
         Color cc, 0, 0
      Else
         Color 127, 0, 0
      EndIf
      Text 0, gh - fontsize20#, cnt
   EndIf

   cnt = player(pl_number)\marks
   If cnt > 5 Then
      Color 0, 127, 0
   ElseIf cnt > 0 Then
      Color cc, 0, 0
   Else
      Color 127, 0, 0
   EndIf
   Text fontsize20# * 17, gh - fontsize20#, cnt

   If player(pl_number)\visor Then
      Color 127, 127, 127
      Text fontsize20# * 8, fontsize20# * 6, Chr$(124) + "  " + Chr$(124) , False, False
      Text fontsize20# * 8, fontsize20# * 7, Chr$(124) + "  " + Chr$(124) , False, False

   EndIf

End Function


;----------------------------------------------------------------------------------------
; this function is when player drops a mark item in the maze so he can keep a trace of
; where he has been.
Function drop_trace(pl_number)
   If maze(player(pl_number)\grid_xpos, player(pl_number)\grid_ypos) = 170 Then
      player(pl_number)\marks = player(pl_number)\marks + 1
      DeleteMazeEntity(player(pl_number)\grid_xpos, player(pl_number)\grid_ypos)
   Else
      If player(pl_number)\marks > 0 Then 
         maze(player(pl_number)\grid_xpos, player(pl_number)\grid_ypos) = 42
         player(pl_number)\marks = player(pl_number)\marks - 1
      EndIf
   EndIf
End Function


;----------------------------------------------------------------------------------------
Function AOB_experience(pl_number, experience_number)
       If Not in_center_room(pl_number, 0, 0, 200) Then
          If sphere_height# (experience_number) > 300 Then
             If out_of_body <> experience_number Or out_of_body = 0 Then
                out_of_body = experience_number
             Else
                out_of_body = 0
             EndIf
          EndIf
       EndIf
End Function


;----------------------------------------------------------------------------------------
Function in_center_room(pl_number, center_X#, center_Z#, roomsize#)
   If ((player(pl_number)\x# < (center_X# + roomsize#) And player(pl_number)\x# > (center_X# - roomsize#)) And (player(pl_number)\z# < (center_Z# + roomsize#) And player(pl_number)\z# > (center_Z# - roomsize#))) Then
      Return 1
   EndIf
   Return 0
End Function


;----------------------------------------------------------------------------------------
Function far_from_room(pl_number, center_X#, center_Z#, roomsize#, farfactor#)
   farsize# = roomsize# * farfactor#
   If Not ((player(pl_number)\x# < (center_X# + farsize#) And player(pl_number)\x# > (center_X# - farsize#)) And (player(pl_number)\z# < (center_Z# + farsize#) And player(pl_number)\z# > (center_Z# - farsize#))) Then
      Return 1
   EndIf
   Return 0
End Function



;----------------------------------------------------------------------------------------
Function body_movement(pl_number)

    ; this is for jumping
    collided = CountCollisions (player(pl_number)\pivot)
    player(pl_number)\vy# = player(pl_number)\vy# - GRAVITY#

	If collided > 0
       For i = 1 To collided

          NY# = CollisionNY(player(pl_number)\pivot, i)
		  If NY# > 0 Then
             player(pl_number)\vy# = player(pl_number)\vy# + GRAVITY#
             If Abs (player(pl_number)\vy#) > 0.9 Then
                player(pl_number)\vy# = -player(pl_number)\vy# / 2
          ;   Else
          ;      player(pl_number)\vy# = 0
             EndIf

		     ; this is for jumping
             If KeyDown (KEY_SPACE) Then player(pl_number)\vy# = player(pl_number)\moving_speed#

             ; this is for sidewalking
             If KeyDown (KEY_LEFT) Then
                player(pl_number)\vx# = -player(pl_number)\moving_speed#
             ElseIf KeyDown (KEY_RIGHT) Then
                player(pl_number)\vx# = player(pl_number)\moving_speed#
             Else
                player(pl_number)\vx# = player(pl_number)\vx# / 2
             EndIf

             ; this is for foreward and backward
             If KeyDown (KEY_UP) Then
                player(pl_number)\vz# = player(pl_number)\moving_speed#
             ElseIf KeyDown (KEY_DOWN) Then
                player(pl_number)\vz# = -player(pl_number)\moving_speed#
             Else
                player(pl_number)\vz# = player(pl_number)\vz# / 2
             EndIf
          EndIf

        ;  NX# = CollisionNX(player(pl_number)\pivot, i)
        ;  If NX# > 0 Then
        ;     If Abs (player(pl_number)\vx#) > 1 Then
        ;        player(pl_number)\vx# = -player(pl_number)\vx#
        ;     Else
        ;        player(pl_number)\vx# = 0
        ;     EndIf
        ;  EndIf

        ;  NZ# = CollisionNZ(player(pl_number)\pivot, i)
        ;  If NZ# > 0 Then
        ;     If Abs (player(pl_number)\vz#) > 1 Then
        ;        player(pl_number)\vz# = -player(pl_number)\vz#
        ;     Else
        ;        player(pl_number)\vz# = 0
        ;     EndIf
        ;  EndIf

       Next
    EndIf

    cam_viewport_yaw# = 180 + cam_viewport_yaw#

    MoveEntity player(pl_number)\pivot, player(pl_number)\vx#, player(pl_number)\vy#, player(pl_number)\vz#

    player(pl_number)\x# = EntityX(player(pl_number)\pivot)
    player(pl_number)\y# = EntityY(player(pl_number)\pivot)
    player(pl_number)\z# = EntityZ(player(pl_number)\pivot) 

    PositionEntity ptr_camera, player(pl_number)\x#, player(pl_number)\y#, player(pl_number)\z#

End Function


;----------------------------------------------------------------------------------------
Function RotatingSphere()
   PositionEntity rotating_sphere, sin_tb#(ingame_counter360) * 512, 600 + cos_tb#(ingame_counter360) * 150, cos_tb#(ingame_counter360) * 512
End Function


;----------------------------------------------------------------------------------------
Function SetLiftingSphere(i)

   sphere_255# = 310 - sphere_height# (i)
   If sphere_255# = 0 Then
      If out_of_body <> i Then
         sphere_direction#(i) = -1
      EndIf
   ElseIf sphere_255# = 255 Then
      sphere_direction#(i) = .25
   Else
      sphere_direction#(i) = -sphere_direction#(i)
   EndIf
End Function


;----------------------------------------------------------------------------------------
Function SphereAnimate()
Local i, sphere_255#

       For i = 1 To 4
          sphere_height# (i) = sphere_height# (i) + sphere_direction# (i)
          sphere_255# = 310 - sphere_height# (i)
          If sphere_255# >= 255
             sphere_direction#(i) = 0
             sphere_height# (i) = 55
          EndIf
          If sphere_255# <= 0 Then
             sphere_direction#(i) = 0
             sphere_height# (i) = 310
          EndIf

          If sphere_255# = 225 Then
             If sphere_direction# (i) = .5 Then sphere_direction# (i) = 1
             If sphere_direction# (i) = -1 Then sphere_direction# (i) = -.5

          ElseIf sphere_255# = 240 Then
             If sphere_direction# (i) = .25 Then sphere_direction# (i) = .5
             If sphere_direction# (i) = -.5 Then sphere_direction# (i) = -.25
          EndIf

           If i = 1 And sphere_direction#(i) <> 0 Then
              PositionEntity sphere1,224,sphere_height#(i),224
              EntityColor sphere1,255,sphere_255,sphere_255

           ElseIf i = 2 And sphere_direction#(i) <> 0 Then
              PositionEntity sphere2,-224,sphere_height#(i),224
              EntityColor sphere2,sphere_255,255,sphere_255

           ElseIf i = 3 And sphere_direction#(i) <> 0 Then
              PositionEntity sphere3,-224,sphere_height#(i),-224
              EntityColor sphere3,sphere_255,sphere_255,255

           ElseIf i = 4 Then
              If sphere_direction#(i) <> 0 Then
                 PositionEntity sphere4,224,sphere_height#(i),-224
              EndIf
              If sphere_255# = 255 Then 
                 EntityColor sphere4,255,255,255
              Else

                 Select ingame_counter360/120
                   Case 0
                    EntityColor sphere4,sphere_255,sphere_255,255
                   Case 3
                    EntityColor sphere4,sphere_255,sphere_255,255
                   Case 1
                    EntityColor sphere4,sphere_255,255,sphere_255
                   Case 2
                    EntityColor sphere4,255,sphere_255,sphere_255
                 End Select

              EndIf
           EndIf

       Next

End Function


;----------------------------------------------------------------------------------------
; starfield management
;----------------------------------------------------------------------------------------
; Define the type for each stars in a starfield
Type star_info
   Field ptr
   Field xpos#
   Field ypos#
   Field zpos#
   Field velocity#
End Type


Function AnimatingStarfield(pl_number)
   Local overlap_x = Rand(1,2000)
   Local overlap_z = Rand(1,4000)
   Local x# = player(pl_number)\x#
   Local y# = player(pl_number)\y#
   Local z# = player(pl_number)\z#

   If Rand(1,5) = 1 Then AddStar(x#, y#, z#, overlap_x, overlap_z)

   For star.star_info = Each star_info
       star\xpos# = star\xpos + star\velocity

       If (star\xpos#) > (x# + overlap_x + 2000) Then
          FreeEntity star\ptr
          Delete star.star_info
       Else
          PositionEntity star\ptr, star\xpos#, star\ypos#, star\zpos#
       EndIf

   Next

End Function


Function DeleteAllStars()
   For star.star_info = Each star_info
      FreeEntity star\ptr
      Delete star.star_info
   Next
End Function


Function AddStar(xref#, yref#, zref#, overlap_x, overlap_z)
   Local starsize# = Rnd(0.5,2.01)
   Local speed = Rand(2,10)

   If starsize# > 2 Then
      starsize# = 18 - speed
   EndIf

   star.star_info = New star_info

   star\ptr      = CreateSphere (2 + Int starsize#)
   star\xpos#     = xref - overlap_x - 2000
   star\zpos#     = zref - overlap_z + 2000
   star\ypos#     = 700 + Rand(1, 200) 
   star\velocity# = speed

   EntityFX star\ptr, 1
   ScaleMesh star\ptr, starsize, starsize, starsize
   EntityColor star\ptr, 255, 255, 255

End Function


;----------------------------------------------------------------------------------------
; maze management routines
;----------------------------------------------------------------------------------------
Function MazeHandle(maze_scale#, pl_number, viewing_radius = 40)
   Local x_start#, y_start#, x_end#, y_end#

   Local lsx = Int ((CurrentLevel\size_x ) / 2)
   Local lsy = Int ((CurrentLevel\size_y ) / 2)
   Local px# = lsx - ((player(pl_number)\x#) / maze_scale#)
   Local py# = lsy - ((player(pl_number)\z#) / maze_scale#)

   player(pl_number)\grid_xpos = Int px#
   player(pl_number)\grid_ypos = Int py#

   x_start = (px# - viewing_radius) : If x1 > x_start Then x_start = 0
   x_end   = (px# + viewing_radius) : If x2 < x_end   Then x_end   = CurrentLevel\size_x
   y_start = (py# - viewing_radius) : If y1 > y_start Then y_start = 0
   y_end   = (py# + viewing_radius) : If y2 < y_end   Then y_end   = CurrentLevel\size_y


   ; in the case of radius 40, it will generate a search grid of 6400 maze elements (80x80)
   Dim maze_lview(viewing_radius * 2, viewing_radius * 2)


   For i = 0 To 359
      ix# = cos_tb#(i)
      iy# = sin_tb#(i)
      found_wall = 0
      xx# = px#
      xy# = py#
      For j = 1 To viewing_radius
         x = Int (xx#)
         y = Int (xy#)
         result = maze(x, y)
         If result = 0 Or result = 128 Then
            If result = 0 And found_wall = 0 Then
               make_wall(lsx, lsy, x, y, maze_scale#)
               maze(x,y) = result + 128
            ElseIf found_wall > 1 And result = 128 Then
               If (Last_added_x <> x) And (Last_added_y <> y) Then
                  If (Abs ix#) < 0.40 And (Abs ix#) > 0.60 And (Abs iy#) < 0.40 And (Abs iy#) > 0.60 Then
                        maze(x, y) = result + 128
                  EndIf
               EndIf
            EndIf
            found_wall = found_wall + 1
         ElseIf  result > 0 And result < 10 And found_wall = 0 Then
            make_inert_projectile(lsx, lsy, x, y, maze_scale#, result)
            maze(x,y) = result + 128
         ElseIf  result = 42 And found_wall = 0 Then
            make_mark(lsx, lsy, x, y, maze_scale#)
            maze(x,y) = result + 128
         EndIf
         xx# = xx# + ix#
         xy# = xy# + iy#
         If xx# > x_end#   Then xx# = x_end#
         If xx# < x_start# Then xx# = x_start#
         If xy# > y_end#   Then xy# = y_end#
         If xy# < y_start# Then xy# = y_start#
      Next
   Next

   ; this removes all blocks that are not in range
;   For themaze.object_info = Each object_info
;       result = maze(themaze\mx, themaze\my)

;       If (in_range (x_start, y_start, x_end, y_end, themaze\mx, themaze\my) = 0) Then
;          If result > 127 Then
;             maze(themaze\mx, themaze\my) = result - 128
;          EndIf
;          If maze(themaze\mx, themaze\my) > 127 Then
;             maze(themaze\mx, themaze\my) = maze(themaze\mx, themaze\my) - 128
;          EndIf
;          FreeEntity themaze\ptr
;          Delete themaze.object_info
       
;       ElseIf result = 170 Then
;          RotateEntity themaze\ptr, sin_tb#(ingame_counter360 * 3) * 30, ingame_counter360 * 3,0
;          themaze\cycle_timer = themaze\cycle_timer + themaze\cycle_increment
;          If themaze\cycle_timer > themaze\cycle_reset Then
;             themaze\cycle_timer = themaze\cycle_start
;             emit_3D_sound("tick1", themaze\ptr)
;          EndIf

;       EndIf
;   Next

End Function


;################################################################################################################
;###   This function is designed to show only what is allowed.  It serves the purpose of optimizing the display
;###   for the 3D world by supplying a viewing distance (viewing_radius) and uses the maze level's information
;###   and informs of what needs to be displayed for the local player's screen output.
;################################################################################################################
;Function make_display_grid(pl_number, viewing_radius)
;   ; load the maze into the temporary table
;   ;-----------------------------------------
;   Local x_start, y_start, x_end, y_end

;   Local lsx = Int ((CurrentLevel\size_x ) / 2)
;   Local lsy = Int ((CurrentLevel\size_y ) / 2)
;   Local px# = lsx - ((player(pl_number)\x#) / maze_scale#)
;   Local py# = lsy - ((player(pl_number)\z#) / maze_scale#)

;   player(pl_number)\grid_xpos = Int px#
;   player(pl_number)\grid_ypos = Int py#

;   x_start = (px# - viewing_radius) : If x_start < 0 Then x_start = 0
;   x_end   = (px# + viewing_radius) : If x_end  >  Then x_end   = CurrentLevel\size_x
;   y_start = (py# - viewing_radius) : If y_start Then y_start = 0
;   y_end   = (py# + viewing_radius) : If y_end   Then y_end   = CurrentLevel\size_y
   

;   ; validate the maze from our viewing point
;   ;-----------------------------------------
;   x = 0 ; this is what we use as our pinpoint
;   y = 0 ; this is what we use as our pinpoint
;   dir_x = 1 ; direction we are moving when scanning for walls and things
;   dir_y = 0 ; direction we are moving when scanning for walls and things
;   rad_x = 1 ; scanning radius (when it reaches the viewing radius, it's the end)
;   rad_y = 1 ; scanning radius (when it reaches the viewing radius, it's the end)
;   end_scanning = False
;   Repeat
;      x = x + dir_x
;      y = y + dir_y

;      ; testing for walls and misc. objects 


;      ; moving along
;      If x >=  rad_x Then  dir_y =  1 : dir_x = 0
;      If y >=  rad_y Then  dir_x = -1 : dir_y = 0
;      If x <= -rad_x Then  dir_y = -1 : dir_x = 0
;      If y <= -rad_y Then  dir_x =  1 : dir_y = 0 : rad_x = rad_x + 1 : rad_y = rad_y + 1

;      If rad_x = viewing_radius And rad_y = viewing_radius Then end_scanning = True

;      l = l + 1 : If l > 1000000 Then Print "this is an infinite loop...":WaitKey:End
;   Until end_scanning

;End Function
;################################################################################################################

; Puts a cube in the 3D world
Function make_wall(x0,y0,x1,y1,maze_scale#)
   themaze.object_info = New object_info
   themaze\xpos# = maze_scale# * (x0 - x1)
   themaze\ypos# = maze_scale# * 10
   themaze\zpos# = maze_scale# * (y0 - y1)
   themaze\mx    = x1
   themaze\my    = y1

   themaze\ptr   = CreateCube()
   ScaleMesh themaze\ptr,maze_scale# / 2,maze_scale# * 8,maze_scale# / 2
 ; EntityFX themaze\ptr,1
   EntityTexture themaze\ptr,ptr_texture(12),0,1
 ;  EntityAlpha themaze\ptr,.75
   EntityShininess themaze\ptr,.5
   PositionEntity themaze\ptr, themaze\xpos#, themaze\ypos#, themaze\zpos#
   EntityType themaze\ptr,world_type

End Function


Function make_mark(x0, y0, x1,y1,maze_scale#)
   themaze.object_info = New object_info
   themaze\xpos#           = maze_scale# * (x0 - x1)
   themaze\ypos#           = 30
   themaze\zpos#           = maze_scale# * (y0 - y1)
   themaze\mx              = x1
   themaze\my              = y1

   themaze\ptr             = CreateCone (16)
   themaze\cycle_start     = 0
   themaze\cycle_increment = 1
   themaze\cycle_reset     = 50
   themaze\cycle_timer     = themaze\cycle_reset
   ScaleMesh themaze\ptr ,5 ,5 ,5
   EntityTexture themaze\ptr,ptr_copyscreen,0,1
   TurnEntity themaze\ptr, 0,30,30
   EntityAlpha themaze\ptr,.75
   EntityShininess themaze\ptr,1
   PositionEntity themaze\ptr, themaze\xpos#, themaze\ypos#, themaze\zpos#
   EntityType themaze\ptr,world_type

End Function


Function make_inert_projectile(x0, y0, x1, y1, maze_scale#, ob_number)
   themaze.object_info     = summon_object.object_info (ob_number, False, False)
   themaze\xpos#           = maze_scale# * (x0 - x1)
   themaze\ypos#           = 5
   themaze\zpos#           = maze_scale# * (y0 - y1)
   themaze\mx              = x1
   themaze\my              = y1
   PositionEntity themaze\ptr, themaze\xpos#, themaze\ypos#, themaze\zpos#
   EntityType themaze\ptr, projectile_type

End Function


; deletes one wall
Function DeleteMazeEntity(x,y, default_falue = 11)
   For themaze.object_info = Each object_info
      If themaze\mx = x And themaze\my = y Then
         If maze(x,y) > 127 Then
            maze(themaze\mx, themaze\my) = default_falue - 128
            FreeEntity themaze\ptr
            Delete themaze.object_info
            Return 1
         EndIf
      EndIf
   Next
   Return 0
End Function


; deletes all the walls
Function DeleteMazeWalls()
   For themaze.object_info = Each object_info
      FreeEntity themaze\ptr
      If maze(themaze\mx, themaze\my) > 127 Then
         maze(themaze\mx, themaze\my) = maze(themaze\mx, themaze\my) - 128
      EndIf
      Delete themaze.object_info
   Next
End Function


; retourne 1 si x,y est entre x1,y1 et x2,y2
Function in_range (x1#,y1#,x2#,y2#,x,y)
   If x > x1# And x < x2# Then
      If y > y1# And y < y2# Then
         Return 1
      EndIf
   EndIf
   Return 0
End Function


;----------------------------------------------------------------------------------------
Function init_maze(exact=0)
    Local cube1, cube2, cube3, cube4, cube5, cube6, cube7, cube8, cube9
    Local mx, my, fx, fy
    Local farx, fary

    maze_boxes = 0

    mx = CurrentLevel\size_x + 1
    my = CurrentLevel\size_y + 1

	Dim maze(mx + 1, my + 1)

    SeedRnd MilliSecs()

    corner_size = 32
    If mx < 205 And my < 205 Then corner_size = 24
    If mx < 169 And my < 169 Then corner_size = 16
    If mx < 125 And my < 125 Then corner_size = 8

    maze_fill(corner_size,Int (corner_size/2) + 1,Int (corner_size/2) + 1)
    maze_fill(corner_size,Int (corner_size/2) + 1,my - Int (corner_size/2))
    maze_fill(corner_size,mx - Int (corner_size/2),Int (corner_size/2) + 1)
    maze_fill(corner_size,mx - Int (corner_size/2),my - Int (corner_size/2))

	For fx=1 To mx: set_maze(fx,1,11,0): set_maze(fx,my,11,0): Next
	For fy=1 To my: set_maze(1,fy,11,0): set_maze(mx,fy,11,0): Next
 
	farx=mx
	fary=my
	SeedRnd MilliSecs()
	ally=1
	allx=1
	exact=exact+1

	Repeat
		Repeat
			allx = allx + getcos(alldir) * exact
			ally = ally + getsin(alldir) * exact
			go = go + exact
			If go>=fary-exact And (alldir=1 Or alldir=3) Then go=0:fary=fary-exact:alldir=wrap4(alldir+1)
			If go>=farx-exact And (alldir=0 Or alldir=2) Then go=0:farx=farx-exact:alldir=wrap4(alldir+1)
		Until (maze(allx,ally) > 0 And (allx < (mx-1) Or ally < (my-1))) Or (farx < 0 Or fary < 0)

		If farx<0 Or fary<0 Then Exit

		If allx>mx Then If ally>my Then Exit
		x=allx
		y=ally
		dir=Rand(0,3)

		Repeat
			For f=0 To 3
				If cango(f,mx,my,x,y,exact) = 1 Then Exit
			Next

			If f=4 Then Exit
			dir=wrap4(dir+Rand(-1,+1))
			If cango(dir,mx,my,x,y,exact) = 1 Then 

			   For f=1 To exact
				  x=x+getcos(dir)
				  y=y+getsin(dir)

                  If CurrentLevel\size_x Mod 7 And CurrentLevel\size_y Mod 3
                     ; size 16 boxes in corner
		             If plot_in_range(16,75,75,x,y) And cube1 = 0 Then
		                cube1 = 1
		                maze_fill(16,75,75)
		             ElseIf plot_in_range(16,mx + 1 - 75,75,x,y) And cube2 = 0 Then
				        cube2 = 1
		                maze_fill(16,mx + 1 - 75,75)
		             ElseIf plot_in_range(16,75,my + 1 - 75,x,y) And cube3 = 0 Then
		   		        cube3 = 1
		                maze_fill(16,75,my + 1 - 75)
		             ElseIf plot_in_range(16,mx + 1 - 75,my + 1 - 75,x,y) And cube4 = 0 Then
				        cube4 = 1
		                maze_fill(16,mx + 1 - 75,my + 1 - 75)
		             ; the big size 32 box in the center (always)
		             ElseIf plot_in_range(32,(mx+1)/2,(my+1)/2,x,y) And cube9 = 0 Then
				        cube9 = 1
                        maze_fill(32,(mx+1)/2,(my+1)/2)
		             Else
		                set_maze(x,y,11,1)
		             EndIf
		          EndIf
		
		          If (CurrentLevel\size_x Mod 7 And CurrentLevel\size_y Mod 3) = 0 Or (mx > 299 And my > 299)
		             ; size 16 boxes in middle sides
		             If plot_in_range(16,mx/2+1,my/4,x,y) And cube5 = 0 Then
		                cube5 = 1
		                maze_fill(16,mx/2+1,my/4)
		             ElseIf plot_in_range(16,mx/4,my/2+1,x,y) And cube6 = 0 Then
				        cube6 = 1
		                maze_fill(16,mx/4,my/2+1)
		             ElseIf plot_in_range(16,mx/2+1,my - my/4 + 1,x,y) And cube7 = 0 Then
		   		        cube7 = 1
		                maze_fill(16,mx/2+1,my - my/4 + 1)
		             ElseIf plot_in_range(16,mx - mx/4 - 1,my/2+1,x,y) And cube8 = 0 Then
				        cube8 = 1
		                maze_fill(16,mx - mx/4 - 1,my/2+1)
				     ; the big size 32 box in the center (always)
		             ElseIf plot_in_range(32,(mx+1)/2,(my+1)/2,x,y) And cube9 = 0 Then
				        cube9 = 1
                        maze_fill(32,(mx+1)/2,(my+1)/2)
		             Else
		                set_maze(x,y,11,1)
		             EndIf

		          EndIf
		
			   Next

			EndIf
		Forever
	Forever

 ;   If CurrentLevel\botrange > 0 Then
 ;      split = CurrentLevel\bots / CurrentLevel\botrange
 ;      For i = 1 To CurrentLevel\botrange
 ;         sprinkle_object_in_maze (split, mx, my, i + 20)
 ;      Next
 ;   EndIf
    If CurrentLevel\weaponrange > 0 Then
       split = CurrentLevel\projectiles / CurrentLevel\weaponrange
       For i = 1 To CurrentLevel\weaponrange
          sprinkle_object_in_maze (split, mx, my, i)
       Next
    EndIf

End Function


Function sprinkle_object_in_maze (amount, mx, my, ob_number)
Local i, j
   For i = 1 To amount
      j = 0
      Repeat
         rx = Rand(1,mx)
         ry = Rand(1,my)
         j = j + 1 : If j > 100 Then Print "infinite loop...":WaitKey:End
      Until maze (rx,ry) = 11
      maze (rx,ry) = ob_number
   Next
End Function


Function maze_fill(size, center_pos_x, center_pos_y)
Local fx, fy

	For fx = (center_pos_x - Int (size/2)) To (center_pos_x + Int (size/2))
       For fy = (center_pos_y - Int (size/2)) To (center_pos_y + Int (size/2))
          set_maze(fx,fy,11,0)
       Next
    Next
End Function


Function plot_in_range (size, center_pos_x, center_pos_y, x, y)
   If x >= (center_pos_x - Int (size/2)) And x <= (center_pos_x + Int (size/2)) Then
      If y >= (center_pos_y - Int (size/2)) And y <= (center_pos_y + Int (size/2))
         Return 1
      EndIf
   EndIf
   Return 0
End Function


Function cango(dir,mx,my,x,y,exact)
Local f, x2, y2

	For f = 1 To exact + 1
	
		x2 = x + getcos(dir) * f
		y2 = y + getsin(dir) * f
		If x2 <= 0 Or y2 <= 0 Or x2 > mx Or y2 > my Then Return 0
		If maze(x2,y2) > 0 Then Return 0
		If maze(x2 + getcos( wrap4(dir + 1) ), y2 + getsin( wrap4(dir + 1) )) > 0 Then Return 0
		If maze(x2 + getcos( wrap4(dir - 1) ), y2 + getsin( wrap4(dir - 1) )) > 0 Then Return 0
		
	Next
	Return 1
End Function


Function wrap4(value)
	Return value -4 * (value>3) + 4 * (value<0)
End Function


; debugging and editing purpose only
Function set_maze(x,y,fx,s)
    If maze(x,y) <> fx Then
    	maze(x,y) = fx
        If s = 1 Then For i=1 To Rand(69,8192):SeedRnd MilliSecs():Next
        maze_boxes = maze_boxes + 1

        If test_layout Then 
	       plot_maze (x,y,fx,3)
           Color 0,0,0
           Rect 1200,0,50,12
           Color 255,255,255
           Text 1200,0,maze_boxes
        EndIf
    EndIf
End Function


; debugging and editing purpose only
Function plot_maze (x,y,fx,z)
		If fx=11 Then Color 255,255,255 : Rect x*z,y*z,z,z
		If fx=12 Then Color 0,255,255 : Rect x*z,y*z,z,z
		If fx=13 Then Color 255,0,255 : Rect x*z,y*z,z,z
		If fx=14 Then Color 255,255,0 : Rect x*z,y*z,z,z
		If fx=15 Then Color 0,0,255 : Rect x*z,y*z,z,z
		If fx=16 Then Color 255,0,0 : Rect x*z,y*z,z,z
		If fx=17 Then Color 0,255,0 : Rect x*z,y*z,z,z
End Function


;----------------------------------------------------------------------------------------
; flipping wall management
;----------------------------------------------------------------------------------------
Function WallUnit()
Local i

    If Rand(0,100) = 1 Then
       If Rand(1,2) = 1 Then
          WallUnitSeed(1,1)
          WallCopySeed(1,2)
          WallCopySeed(1,3)
          WallCopySeed(1,4)
       Else
          WallUnitSeed(1,1)
          WallUnitSeed(2,1)
          WallUnitSeed(3,1)
          WallUnitSeed(4,1)
       EndIf

       WallUnitHandle()

    EndIf

    If Rand(0,100) = 1 Then
       WallUnitSeed(5,2)

       WallUnitHandle()
    EndIf

	fx_u# = fx_u + fx_direction#
	fx_v# = fx_v - fx_direction#

    If fx_u# > 0.2 Or fx_u# < 0.01
	   If fx_direction# = + 0.001
	      fx_direction# = - 0.001
	   Else
	      fx_direction# = + 0.001
	   EndIf
    EndIf

	; Rotate, move and scale texture on the box...

    For i = 1 To 5
       TextureAnimateHandle(walleffect(i), wall_texture_number(i))
    Next

    RotateWallUnit()

End Function


;----------------------------------------------------------------------------------------
Function WallUnitSeed(wallnumber, walltype)
       If walltype = 1 Then
          wall_texture_number(wallnumber) = Rand(1,4)
       Else
          wall_texture_number(wallnumber) = Rand(5,8)
       EndIf

       walleffect(wallnumber) = Rand(1,8)
       wallfxzoom(wallnumber) = Rand(1,5) * 4
       walldisplace#(wallnumber) = Rand(0.01,0.05)
End Function


;----------------------------------------------------------------------------------------
Function WallCopySeed(wallsource, walldest)
       wall_texture_number(walldest) = wall_texture_number(wallsource)

       walleffect(walldest) = walleffect(wallsource) 
       wallfxzoom(walldest) = walleffect(wallsource)
       walldisplace#(walldest) = walldisplace#(wallsource)
End Function


;----------------------------------------------------------------------------------------
Function SetRotatingWall(wall)
       If wall_angle(wall) = 0 Then
          wall_rotate(wall) = 1
       Else
          wall_rotate(wall) = -1
       EndIf
End Function


;----------------------------------------------------------------------------------------
Function WallUnitHandle()
Local i

       DeleteWallUnit()

       For i = 1 To 5
          wall.object_info    = New object_info
          wall\number         = 181
          wall\xpos#          = 0
          wall\ypos#          = 0
          wall\life_increment = 0
          wall\life           = 50
          wall\mx             = Int ((CurrentLevel\size_x ) * 0.5 - wall\xpos# / maze_scale#)
          wall\my             = Int ((CurrentLevel\size_y ) * 0.5 - wall\zpos# / maze_scale#)
          wall\ptr            = CreateCube ()

          EntityTexture wall\ptr, ptr_texture(wall_texture_number(i)), 0, 1
          EntityFX wall\ptr, 1

          If i = 1 Then
             ScaleMesh wall\ptr,128,64,1
             PositionEntity wall\ptr, 0, 64, 128

          ElseIf i = 2 Then
             ScaleMesh wall\ptr,1,64,128
             PositionEntity wall\ptr, -128, 64, 0

          ElseIf i = 3 Then
             ScaleMesh wall\ptr,128,64,1
             PositionEntity wall\ptr, 0, 64, -128

          ElseIf i = 4 Then
             ScaleMesh wall\ptr,1,64,128
             PositionEntity wall\ptr, 128, 64, 0

          ElseIf i = 5 Then
             ScaleMesh wall\ptr,128,1,128
             PositionEntity wall\ptr, 0, 128, 0

          EndIf

          EntityType wall\ptr,world_type

       Next

End Function


;----------------------------------------------------------------------------------------
Function DeleteWallUnit()
   For wall.object_info = Each object_info
      If wall\number = 181 Then
         FreeEntity wall\ptr
         Delete wall.object_info
      EndIf
   Next
End Function


;----------------------------------------------------------------------------------------
Function RotateWallUnit()
Local i = 1

       For wall.object_info = Each object_info
          If wall\number = 181 Then
             wall_angle (i) = wall_angle (i) + wall_rotate (i)

             If wall_angle (i) = 180 Or wall_angle (i) = 0 Then
                wall_rotate(i) = 0
             EndIf
 
             If i = 1 
                RotateEntity wall\ptr, -wall_angle(i), 0, 0
                PositionEntity wall\ptr, 0, 64, 128 + wall_angle(i)
 
             ElseIf i = 2 Then
                RotateEntity wall\ptr, 0, 0, wall_angle(i)
                PositionEntity wall\ptr, -128 - wall_angle(i) , 64, 0 

             ElseIf i = 3 Then
                RotateEntity wall\ptr, wall_angle(i), 0, 0
                PositionEntity wall\ptr, 0, 64, -128 - wall_angle(i)

             ElseIf i = 4 Then
                RotateEntity wall\ptr, 0, 0, -wall_angle(i)
                PositionEntity wall\ptr, 128 + wall_angle(i), 64, 0

             ElseIf i = 5 Then
                PositionEntity wall\ptr, 0, 128 + wall_angle(i), 0

             EndIf

             i = i + 1

          EndIf

       Next

End Function


;----------------------------------------------------------------------------------------
Function one_door_open()
   If wall_angle (1) > 0 Then Return 1
   If wall_angle (2) > 0 Then Return 1
   If wall_angle (3) > 0 Then Return 1
   If wall_angle (4) > 0 Then Return 1
   If wall_angle (5) > 0 Then Return 1
   Return 0
End Function


;----------------------------------------------------------------------------------------
Function CloseAllWalls()
   If wall_angle (1) > 0 Then wall_rotate(1) = -1
   If wall_angle (2) > 0 Then wall_rotate(2) = -1
   If wall_angle (3) > 0 Then wall_rotate(3) = -1
   If wall_angle (4) > 0 Then wall_rotate(4) = -1
   If wall_angle (5) > 0 Then wall_rotate(5) = -1

End Function


;----------------------------------------------------------------------------------------
; texture management
;----------------------------------------------------------------------------------------
Function TextureAnimateHandle(walleffect, walltexture)

    If walleffect = 1 Then
	   RotateTexture ptr_texture(walltexture), fx_u * 100 * wallfxzoom(walltexture)
	   PositionTexture ptr_texture(walltexture), fx_u * wallfxzoom(walltexture), fx_v * wallfxzoom(walltexture)
	   ScaleTexture ptr_texture(walltexture), fx_u + walldisplace#(walltexture) , fx_v + walldisplace#(walltexture)

    ElseIf walleffect = 2 Then
	   RotateTexture ptr_texture(walltexture), fx_v * 100 * wallfxzoom(walltexture)
	   PositionTexture ptr_texture(walltexture), fx_v * wallfxzoom(walltexture), fx_u * wallfxzoom(walltexture)
	   ScaleTexture ptr_texture(walltexture), fx_v + walldisplace#(walltexture) , fx_u + walldisplace#(walltexture)
	
	ElseIf walleffect = 3 Then
	   PositionTexture ptr_texture(walltexture), fx_u * wallfxzoom(walltexture), fx_u * wallfxzoom(walltexture)
	   ScaleTexture ptr_texture(walltexture), fx_u + walldisplace#(walltexture) , fx_u + walldisplace#(walltexture)

	
	ElseIf walleffect = 4 Then
	   PositionTexture ptr_texture(walltexture), fx_v * wallfxzoom(walltexture), fx_v * wallfxzoom(walltexture)
	   ScaleTexture ptr_texture(walltexture), fx_v + walldisplace#(walltexture) , fx_v + walldisplace#(walltexture)

	ElseIf walleffect = 5 Then
	   RotateTexture ptr_texture(walltexture), fx_u * 100 * wallfxzoom(walltexture)
	
	ElseIf walleffect = 6 Then
	   RotateTexture ptr_texture(walltexture), fx_v * 10 * wallfxzoom(walltexture)
	
	ElseIf walleffect = 7 Then
	   ScaleTexture ptr_texture(walltexture), cos_tb#(ingame_counter360 * fx_u) , sin_tb#(ingame_counter360 * fx_v)
	   RotateTexture ptr_texture(walltexture), ingame_counter360 * wallfxzoom(walltexture)

	Else
	   PositionTexture ptr_texture(walltexture), sin_tb#(ingame_counter360) ,cos_tb#(ingame_counter360)
	EndIf

End Function


;----------------------------------------------------------------------------------------
Function UpdateNoiseTexture(texnumber)
  Local bit_32,r,i,x,y

  SetBuffer TextureBuffer(ptr_texture(texnumber))
  LockBuffer TextureBuffer(ptr_texture(texnumber))
  For i=1 To Rand(0,16)                             ;Add random number of pixels in random colors
      r=Rand(0,255)
      WritePixelFast Rand(0,63),Rand(0,31),0 + r Shl 8 + r Shl 16 + r Shl 24
  Next 
  For x=0 To 63                                   ;Recalculate color if every pixel from upper half of texture from color-
    For y=0 To 31                                 ;values of itself and its x-neighbours
      bit_32= ( ( ReadPixelFast (x-1,y) /2 + ReadPixelFast (x+1,y)) / 2 + ReadPixelFast (x,y))
      WritePixelFast x,y,bit_32                   ;Write pixel in upper half
      WritePixelFast 63 - x,63 - y,bit_32              ;...and lower half
    Next
  Next
  UnlockBuffer TextureBuffer(ptr_texture(texnumber))

  SetBuffer BackBuffer()

End Function                                       


;----------------------------------------------------------------------------------------
; Mostly everything concerning the insides of a CubeRoom
;----------------------------------------------------------------------------------------
Function CubeRoomHandle()

    If cuberoom = 1 Then
       newscrollitem = CubeScroller(newscrollitem, 0.0, 400, 0.0, 200.0, 16.0, 24.0, Asc(Mid$(scrolltext$,sc_offset,1)), 2.5, -45.0)
       If newscrollitem = 0 Then 
          sc_offset = sc_offset + 1
          If sc_offset > Len(scrolltext$) Then sc_offset = 1   
       EndIf
    EndIf

    all_players_far = far_from_room(1, 0, 0, 180, 2)
    ; should verify that "all" players are far from room
    If all_players_far Then CloseAllWalls()

    ; should verify that at least one player is not far from the room and that at least 1 player is in center room
    If (Not far_from_room(1, 0, 0, 180, 2)) Or (one_door_open() = 1 And (Not in_center_room(1, 0, 0, 180)) ) Then
      cuberoom = 1
    Else
       If cuberoom = 1 Then
          DeleteCubeScroller()
       EndIf
    EndIf

End Function


;----------------------------------------------------------------------------------------
; Define a type for scrolltext-cubes: entityhandle, x- and y position.
Type scrollcube
   Field ptr
   Field startcycle#
   Field endcycle#
   Field cyclecount#
   Field xpos#
   Field ypos#
   Field zpos#
End Type


Function CubeScroller(scrollerhandle, Xoffset#, Yoffset#, Zoffset#, radius#, Xtilesize#, Ytilesize#, character, speed#, angle#)
Local newX# = (1.6 * Xtilesize#) / speed#

   scrollerhandle = scrollerhandle + 1

  If scrollerhandle > newX#

    If character > 32 And character <= 96 And character <> 32 Then      
      scrollingtextcube.scrollcube  = New scrollcube
      scrollingtextcube\startcycle# = 360
      scrollingtextcube\endcycle#   = -360
      scrollingtextcube\cyclecount# = scrollingtextcube\startcycle#
      scrollingtextcube\xpos#       = 0
      scrollingtextcube\ypos#       = 0
      scrollingtextcube\zpos#       = 0
      scrollingtextcube\ptr = create_flat()
      EntityFX scrollingtextcube\ptr,1

      ScaleMesh scrollingtextcube\ptr, Xtilesize#, Ytilesize#, 0.5
      EntityTexture scrollingtextcube\ptr, ptr_texture(character), 0, 1   
      EntityTexture scrollingtextcube\ptr, ptr_texture(10), 0, 2

    EndIf
    scrollerhandle = 0                 
  EndIf

  For scrollingtextcube.scrollcube = Each scrollcube 
              
    If scrollingtextcube\cyclecount# <= scrollingtextcube\endcycle#                       
      FreeEntity scrollingtextcube\ptr
      Delete scrollingtextcube.scrollcube

    Else
      scrollingtextcube\xpos# = cos_tb10#(3600 + (scrollingtextcube\cyclecount# * 5)) * radius#
      scrollingtextcube\zpos# = sin_tb10#(3600 + (scrollingtextcube\cyclecount# * 5)) * radius#
      PositionEntity scrollingtextcube\ptr, Xoffset# + scrollingtextcube\xpos#, Yoffset# + scrollingtextcube\ypos#, Zoffset# + scrollingtextcube\zpos#

   ;   pitch# = angle#
   ;   yaw#   = 180 - scrollingtextcube\cyclecount# * 0.5
   ;   roll#  = 0
   ;   RotateEntity scrollingtextcube\ptr, angle#, (180 - scrollingtextcube\cyclecount# * 0.5), 0
      RotateEntity scrollingtextcube\ptr, angle#, 90 + scrollingtextcube\cyclecount# * 0.5, 0

      a# = (scrollingtextcube\startcycle# - Abs(scrollingtextcube\cyclecount#)) * 0.02
      EntityAlpha scrollingtextcube\ptr, a#

      scrollingtextcube\cyclecount# = scrollingtextcube\cyclecount# - speed#

    EndIf
  Next

  Return scrollerhandle

End Function


Function DeleteCubeScroller()
  For scrollingtextcube.scrollcube = Each scrollcube
      FreeEntity scrollingtextcube\ptr
      Delete scrollingtextcube.scrollcube
  Next
End Function


;----------------------------------------------------------------------------------------
; These are the projectiles management functions
;----------------------------------------------------------------------------------------

Function weapon_name$(weapon_number)
   Select weapon_number
      Case 0 Return "          "
      Case 1 Return "HandBall  "
      Case 2 Return "PoolBall  "
      Case 3 Return "RubberDisc"
      Case 4 Return "BeachBall "
      Case 5 Return "AmigaBall "
      Case 6 Return "Rocket    "
      Case 7 Return "UNDEFINED "
      Case 8 Return "UNDEFINED "
      Case 9 Return "UNDEFINED "
   End Select
End Function




;----------------------------------------------------------------------------------------
Function clear_all_bullets()
   For b.object_info = Each object_info
      If b\number < 10 Then
         FreeEntity b\ptr
         FreeBrush b\brush
         Delete b.object_info
      EndIf
   Next

End Function


;----------------------------------------------------------------------------------------
Function fire_projectile(ptr_shooter, pl_number)
Local xx#, xy#, xz#

   Projectile_in_action = player(pl_number)\current_weapon

   ; no weapons selected, so get back
   If Projectile_in_action = 0 Then Return

   ; weapon has no bullets, so return
   If player(pl_number)\bullets[ Projectile_in_action ] = 0 Then Return

   ; weapon has not yet recharged, so return
   If player(pl_number)\shot_timer > 0 Then Return

   ; everything is fine, emit the projectile
   b.object_info = summon_object.object_info (Projectile_in_action)

   If Projectile_in_action = 1 Then            ; this is the Handball
      player(pl_number)\shot_timer = 15
      snd$ = "shoot1"

   ElseIf Projectile_in_action = 2 Then        ; this is the Poolball
      player(pl_number)\shot_timer = 35
      snd$ = "shoot1"

   ElseIf Projectile_in_action = 3 Then        ; this is the Rubberdisc
      player(pl_number)\shot_timer = 20
      snd$ = "shoot3"

   ElseIf Projectile_in_action = 4 Then        ; this is the Beachball
      player(pl_number)\shot_timer = 50
      snd$ = "shoot1"

   ElseIf Projectile_in_action = 5 Then        ; this is the Amigaball
      player(pl_number)\shot_timer = 20
      snd$ = "shoot2"

   ElseIf Projectile_in_action = 6 Then        ; this is the Rocket
      player(pl_number)\shot_timer = 25
      snd$ = "rocket1"

   ElseIf Projectile_in_action = 7 Then        ; this is the ...
      Return

   ElseIf Projectile_in_action = 8 Then        ; this is the ...
      Return

   ElseIf Projectile_in_action = 9 Then        ; this is the ...
      Return

   EndIf

   p_piv = CreatePivot()
   PositionEntity p_piv, EntityX(ptr_shooter), EntityY(ptr_shooter), EntityZ(ptr_shooter)
   RotateEntity p_piv, EntityPitch(ptr_shooter), EntityYaw(ptr_shooter), EntityRoll(ptr_shooter)
   If Projectile_in_action < 6 Then
      MoveEntity p_piv,0,1,5
   Else
      MoveEntity p_piv,0,0,5
   EndIf

   PositionEntity b\ptr, EntityX(ptr_shooter), EntityY(ptr_shooter), EntityZ(ptr_shooter)
   EntityType b\ptr,projectile_type

   vectx# = EntityX(p_piv) - EntityX(ptr_shooter)
   vecty# = EntityY(p_piv) - EntityY(ptr_shooter)
   vectz# = EntityZ(p_piv) - EntityZ(ptr_shooter)
   TFormVector vectx#, vecty#, vectz#, p_piv, ptr_camera

   b\Vx# = TFormedX()
   b\Vz# = TFormedZ()
   b\Vy# = TFormedY()

   If  Projectile_in_action = 5 Then
      AlignToVector b\ptr, b\Vx#, b\Vy#, b\Vz#, 1, 1
   Else
      AlignToVector b\ptr, b\Vx#, b\Vy#, b\Vz#, 2, 1
   EndIf

   b\Velocity# = Sqr(b\Vx#^2 + b\Vy#^2 + b\Vz#^2)
   b\mx = Int ((CurrentLevel\size_x ) * 0.5 - b\xpos# / maze_scale#)
   b\my = Int ((CurrentLevel\size_y ) * 0.5 - b\zpos# / maze_scale#)

   FreeEntity p_piv

   ; update player's information
   player(pl_number)\bullets[Projectile_in_action] = player(pl_number)\bullets[Projectile_in_action] - 1

   emit_3D_sound(snd$, b\ptr)

End Function 


;----------------------------------------------------------------------------------------
; Object management routines
;----------------------------------------------------------------------------------------
; regroups "maze_info" and "projectile"
Type object_info
   Field object_type        ; 1 = 3D model, 2 = texture
   Field number             ; object position in list of objects (1 to 9 are projectiles) 
   Field ptr                ; this is where we point to the 3D entity
   Field brush              ; points to the brush data

   Field xpos#, ypos#, zpos#
   Field oldx#, oldy#, oldz#; this is used for the rotation of the object (if applicable)
   Field mx, my             ; maze level coordinates

   Field cycle_timer        ; this is used for example to have a rotating object
   Field cycle_increment    ; this is the cycle incrementation value (- or + values work)
   Field cycle_reset        ; this is the cycle reset point
   Field cycle_start        ; this is the cycle start point

   Field life               ; this is the life counter of the bullet in # of frames
                            ;       (ex. 2000 = 40 seconds If life_increment is -1)
   Field life_increment     ; this is used in conjunction with "\life"
                            ;       (use this value as 0 to have infinite life)
   Field life_fade          ; 0 = no fade, 1 = fade in, 2 = fade out, 3 = fade in / out


   Field rotate             ; indicates roughly how the object rotates
                            ;       0 = no rotation
                            ;       1 = full rotate on movement
                            ;       2 = horizontal on movement
                            ;       3 = vertical on movement
                            ;       4 = horizontal on cycle
                            ;       5 = vertical on cycle
                            ;       6 = orbiting cycle ( orbits around xpos# and zpos#)

                            ; ************************ Physics engine / if applicable
   Field use_physics_engine ; 1 = YES, 0 = NO
   Field radius#            ; radius of the object (for collision detection)
   Field Mass#              ; mass of the object
   Field size#              ; size of the object
   Field vx#,vy#,vz#        ; force vectors of the object
   Field Velocity#          ; sum of all velocities

End Type


Function summon_object.object_info (ob_number, use_physics = True, use_life = True)
   x.object_info = New object_info
   x\number = ob_number

   Select ob_number
   Case 1            ; this is the Handball
      x\radius# = 0.25
      x\size# = x\radius# * 2
      x\ptr = CreateSphere(5)
      x\use_physics_engine = use_physics
      x\Mass# = 0.5
      x\rotate = 1
      x\life = 2000
      x\life_increment = -use_life
      ScaleMesh  x\ptr, x\size#, x\size#, x\size#
      EntityRadius x\ptr, x\radius# * 2
      x\brush=CreateBrush()
      BrushTexture x\brush,ptr_texture(23)
      BrushColor x\brush,255,255,255
      PaintEntity x\ptr, x\brush 

   Case 2        ; this is the Poolball
      x\radius# = 1
      x\size# = x\radius# * 2
      x\ptr = CreateSphere(7)
      x\use_physics_engine = True
      x\Mass# = 1.1
      x\rotate = 1
      x\life = 2000
      x\life_increment = -use_life
      ScaleMesh  x\ptr, x\size#, x\size#, x\size#
      EntityRadius x\ptr, x\radius# * 2
      x\brush=CreateBrush()
      BrushTexture x\brush,ptr_texture(22)
      BrushColor x\brush,255,255,255 
      PaintEntity x\ptr, x\brush 

   Case 3        ; this is the Rubberdisc
      x\radius# = 0.5
      x\size# = x\radius# * 2
      x\ptr = CreateCylinder(10) 
      x\use_physics_engine = True
      x\Mass# = 2
      x\rotate = 2
      x\life = 1000
      x\life_increment = -use_life
      ScaleMesh  x\ptr, x\size#, x\size#/5, x\size#
      EntityRadius x\ptr, x\radius# / 2.5, x\size#
      x\brush=CreateBrush()
      BrushTexture x\brush, ptr_texture(25)
      BrushColor x\brush,255,255,255
      PaintEntity x\ptr, x\brush 

   Case 4        ; this is the Beachball
      x\radius# = 2.25
      x\size# = x\radius# * 2
      x\ptr = CreateSphere(10)
      x\use_physics_engine = True
      x\Mass# = 2.5
      x\rotate = 1
      x\life = 2000
      x\life_increment = -use_life
      ScaleMesh  x\ptr, x\size#, x\size#, x\size#
      EntityRadius x\ptr, x\radius# * 2
      x\brush=CreateBrush()
      BrushTexture x\brush,ptr_texture(24)
      BrushColor x\brush,255,255,255
      PaintEntity x\ptr, x\brush 

   Case 5        ; this is the Amigaball
      x\radius# = 0.5
      x\size# = x\radius# * 2
      x\ptr = CreateSphere(6)
      x\use_physics_engine = True 
      x\Mass# = 2.5
      x\rotate = 1
      x\life = 2000
      x\life_increment = -use_life
      ScaleMesh  x\ptr, x\size#, x\size#, x\size#
      EntityRadius x\ptr, x\radius# * 2
      x\brush=CreateBrush()
      BrushTexture x\brush,ptr_texture(21)
      BrushColor x\brush,255,255,255 
      PaintEntity x\ptr, x\brush 

   Case 6        ; this is the Rocket
      x\radius# = 0.35
      x\size# = x\radius# * 2
      x\ptr = CreateCylinder(10) 
      x\use_physics_engine = 2
      x\Mass# = 10
      x\rotate = 0
      x\life = 300
      x\life_increment = -use_life
      ScaleMesh  x\ptr, x\size#, x\size# * 4, x\size#
      EntityRadius x\ptr, x\radius# / 2.5, x\radius#
      x\brush=CreateBrush()
      BrushTexture x\brush, ptr_texture(21)
      BrushColor x\brush,255,255,255
      PaintEntity x\ptr, x\brush 

   Case 7

   Case 8

   Case 9

   End Select

   Return x.object_info

End Function


Function update_objects ()

   Return
End Function


Function object_handle()
Local Hit_Habitants, Hit_World, Hit_Projectiles

   projectilecount = 0

   For b.object_info = Each object_info
      If b\ptr = 0 Then object_deleted = True Else object_deleted = False

      If Not object_deleted Then
         Hit_World       = EntityCollided(b\ptr, world_type) 
         Hit_Projectiles = EntityCollided(b\ptr, projectile_type)
         Hit_Habitants   = EntityCollided(b\ptr, habitant_type) 
         If b\number < 10 And b\number > 0 Then projectilecount = projectilecount + 1

         b\OldX# = b\xpos#
         b\OldY# = b\ypos#
         b\OldZ# = b\zpos#
         If b\use_physics_engine <> False Then
            If Hit_Projectiles Then
               b.object_info = collided_with.object_info (b.object_info, Hit_Projectiles)
               b.object_info = apply_physics.object_info (b.object_info, Hit_Habitants, Hit_World, Hit_Projectiles)
            Else
               b.object_info = apply_physics.object_info (b.object_info, Hit_Habitants, Hit_World, Hit_Projectiles)
            EndIf
            TranslateEntity b\ptr, b\Vx#, b\Vy#, b\Vz#, True 
         EndIf

         If Hit_Habitants <> False And b\number < 10 Then
            found_player = False
            For i = 1 To max_number_of_players
               If player(i)\pivot = Hit_Habitants Then found_player = i : i = max_number_of_players
            Next 

            If found_player > 0 Then
               object_deleted = projectile_habitant_hit(b.object_info, found_player)
            EndIf
         EndIf
      EndIf

      If Not object_deleted Then
         b\xpos# = EntityX#(b\ptr, True) 
         b\ypos# = EntityY#(b\ptr, True) 
         b\zpos# = EntityZ#(b\ptr, True)
         XAngleAdjust# = ((b\xpos# - b\OldX#) / b\radius#) * (90.0 / Pi)
         YAngleAdjust# = ((b\ypos# - b\OldY#) / b\radius#) * (90.0 / Pi)
         ZAngleAdjust# = ((b\zpos# - b\OldZ#) / b\radius#) * (90.0 / Pi)

         If b\rotate = 1 Then
            TurnEntity b\ptr, ZAngleAdjust#, 0, -XAngleAdjust#, True
         ElseIf b\rotate = 2 Then 
            TurnEntity b\ptr, 0, ZAngleAdjust# - XAngleAdjust#, 0, True
         EndIf

         If b\life_increment <> 0 Then
            b\life = b\life + b\life_increment
            If b\life = 0
               FreeEntity b\ptr
               FreeBrush b\brush
               Delete b.object_info
            ElseIf b\life < 50 Then
               EntityAlpha b\ptr,b\life * 0.02
            EndIf
         EndIf

      EndIf

   Next

End Function 


Function projectile_habitant_hit(x.object_info, pl_number)
   Hitting_Force = Abs (x\Velocity#) * (x\mass# * 0.5)
   If Hitting_Force < player(pl_number)\shield_level# Then
      ; player captures projectile
      capture_object(x.object_info, pl_number)
      Return True
   Else
      ; player is hurt
      player(pl_number)\resistance_level# = player(pl_number)\resistance_level# - Hitting_Force
      Return False
   EndIf
End Function


Function capture_object(x.object_info, pl_number)
   player(pl_number)\bullets[x\number] = player(pl_number)\bullets[x\number] + 1
   FreeEntity x\ptr
   FreeBrush x\brush
   Delete x.object_info
   emit_3D_sound("pick1", player(pl_number)\pivot)

End Function


Function collided_with.object_info(x.object_info, Entity_Hit )
   For b.object_info = Each object_info
      If b\use_physics_engine = False Then b\use_physics_engine = True
      If b\life_increment = False Then  b\life_increment = -1 : maze(b\mx, b\my) = 11

      If b\ptr = Entity_Hit Then
         xx# = x\Vx#
         xy# = x\Vy#
         xz# = x\Vz#
         Velocity# = x\Velocity#

         x\Velocity# = GROUND_FRICTION# * b\Velocity# / x\Mass#
         x\Vx# = (xx# - b\Velocity#)
         x\Vy# = (xy# - b\Velocity#)
         x\Vz# = (xz# - b\Velocity#)

         b\Velocity# = GROUND_FRICTION# * Velocity# / b\Mass#
         b\Vx# = (b\Vx# - x\Velocity#)
         b\Vy# = (b\Vy# - x\Velocity#)
         b\Vz# = (b\Vz# - x\Velocity#)

         Return x.object_info
      EndIf
   Next
End Function


Function apply_physics.object_info(x.object_info, Hit_Habitants, Hit_World, Hit_Projectiles)
      Local Nx#, Ny#, Nz#, NFx#, NFy#, NFz#, VdotN#, Entity_Hit

      If Hit_Habitants Or Hit_World Or Hit_Projectiles Then
         If Hit_Habitants Then

         EndIf
         Entity_Hit = 1
      Else
         Entity_Hit = 0
      EndIf

    ;  If Entity_Hit And *********** Then


      If x\Velocity# > 0 ; Calculate the direction vector. The direction vector has a length of 1. 

         Direction_X# = x\Vx# / x\Velocity#
         Direction_Y# = x\Vy# / x\Velocity#
         Direction_Z# = x\Vz# / x\Velocity#

         ; Compute air friction. ; Air friction is dependent on the speed of the entity, and will prevent it from accelerting forever. 
         x\Velocity# = x\Velocity# - (AIR_FRICTION# * x\Velocity# * x\size# / x\Mass#)

         If (x\Velocity# < 0) Then x\Velocity# = 0

         ; Convert the entity's velocity and direction back into a motion vector.
         x\Vx# = Direction_X# * x\Velocity#
         x\Vy# = Direction_Y# * x\Velocity#
         x\Vz# = Direction_Z# * x\Velocity#

         ; If the entity collided with the level, apply ground friction. 
         If Entity_Hit > 0 ; Compute ground friction. Ground friction is not dependent on the speed of the entity. 
            x\Velocity# = x\Velocity# - (GROUND_FRICTION# * x\Velocity# * x\size# / x\Mass#)
         EndIf 

         ; If the entity collided with the level, make it bounce. 
         If Entity_Hit > 0 Then
            ; Calculate bounce: 
            ; Get the normal of the surface which the entity collided with. 
            Nx# = CollisionNX(x\ptr, 1)
            Ny# = CollisionNY(x\ptr, 1)
            Nz# = CollisionNZ(x\ptr, 1)
            ; Compute the dot product of the entity's motion vector and the normal of the surface collided with. 
            VdotN# = (x\Vx# * Nx# + x\Vy# * Ny# + x\Vz# * Nz#)

            ; Calculate the normal force.
            NFx# = -2.0 * Nx# * VdotN#
            NFy# = -2.0 * Ny# * VdotN#
            NFz# = -2.0 * Nz# * VdotN#

            x\Vx# = x\Vx# + NFx#
            x\Vy# = x\Vy# + NFy#
            x\Vz# = x\Vz# + NFz#

         EndIf 

         x\mx = Int ((CurrentLevel\size_x ) * 0.5 - x\xpos# / maze_scale#)
         x\my = Int ((CurrentLevel\size_y ) * 0.5 - x\zpos# / maze_scale#)

      EndIf 

      ; Apply gravity:
      If x\use_physics_engine = 1 Then x\Vy# = x\Vy# - GRAVITY# * x\Mass#

      Return x.object_info
End Function


;----------------------------------------------------------------------------------------
; Player management routines
;----------------------------------------------------------------------------------------

; this is the per-habitant data
Type habitant_info
   Field hab_type          ; 1     = player
                           ; 2 - 10 = bot
   Field owner_ip          ; the ip address of the owning system of this habitant
   Field current_level     ; incremental number representing level
   Field grid_xpos         ; x position in maze grid
   Field grid_ypos         ; y position in maze grid
   Field in_world          ; 0 = no, 1 = yes
   Field pivot             ; pointer to the player's pivot data
   Field x#                ; world X coordinates
   Field Y#                ; world Y coordinates
   Field Z#                ; world Z coordinates
;   Field pitch#          ;
;   Field yaw#            ;
;   Field roll#           ;
   Field vx#,vy#,vz#        ; force vectors of the object
   Field Velocity#          ; sum of all vectors

   Field out_of_body       ; determines First Person view or 3rd person (0 = first person)
   Field shot_timer        ; indicates how much time for recharge
   Field current_weapon    ; player's currently selected weapon
   Field visor             ; 0 = don't dhow visor, 1 = show visor
   Field bullets[9]        ; bulletcount for every weapons
   Field throwing_strength ; players throwing strength
   Field moving_speed#     ; This is the actual player moving speed
   Field walking_speed#    ; This is the default speed in which the player moves
   Field running_speed#    ; This is the default speed in which the player moves
   Field compass           ; 0 = no compass, 1 = standard compass, 2 = advanced compass
   Field marks             ; number of marks that player has

   Field shield_level#     ; This combined with the collision with an object defines
                           ; the vulnerability of a player
                           ; 0 = vulnerable 100% of the time
                           ; 5 = good level of shield
                           ; 10= highest level of shield

   Field resistance_level# ; This value indicates if player lives or dies
                           ; 0 = death
                           ; 50 = good level of resistance
                           ; 100 = highest level of resistance

End Type


Function new_player(pl_number, lvl)
   player.habitant_info(pl_number) = New habitant_info
   player(pl_number)\hab_type = 1
   player(pl_number)\owner_ip = 0     ; this is wrong atm, should contain the machine's IP address
   player(pl_number)\current_level = lvl

   player(pl_number)\in_world = 0

   player(pl_number)\x# = 0
   player(pl_number)\y# = 4
   player(pl_number)\z# = 0

   player(pl_number)\vx# = 0
   player(pl_number)\vy# = 0
   player(pl_number)\vz# = 0

   player(pl_number)\out_of_body = 0
   player(pl_number)\shot_timer  = 0
   player(pl_number)\current_weapon = 0

   player(pl_number)\bullets[1] = 50
   player(pl_number)\bullets[2] = 30
   player(pl_number)\bullets[3] = 50
   player(pl_number)\bullets[4] = 20
   player(pl_number)\bullets[5] = 50
   player(pl_number)\bullets[6] = 10
   player(pl_number)\bullets[7] = 0
   player(pl_number)\bullets[8] = 0
   player(pl_number)\bullets[9] = 0

   player(pl_number)\throwing_strength = 5

   player(pl_number)\walking_speed# = 0.5
   player(pl_number)\running_speed# = 1.5

   player(pl_number)\shield_level# = 4
   player(pl_number)\resistance_level# = 50

   number_of_players = number_of_players + 1
End Function


Function player_init_level(pl_number)
   player(pl_number)\compass = CurrentLevel\compass
   player(pl_number)\marks   = CurrentLevel\marks

End Function

Function player_exists(pl_number)
   For player.habitant_info(pl_number)  = Each habitant_info
      Return True
   Next
   Return False
End Function


Function remove_player(pl_number)
;   FreeEntity player(pl_number)\pivot
   Delete player.habitant_info(pl_number)
   number_of_players = number_of_players - 1

End Function


Function insert_local_player_in_world(pl_number)
   player(pl_number)\pivot = CreatePivot()
   PositionEntity player(pl_number)\pivot, player(pl_number)\x#, player(pl_number)\y#, player(pl_number)\z#
   EntityRadius player(pl_number)\pivot,4
   EntityType player(pl_number)\pivot,habitant_type

   ptr_camera = CreateCamera()
   CameraViewport ptr_camera,0, 0, GraphicsWidth (), GraphicsHeight ()
   CameraRange ptr_camera,1.5,2000
   PositionEntity ptr_camera, player(pl_number)\x#, player(pl_number)\y#, player(pl_number)\z#
;   EntityType ptr_camera,habitant_type
   player(pl_number)\in_world = 1

   ptr_microphone = CreateListener (ptr_camera, 0.1, 0.1, 0.1)

End Function


Function remove_player_from_world(pl_number)
   FreeEntity player(pl_number)\pivot
   FreeEntity ptr_camera
   player(pl_number)\y# = player(pl_number)\y# + 1
   player(pl_number)\vx# = 0
   player(pl_number)\vy# = 0
   player(pl_number)\vz# = 0
   player(pl_number)\in_world = 0
End Function


;----------------------------------------------------------------------------------------
; Bot management routines
;----------------------------------------------------------------------------------------
; this is the per-bot data


Function habitant_handle()
   For i = 1 To number_of_players
    ;  Hit_World       = EntityCollided(player(i)\pivot, world_type) 
    ;  Hit_Habitants   = EntityCollided(player(i)\pivot, habitant_type) 

      Hit_Projectiles = EntityCollided(player(i)\pivot, projectile_type)
      If Hit_Projectiles > 0 Then
         For x.object_info = Each object_info
            If x\ptr = Hit_Projectiles Then
               projectile_habitant_hit(x.object_info, i)
            EndIf
         Next
      EndIf
   Next

End Function


Function update_habitants()

End Function


Function manage_bot_ai.habitant_info(bot.habitant_info)
; will determine bot priorities depending on it's type
;
; ============= bot AI for an evil doer =============
; #1 priority = make sure the bot has enough projectiles
; #2 priority = attack players
; #3 priority = search for players
;


   Return bot.habitant_info
End Function


Function insert_local_bot_in_world()

End Function


;----------------------------------------------------------------------------------------
; Setup the graphics mode, put all the graphics, load the textures, etc etc
;----------------------------------------------------------------------------------------
Function InitWorld(pl_number)
   StopChannel MusicChannel

   set_graphics_mode()

   InitTextures()

   ptr_light = CreateLight()
   MoveEntity ptr_light, 0, 2, 20
   ;PointEntity ptr_light, ptr_camera

   ptr_ground	= CreatePlane()
   EntityTexture ptr_ground, ptr_texture(9),0,1
   EntityAlpha ptr_ground, 0.75
   EntityType ptr_ground, world_type

   ptr_mirror = CreateMirror(ptr_ground)

   ; this is the super planetlike sphere
   huge_sphere = CreateSphere(32)
   ScaleMesh huge_sphere, 200,200,200
   PositionEntity huge_sphere, 0,600,0
   EntityTexture huge_sphere, ptr_copyscreen,0,1
   EntityType huge_sphere, world_type

   ; this sphere rotates around...
   rotating_sphere = CreateSphere(12)
   ScaleMesh rotating_sphere,64,64,64
   EntityTexture rotating_sphere, ptr_texture(10),0,1
   EntityType rotating_sphere, world_type

   sphere1 = CreateSphere(24)
   ScaleMesh sphere1, 64, 64, 64
   PositionEntity sphere1, 224, 48,224
   EntityType sphere1, world_type

   sphere2 = CreateSphere(24)
   ScaleMesh sphere2, 64, 64, 64
   PositionEntity sphere2, -224, 48,224
   EntityType sphere2, world_type

   sphere3 = CreateSphere(24)
   ScaleMesh sphere3, 64, 64, 64
   PositionEntity sphere3, -224, 48,-224
   EntityType sphere3, world_type

   sphere4 = CreateSphere(24)
   ScaleMesh sphere4, 64, 64, 64
   PositionEntity sphere4, 224, 48, -224
   EntityType sphere4, world_type

   Collisions habitant_type, world_type,2, 2
   Collisions habitant_type, habitant_type, 1, 2
   Collisions habitant_type, projectile_type, 1, 2

   Collisions projectile_type, world_type,2,2
   Collisions projectile_type, projectile_type, 1, 2
   Collisions projectile_type, habitant_type, 1, 2

   ;MakeCubeRoom()

   WallUnitSeed(1,1)
   WallCopySeed(1,2)
   WallCopySeed(1,3)
   WallCopySeed(1,4)
   WallUnitSeed(5,2)

   WallUnitHandle()

End Function


; remove all objects / textures from memory
Function DeleteWorld()
   DeleteMazeWalls()
   DeleteAllStars()
   DeleteWallUnit()
   DeleteCubeScroller()

   clear_all_bullets()
   clear_all_textures()
End Function


Function InitNewGame(lvl, pl_number)
   cam_viewport_pitch# = 0
   cam_viewport_yaw# = 0

   cuberoom = 0           ; this flag indicates if the cuberoom is built or not (1 or 0)
   newscrollitem = 0      ;Delaycounter For Next scrolltext-cube
   ingame_counter360 = 0
   sc_offset = 1          ;Offset For relevant character of scrolltext$

   fx_direction# = +0.001
   fx_u# = + 0.01
   fx_v# = + 0.2

   For i = 1 To 5: wall_angle(i) = 0: wall_rotate(i) = 0: Next
   For i = 1 To 5: sphere_height#(i) = 55: sphere_direction#(i) = 0: Next

   CurrentGameLevel = SetCurrentLevel(lvl) ; new game
   init_scrolltext()
   init_maze(1)

   new_player(pl_number, lvl)
   player_init_level(pl_number)

End Function


Function init_scrolltext()
      scrolltext$ = "              .........Welcome.to.<<<" + Trim$(CurrentLevel\desc$) + ">>>............Here's.the.rundown.of.things.........      ....."

      If CurrentLevel\mysteries > 0 Then
         scrolltext$ = scrolltext$ + "number.of.mysteries:.....<" + CurrentLevel\mysteries + ">"
      Else
         scrolltext$ = scrolltext$ + "no.mysteries"
      EndIf

      If CurrentLevel\marks > 0 Then
         scrolltext$ = scrolltext$ + "..........number.of.marks.given.to.you:.....<" + CurrentLevel\marks + ">"
      EndIf

      If CurrentLevel\map > 0 Or CurrentLevel\compass > 0 Then
         scrolltext$ = scrolltext$ + ".........."
         If CurrentLevel\map = 1 Then
            scrolltext$ = scrolltext$ + "a.map"
         ElseIf CurrentLevel\map = 2 Then
            scrolltext$ = scrolltext$ + "an.upgraded.map"
         ElseIf CurrentLevel\map = 3 Then
            scrolltext$ = scrolltext$ + "a.very.detailed.map"
         EndIf

         If CurrentLevel\map > 0 And CurrentLevel\compass > 0 Then
            scrolltext$ = scrolltext$ + ".and."
         EndIf

         If CurrentLevel\compass = 1 Then
            scrolltext$ = scrolltext$ + "a.compass"
         ElseIf CurrentLevel\compass = 2 Then
            scrolltext$ = scrolltext$ + "an.upgraded.compass"
         ElseIf CurrentLevel\compass = 3 Then
            scrolltext$ = scrolltext$ + "a.very.useful.compass"
         EndIf

         scrolltext$ = scrolltext$ + ".was.supplied.to.you"
      EndIf

      scrolltext$ = scrolltext$ + ".........."
      If CurrentLevel\bots > 0 Then
         scrolltext$ = scrolltext$ + "number.of.evil.doers:.....<" + CurrentLevel\bots + ">"
      EndIf

      If CurrentLevel\weaponrange = 0 Then
         scrolltext$ = scrolltext$ + ".....There.are.absolutely.no.weapons.around"
      Else
         scrolltext$ = scrolltext$ + ".....a.load.of.<" + CurrentLevel\projectiles + ">.projectiles.are.spread.around,.which.are"
         If CurrentLevel\weaponrange = 1 Then
            scrolltext$ = scrolltext$ + ".all."
         Else
            scrolltext$ = scrolltext$ + ":....."
         EndIf
         For i = 1 To CurrentLevel\weaponrange
            If i > 1 Then scrolltext$ = scrolltext$ + ",....."
            scrolltext$ = scrolltext$ + Trim$(weapon_name$(i)) + "s"
         Next
      EndIf
      scrolltext$ = scrolltext$ + "........."

      scrolltext$ = Upper$(scrolltext$)

End Function


;----------------------------------------------------------------------------------------
; Setup the textures
;----------------------------------------------------------------------------------------

Function InitTextures()
   Local i,j,x,y,x2,y2,xf#,yf#,r,g,b,r2,g2,b2,texture_number,number_of_textures,number_of_funcs,func$

   Restore texture_data
   Read number_of_textures

   Dim ptr_texture(number_of_textures)

   For i = 1 To number_of_textures
      Read texture_number
      Read number_of_funcs

      For j = 1 To number_of_funcs

         Read func$

         Select Lower$(func$)

         Case "tex"
            Read x,y
            ptr_texture(texture_number) = CreateTexture (x, y, 1 + 2 + 256)
            SetBuffer TextureBuffer (ptr_texture(texture_number))

         Case "fnt"
            Read fn$, x, y, start_char, end_char
            ptr_fnt = LoadFont(fn$, 85, True, False, False)
            SetFont ptr_fnt
            ClsColor 255,0,0
            l = (end_char - start_char) + i
            ch_offset = i - start_char
            For k = i To l
               ptr_texture(i) = CreateTexture (x, y, 256)
               SetBuffer TextureBuffer (ptr_texture(i))
               Cls
               Color 0, 255, 0
               Text 31,29,Chr$(ch_offset + k),True,True
               i = i + 1
            Next
            FreeFont ptr_fnt

         Case "color"
            Read r,g,b
            Color r,g,b

         Case "fcolor"
            Read r,g,b
            ClsColor r,g,b

         Case "fill"
            Read x1,y1,x2,y2
            Rect x1,y1,x2,y2,1

         Case "rect"
            Read x1,y1,x2,y2
            Rect x1,y1,x2,y2,0

         Case "oval"
            Read x1,y1,x2,y2
            Oval x1,y1,x2,y2

         Case "scale"
            Read xf#,yf#
            ScaleTexture ptr_texture(texture_number),xf#,yf#

         Case "gradient"
            Read r,g,b,x,y,r2,g2,b2,x2,y2
            make_gradient(r,g,b,x,y,r2,g2,b2,x2,y2)

       ;  Case "unblurr"
       ;     TextureFilter ptr_texture(texture_number),

         End Select

      Next

   Next

   ; will be reorganized later
   ptr_texture(22) = create_pox_tex    (000,255,255,000,000,255,.25,.25)
   ptr_texture(23) = create_stripe_tex (2,.25,.25)
   ptr_texture(24) = create_stripe_tex (1,.5,.5)

End Function


Function clear_all_textures()
   For i = 1 To number_of_textures
      FreeEntity ptr_texture(texture_number)
   Next

End Function


Function make_gradient(r1, g1, b1, x1, y1, r2, g2, b2, x2, y2)
   Return
End Function


Function create_pox_tex(red1, green1, blue1, red2, green2, blue2, scale_u#, scale_v#) 
   texture_handle = CreateTexture(32,32,256) 
   SetBuffer TextureBuffer(texture_handle) 
   Color red1,green1,blue1 
   Rect 0,0,32,32
   Color red2,green2,blue2 
   Oval 0,0,16,16,1 
   Oval 16,16,16,16,1 
   ScaleTexture texture_handle, scale_u#, scale_v# 
   Return texture_handle 

End Function 


Function create_stripe_tex(direction,scale_u#,scale_v#)
   If direction = 1 Then h = 1: v = 0
   If direction = 2 Then h = 0: v = 1

   texture_handle = CreateTexture(32,32,256) 
   SetBuffer TextureBuffer(texture_handle)
   Color 255,255,0 
   Rect 0,0,32,32
   Color 0,255,0 
   Rect 8 * h, 8 * v, 32 * v + 8 * h, 8 * v + 32 * h
   Color 255,0,0 
   Rect 16 * h, 16 * v, 32 * v + 8 * h, 8 * v + 32 * h
   Color 0,0,255 
   Rect 24 * h, 24 * v, 32 * v + 8 * h, 8 * v + 32 * h
   ScaleTexture texture_handle,scale_u#,scale_v# 
   Return texture_handle 

End Function 


;----------------------------------------------------------------------------------------
.texture_data
;----------------------------------------------------------------------------------------
Data 96
Data 1, 6,"tex",64,64,"color",64,128,255,"fill",0,0,64,64,"color",255,128,64,"fill",0,0,32,32,"fill",32,32,32,32
Data 2, 6,"tex",64,64,"color",255,0,0,"fill",0,0,64,64,"color",0,255,0,"fill",0,0,32,32,"fill",32,32,32,32
Data 3, 6,"tex",64,64,"color",0,0,255,"fill",0,0,64,64,"color",255,255,0,"fill",0,0,32,32,"fill",32,32,32,32
Data 4, 6,"tex",64,64,"color",255,0,255,"fill",0,0,64,64,"color",0,255,255,"fill",0,0,32,32,"fill",32,32,32,32
Data 5, 5,"tex",64,64,"color",128,64,255,"fill",0,0,64,64,"color",255,128,64,"oval",0,0,64,64
Data 6, 5,"tex",64,64,"color",128,255,64,"fill",0,0,64,64,"color",128,64,255,"oval",0,0,64,64
Data 7, 5,"tex",64,64,"color",0,255,0,"fill",0,0,64,64,"color",255,0,0,"oval",0,0,64,64
Data 8, 5,"tex",64,64,"color",0,0,255,"fill",0,0,64,64,"color",0,255,0,"oval",0,0,64,64
Data 9, 8,"tex",64,64,"color",255,255,255,"fill",0,0,64,64,"color",128,128,128,"fill",0,0,32,32,"color",64,64,64,"fill",32,32,32,32,"scale",2,2
Data 10,1,"tex",64,64
Data 11,7,"tex",64,64,"color",128,160,192,"fill",0,0,64,64,"color",96,128,160,"fill",0,0,32,32,"fill",32,32,32,32,"scale",1,0.1
Data 12,8,"tex",64,64,"color",255,192,160,"fill",0,0,64,64,"color",0,128,255,"rect",0,0,64,64,"rect",1,1,63,63,"rect",2,1,62,63,"scale",1,0.1
Data 13,0
Data 14,0
Data 15,0
Data 16,0
Data 17,2,"tex",64,64,"gradient",0,0,0,0,0,255,255,255,64,64
Data 18,0
Data 19,0
Data 20,0
Data 21,7,"tex",32,32,"color",255,255,255,"fill",0,0,32,32,"color",224,0,0,"fill",0,0,16,16,"fill",16,16,16,16,"scale",.166667,.333334
Data 22,0
Data 23,0
Data 24,0
Data 25,6,"tex",32,32,"color",0,255,0,"fill",0,0,32,32,"color",255,0,255,"fill",0,0,16,16,"fill",16,16,16,16
Data 26,0
Data 27,0
Data 28,0
Data 29,0
Data 30,0
Data 31,0
Data 32,1,"fnt","Courrier New",64,64,32,96



;----------------------------------------------------------------------------------------
; Level management routines
;----------------------------------------------------------------------------------------
; level basic definition
Type level_info
   Field number
   Field desc$
   Field size_x
   Field size_y
   Field marks
   Field mysteries
   Field map
   Field compass
   Field botrange
   Field bots
   Field weaponrange
   Field projectiles
End Type


Function InitLevelTable()
Local i

   Restore level_data
   Read number_of_levels

   For i = 1 To number_of_levels
      level.level_info = New level_info
      Read level\number
      Read level\desc$
      Read level\size_x,      level\size_y
      Read level\marks
      Read level\mysteries
      Read level\map
      Read level\compass
      Read level\botrange,    level\bots
      Read level\weaponrange, level\projectiles
   Next

End Function


Function SetCurrentLevel(level_number)
   For level.level_info = Each level_info
      If level\number = level_number Then
         CurrentLevel\number      = level\number
         CurrentLevel\desc$       = level\desc$
         CurrentLevel\size_x      = level\size_x
         CurrentLevel\size_y      = level\size_y
         CurrentLevel\marks       = level\marks
         CurrentLevel\mysteries   = level\mysteries
         CurrentLevel\map         = level\map
         CurrentLevel\compass     = level\compass
         CurrentLevel\botrange    = level\botrange
         CurrentLevel\bots        = level\bots
         CurrentLevel\weaponrange = level\weaponrange
         CurrentLevel\projectiles = level\projectiles

         Return level_number
      EndIf
   Next
End Function


;----------------------------------------------------------------------------------------
.level_data
;----------------------------------------------------------------------------------------
;    lvl label           size     marks  mystries  map  compass  bot #     weapon #
;----------------------------------------------------------------------------------------
Data 33
Data 1, "TEST PATTERN ", 89, 89,  5,     1,        0,   0,       0,  0,    0,     0    ;6226 tiles, none
Data 2, "GLASSHOUSE   ", 97, 97,  8,     2,        0,   0,       0,  0,    1,     10   ;6978 tiles, none
Data 3, "ENCOUNTER    ", 89, 121, 10,    4,        0,   0,       1,  1,    1,     20   ;7862 tiles, 2 deformed
Data 4, "MIRRORIZE    ", 125,97,  10,    6,        0,   1,       1,  5,    1,     30   ;8848 tiles, 2 deformed
Data 5, "*            ", 109,125, 12,    8,        0,   1,       2,  8,    1,     40   ;9670 tiles, 4 deformed
Data 6, "WHITE NOISE  ", 125,125, 15,    10,       0,   1,       2,  10,   2,     50   ;10498 tiles, none
Data 7, "*            ", 173,101, 15,    12,       0,   1,       2,  13,   2,     60   ;13260 tiles, 2 full, 2 deformed
Data 8, "*            ", 125,173, 15,    15,       1,   1,       2,  15,   2,     70   ;15058 tiles, none
Data 9, "*            ", 201,145, 20,    18,       1,   1,       3,  18,   2,     80   ;18886 tiles, none
Data 10,"*            ", 175,201, 20,    20,       1,   2,       3,  22,   3,     90   ;22268 tiles, 4 centers
Data 11,"*            ", 233,125, 20,    25,       1,   2,       3,  25,   3,     100  ;25238 tiles, 4 full
Data 12,"PANCAKE      ", 203,203, 25,    30,       1,   2,       3,  25,   3,     110  ;25157 tiles, 4 centers
Data 13,"*            ", 213,189, 25,    33,       1,   2,       3,  31,   3,     120  ;31082 tiles, 4 centers
Data 14,"*            ", 173,269, 30,    35,       2,   2,       4,  34,   4,     130  ;34242 tiles, centers
Data 15,"*            ", 285,169, 33,    40,       2,   2,       4,  35,   4,     140  ;35062 tiles, corners
Data 16,"VIDEODROME   ", 253,197, 35,    45,       2,   3,       4,  35,   5,     150  ;35898 tiles, centers
Data 17,"*            ", 233,233, 40,    50,       2,   3,       4,  38,   5,     160  ;38130 tiles, corners
Data 18,"*            ", 197,285, 50,    60,       2,   3,       4,  39,   5,     170  ;39066 tiles, centers
Data 19,"*            ", 253,253, 50,    70,       2,   3,       5,  43,   5,     180  ;43010 tiles, corners
Data 20,"THE ZONE     ", 333,221, 75,    85,       2,   3,       5,  47,   5,     190  ;47826 tiles, centers
Data 21,"*            ", 285,285, 80,    90,       2,   3,       6,  51,   5,     200  ;51650 tiles, centers
Data 22,"*            ", 223,365, 99,    99,       2,   3,       6,  51,   5,     210  ;51679 tiles, corners
Data 23,"ALMOST THERE ", 299,301, 100,   105,      2,   3,       6,  55,   5,     220  ;55987 tiles, corners
Data 24,"*            ", 365,283, 110,   120,      2,   3,       7,  62,   5,     230  ;62660 tiles, corners
Data 25,"SQUAREWAVE   ", 333,333, 127,   127,      2,   3,       7,  66,   5,     240  ;66530 tiles, centers
Data 26,"*            ", 221,517, 130,   139,      2,   3,       7,  68,   5,     250  ;68250 tiles, centers
Data 27,"*            ", 377,323, 144,   157,      2,   3,       8,  72,   5,     260  ;72338 tiles, all 8
Data 28,"*            ", 283,449, 157,   163,      2,   3,       8,  74,   5,     270  ;74587 tiles, corners
Data 29,"*            ", 421,333, 166,   170,      2,   3,       8,  81,   5,     280  ;81226 tiles, centers
Data 30,"*            ", 379,379, 179,   179,      2,   3,       8,  83,   5,     290  ;83225 tiles, all 8
Data 31,"SIXTEEN NINE ", 517,303, 188,   200,      2,   3,       9,  89,   5,     300  ;89397 tiles, centers
Data 32,"MEGABLOCKS   ", 401,401, 201,   201,      2,   3,       9,  92,   5,     350  ;92008 tiles, all 8
Data 33,"OMFG WTFITS  ", 449,397, 233,   245,      2,   3,       9,  100,  5,     500  ;100779 tiles, all 8


;----------------------------------------------------------------------------------------
; Maze wall management routine
;----------------------------------------------------------------------------------------
; level basic definition
Type wall_info
   Field number
   Field desc$
   Field tex
   Field alpha
   Field shine
   Field scale_x
   Field scale_y
End Type


Function InitMazewallTable()
Local i

   Restore wall_data
   Read number_of_wall_defs

   For i = 1 To number_of_wall_defs
      wall.wall_info = New wall_info
      Read wall\number
      Read wall\desc$
      Read wall\tex
      Read wall\alpha
      Read wall\shine
      Read wall\scale_x
      Read wall\scale_y
   Next

End Function


;----------------------------------------------------------------------------------------
.wall_data
;----------------------------------------------------------------------------------------
;    nbr label        tex  alpha  shine  scale x  y    (These should be used for brushes)
;----------------------------------------------------------------------------------------
Data 9
Data 1, "GLASS     ", 11,  .75,   0,           1, 1  
Data 2, "          ", 0,   0,     0,           0, 0
Data 3, "          ", 0,   0,     0,           0, 0
Data 4, "          ", 0,   0,     0,           0, 0
Data 5, "          ", 0,   0,     0,           0, 0
Data 6, "          ", 0,   0,     0,           0, 0
Data 7, "          ", 0,   0,     0,           0, 0
Data 8, "          ", 0,   0,     0,           0, 0
Data 9, "          ", 0,   0,     0,           0, 0



