;----------------------------------------------------------------------------------------
; KEYBOARD FUNCTIONS AND DEFINITIONS
;----------------------------------------------------------------------------------------

; This is the whole scancode table

Const KEY_ESCAPE = 1 
Const KEY_1 = 2 
Const KEY_2 = 3 
Const KEY_3 = 4 
Const KEY_4 = 5 
Const KEY_5 = 6 
Const KEY_6 = 7 
Const KEY_7 = 8 
Const KEY_8 = 9 
Const KEY_9 = 10 
Const KEY_0 = 11 
Const KEY_MINUS = 12 
Const KEY_EQUALS = 13 
Const KEY_BACKSPACE = 14 
Const KEY_TAB = 15 
Const KEY_Q = 16 
Const KEY_W = 17 
Const KEY_E = 18 
Const KEY_R = 19 
Const KEY_T = 20 
Const KEY_Y = 21 
Const KEY_U = 22 
Const KEY_I = 23 
Const KEY_O = 24 
Const KEY_P = 25 
Const KEY_LEFT_BRACKET = 26 
Const KEY_RIGHT_BRACKET = 27 
Const KEY_RETURN = 28 
Const KEY_LEFT_CONTROL = 29 
Const KEY_A = 30 
Const KEY_S = 31 
Const KEY_D = 32 
Const KEY_F = 33 
Const KEY_G = 34 
Const KEY_H = 35 
Const KEY_J = 36 
Const KEY_K = 37 
Const KEY_L = 38 
Const KEY_SEMICOLON = 39 
Const KEY_APOSTROPHE = 40 
Const KEY_GRAVE = 41 
Const KEY_LEFT_SHIFT = 42 
Const KEY_BACKSLASH = 43 
Const KEY_Z = 44 
Const KEY_X = 45 
Const KEY_C = 46 
Const KEY_V = 47 
Const KEY_B = 48 
Const KEY_N = 49 
Const KEY_M = 50 
Const KEY_COMMA = 51 
Const KEY_PERIOD = 52 
Const KEY_SLASH = 53 
Const KEY_RIGHT_SHIFT = 54 
Const KEY_MULTIPLY = 55 
Const KEY_LEFT_ALT = 56 
Const KEY_SPACE = 57 
Const KEY_CAPITAL = 58 
Const KEY_F1 = 59 
Const KEY_F2 = 60 
Const KEY_F3 = 61 
Const KEY_F4 = 62 
Const KEY_F5 = 63 
Const KEY_F6 = 64 
Const KEY_F7 = 65 
Const KEY_F8 = 66 
Const KEY_F9 = 67 
Const KEY_F10 = 68 
Const KEY_NUMLOCK = 69 
Const KEY_SCROLLLOCK = 70 
Const KEY_NUMPAD7 = 71 
Const KEY_NUMPAD8 = 72 
Const KEY_NUMPAD9 = 73 
Const KEY_SUBTRACT = 74 
Const KEY_NUMPAD4 = 75 
Const KEY_NUMPAD5 = 76 
Const KEY_NUMPAD6 = 77 
Const KEY_ADD = 78 
Const KEY_NUMPAD1 = 79 
Const KEY_NUMPAD2 = 80 
Const KEY_NUMPAD3 = 81 
Const KEY_NUMPAD0 = 82 
Const KEY_DECIMAL = 83 
Const KEY_OEM_102 = 86 
Const KEY_F11 = 87 
Const KEY_F12 = 88 
Const KEY_F13 = 100 
Const KEY_F14 = 101 
Const KEY_F15 = 102 
Const KEY_KANA = 112 
Const KEY_ABNT_C1 = 115 
Const KEY_CONVERT = 121 
Const KEY_NOCONVERT = 123 
Const KEY_YEN = 125 
Const KEY_ABNT_C2 = 126 
Const KEY_NUMPAD_EQUALS = 141 
Const KEY_PREVTRACK = 144 
Const KEY_AT = 145 
Const KEY_COLON = 146 
Const KEY_UNDERLINE = 147 
Const KEY_KANJI = 148 
Const KEY_STOP = 149 
Const KEY_AX = 150 
Const KEY_UNLABELED = 151 
Const KEY_NEXTTRACK = 153 
Const KEY_ENTER = 156 
Const KEY_RIGHT_CONTROL = 157 
Const KEY_MUTE = 160 
Const KEY_CALCULATOR = 161 
Const KEY_PLAY_PAUSE = 162 
Const KEY_MEDIASTOP = 164 
Const KEY_VOLUME_DOWN = 174 
Const KEY_VOLUME_UP = 176 
Const KEY_WEB_HOME = 178 
Const KEY_NUMPAD_COMMA = 179 
Const KEY_DIVIDE = 181 
Const KEY_SYSREQ = 183 
Const KEY_RIGHT_ALT = 184 
Const KEY_PAUSE = 197 
Const KEY_HOME = 199 
Const KEY_UP = 200 
Const KEY_PAGEUP = 201 
Const KEY_LEFT = 203 
Const KEY_RIGHT = 205 
Const KEY_END = 207 
Const KEY_DOWN = 208 
Const KEY_NEXT = 209 
Const KEY_INSERT = 210 
Const KEY_DELETE = 211 
Const KEY_LEFTWINDOWS = 219 
Const KEY_RIGHTWINDOWS = 220 
Const KEY_APPS = 221 
Const KEY_POWER = 222 
Const KEY_SLEEP = 223 
Const KEY_WAKE = 227 
Const KEY_WEBSEARCH = 229 
Const KEY_WEBFAVORITES = 230 
Const KEY_WEBREFRESH = 231 
Const KEY_WEBSTOP = 232 
Const KEY_WEBFORWARD = 233 
Const KEY_WEBBACK = 234 
Const KEY_MYCOMPUTER = 235 
Const KEY_MAIL = 236 
Const KEY_MEDIASELECT = 237


Dim key_click(255) 

Function key_clicked(key_number)
      If KeyDown(key_number) Then
         If key_click(key_number) = 0 Then
            key_click(key_number) = 1
         ElseIf key_click(key_number) = 1 Then
            key_click(key_number) = 2
         EndIf
      Else
         key_click(key_number) = 0
      EndIf
      If key_click(key_number) = 1 Then Return 1 Else Return 0
End Function



;----------------------------------------------------------------------------------------
; MOUSE FUNCTIONS AND DEFINITIONS
;----------------------------------------------------------------------------------------

Dim mouse_click(3)

Function update_mouse_click()
; global mouse_click(3)
   For i = 1 To 3
      If MouseDown(i) Then
         If mouse_click(i) = 0 Then
            mouse_click(i) = 1
         ElseIf mouse_click(i) = 1 Then
            mouse_click(i) = 2
         EndIf
      Else
         mouse_click(i) = 0
      EndIf
   Next
End Function


