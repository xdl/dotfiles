#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.
SetTitleMatchMode, 2 ; Match Windows title anywhere
; Another option: ahk_exe Godot_v4.1.1-stable_win64.exe

F3::
if WinExist("Godot Engine")
        WinActivate ; Use the window found by WinExist.
        WinWaitActive
        Send, {F5}
