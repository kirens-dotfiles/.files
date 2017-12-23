{ }:
''
! clear all CapsLock modifiers
clear Lock
! Set CapsLock to BackSpace
keycode 66 = BackSpace Delete Caps_Lock
! Add CapsLock modifier again
add Lock = Caps_Lock

! Remove Shift_L and Alt_L
remove Shift = Shift_L
remove Mod1 = Alt_L 
! remap lAlt <-> lShift
keysym Alt_L = Shift_L
keysym Shift_L = Alt_L
! Add as modifiers again
add Shift = Shift_L
add Mod1 = Alt_L
''
