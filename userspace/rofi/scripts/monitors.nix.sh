{ xrandr, rofi, xinput }:
''
print_menu() {
  printf "Normal\nOnly internal monitor\0"
  printf "External\nOnly HDMI monitor\0"
  printf "Stacked\nExternal monitor above internal\0"
  printf "Mirrored\nExternal monitor mirroring internal\0"
  printf "Current\nOnly perform post remap hook\0"
}
option_count=4

res=''$(print_menu | ${rofi} -dmenu -i -sep '\0' -lines "$option_count" -eh 2 -p 'Monitor Layout' -no-custom -format i)

if [ -z "$res" ] ; then
    exit
fi

case "$res" in
    0)
        ${xrandr} \
          --output HDMI-2 --off \
          --output HDMI-1 --off \
          --output DP-1 --off \
          --output eDP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal \
          --output VIRTUAL-1 --off
        ;;
    1)
        ${xrandr} \
          --output HDMI-2 --off \
          --output HDMI-1 --mode 1920x1080 --pos 0x0 --rotate normal \
          --output DP-1 --off \
          --output eDP-1 --off \
          --output VIRTUAL-1 --off
        ;;
    2)
        ${xrandr} \
          --output HDMI-2 --off \
          --output HDMI-1 --mode 1920x1080 --pos 0x0 --rotate normal \
          --output DP-1 --off \
          --output eDP-1 --primary --mode 1920x1080 --pos 0x1080 --rotate normal \
          --output VIRTUAL-1 --off
        ;;
    3)
        ${xrandr} \
          --output HDMI-2 --off \
          --output HDMI-1 --mode 1920x1080 --pos 0x0 --rotate normal \
          --output DP-1 --off \
          --output eDP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal \
          --output VIRTUAL-1 --off
        ;;
    *)
        ;;
esac

# Remap touchscreen properly
${xinput} map-to-output $(xinput list --id-only "ELAN Touchscreen") eDP1
${xinput} map-to-output $(xinput list --id-only "ELAN Touchscreen Pen") eDP1
''
