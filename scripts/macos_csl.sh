__ScriptVersion="0.1"


function usage ()
{
    echo "Usage :  $0 [options] [--]

    Options:
    -h|help       Display this message
    -s|set-screenshot-location <Screenshot Location> Sets screenshot location
    -g|get-screenshot-location  Gets screenshot path
    -v|version    Display script version"

}    # ----------  end of function usage  ----------

# directory needs to exist, otherwise screenshots will keep getting dumped to default location: ~/Desktop
function setScreenshotPath () {
    path=$1
    defaults write com.apple.screencapture location $path
    killall SystemUIServer
    echo "com.apple.screencapture location set to $path"
}

# directory needs to exist, otherwise screenshots will keep getting dumped to default location:
function getScreenshotPath () {
    defaults read com.apple.screencapture location
}

#-----------------------------------------------------------------------
#  Handle command line arguments
#-----------------------------------------------------------------------

while getopts ":hvs:g" opt
do
  case $opt in

    s|set-screenshot   )  setScreenshotPath $OPTARG; exit 0 ;;

    g|get-screenshot   )  getScreenshotPath; exit 0 ;;

    h|help     )  usage; exit 0   ;;

    v|version  )  echo "$0 -- Version $__ScriptVersion"; exit 0   ;;

    * )  echo -e "\n  Option does not exist : $OPTARG\n"
          usage; exit 1   ;;

  esac    # --- end of case ---
done
shift $(($OPTIND-1))
