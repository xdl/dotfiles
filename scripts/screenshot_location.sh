# Change screenshot location for macos
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

function getScreenshotPath () {
    defaults read com.apple.screencapture location
}

#-----------------------------------------------------------------------
#  Handle command line arguments
#-----------------------------------------------------------------------

#http://unix.stackexchange.com/a/320115
if [[ $1 == "" ]]; then
    #defaults read com.apple.screencapture location | open --reveal
    open $(defaults read com.apple.screencapture location)
    exit 0;
fi

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
