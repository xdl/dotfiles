__ScriptVersion="0.1"


function usage ()
{
    echo "Usage :  $0 [options] [--]

    Options:
    -h|help       Display this message
    -s|screenshot <Screenshot Location> Sets screenshot path
    -v|version    Display script version"

}    # ----------  end of function usage  ----------

# directory needs to exist, otherwise screenshots will keep getting dumped to default location:
function setScreenshotPath () {
    path=$1
    defaults write com.apple.screencapture location $path
    killall SystemUIServer
    echo "com.apple.screencapture location set to $path"
}

#-----------------------------------------------------------------------
#  Handle command line arguments
#-----------------------------------------------------------------------

while getopts ":hvs:" opt
do
  case $opt in

    s|screenshot   )  setScreenshotPath $OPTARG; exit 0 ;;

    h|help     )  usage; exit 0   ;;

    v|version  )  echo "$0 -- Version $__ScriptVersion"; exit 0   ;;

    * )  echo -e "\n  Option does not exist : $OPTARG\n"
          usage; exit 1   ;;

  esac    # --- end of case ---
done
shift $(($OPTIND-1))
