package strsolver

class Flags(
			_tmpFileName : String="tmp.txt", 
			_strategy : String="-F", 
			_nuxmvTime : String="30", 
			_windowSize : String="30", 
			_useParikh : Boolean=false
			){
  var nuxmvTime = _nuxmvTime
  var windowSize = _windowSize
  var nuxmvTimeout = false
  var measuretime = false
  var useParikh = _useParikh
  var strategy = _strategy
  var tmpFileName = _tmpFileName
}