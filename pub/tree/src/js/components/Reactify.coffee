recl = React.createClass
span = React.DOM.span
div  = React.DOM.span
load = require './LoadComponent.coffee'

codemirror        = React.createFactory require './CodeMirror.coffee'
list              = React.createFactory require './ListComponent.coffee'
kids              = React.createFactory require './KidsComponent.coffee'
toc               = React.createFactory require './TocComponent.coffee'
lost              = React.createFactory recl render: -> (div {}, "lost")
components = 
  kids:kids
  list:list
  lost:lost
  toc:toc
  codemirror:codemirror

module.exports = recl
  displayName: "Virtual"
  render: -> @walk @props.manx
  walk: (obj,key) -> switch
    # manx: {fork: ["string", {gn:"string" ga:{dict:"string"} c:{list:"manx"}}]}
    when !obj? then (span {className:"loading"}, (load {}, ""))
    when typeof obj == "string" then obj
    when obj.gn?
      React.createElement components[obj.gn] ? obj.gn,
                          $.extend {key}, obj.ga
                          obj.c.map @walk
    else throw "Bad react-json #{JSON.stringify obj}"
