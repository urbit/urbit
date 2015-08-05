recl       = React.createClass
{div,span} = React.DOM
load = React.createFactory require './LoadComponent.coffee'

codemirror        = require './CodeMirror.coffee'
list              = require './ListComponent.coffee'
kids              = require './KidsComponent.coffee'
toc               = require './TocComponent.coffee'
lost              = recl render: -> (div {}, "lost")
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
