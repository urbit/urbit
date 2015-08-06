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
  walk: (elem,key) -> switch
    # manx: {fork: ["string", {gn:"string" ga:{dict:"string"} c:{list:"manx"}}]}
    when !elem? then (load {}, "")
    when typeof elem == "string" then elem
    when elem.gn?
      React.createElement components[elem.gn] ? elem.gn,
                          $.extend {key}, elem.ga
                          elem.c?.map @walk
    else throw "Bad react-json #{JSON.stringify elem}"
