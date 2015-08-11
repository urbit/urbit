recl       = React.createClass
rele       = React.createElement
{div,span} = React.DOM
load       = React.createFactory require './LoadComponent.coffee'

walk = (root,_nil,_str,_comp)->
  # manx: {fork: ["string", {gn:"string" ga:{dict:"string"} c:{list:"manx"}}]}
  _walk = (elem,key)-> switch
    when !elem? then _nil()
    when typeof elem == "string" then _str elem
    when elem.gn?
      {gn,ga,c} = elem
      c = (c?.map _walk) ? []
      _comp.call elem, {gn,ga,c}, key
    else throw "Bad react-json #{JSON.stringify elem}"
  _walk root

Virtual = recl
  displayName: "Virtual"
  render: ->
    {components} = window.tree
    walk @props.manx,
      ()-> (load {},"")
      (str)-> str
      ({gn,ga,c},key)-> rele (components[gn] ? gn),
                             (_.extend {key}, ga),
                             c

reactify = (manx,key)-> rele Virtual, {manx,key}
module.exports = _.extend reactify, {walk,Virtual}
