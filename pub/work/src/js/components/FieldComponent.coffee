recl = React.createClass
rece = React.createElement
{div,textarea} = React.DOM

WorkActions   = require '../actions/WorkActions.coffee'

module.exports = recl
  displayName: 'Field'
  getInitialState: -> invalid:no
  shouldComponentUpdate: (props)->
    while @oldValue?.length
      if @oldValue[0] is props.defaultValue
        return false
      else @oldValue.shift()
    true
  
  render: ->
    className = "#{@props.className ? @props._key} field ib"
    if @state.invalid then className += " invalid"

    props = _.extend {}, @props, {
      @onKeyUp
      ref:'input'
      defaultValue: @props.render @props.defaultValue
      className: 'input ib'
    }

    div {className},
      if @props.textarea then (textarea props)
      else
        props.contentEditable = true
        props.dangerouslySetInnerHTML = 
          __html: $('<div>').text(props.defaultValue).html()
        (div props)

  onKeyUp: (e) ->
    $t = $(e.target).closest '.field'
    _val = @getVal()
    if @props.item.ghost and _val is ""
      return
    val = @parse _val
    unless @validate val
      @setState invalid:yes
      return
    @setState invalid:no
    
    unless @equal @props.defaultValue, val
      @oldValue ?= []
      @oldValue.push val
      if @to then clearTimeout @to
      @to = setTimeout =>
          {item,_key,index} = @props
          if item.version >= 0
            WorkActions.setItem item, _key, val
          else WorkActions.newItem {},
            id:        item.id
            tags:      item.tags
            audience:  item.audience
            "#{_key}": val
        ,1000

  getVal: ->
    if @props.textarea
      $(@refs.input.getDOMNode()).val()
    else $(@refs.input.getDOMNode()).text()
      
  parse: (text)-> switch @props._key
    when 'tags'      then text.trim().split(" ")
    when 'audience'  then text.trim().split(" ").map (a) -> "~#{a}".toLowerCase()
    when 'date_due'
      d = text.slice(1).replace(/\./g, "-")
      return NaN if d.length < 8
      new Date(d).valueOf()
    else text

  equal: (vol=(@parse ""),val) -> switch @props._key
    when 'tags', 'audience'
      (_.xor(vol,val).length is 0)
    when 'date_due'
      vol.valueOf() is val
    else vol is val

  validate: (val) -> switch @props._key
    when 'date_due'
      !isNaN(val)
    when 'audience'
      for a in val
        [ship,station,rest...] = a.split("/")
        return no unless (rest.length is 0) and ship and station
        return no if ship[0] isnt "~"             
        return no if ship < 3
        return no if station < 3
      yes
    else yes

