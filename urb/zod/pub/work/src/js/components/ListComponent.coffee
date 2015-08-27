recl = React.createClass
rece = React.createElement
{div,h1,input,textarea} = React.DOM

WorkStore     = require '../stores/WorkStore.coffee'
WorkActions   = require '../actions/WorkActions.coffee'
ItemComponent = require './ItemComponent.coffee'

ListeningComponent = require './ListeningComponent.coffee'
FilterComponent = require './FilterComponent.coffee'
SortComponent = require './SortComponent.coffee'

module.exports = recl
  displayName: 'List'
  stateFromStore: -> window.canSort = WorkStore.canSort(); {
    list:WorkStore.getList()
    canSort:WorkStore.canSort()
    listening:WorkStore.getListening()
    sorts:WorkStore.getSorts()
    filters:WorkStore.getFilters()
    expand:false
    updated:WorkStore.getUpdated()
  }

  getInitialState: -> @stateFromStore()
  _onChangeStore: -> @setState @stateFromStore()

  alias: ->
    @$el = $ @getDOMNode()
    @$items = @$el.find('.items').children()

  _focus: (e,i) -> @setState {selected:i.props.index}

  _dragStart: (e,i) -> @dragged = i.dragged

  _dragEnd: (e,i) -> 
    from = Number @dragged.closest('.item-wrap').attr('data-index')
    to = Number @over.closest('.item-wrap').attr('data-index')
    if from<to then to--
    if @drop is 'after' then to++
    WorkActions.moveItem (id for {id} in @state.list), to, from
    @dragged.removeClass 'hidden'
    @placeholder.remove()

  _dragOver: (e,i) ->
    e.preventDefault()
    $t = $(e.target).closest('.item')
    if $t.hasClass 'placeholder' then return
    if $t.length is 0 then return
    @over = $t
    if not @dragged.hasClass('hidden') then @dragged.addClass 'hidden'
    if (e.clientY - $t[0].offsetTop) < ($t[0].offsetHeight / 2)
      @drop = 'before'
      @placeholder.insertBefore $t
    else
      @drop = 'after'
      @placeholder.insertAfter $t

  title_keyDown: (e,i) ->
    kc = e.keyCode

    switch kc
      # enter - add new
      when 13
        {index,item} = i.props
        if window.getSelection().getRangeAt(0).endOffset is 0
          ins = @state.selected
        else
          index++
          ins = @state.selected+1 # XX consolidate
          @setState {selected:ins,select:true}
        {tags,audience} = item
        WorkActions.newItem index, {tags,audience}
      # backspace - remove if at 0
      when 8
        if  (window.getSelection().getRangeAt(0).endOffset is 0) and
            (e.target.innerText.length is 0)
          if i.props.item.ghost
            WorkActions.moveGhost null
          else
            if @state.selected isnt 0
              @setState {selected:@state.selected-1,select:"end"}
            WorkActions.removeItem i.props.item
          e.preventDefault()
      # up
      when 38
        last = @state.selected-1
        if last<0 then last = @state.list.length-1
        @$items.eq(last).find('.title .input').focus()
        @setState {select:"end"}
      # down
      when 40
        next = @state.selected+1
        if next is @state.list.length then next = 0
        @$items.eq(next).find('.title .input').focus()
        @setState {select:"end"}

    # cancel these
    if (kc is 13) or (kc is 38) or (kc is 40) then e.preventDefault()

  _changeListening: ->

  _changeFilter: (key,val) -> WorkActions.setFilter key,val

  _changeSort: (key,val) -> WorkActions.setSort key,val

  componentDidMount: -> 
    @placeholder = $ "<div class='item placeholder'><div class='sort'>x</div></div>"
    WorkStore.addChangeListener @_onChangeStore
    WorkActions.listenList @props.list
    @alias()

  componentDidUpdate: (_props,_state)-> 
    @alias()
    if @state.select
      $title = @$items.eq(@state.selected).find('.title .input')
      if @state.selected?
        $title.focus()
      if @state.select is "end"
        r = window.getSelection().getRangeAt(0)
        r.setStart $title[0],0
        r.setEnd $title[0],0
        s = window.getSelection()
        s.removeAllRanges()
        s.addRange r
      @setState {select:false}

  render: ->
    (div {},
      (div {className:'ctrl'},
        rece(ListeningComponent, {
          listening:@state.listening
          onChange:@_changeListening
        })
        (div {className:'transforms'},
          rece(FilterComponent, {
            filters:@state.filters
            onChange:@_changeFilter
          })
          rece(SortComponent, {
            sorts:@state.sorts
            onChange:@_changeSort
          })
        )
      )
      (div {
        className:'items'
        onDragOver:@_dragOver
        }, _.map @state.list,(item,index) => 
            className = "item-wrap"
            key = item.id
            if item.ghost then className += " ghost"
            (div {className,key,'data-index':index},
              rece(ItemComponent,{
                item
                index
                @_focus
                @title_keyDown
                draggable:@state.canSort
                @_dragStart
                @_dragEnd})
             )
      )
    )
