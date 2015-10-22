# See /hook/core/sole/lib
str = JSON.stringify
clog = (a)-> console.log a
class window.Share
  constructor: (@buf = "", @ven = [0, 0], @leg = []) ->
  #
  abet: -> buf:@buf, leg:@leg.slice(), ven:@ven.slice()
  apply: (ted)->
    switch
      when 'nop' == ted then return
      when ted.map then ted.map @apply, @
      else switch Object.keys(ted)[0]
        when 'set' then @buf = ted.set
        when 'del' then @buf = @buf.slice(0,ted.del) + @buf.slice(ted.del + 1)
        when 'ins'
          {at,cha} = ted.ins
          @buf = @buf.slice(0,at) + cha + @buf.slice(at)
        else throw "%sole-edit -lost.#{str ted}"
  #
  transmute: (sin,dex)->
    switch
      when sin == 'nop' or dex == 'nop' then dex
      when sin.reduce
        sin.reduce ((dex,syn) => @transmute(syn,dex)), dex
      when dex.map then dex.map (dax) => @transmute(sin,dax)
      when dex.set != undefined then dex
      else switch Object.keys(sin)[0]
        when 'set' then 'nop'
        when 'del'
          if sin.del is dex.del then return 'nop'
          dex = $.extend true, {}, dex  # clone
          switch Object.keys(dex)[0]
            when 'del' then if sin.del < dex.del    then dex.del--
            when 'ins' then if sin.del < dex.ins.at then dex.ins.at--
          return dex
        when 'ins'
          dex = $.extend true, {}, dex  # clone
          {at,cha} = sin.ins
          switch Object.keys(dex)[0]
            when 'del' then if at < dex.del then dex.del++
            when 'ins' then if at < dex.ins.at or
                              (at == dex.ins.at and !(cha <= dex.ins.cha))
                dex.ins.at++
          return dex
        else throw "%sole-edit -lost.#{str sin}"
  #
  commit: (ted)->
    @ven[0]++
    @leg.push ted
    @apply ted
  #
  inverse: (ted)->
    switch
      when 'nop' == ted then ted
      when ted.map
        ted.map( (tad)=> res=@inverse tad; @apply tad; res).reverse()
      else switch Object.keys(ted)[0]
        when 'set' then set: @buf
        when 'ins' then del: ted.ins
        when 'del' then ins: at: ted.del, cha: @buf[ted.del]
        else throw "%sole-edit -lost.#{str ted}"
  #
  receive: ({ler,ted})->
    if !(ler[1] is @ven[1]) 
      throw "-out-of-sync.[#{str ler} #{str @ven}]"
    @leg = @leg.slice @leg.length + ler[0] - @ven[0] 
    dat = @transmute @leg, ted
    @ven[1]++; @apply dat; dat
  #
  remit: -> throw 'stub'
  transmit: (ted)->
    act = {ted, ler:[@ven[1], @ven[0]]}
    @commit ted
    return act
  #
  transceive: ({ler,ted})->
    old = new Share @buf
    dat = @receive {ler, ted}
    old.inverse dat
  #
  transpose: (ted,pos)->
    if pos == undefined then @transpose @leg, ted
    else ((@transmute ted, ins: at: pos).ins ? at:0).at
