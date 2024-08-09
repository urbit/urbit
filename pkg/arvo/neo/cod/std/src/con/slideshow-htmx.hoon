/@  slideshow
/-  feather-icons
:-  [%slideshow %$ %htmx]
|=  =slideshow
|=  =bowl:neo
|^
  ;div.slideshow.fc.relative.wf.hf
    ;+  controls
    ;div.frw.js.af.scroll-y.hf
      ;+  editor
      ;+  previewer
      ;+  deck
    ==
    ;+  script
    ;+  style
  ==
++  controls
  =/  cls  "p-1 br1 b1 hover"
  ;form.p2.frw.js.ac.g3.sticky.wf
    =style  "top:0; left: 0;"
    =hx-post  "/hawk{(en-tape:pith:neo here.bowl)}?stud=slideshow-diff"
    =hx-swap  "none"
    =head  "mode"
    ;button
      =class  "{cls} {(trip ?:(=(%edit mode.slideshow) 'toggled' ''))}"
      =morph-retain  "class"
      =type  "submit"
      =onclick  "shChangeMode(this, ['.sh-editor'])"
      =mode  "edit"
      ; edit
    ==
    ;button
      =class  "{cls} {(trip ?:(=(%both mode.slideshow) 'toggled' ''))}"
      =morph-retain  "class"
      =type  "submit"
      =onclick  "shChangeMode(this, ['.sh-editor', '.sh-previewer'])"
      =mode  "both"
      ; both
    ==
    ;button
      =class  "{cls} {(trip ?:(=(%preview mode.slideshow) 'toggled' ''))}"
      =morph-retain  "class"
      =type  "submit"
      =onclick  "shChangeMode(this, ['.sh-previewer'])"
      =mode  "preview"
      ; preview
    ==
    ;div.grow;
    ;button
      =class  "{cls} {(trip ?:(=(%present mode.slideshow) 'toggled' ''))}"
      =morph-retain  "class"
      =type  "submit"
      =onclick  "shChangeMode(this, ['.deck'])"
      =mode  "present"
      ; present
    ==
  ==
++  editor
  =/  open  ?|(=(%edit mode.slideshow) =(%both mode.slideshow))
  =/  hid  ?:(open "" "hidden")
  ;div
    =class  "sh-editor sh-tab {hid} basis-half grow fc"
    =morph-retain  "class"
    ;form
      =class  "fc p1 g1 scroll-y relative grow"
      =style  "min-width: 300px; height: 100%;"
      =hx-post  "/hawk{(en-tape:pith:neo here.bowl)}?stud=slideshow"
      =hx-swap  "morph"
      =hx-target  "closest .slideshow"
      =hx-select  ".slideshow"
      =hx-trigger  "input changed delay:0.4s from:find textarea, input changed delay:0.4s from:find input"
      ;input.p2.mono.bd1.br1
        =name  "classes"
        =placeholder  "prose p3"
        =type  "text"
        =autocomplete  "off"
        =value  (trip class.slideshow)
        =oninput  "$(this).attr('value', this.value);"
        ;
      ==
      ;textarea.p-page.pre.mono.scroll-x.grow.bd1.m0.br1
        =name  "code"
        =morph-no-swap  ""
        =oninput  "this.setAttribute('value', this.value);"
        =spellcheck  "false"
        =value  (trip code.slideshow)
        =placeholder  "# new slideshow"
        ; {(trip code.slideshow)}
      ==
      ;div.absolute
        =style  "top: 11px; right: 11px;"
        ;div.loader
          ;span.loaded(style "opacity: 0;"): ---
          ;span.loading
            ;+  loading.feather-icons
          ==
        ==
      ==
    ==
  ==
++  error
  |=  =tang
  ;div.fc.g3.p3.s0
    ;div.pre.mono
      ;*
      %+  turn  (scag 25 tang)
      |=  =tank
      ;span: {(of-wall:format (~(win re tank) 0 80))}
    ==
    ;div.pre.numbered.mono
      ;span: ;>
      ;span;
      ;*
      %+  turn  (to-wain:format code.slideshow)
      |=  t=@t
      ;span: {(trip t)}
    ==
  ==
++  previewer
  =/  open  ?|(=(%preview mode.slideshow) =(%both mode.slideshow))
  =/  hid  ?:(open "" "hidden")
  ;main
    =class  "sh-previewer sh-tab grow hf basis-half scroll-x scroll-y br1 {hid}"
    =style  "min-width: 300px;"
    =morph-retain  "class"
    ;+
    ?~  result.slideshow
      ;div.prose.p3
        ;h1: empty slideshow
      ==
    =/  res  u.result.slideshow
    ?-  -.res
      %.n  (error +.res)
        %.y
      ;div.p-page
        ;*
        =<  p
        %^    spin
            `(list manx)`+.res
          0
        |=  [m=manx a=@ud]
        :_  +(a)
        =/  bdr  ?:(=(slide.slideshow a) "bd2" "bd1")
        ;div.mw-page.ma.mt3.p1.bd1
          ;p.f3: {<+(a)>}
          ;div
            =style  "container-type: inline-size;"
            ;+  m(a.g [[%class (trip class.slideshow)] a.g.m])
          ==
        ==
      ==
    ==
  ==
++  deck
  =/  hid  ?:(=(%present mode.slideshow) "" "hidden")
  ;form
    =class  "deck b0 sh-tab grow basis-half scroll-x scroll-y wf hf {hid}"
    =hx-post  "/hawk{(en-tape:pith:neo here.bowl)}?stud=slideshow-diff"
    =hx-trigger  "sh-change-slide"
    =hx-swap  "none"
    =head  "slide"
    =current  (scow %ud slide.slideshow)
    =tabindex  "0"
    =onkeydown  "shHandleKey(event, $(this))"
    =style  "min-width: 300px; height: 100%;"
    =morph-retain  "class"
    ;+
    ?~  result.slideshow
      ;div.prose.p3
        ;h1: empty slideshow
      ==
    =/  res  u.result.slideshow
    ?-  -.res
      %.n  (error +.res)
        %.y
      =/  slides  `(list manx)`+.res
      =/  prev-t  (scow %ud ?~(slide.slideshow 0 (dec slide.slideshow)))
      =/  next-t  (scow %ud (min (dec (lent slides)) +(slide.slideshow)))
      ;div.relative.wf.hf
        ;div.present-controls.fr.g1.ac.js.absolute.p2.o7.z1
          =style  "top: 0; right: 0;"
          ;button.p2.b1.br1.bd1.hover
            =type  "button"
            =onclick  "shPrevious($(this).closest('.deck'))"
            ;+  chevron-left.feather-icons
          ==
          ;span.sh-counter.f3.p2.b1.br1: {<+(slide.slideshow)>}/{<(lent slides)>}
          ;button.p2.b1.br1.bd1.hover
            =type  "button"
            =onclick  "shNext($(this).closest('.deck'))"
            ;+  chevron-right.feather-icons
          ==
          ;button.sh-fs-tog.p2.b1.br1.bd1.hover
            =type  "button"
            =onclick  "shToggleFullscreen($(this).closest('.deck'));"
            ; ⛶
          ==
        ==
        ;*
        =<  p
        %^    spin
            slides
          0
        |=  [m=manx a=@ud]
        :_  +(a)
        =/  hid  ?:(=(slide.slideshow a) "" "hidden")
        =/  extra  "wf hf"
        =/  cls  "{(trip class.slideshow)} {extra}"
        ;div
          =class  "wf hf {hid}"
          =sh-slide  (scow %ud a)
          ;div.wf.hf
            =style  "container-type: inline-size;"
            ;+  m(a.g [[%class cls] a.g.m])
          ==
        ==
      ==
    ==
  ==
++  script
  ;script
    ;+  ;/  %-  trip
    '''
    function shPrevious(deck) {
      let current = deck.attr('current');
      let i = parseInt(current);
      let slides = deck.find('[sh-slide]');
      let n = Math.max(0, i-1)
      slides.addClass('hidden');
      slides.filter(`[sh-slide='${n}']`).removeClass('hidden');
      deck.attr('current', n);
      deck.find('.sh-counter').text((n + 1)+`/${slides.length}`);
      triggerSlideSave(deck[0]);
    }
    function shNext(deck) {
      let current = deck.attr('current');
      let i = parseInt(current);
      let slides = deck.find('[sh-slide]');
      let n = Math.min(slides.length-1, i+1)
      slides.addClass('hidden');
      slides.filter(`[sh-slide='${n}']`).removeClass('hidden');
      deck.attr('current', n);
      deck.find('.sh-counter').text((n + 1)+`/${slides.length}`);
      triggerSlideSave(deck[0]);
    }
    function triggerSlideSave(that) {
      const evt = new Event("sh-change-slide");
      that.dispatchEvent(evt);
    }
    function shToggleFullscreen(deck) {
      if (!!deck.attr('full')) {
        deck.removeAttr('full');
        document.exitFullscreen();
      } else {
        deck.attr('full', 'true');
        deck[0]?.requestFullscreen();
      }
    }
    function shHandleKey(ev, deck) {
      let key = ev.key;
      if (['ArrowLeft'].includes(key)) {
        ev.preventDefault();
        ev.stopPropagation();
        shPrevious(deck);
      } else if (['ArrowRight', ' '].includes(key)) {
        ev.preventDefault();
        ev.stopPropagation();
        shNext(deck);
      }
    }
    function shChangeMode(that, tabs) {
      $(that).siblings().removeClass('toggled');
      $(that).addClass('toggled');
      $(that).closest('.slideshow').find('.sh-tab').addClass('hidden');
      tabs.forEach(t => {
        $(that).closest('.slideshow').find(t).removeClass('hidden');
        $(that).closest('.slideshow').find(t)[0]?.focus();
      })
    }
    '''
  ==
++  style
  ;style
  ;+  ;/  %-  trip
  '''
    .slide {
      overflow-y: auto;
      padding: 4cqw 6cqw;
      font-size: 2.4cqw;
    }
  '''
  ==
--
