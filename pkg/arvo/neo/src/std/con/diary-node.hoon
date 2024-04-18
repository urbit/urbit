/@  diary
:-  [%diary %node]
|=  dia=diary
^-  manx
;div.flex.flex-col.gap-3.bg-green-200.border.border-green-300.rounded.p-3
  ;h1.text-lg.font-bold: {(trip name.dia)}
  ;p: this node is a %diary stud
  ;p: its children are %txt studs
  ;div.flex.gap-2
    ;button.p-2.bg-slate-300.rounded.border
      ; new entry
    ==
    ;button.p-2.bg-slate-300.rounded.border
      ; delete entry by path
    ==
  ==
==
