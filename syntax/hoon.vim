"hoon.vim: Hoon syntax file
"Credit goes to Fode
"


if exists("b:current_syntax")
  finish
endif

syn case match


" Declerations
hi def link     hoonDeclaration   Define 
hi def link     hoonSymbol        Constant 
hi def link     hoonAtom          Keyword
hi def link     hoonRune          Operator
hi def link     hoonIdentifier    Identifier
hi def link     hoonBranch        Conditional
hi def link     hoonType          Type
hi def link     hoonName          Constant
hi def link     hoonNumber        Number
hi def link     hoonComment       Comment
hi def link     hoonTodo          Todo
hi def link     hoonString        String

syn match       hoonDeclaration   "++" nextgroup=hoonSymbolDec skipwhite 
syn match       hoonSymbol        /%\w*/
"syn match       hoonBranch        /?[^\w\s]/ 
syn keyword     hoonAtom          @
syn match       hoonName          "\w*" contained
syn match       hoonSymbolDec     "\w*" contained contains=hoonName

" numbers
" As I understand it, numbers may be in decimal, hex, or binary, and they may
" contain dots (functioning merely as separators, as in the American comma).
" XXX It appears that a number can span lines if (and only if?) the lines end
" in a dot.  This mostly causes issues with hex numbers across mulitple lines
" (as in hoon.hoon line 3067).
syn match       hoonNumber        "[0123456789]\+[0123456789\.]*"
syn match       hoonNumber        "0x[0123456789abcdef]\+[0123456789abcdef\.]*"
syn match       hoonNumber        "0b[01]\+[01\.]*"

" comments

syn region      hoonComment       start="::" end="$" contains=@spell,hoonTodo
syn keyword     hoonTodo          contained XX XXX TODO FIXME

" strings

syn region      hoonString        start=+'+ skip=+\\[\\']+ end=+'+ contains=@spell

" match digraphs
" XXX digraphs starting with '=' in e.g. paramater naming when this is really
" the monograph '=' followed by a digraph.  Example:  hoon.hoon line 218
" XXX we should match some of the monographs, I'm just not totally sure which
" ones.  Certainly, $ and ~ seem important, but I'm not sure of others.

syn match       hoonRune          "||"
syn match       hoonRune          "|_"
syn match       hoonRune          "|%"
syn match       hoonRune          "|:"
syn match       hoonRune          "|\."
syn match       hoonRune          "|-"
syn match       hoonRune          "|\^"
syn match       hoonRune          "|+"
syn match       hoonRune          "|\*"
syn match       hoonRune          "|="
syn match       hoonRune          "|?"
syn match       hoonRune          "%_"
syn match       hoonRune          "%:"
syn match       hoonRune          "%\."
syn match       hoonRune          "%\^"
syn match       hoonRune          "%+"
syn match       hoonRune          "%-"
syn match       hoonRune          "%\~"
syn match       hoonRune          "%\*"
syn match       hoonRune          "%="
syn match       hoonRune          "\$|"
syn match       hoonRune          "\$_"
syn match       hoonRune          "\$:"
syn match       hoonRune          "\$%"
syn match       hoonRune          "\$,"
syn match       hoonRune          "\$&"
syn match       hoonRune          "\$?"
syn match       hoonRune          ":_"
syn match       hoonRune          ":\~"
syn match       hoonRune          ":/"
syn match       hoonRune          ":\^"
syn match       hoonRune          ":+"
syn match       hoonRune          ":-"
syn match       hoonRune          ":\~"
syn match       hoonRune          ":\*"
syn match       hoonRune          "\.+"
syn match       hoonRune          "\.\*"
syn match       hoonRune          "\.="
syn match       hoonRune          "\.?"
syn match       hoonRune          "\.\^"
syn match       hoonRune          "#<"
syn match       hoonRune          "#>"
syn match       hoonRune          "\^|"
syn match       hoonRune          "\^\."
syn match       hoonRune          "\^-"
syn match       hoonRune          "\^+"
syn match       hoonRune          "\^&"
syn match       hoonRune          "\^\~"
syn match       hoonRune          "\^="
syn match       hoonRune          "\^?"
syn match       hoonRune          "\~|"
syn match       hoonRune          "\~\$"
syn match       hoonRune          "\~%"
syn match       hoonRune          "\~:"
syn match       hoonRune          "\~/"
syn match       hoonRune          "\~<"
syn match       hoonRune          "\~>"
syn match       hoonRune          "\~#"
syn match       hoonRune          "\~\^"
syn match       hoonRune          "\~+"
syn match       hoonRune          "\~&"
syn match       hoonRune          "\~="
syn match       hoonRune          "\~!"
syn match       hoonRune          ";_"
syn match       hoonRune          ";,"
syn match       hoonRune          ";%"
syn match       hoonRune          ";:"
syn match       hoonRune          ";\."
syn match       hoonRune          ";<"
syn match       hoonRune          ";>"
syn match       hoonRune          ";-"
syn match       hoonRune          ";+"
syn match       hoonRune          ";&"
syn match       hoonRune          ";\~"
syn match       hoonRune          ";;"
syn match       hoonRune          ";\*"
syn match       hoonRune          ";="
syn match       hoonRune          ";?"
syn match       hoonRune          "=|"
syn match       hoonRune          "=\."
syn match       hoonRune          "=\^"
syn match       hoonRune          "=:"
syn match       hoonRune          "=<"
syn match       hoonRune          "=>"
syn match       hoonRune          "=-"
syn match       hoonRune          "=+"
syn match       hoonRune          "=\~"
syn match       hoonRune          "?|"
syn match       hoonRune          "?:"
syn match       hoonRune          "?\."
syn match       hoonRune          "?<"
syn match       hoonRune          "?>"
syn match       hoonRune          "?-"
syn match       hoonRune          "?\^"
syn match       hoonRune          "?="
syn match       hoonRune          "?+"
syn match       hoonRune          "?&"
syn match       hoonRune          "?@"
syn match       hoonRune          "?\~"
syn match       hoonRune          "?!"
syn match       hoonRune          "!:"
syn match       hoonRune          "!,"
syn match       hoonRune          "!;"
syn match       hoonRune          "!\^"
syn match       hoonRune          "!>"
syn match       hoonRune          "!="

" match identifiers
" These are just pulled from hoon.hoon using:
" cat hoon.hoon | sed -n -e 's/^++  \<\([^ ]*\)\>.*/\1/p'

syn keyword     hoonIdentifier      stub axis beer bloq bozo calf char chop claw coat coil coin cord date dime dram edge foot gear gene gens gent genu goon hair hapt like limb line list odor tarp time tree nail pass path pint port prop reef ring rule shoe span spot tank tape term tile tone tool toon tope tune type udal udon ulna umph unit upas urge vase vise wall wing wine wonk map qeu set add cap dec div gte gth lte lth mas max min mod mul peg sub bind clap drop fall mate need some flop homo lent levy lien reel roll skid skim skip scag slag snag sort swag turn weld from long lone mill none over pull push spin bex can cat cut end lsh met rap rep rip rsh con dis mix aor dor gor hor vor fnv mug po si fe rlyd rlyh rlyq rlys ryld rylh rylq ryls year yore yell yule yall yawn yelp yo hard soft apt in ept by to mo sa cue jam mat rub last lust cold cook easy fail full funk here jest just knee mask next sear shim stag stew stir stun bend comp glue pfix plug pose sfix bass boss ifix more most plus slug star ace bar bas buc cab cen col com doq dot fas gal gar hax kel ker ket lus hep pel pam per pat sel sem ser sig soq tar tec tis wut zap dog doh dun duq duz gap gay vul alf aln alp bet bin but dem dit gul gon hex hig hit low mes nix nud poy qit qut sym ven vit rash rush scan cass crip mesc runt sand sane trim trip teff turf tuba tufa tuft wack wick woad wood re ab ag co ne mu so scot scow slaw slay smyt un mack mink mock mook mang mong mung berk diff loss locz lore role lump limp hump husk lurk lusk shad shaf shak sham shas shax shaw og show at cell core cube face bean flay foil fork cove comb cond cons fitz flan flip flor hike hoax hoof jock look make rain ream reck seed sell pave loot slam slim slit slap slop skol spat spud slot slum stab wash al ap ut vang vast vest vice curd duct helm hilt move ovum pane pone ship vane vile wire writ adit vent vial vint is come keep load peek poke veer volt wish

let b:current_syntax = "hoon"

