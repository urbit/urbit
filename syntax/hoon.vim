"hoon.vim: Hoon syntax file
"Credit goes to Fode
"


if exists("b:current_syntax")
  finish
endif

syn case match


" comments
" Declerations
hi def link     hoonDeclaration   Define 
hi def link     hoonSymbol        Constant 
hi def link     hoonAtom          Keyword
hi def link     hoonRune          Keyword
hi def link     hoonFunction      Function
hi def link     hoonBranch        Conditional
hi def link     hoonType          Type
hi def link     hoonName          Constant

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

syn keyword     hoonFunction      stub axis beer bloq bozo calf char chop claw coat coil coin cord date dime dram edge foot gear gene gens gent genu goon hair hapt like limb line list odor tarp time tree nail pass path pint port prop reef ring rule shoe span spot tank tape term tile tone tool toon tope tune type udal udon ulna umph unit upas urge vase vise wall wing wine wonk map qeu set add cap dec div gte gth lte lth mas max min mod mul peg sub bind clap drop fall mate need some flop homo lent levy lien reel roll skid skim skip scag slag snag sort swag turn weld from long lone mill none over pull push spin bex can cat cut end lsh met rap rep rip rsh con dis mix aor dor gor hor vor fnv mug po si fe rlyd rlyh rlyq rlys ryld rylh rylq ryls year yore yell yule yall yawn yelp yo hard soft apt in ept by to mo sa cue jam mat rub last lust cold cook easy fail full funk here jest just knee mask next sear shim stag stew stir stun bend comp glue pfix plug pose sfix bass boss ifix more most plus slug star ace bar bas buc cab cen col com doq dot fas gal gar hax kel ker ket lus hep pel pam per pat sel sem ser sig soq tar tec tis wut zap dog doh dun duq duz gap gay vul alf aln alp bet bin but dem dit gul gon hex hig hit low mes nix nud poy qit qut sym ven vit rash rush scan cass crip mesc runt sand sane trim trip teff turf tuba tufa tuft wack wick woad wood re ab ag co ne mu so scot scow slaw slay smyt un mack mink mock mook mang mong mung berk diff loss locz lore role lump limp hump husk lurk lusk shad shaf shak sham shas shax shaw og show at cell core cube face bean flay foil fork cove comb cond cons fitz flan flip flor hike hoax hoof jock look make rain ream reck seed sell pave loot slam slim slit slap slop skol spat spud slot slum stab wash al ap ut vang vast vest vice curd duct helm hilt move ovum pane pone ship vane vile wire writ adit vent vial vint is come keep load peek poke veer volt wish

syn match       hoonDeclaration   "++" nextgroup=hoonSymbolDec skipwhite 
syn match       hoonBranch        /?[^\w\s]/ 
syn match       hoonSymbol        /%\w*/
syn keyword     hoonAtom          @
syn match       hoonName          "\w*" contained
syn match       hoonSymbolDec     "\w*" contained contains=hoonName

" strings

hi def link     hoonComment       Comment

syn region      hoonComment       start="::" end="$" contains=@spell


hi def link     hoonString        String
syn region      hoonString        start=+'+ end=+'+ contains=@spell

let b:current_syntax = "hoon"

