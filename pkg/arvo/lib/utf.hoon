  ::  /lib/utf
::::
::  String library for UTF-8 and UTF-32 specific functions.
::
::  This file is not subject to kelvin versioning and the interface should
::  not be considered official.
::
|%
::  ++utf8
::
::  Container for UTF-8 specific functions.
::
::  Source
++  utf8
  |%
  ::    $low
  ::
  ::  A UTF-8 multibyte character (not necessarily a single byte).
  ::
  ::  Source
  +$  low   ?(@tD @tE @tF @tG)
  ::    $calf
  ::
  ::  A list of UTF-8 multibyte characters (not a tape, which is `(list @tD)`).
  ::  Since bitwidth letters aren't coercive, this union is advisory.
  ::    Examples
  ::      > `(list @ux)``(list @)`"Xanadu"
  ::      ~[0x58 0x61 0x6e 0x61 0x64 0x75]
  ::      > `(list @ux)``(list @)`"𐐞𐐰𐑌𐐲𐐼𐐭"
  ::      ~[0xf0 0x90 0x90 0x9e 0xf0 0x90 0x90 0xb0 0xf0 0x90 0x91 0x8c 0xf0 0x90 0x90 0xb2 0xf0 0x90 0x90 0xbc 0xf0 0x90 0x90 0xad]
  ::  Source
  +$  calf  (list low)
  ::    +lasso:  cord -> calf
  ::
  ::  Convert a cord into a calf; that is, unify UTF-8 multi-byte characters into
  ::  a single $low (possibly multiple bytes) throughout a cord.
  ::    Examples
  ::      > (lasso '𐐞𐐰𐑌𐐲𐐼𐐭')
  ::      ~['𐐞' '𐐰' '𐑌' '𐐲' '𐐼' '𐐭']
  ::      > `(list @ux)``(list @)`(lasso '𐐞𐐰𐑌𐐲𐐼𐐭')
  ::      ~[0x9e90.90f0 0xb090.90f0 0x8c91.90f0 0xb290.90f0 0xbc90.90f0 0xad90.90f0]
  ::  Source
  ++  lasso
    |=  a=@t
    ^-  calf
    =|  i=@
    =/  len=@  (met 3 a)
    ~|  'Invalid UTF-8 string'
    |-  ^-  calf
    ?:  =(len i)  ~
    =/  led=@t  (cut 3 [i 1] a)
    ?:  =(0x7f (con led 0x7f))                       ::  1B
      [led $(i +(i))]
    ?:  =(0xdf (con led 0x1f))                       ::  2B
      ?>  =(0xbf (con 0x3f (cut 3 [+(i) 1] a)))
      [(cut 3 [i 2] a) $(i (add 2 i))]
    ?:  =(0xef (con led 0xf))                        ::  3B
      ?>  =(0xbf (con 0x3f (cut 3 [+(i) 1] a)))
      ?>  =(0xbf (con 0x3f (cut 3 [(add 2 i) 1] a)))
      [(cut 3 [i 3] a) $(i (add 3 i))]
    ?>  =(0xf7 (con led 0x7))                        ::  4B
    ?>  =(0xbf (con 0x3f (cut 3 [+(i) 1] a)))
    ?>  =(0xbf (con 0x3f (cut 3 [(add 2 i) 1] a)))
    ?>  =(0xbf (con 0x3f (cut 3 [(add 3 i) 1] a)))
    [(cut 3 [i 4] a) $(i (add 4 i))]

  ::    +brand:  calf -> cord
  ::
  ::  Convert a calf back into a cord; that is, split concatenate UTF-8 multi-byte characters
  ::  back into bytes in a cord.
  ::    Examples
  ::      > (brand (lasso '𐐞𐐰𐑌𐐲𐐼𐐭'))
  ::      '𐐞𐐰𐑌𐐲𐐼𐐭'
  ::  Source
  ++  brand  |=(=calf `cord`(rap 3 calf))
  ::  ++of-utf32:utf8
  ::
  ::  Convert UTF-32 atom to UTF-8 atom.
  ::
  ::  Source
  ++  of-utf32  tuft
  ::
  ::  ++to-utf32:utf8
  ::
  ::  Convert UTF-8 atom to UTF-32 atom.
  ::
  ::  Source
  ++  to-utf32  taft
  ::
  ::  ++upper:utf8
  ::
  ::  Convert UTF-8 atom to upper-case (all scripts).
  ::
  ::  Source
  ++  upper  |=(=@t (tuft (upper:utf32 (taft t))))
  ::
  ::  ++lower:utf8
  ::
  ::  Convert UTF-8 atom to lower-case (all scripts).
  ::
  ::  Source
  ++  lower  |=(=@t (tuft (lower:utf32 (taft t))))
  --
::  ++utf32
::
::  Container for UTF-32 specific functions.
::
::  Source
++  utf32
  |%
  ::  ++of-utf8:utf32
  ::
  ::  Convert UTF-8 atom to UTF-32 atom.
  ::
  ::  Source
  ++  of-utf8  taft
  ::
  ::  ++to-utf8:utf32
  ::
  ::  Convert UTF-32 atom to UTF-8 atom.
  ::
  ::  Source
  ++  to-utf8  tuft
  ::
  ::  ++upper:utf32
  ::
  ::  Convert UTF-32 atom to upper-case (all scripts).
  ::
  ::  Source
  ++  upper
    |=  =@c
    ^-  @c
    %+  rap  5
    %+  turn  (rip 5 c)
    |=  a=@c
    (fall (~(get by cuss-map) a) a)
  ::
  ::  ++lower:utf32
  ::
  ::  Convert UTF-32 atom to lower-case (all scripts).
  ::
  ::  Source
  ++  lower
    |=  =@c
    ^-  @c
    %+  rap  5
    %+  turn  (rip 5 c)
    |=  a=@c
    (fall (~(get by cass-map) a) a)
  ::
  ::  ++cass-map:utf32
  ::
  ::  Character case mappings from upper to lower.
  ::
  ::  Source
  ++  cass-map
    ^~
    %-  ~(gas by *(map @c @c))
    =/  =tour  (tuba (trip case-src))
    =|  pairs=(list [@c @c])
    |-  ^-  (list [@c @c])
    ?~  tour
      pairs
    ?>  ?=(^ t.tour)
    $(tour t.t.tour, pairs [[i.tour i.t.tour] pairs])
  ::
  ::  ++cass-map:utf32
  ::
  ::  Character case mappings from lower to upper.
  ::
  ::  Source
  ++  cuss-map
    ^~
    %-  ~(gas by *(map @c @c))
    =/  =tour  (tuba (trip case-src))
    =|  pairs=(list [@c @c])
    |-  ^-  (list [@c @c])
    ?~  tour
      pairs
    ?>  ?=(^ t.tour)
    $(tour t.t.tour, pairs [[i.t.tour i.tour] pairs])
  ::
  ::  ++case-src:utf32
  ::
  ::  Raw character case mapping source.
  ::
  ::  Source
  ++  case-src
    'AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZzµμÀàÁáÂâÃãÄäÅåÆæÇçÈè\
    /ÉéÊêËëÌìÍíÎîÏïÐðÑñÒòÓóÔôÕõÖöØøÙùÚúÛûÜüÝýÞþĀāĂăĄąĆćĈĉĊċČčĎďĐđĒēĔĕĖėĘęĚěĜĝ\
    /ĞğĠġĢģĤĥĦħĨĩĪīĬĭĮįĲĳĴĵĶķĹĺĻļĽľĿŀŁłŃńŅņŇňŊŋŌōŎŏŐőŒœŔŕŖŗŘřŚśŜŝŞşŠšŢţŤťŦŧŨũ\
    /ŪūŬŭŮůŰűŲųŴŵŶŷŸÿŹźŻżŽžſsƁɓƂƃƄƅƆɔƇƈƉɖƊɗƋƌƎǝƏəƐɛƑƒƓɠƔɣƖɩƗɨƘƙƜɯƝɲƟɵƠơƢƣƤƥƦʀ\
    /ƧƨƩʃƬƭƮʈƯưƱʊƲʋƳƴƵƶƷʒƸƹƼƽǄǆǅǆǇǉǈǉǊǌǋǌǍǎǏǐǑǒǓǔǕǖǗǘǙǚǛǜǞǟǠǡǢǣǤǥǦǧǨǩǪǫǬǭǮǯǱǳ\
    /ǲǳǴǵǶƕǷƿǸǹǺǻǼǽǾǿȀȁȂȃȄȅȆȇȈȉȊȋȌȍȎȏȐȑȒȓȔȕȖȗȘșȚțȜȝȞȟȠƞȢȣȤȥȦȧȨȩȪȫȬȭȮȯȰȱȲȳȺⱥȻȼ\
    /ȽƚȾⱦɁɂɃƀɄʉɅʌɆɇɈɉɊɋɌɍɎɏͅιͰͱͲͳͶͷͿϳΆάΈέΉήΊίΌόΎύΏώΑαΒβΓγΔδΕεΖζΗηΘθΙιΚκΛλΜμΝνΞ\
    /ξΟοΠπΡρΣσΤτΥυΦφΧχΨψΩωΪϊΫϋςσϏϗϐβϑθϕφϖπϘϙϚϛϜϝϞϟϠϡϢϣϤϥϦϧϨϩϪϫϬϭϮϯϰκϱρϴθϵεϷϸϹ\
    /ϲϺϻϽͻϾͼϿͽЀѐЁёЂђЃѓЄєЅѕІіЇїЈјЉљЊњЋћЌќЍѝЎўЏџАаБбВвГгДдЕеЖжЗзИиЙйКкЛлМмНнОоП\
    /пРрСсТтУуФфХхЦцЧчШшЩщЪъЫыЬьЭэЮюЯяѠѡѢѣѤѥѦѧѨѩѪѫѬѭѮѯѰѱѲѳѴѵѶѷѸѹѺѻѼѽѾѿҀҁҊҋҌҍҎ\
    /ҏҐґҒғҔҕҖҗҘҙҚқҜҝҞҟҠҡҢңҤҥҦҧҨҩҪҫҬҭҮүҰұҲҳҴҵҶҷҸҹҺһҼҽҾҿӀӏӁӂӃӄӅӆӇӈӉӊӋӌӍӎӐӑӒӓӔӕӖ\
    /ӗӘәӚӛӜӝӞӟӠӡӢӣӤӥӦӧӨөӪӫӬӭӮӯӰӱӲӳӴӵӶӷӸӹӺӻӼӽӾӿԀԁԂԃԄԅԆԇԈԉԊԋԌԍԎԏԐԑԒԓԔԕԖԗԘԙԚԛԜԝԞ\
    /ԟԠԡԢԣԤԥԦԧԨԩԪԫԬԭԮԯԱաԲբԳգԴդԵեԶզԷէԸըԹթԺժԻիԼլԽխԾծԿկՀհՁձՂղՃճՄմՅյՆնՇշՈոՉչՊպՋջՌ\
    /ռՍսՎվՏտՐրՑցՒւՓփՔքՕօՖֆႠⴀႡⴁႢⴂႣⴃႤⴄႥⴅႦⴆႧⴇႨⴈႩⴉႪⴊႫⴋႬⴌႭⴍႮⴎႯⴏႰⴐႱⴑႲⴒႳⴓႴⴔႵⴕႶⴖႷⴗႸⴘႹ\
    /ⴙႺⴚႻⴛႼⴜႽⴝႾⴞႿⴟჀⴠჁⴡჂⴢჃⴣჄⴤჅⴥჇⴧჍⴭᏸᏰᏹᏱᏺᏲᏻᏳᏼᏴᏽᏵᲀвᲁдᲂоᲃсᲄтᲅтᲆъᲇѣᲈꙋᲐაᲑბᲒგᲓდᲔეᲕვᲖ\
    /ზᲗთᲘიᲙკᲚლᲛმᲜნᲝოᲞპᲟჟᲠრᲡსᲢტᲣუᲤფᲥქᲦღᲧყᲨშᲩჩᲪცᲫძᲬწᲭჭᲮხᲯჯᲰჰᲱჱᲲჲᲳჳᲴჴᲵჵᲶჶᲷჷᲸჸᲹჹᲺ\
    /ჺᲽჽᲾჾᲿჿḀḁḂḃḄḅḆḇḈḉḊḋḌḍḎḏḐḑḒḓḔḕḖḗḘḙḚḛḜḝḞḟḠḡḢḣḤḥḦḧḨḩḪḫḬḭḮḯḰḱḲḳḴḵḶḷḸḹḺḻḼḽḾḿṀ\
    /ṁṂṃṄṅṆṇṈṉṊṋṌṍṎṏṐṑṒṓṔṕṖṗṘṙṚṛṜṝṞṟṠṡṢṣṤṥṦṧṨṩṪṫṬṭṮṯṰṱṲṳṴṵṶṷṸṹṺṻṼṽṾṿẀẁẂẃẄẅẆẇẈ\
    /ẉẊẋẌẍẎẏẐẑẒẓẔẕẛṡẞßẠạẢảẤấẦầẨẩẪẫẬậẮắẰằẲẳẴẵẶặẸẹẺẻẼẽẾếỀềỂểỄễỆệỈỉỊịỌọỎỏỐốỒồỔổỖ\
    /ỗỘộỚớỜờỞởỠỡỢợỤụỦủỨứỪừỬửỮữỰựỲỳỴỵỶỷỸỹỺỻỼỽỾỿἈἀἉἁἊἂἋἃἌἄἍἅἎἆἏἇἘἐἙἑἚἒἛἓἜἔἝἕἨἠἩ\
    /ἡἪἢἫἣἬἤἭἥἮἦἯἧἸἰἹἱἺἲἻἳἼἴἽἵἾἶἿἷὈὀὉὁὊὂὋὃὌὄὍὅὙὑὛὓὝὕὟὗὨὠὩὡὪὢὫὣὬὤὭὥὮὦὯὧᾈᾀᾉᾁᾊᾂᾋ\
    /ᾃᾌᾄᾍᾅᾎᾆᾏᾇᾘᾐᾙᾑᾚᾒᾛᾓᾜᾔᾝᾕᾞᾖᾟᾗᾨᾠᾩᾡᾪᾢᾫᾣᾬᾤᾭᾥᾮᾦᾯᾧᾸᾰᾹᾱᾺὰΆάᾼᾳιιῈὲΈέῊὴΉήῌῃΐΐῘῐῙῑῚὶΊ\
    /ίΰΰῨῠῩῡῪὺΎύῬῥῸὸΌόῺὼΏώῼῳΩωKkÅåℲⅎⅠⅰⅡⅱⅢⅲⅣⅳⅤⅴⅥⅵⅦⅶⅧⅷⅨⅸⅩⅹⅪⅺⅫⅻⅬⅼⅭⅽⅮⅾⅯⅿↃↄⒶⓐⒷⓑⒸⓒⒹ\
    /ⓓⒺⓔⒻⓕⒼⓖⒽⓗⒾⓘⒿⓙⓀⓚⓁⓛⓂⓜⓃⓝⓄⓞⓅⓟⓆⓠⓇⓡⓈⓢⓉⓣⓊⓤⓋⓥⓌⓦⓍⓧⓎⓨⓏⓩⰀⰰⰁⰱⰂⰲⰃⰳⰄⰴⰅⰵⰆⰶⰇⰷⰈⰸⰉⰹⰊⰺⰋⰻⰌⰼⰍ\
    /ⰽⰎⰾⰏⰿⰐⱀⰑⱁⰒⱂⰓⱃⰔⱄⰕⱅⰖⱆⰗⱇⰘⱈⰙⱉⰚⱊⰛⱋⰜⱌⰝⱍⰞⱎⰟⱏⰠⱐⰡⱑⰢⱒⰣⱓⰤⱔⰥⱕⰦⱖⰧⱗⰨⱘⰩⱙⰪⱚⰫⱛⰬⱜⰭⱝⰮⱞⰯⱟⱠⱡⱢ\
    /ɫⱣᵽⱤɽⱧⱨⱩⱪⱫⱬⱭɑⱮɱⱯɐⱰɒⱲⱳⱵⱶⱾȿⱿɀⲀⲁⲂⲃⲄⲅⲆⲇⲈⲉⲊⲋⲌⲍⲎⲏⲐⲑⲒⲓⲔⲕⲖⲗⲘⲙⲚⲛⲜⲝⲞⲟⲠⲡⲢⲣⲤⲥⲦⲧⲨⲩⲪⲫⲬ\
    /ⲭⲮⲯⲰⲱⲲⲳⲴⲵⲶⲷⲸⲹⲺⲻⲼⲽⲾⲿⳀⳁⳂⳃⳄⳅⳆⳇⳈⳉⳊⳋⳌⳍⳎⳏⳐⳑⳒⳓⳔⳕⳖⳗⳘⳙⳚⳛⳜⳝⳞⳟⳠⳡⳢⳣⳫⳬⳭⳮⳲⳳꙀꙁꙂꙃꙄꙅꙆꙇꙈꙉꙊ\
    /ꙋꙌꙍꙎꙏꙐꙑꙒꙓꙔꙕꙖꙗꙘꙙꙚꙛꙜꙝꙞꙟꙠꙡꙢꙣꙤꙥꙦꙧꙨꙩꙪꙫꙬꙭꚀꚁꚂꚃꚄꚅꚆꚇꚈꚉꚊꚋꚌꚍꚎꚏꚐꚑꚒꚓꚔꚕꚖꚗꚘꚙꚚꚛꜢꜣꜤꜥꜦꜧꜨꜩꜪ\
    /ꜫꜬꜭꜮꜯꜲꜳꜴꜵꜶꜷꜸꜹꜺꜻꜼꜽꜾꜿꝀꝁꝂꝃꝄꝅꝆꝇꝈꝉꝊꝋꝌꝍꝎꝏꝐꝑꝒꝓꝔꝕꝖꝗꝘꝙꝚꝛꝜꝝꝞꝟꝠꝡꝢꝣꝤꝥꝦꝧꝨꝩꝪꝫꝬꝭꝮꝯꝹꝺꝻꝼꝽ\
    /ᵹꝾꝿꞀꞁꞂꞃꞄꞅꞆꞇꞋꞌꞍɥꞐꞑꞒꞓꞖꞗꞘꞙꞚꞛꞜꞝꞞꞟꞠꞡꞢꞣꞤꞥꞦꞧꞨꞩꞪɦꞫɜꞬɡꞭɬꞮɪꞰʞꞱʇꞲʝꞳꭓꞴꞵꞶꞷꞸꞹꞺꞻꞼꞽꞾꞿꟀꟁꟂ\
    /ꟃꟄꞔꟅʂꟆᶎꟇꟈꟉꟊꟐꟑꟖꟗꟘꟙꟵꟶꭰᎠꭱᎡꭲᎢꭳᎣꭴᎤꭵᎥꭶᎦꭷᎧꭸᎨꭹᎩꭺᎪꭻᎫꭼᎬꭽᎭꭾᎮꭿᎯꮀᎰꮁᎱꮂᎲꮃᎳꮄᎴꮅᎵꮆᎶꮇᎷꮈᎸꮉᎹꮊ\
    /ᎺꮋᎻꮌᎼꮍᎽꮎᎾꮏᎿꮐᏀꮑᏁꮒᏂꮓᏃꮔᏄꮕᏅꮖᏆꮗᏇꮘᏈꮙᏉꮚᏊꮛᏋꮜᏌꮝᏍꮞᏎꮟᏏꮠᏐꮡᏑꮢᏒꮣᏓꮤᏔꮥᏕꮦᏖꮧᏗꮨᏘꮩᏙꮪᏚꮫᏛꮬᏜꮭᏝꮮ\
    /ᏞꮯᏟꮰᏠꮱᏡꮲᏢꮳᏣꮴᏤꮵᏥꮶᏦꮷᏧꮸᏨꮹᏩꮺᏪꮻᏫꮼᏬꮽᏭꮾᏮꮿᏯﬅﬆＡａＢｂＣｃＤｄＥｅＦｆＧｇＨｈＩ\
    /ｉＪｊＫｋＬｌＭｍＮｎＯｏＰｐＱｑＲｒＳｓＴｔＵｕＶｖＷｗＸｘＹｙＺｚ𐐀𐐨\
    /𐐁𐐩𐐂𐐪𐐃𐐫𐐄𐐬𐐅𐐭𐐆𐐮𐐇𐐯𐐈𐐰𐐉𐐱𐐊𐐲𐐋𐐳𐐌𐐴𐐍𐐵𐐎𐐶𐐏𐐷𐐐𐐸𐐑𐐹𐐒𐐺𐐓𐐻𐐔𐐼𐐕𐐽𐐖𐐾𐐗𐐿𐐘𐑀𐐙𐑁𐐚𐑂𐐛𐑃𐐜𐑄𐐝𐑅𐐞𐑆𐐟𐑇𐐠𐑈𐐡𐑉𐐢𐑊𐐣𐑋𐐤𐑌\
    /𐐥𐑍𐐦𐑎𐐧𐑏𐒰𐓘𐒱𐓙𐒲𐓚𐒳𐓛𐒴𐓜𐒵𐓝𐒶𐓞𐒷𐓟𐒸𐓠𐒹𐓡𐒺𐓢𐒻𐓣𐒼𐓤𐒽𐓥𐒾𐓦𐒿𐓧𐓀𐓨𐓁𐓩𐓂𐓪𐓃𐓫𐓄𐓬𐓅𐓭𐓆𐓮𐓇𐓯𐓈𐓰𐓉𐓱𐓊𐓲𐓋𐓳𐓌𐓴𐓍𐓵𐓎𐓶𐓏𐓷𐓐𐓸\
    /𐓑𐓹𐓒𐓺𐓓𐓻𐕰𐖗𐕱𐖘𐕲𐖙𐕳𐖚𐕴𐖛𐕵𐖜𐕶𐖝𐕷𐖞𐕸𐖟𐕹𐖠𐕺𐖡𐕼𐖣𐕽𐖤𐕾𐖥𐕿𐖦𐖀𐖧𐖁𐖨𐖂𐖩𐖃𐖪𐖄𐖫𐖅𐖬𐖆𐖭𐖇𐖮𐖈𐖯𐖉𐖰𐖊𐖱𐖌𐖳𐖍𐖴𐖎𐖵𐖏𐖶𐖐𐖷𐖑𐖸𐖒𐖹\
    /𐖔𐖻𐖕𐖼𐲀𐳀𐲁𐳁𐲂𐳂𐲃𐳃𐲄𐳄𐲅𐳅𐲆𐳆𐲇𐳇𐲈𐳈𐲉𐳉𐲊𐳊𐲋𐳋𐲌𐳌𐲍𐳍𐲎𐳎𐲏𐳏𐲐𐳐𐲑𐳑𐲒𐳒𐲓𐳓𐲔𐳔𐲕𐳕𐲖𐳖𐲗𐳗𐲘𐳘𐲙𐳙𐲚𐳚𐲛𐳛𐲜𐳜𐲝𐳝𐲞𐳞𐲟𐳟𐲠𐳠𐲡𐳡\
    /𐲢𐳢𐲣𐳣𐲤𐳤𐲥𐳥𐲦𐳦𐲧𐳧𐲨𐳨𐲩𐳩𐲪𐳪𐲫𐳫𐲬𐳬𐲭𐳭𐲮𐳮𐲯𐳯𐲰𐳰𐲱𐳱𐲲𐳲𑢠𑣀𑢡𑣁𑢢𑣂𑢣𑣃𑢤𑣄𑢥𑣅𑢦𑣆𑢧𑣇𑢨𑣈𑢩𑣉𑢪𑣊𑢫𑣋𑢬𑣌𑢭𑣍𑢮𑣎𑢯𑣏𑢰𑣐𑢱𑣑𑢲𑣒\
    /𑢳𑣓𑢴𑣔𑢵𑣕𑢶𑣖𑢷𑣗𑢸𑣘𑢹𑣙𑢺𑣚𑢻𑣛𑢼𑣜𑢽𑣝𑢾𑣞𑢿𑣟𖹀𖹠𖹁𖹡𖹂𖹢𖹃𖹣𖹄𖹤𖹅𖹥𖹆𖹦𖹇𖹧𖹈𖹨𖹉𖹩𖹊𖹪𖹋𖹫𖹌𖹬𖹍𖹭𖹎𖹮𖹏𖹯𖹐𖹰𖹑𖹱𖹒𖹲𖹓𖹳𖹔𖹴𖹕𖹵𖹖𖹶\
    /𖹗𖹷𖹘𖹸𖹙𖹹𖹚𖹺𖹛𖹻𖹜𖹼𖹝𖹽𖹞𖹾𖹟𖹿𞤀𞤢𞤁𞤣𞤂𞤤𞤃𞤥𞤄𞤦𞤅𞤧𞤆𞤨𞤇𞤩𞤈𞤪𞤉𞤫𞤊𞤬𞤋𞤭𞤌𞤮𞤍𞤯𞤎𞤰𞤏𞤱𞤐𞤲𞤑𞤳𞤒𞤴𞤓𞤵𞤔𞤶𞤕𞤷𞤖𞤸𞤗𞤹𞤘𞤺𞤙𞤻𞤚𞤼\
    /𞤛𞤽𞤜𞤾𞤝𞤿𞤞𞥀𞤟𞥁𞤠𞥂𞤡𞥃'
  --
--
::  Source for character strings:
::
:: 'A'  0x41 Latin Capital Letter A
:: 'B'  0x42 Latin Capital Letter B
:: 'C'  0x43 Latin Capital Letter C
:: 'D'  0x44 Latin Capital Letter D
:: 'E'  0x45 Latin Capital Letter E
:: 'F'  0x46 Latin Capital Letter F
:: 'G'  0x47 Latin Capital Letter G
:: 'H'  0x48 Latin Capital Letter H
:: 'I'  0x49 Latin Capital Letter I
:: 'J'  0x4a Latin Capital Letter J
:: 'K'  0x4b Latin Capital Letter K
:: 'L'  0x4c Latin Capital Letter L
:: 'M'  0x4d Latin Capital Letter M
:: 'N'  0x4e Latin Capital Letter N
:: 'O'  0x4f Latin Capital Letter O
:: 'P'  0x50 Latin Capital Letter P
:: 'Q'  0x51 Latin Capital Letter Q
:: 'R'  0x52 Latin Capital Letter R
:: 'S'  0x53 Latin Capital Letter S
:: 'T'  0x54 Latin Capital Letter T
:: 'U'  0x55 Latin Capital Letter U
:: 'V'  0x56 Latin Capital Letter V
:: 'W'  0x57 Latin Capital Letter W
:: 'X'  0x58 Latin Capital Letter X
:: 'Y'  0x59 Latin Capital Letter Y
:: 'Z'  0x5a Latin Capital Letter Z
:: 'À'  0xc0 Latin Capital Letter A
:: 'Á'  0xc1 Latin Capital Letter A
:: 'Â'  0xc2 Latin Capital Letter A
:: 'Ã'  0xc3 Latin Capital Letter A
:: 'Ä'  0xc4 Latin Capital Letter A
:: 'Å'  0xc5 Latin Capital Letter A
:: 'Æ'  0xc6 Latin Capital Letter Ae
:: 'Ç'  0xc7 Latin Capital Letter C
:: 'È'  0xc8 Latin Capital Letter E
:: 'É'  0xc9 Latin Capital Letter E
:: 'Ê'  0xca Latin Capital Letter E
:: 'Ë'  0xcb Latin Capital Letter E
:: 'Ì'  0xcc Latin Capital Letter I
:: 'Í'  0xcd Latin Capital Letter I
:: 'Î'  0xce Latin Capital Letter I
:: 'Ï'  0xcf Latin Capital Letter I
:: 'Ð'  0xd0 Latin Capital Letter Eth
:: 'Ñ'  0xd1 Latin Capital Letter N
:: 'Ò'  0xd2 Latin Capital Letter O
:: 'Ó'  0xd3 Latin Capital Letter O
:: 'Ô'  0xd4 Latin Capital Letter O
:: 'Õ'  0xd5 Latin Capital Letter O
:: 'Ö'  0xd6 Latin Capital Letter O
:: 'Ø'  0xd8 Latin Capital Letter O
:: 'Ù'  0xd9 Latin Capital Letter U
:: 'Ú'  0xda Latin Capital Letter U
:: 'Û'  0xdb Latin Capital Letter U
:: 'Ü'  0xdc Latin Capital Letter U
:: 'Ý'  0xdd Latin Capital Letter Y
:: 'Þ'  0xde Latin Capital Letter Thorn
:: 'Ā'  0x100 Latin Capital Letter A
:: 'Ă'  0x102 Latin Capital Letter A
:: 'Ą'  0x104 Latin Capital Letter A
:: 'Ć'  0x106 Latin Capital Letter C
:: 'Ĉ'  0x108 Latin Capital Letter C
:: 'Ċ'  0x10a Latin Capital Letter C
:: 'Č'  0x10c Latin Capital Letter C
:: 'Ď'  0x10e Latin Capital Letter D
:: 'Đ'  0x110 Latin Capital Letter D
:: 'Ē'  0x112 Latin Capital Letter E
:: 'Ĕ'  0x114 Latin Capital Letter E
:: 'Ė'  0x116 Latin Capital Letter E
:: 'Ę'  0x118 Latin Capital Letter E
:: 'Ě'  0x11a Latin Capital Letter E
:: 'Ĝ'  0x11c Latin Capital Letter G
:: 'Ğ'  0x11e Latin Capital Letter G
:: 'Ġ'  0x120 Latin Capital Letter G
:: 'Ģ'  0x122 Latin Capital Letter G
:: 'Ĥ'  0x124 Latin Capital Letter H
:: 'Ħ'  0x126 Latin Capital Letter H
:: 'Ĩ'  0x128 Latin Capital Letter I
:: 'Ī'  0x12a Latin Capital Letter I
:: 'Ĭ'  0x12c Latin Capital Letter I
:: 'Į'  0x12e Latin Capital Letter I
:: 'İ'  0x130 Latin Capital Letter I
:: 'Ĳ'  0x132 Latin Capital Ligature Ij
:: 'Ĵ'  0x134 Latin Capital Letter J
:: 'Ķ'  0x136 Latin Capital Letter K
:: 'Ĺ'  0x139 Latin Capital Letter L
:: 'Ļ'  0x13b Latin Capital Letter L
:: 'Ľ'  0x13d Latin Capital Letter L
:: 'Ŀ'  0x13f Latin Capital Letter L
:: 'Ł'  0x141 Latin Capital Letter L
:: 'Ń'  0x143 Latin Capital Letter N
:: 'Ņ'  0x145 Latin Capital Letter N
:: 'Ň'  0x147 Latin Capital Letter N
:: 'Ŋ'  0x14a Latin Capital Letter Eng
:: 'Ō'  0x14c Latin Capital Letter O
:: 'Ŏ'  0x14e Latin Capital Letter O
:: 'Ő'  0x150 Latin Capital Letter O
:: 'Œ'  0x152 Latin Capital Ligature Oe
:: 'Ŕ'  0x154 Latin Capital Letter R
:: 'Ŗ'  0x156 Latin Capital Letter R
:: 'Ř'  0x158 Latin Capital Letter R
:: 'Ś'  0x15a Latin Capital Letter S
:: 'Ŝ'  0x15c Latin Capital Letter S
:: 'Ş'  0x15e Latin Capital Letter S
:: 'Š'  0x160 Latin Capital Letter S
:: 'Ţ'  0x162 Latin Capital Letter T
:: 'Ť'  0x164 Latin Capital Letter T
:: 'Ŧ'  0x166 Latin Capital Letter T
:: 'Ũ'  0x168 Latin Capital Letter U
:: 'Ū'  0x16a Latin Capital Letter U
:: 'Ŭ'  0x16c Latin Capital Letter U
:: 'Ů'  0x16e Latin Capital Letter U
:: 'Ű'  0x170 Latin Capital Letter U
:: 'Ų'  0x172 Latin Capital Letter U
:: 'Ŵ'  0x174 Latin Capital Letter W
:: 'Ŷ'  0x176 Latin Capital Letter Y
:: 'Ÿ'  0x178 Latin Capital Letter Y
:: 'Ź'  0x179 Latin Capital Letter Z
:: 'Ż'  0x17b Latin Capital Letter Z
:: 'Ž'  0x17d Latin Capital Letter Z
:: 'Ɓ'  0x181 Latin Capital Letter B
:: 'Ƃ'  0x182 Latin Capital Letter B
:: 'Ƅ'  0x184 Latin Capital Letter Tone
:: 'Ɔ'  0x186 Latin Capital Letter Open
:: 'Ƈ'  0x187 Latin Capital Letter C
:: 'Ɗ'  0x18a Latin Capital Letter D
:: 'Ƌ'  0x18b Latin Capital Letter D
:: 'Ǝ'  0x18e Latin Capital Letter Reversed
:: 'Ə'  0x18f Latin Capital Letter Schwa
:: 'Ɛ'  0x190 Latin Capital Letter Open
:: 'Ƒ'  0x191 Latin Capital Letter F
:: 'Ɠ'  0x193 Latin Capital Letter G
:: 'Ɣ'  0x194 Latin Capital Letter Gamma
:: 'Ɩ'  0x196 Latin Capital Letter Iota
:: 'Ɨ'  0x197 Latin Capital Letter I
:: 'Ƙ'  0x198 Latin Capital Letter K
:: 'Ɯ'  0x19c Latin Capital Letter Turned
:: 'Ɲ'  0x19d Latin Capital Letter N
:: 'Ɵ'  0x19f Latin Capital Letter O
:: 'Ơ'  0x1a0 Latin Capital Letter O
:: 'Ƣ'  0x1a2 Latin Capital Letter Oi
:: 'Ƥ'  0x1a4 Latin Capital Letter P
:: 'Ƨ'  0x1a7 Latin Capital Letter Tone
:: 'Ʃ'  0x1a9 Latin Capital Letter Esh
:: 'Ƭ'  0x1ac Latin Capital Letter T
:: 'Ʈ'  0x1ae Latin Capital Letter T
:: 'Ư'  0x1af Latin Capital Letter U
:: 'Ʊ'  0x1b1 Latin Capital Letter Upsilon
:: 'Ʋ'  0x1b2 Latin Capital Letter V
:: 'Ƴ'  0x1b3 Latin Capital Letter Y
:: 'Ƶ'  0x1b5 Latin Capital Letter Z
:: 'Ʒ'  0x1b7 Latin Capital Letter Ezh
:: 'Ƹ'  0x1b8 Latin Capital Letter Ezh
:: 'Ƽ'  0x1bc Latin Capital Letter Tone
:: 'Ǆ'  0x1c4 Latin Capital Letter Dz
:: 'Ǉ'  0x1c7 Latin Capital Letter Lj
:: 'Ǌ'  0x1ca Latin Capital Letter Nj
:: 'Ǎ'  0x1cd Latin Capital Letter A
:: 'Ǐ'  0x1cf Latin Capital Letter I
:: 'Ǒ'  0x1d1 Latin Capital Letter O
:: 'Ǔ'  0x1d3 Latin Capital Letter U
:: 'Ǖ'  0x1d5 Latin Capital Letter U
:: 'Ǘ'  0x1d7 Latin Capital Letter U
:: 'Ǚ'  0x1d9 Latin Capital Letter U
:: 'Ǜ'  0x1db Latin Capital Letter U
:: 'Ǟ'  0x1de Latin Capital Letter A
:: 'Ǡ'  0x1e0 Latin Capital Letter A
:: 'Ǣ'  0x1e2 Latin Capital Letter Ae
:: 'Ǥ'  0x1e4 Latin Capital Letter G
:: 'Ǧ'  0x1e6 Latin Capital Letter G
:: 'Ǩ'  0x1e8 Latin Capital Letter K
:: 'Ǫ'  0x1ea Latin Capital Letter O
:: 'Ǭ'  0x1ec Latin Capital Letter O
:: 'Ǯ'  0x1ee Latin Capital Letter Ezh
:: 'Ǳ'  0x1f1 Latin Capital Letter Dz
:: 'Ǵ'  0x1f4 Latin Capital Letter G
:: 'Ǻ'  0x1fa Latin Capital Letter A
:: 'Ǽ'  0x1fc Latin Capital Letter Ae
:: 'Ǿ'  0x1fe Latin Capital Letter O
:: 'Ȁ'  0x200 Latin Capital Letter A
:: 'Ȃ'  0x202 Latin Capital Letter A
:: 'Ȅ'  0x204 Latin Capital Letter E
:: 'Ȇ'  0x206 Latin Capital Letter E
:: 'Ȉ'  0x208 Latin Capital Letter I
:: 'Ȋ'  0x20a Latin Capital Letter I
:: 'Ȍ'  0x20c Latin Capital Letter O
:: 'Ȏ'  0x20e Latin Capital Letter O
:: 'Ȑ'  0x210 Latin Capital Letter R
:: 'Ȓ'  0x212 Latin Capital Letter R
:: 'Ȕ'  0x214 Latin Capital Letter U
:: 'Ȗ'  0x216 Latin Capital Letter U
:: 'Ά'  0x386 Greek Capital Letter Alpha
:: 'Έ'  0x388 Greek Capital Letter Epsilon
:: 'Ή'  0x389 Greek Capital Letter Eta
:: 'Ί'  0x38a Greek Capital Letter Iota
:: 'Ό'  0x38c Greek Capital Letter Omicron
:: 'Ύ'  0x38e Greek Capital Letter Upsilon
:: 'Ώ'  0x38f Greek Capital Letter Omega
:: 'Α'  0x391 Greek Capital Letter Alpha
:: 'Β'  0x392 Greek Capital Letter Beta
:: 'Γ'  0x393 Greek Capital Letter Gamma
:: 'Δ'  0x394 Greek Capital Letter Delta
:: 'Ε'  0x395 Greek Capital Letter Epsilon
:: 'Ζ'  0x396 Greek Capital Letter Zeta
:: 'Η'  0x397 Greek Capital Letter Eta
:: 'Θ'  0x398 Greek Capital Letter Theta
:: 'Ι'  0x399 Greek Capital Letter Iota
:: 'Κ'  0x39a Greek Capital Letter Kappa
:: 'Λ'  0x39b Greek Capital Letter Lamda
:: 'Μ'  0x39c Greek Capital Letter Mu
:: 'Ν'  0x39d Greek Capital Letter Nu
:: 'Ξ'  0x39e Greek Capital Letter Xi
:: 'Ο'  0x39f Greek Capital Letter Omicron
:: 'Π'  0x3a0 Greek Capital Letter Pi
:: 'Ρ'  0x3a1 Greek Capital Letter Rho
:: 'Σ'  0x3a3 Greek Capital Letter Sigma
:: 'Τ'  0x3a4 Greek Capital Letter Tau
:: 'Υ'  0x3a5 Greek Capital Letter Upsilon
:: 'Φ'  0x3a6 Greek Capital Letter Phi
:: 'Χ'  0x3a7 Greek Capital Letter Chi
:: 'Ψ'  0x3a8 Greek Capital Letter Psi
:: 'Ω'  0x3a9 Greek Capital Letter Omega
:: 'Ϊ'  0x3aa Greek Capital Letter Iota
:: 'Ϋ'  0x3ab Greek Capital Letter Upsilon
:: 'Ά'  0x3ac Greek Capital Letter Alpha
:: 'Έ'  0x3ad Greek Capital Letter Epsilon
:: 'Ή'  0x3ae Greek Capital Letter Eta
:: 'Ί'  0x3af Greek Capital Letter Iota
:: 'Ϋ́'  0x3b0 Greek Capital Letter Upsilon
:: 'Α'  0x3b1 Greek Capital Letter Alpha
:: 'Β'  0x3b2 Greek Capital Letter Beta
:: 'Γ'  0x3b3 Greek Capital Letter Gamma
:: 'Δ'  0x3b4 Greek Capital Letter Delta
:: 'Ε'  0x3b5 Greek Capital Letter Epsilon
:: 'Ζ'  0x3b6 Greek Capital Letter Zeta
:: 'Η'  0x3b7 Greek Capital Letter Eta
:: 'Θ'  0x3b8 Greek Capital Letter Theta
:: 'Ι'  0x3b9 Greek Capital Letter Iota
:: 'Κ'  0x3ba Greek Capital Letter Kappa
:: 'Λ'  0x3bb Greek Capital Letter Lamda
:: 'Μ'  0x3bc Greek Capital Letter Mu
:: 'Ν'  0x3bd Greek Capital Letter Nu
:: 'Ξ'  0x3be Greek Capital Letter Xi
:: 'Ο'  0x3bf Greek Capital Letter Omicron
:: 'Π'  0x3c0 Greek Capital Letter Pi
:: 'Ρ'  0x3c1 Greek Capital Letter Rho
:: 'Σ'  0x3c2 Greek Capital Letter Final
:: 'Σ'  0x3c3 Greek Capital Letter Sigma
:: 'Τ'  0x3c4 Greek Capital Letter Tau
:: 'Υ'  0x3c5 Greek Capital Letter Upsilon
:: 'Φ'  0x3c6 Greek Capital Letter Phi
:: 'Χ'  0x3c7 Greek Capital Letter Chi
:: 'Ψ'  0x3c8 Greek Capital Letter Psi
:: 'Ω'  0x3c9 Greek Capital Letter Omega
:: 'Ϊ'  0x3ca Greek Capital Letter Iota
:: 'Ϋ'  0x3cb Greek Capital Letter Upsilon
:: 'Ό'  0x3cc Greek Capital Letter Omicron
:: 'Ύ'  0x3cd Greek Capital Letter Upsilon
:: 'Ώ'  0x3ce Greek Capital Letter Omega
:: 'Ϗ'  0x3cf Capital Kai Symbol 
:: 'Β'  0x3d0 Beta Symbol  
:: 'Θ'  0x3d1 Theta Symbol  
:: 'ϒ'  0x3d2 Upsilon With Hook Symbol
:: 'ϓ'  0x3d3 Upsilon With Acute And
:: 'ϔ'  0x3d4 Upsilon With Diaeresis And
:: 'Φ'  0x3d5 Phi Symbol  
:: 'Π'  0x3d6 Pi Symbol  
:: 'Ϗ'  0x3d7 Kai Symbol  
:: 'Ϙ'  0x3d9 Greek Capital Letter Archaic
:: 'Ϛ'  0x3db Greek Capital Letter Stigma
:: 'Ϝ'  0x3dd Greek Capital Letter Digamma
:: 'Ϟ'  0x3df Greek Capital Letter Koppa
:: 'Ϡ'  0x3e1 Greek Capital Letter Sampi
:: 'Ϣ'  0x3e2 Coptic Capital Letter Shei
:: 'Ϥ'  0x3e4 Coptic Capital Letter Fei
:: 'Ϧ'  0x3e6 Coptic Capital Letter Khei
:: 'Ϩ'  0x3e8 Coptic Capital Letter Hori
:: 'Ϫ'  0x3ea Coptic Capital Letter Gangia
:: 'Ϭ'  0x3ec Coptic Capital Letter Shima
:: 'Ϯ'  0x3ee Coptic Capital Letter Dei
:: 'Ё'  0x401 Cyrillic Capital Letter Io
:: 'Ђ'  0x402 Cyrillic Capital Letter Dje
:: 'Ѓ'  0x403 Cyrillic Capital Letter Gje
:: 'Є'  0x404 Cyrillic Capital Letter Ukrainian
:: 'Ѕ'  0x405 Cyrillic Capital Letter Dze
:: 'І'  0x406 Cyrillic Capital Letter Byelorussian-Ukrainian
:: 'Ї'  0x407 Cyrillic Capital Letter Yi
:: 'Ј'  0x408 Cyrillic Capital Letter Je
:: 'Љ'  0x409 Cyrillic Capital Letter Lje
:: 'Њ'  0x40a Cyrillic Capital Letter Nje
:: 'Ћ'  0x40b Cyrillic Capital Letter Tshe
:: 'Ќ'  0x40c Cyrillic Capital Letter Kje
:: 'Ў'  0x40e Cyrillic Capital Letter Short
:: 'Џ'  0x40f Cyrillic Capital Letter Dzhe
:: 'А'  0x410 Cyrillic Capital Letter A
:: 'Б'  0x411 Cyrillic Capital Letter Be
:: 'В'  0x412 Cyrillic Capital Letter Ve
:: 'Г'  0x413 Cyrillic Capital Letter Ghe
:: 'Д'  0x414 Cyrillic Capital Letter De
:: 'Е'  0x415 Cyrillic Capital Letter Ie
:: 'Ж'  0x416 Cyrillic Capital Letter Zhe
:: 'З'  0x417 Cyrillic Capital Letter Ze
:: 'И'  0x418 Cyrillic Capital Letter I
:: 'Й'  0x419 Cyrillic Capital Letter Short
:: 'К'  0x41a Cyrillic Capital Letter Ka
:: 'Л'  0x41b Cyrillic Capital Letter El
:: 'М'  0x41c Cyrillic Capital Letter Em
:: 'Н'  0x41d Cyrillic Capital Letter En
:: 'О'  0x41e Cyrillic Capital Letter O
:: 'П'  0x41f Cyrillic Capital Letter Pe
:: 'Р'  0x420 Cyrillic Capital Letter Er
:: 'С'  0x421 Cyrillic Capital Letter Es
:: 'Т'  0x422 Cyrillic Capital Letter Te
:: 'У'  0x423 Cyrillic Capital Letter U
:: 'Ф'  0x424 Cyrillic Capital Letter Ef
:: 'Х'  0x425 Cyrillic Capital Letter Ha
:: 'Ц'  0x426 Cyrillic Capital Letter Tse
:: 'Ч'  0x427 Cyrillic Capital Letter Che
:: 'Ш'  0x428 Cyrillic Capital Letter Sha
:: 'Щ'  0x429 Cyrillic Capital Letter Shcha
:: 'Ъ'  0x42a Cyrillic Capital Letter Hard
:: 'Ы'  0x42b Cyrillic Capital Letter Yeru
:: 'Ь'  0x42c Cyrillic Capital Letter Soft
:: 'Э'  0x42d Cyrillic Capital Letter E
:: 'Ю'  0x42e Cyrillic Capital Letter Yu
:: 'Я'  0x42f Cyrillic Capital Letter Ya
:: 'Ѡ'  0x460 Cyrillic Capital Letter Omega
:: 'Ѣ'  0x462 Cyrillic Capital Letter Yat
:: 'Ѥ'  0x464 Cyrillic Capital Letter Iotified
:: 'Ѧ'  0x466 Cyrillic Capital Letter Little
:: 'Ѩ'  0x468 Cyrillic Capital Letter Iotified
:: 'Ѫ'  0x46a Cyrillic Capital Letter Big
:: 'Ѭ'  0x46c Cyrillic Capital Letter Iotified
:: 'Ѯ'  0x46e Cyrillic Capital Letter Ksi
:: 'Ѱ'  0x470 Cyrillic Capital Letter Psi
:: 'Ѳ'  0x472 Cyrillic Capital Letter Fita
:: 'Ѵ'  0x474 Cyrillic Capital Letter Izhitsa
:: 'Ѷ'  0x476 Cyrillic Capital Letter Izhitsa
:: 'Ѹ'  0x478 Cyrillic Capital Letter Uk
:: 'Ѻ'  0x47a Cyrillic Capital Letter Round
:: 'Ѽ'  0x47c Cyrillic Capital Letter Omega
:: 'Ѿ'  0x47e Cyrillic Capital Letter Ot
:: 'Ҁ'  0x480 Cyrillic Capital Letter Koppa
:: 'Ґ'  0x490 Cyrillic Capital Letter Ghe
:: 'Ғ'  0x492 Cyrillic Capital Letter Ghe
:: 'Ҕ'  0x494 Cyrillic Capital Letter Ghe
:: 'Җ'  0x496 Cyrillic Capital Letter Zhe
:: 'Ҙ'  0x498 Cyrillic Capital Letter Ze
:: 'Қ'  0x49a Cyrillic Capital Letter Ka
:: 'Ҝ'  0x49c Cyrillic Capital Letter Ka
:: 'Ҟ'  0x49e Cyrillic Capital Letter Ka
:: 'Ҡ'  0x4a0 Cyrillic Capital Letter Bashkir
:: 'Ң'  0x4a2 Cyrillic Capital Letter En
:: 'Ҥ'  0x4a4 Cyrillic Capital Ligature En
:: 'Ҧ'  0x4a6 Cyrillic Capital Letter Pe
:: 'Ҩ'  0x4a8 Cyrillic Capital Letter Abkhasian
:: 'Ҫ'  0x4aa Cyrillic Capital Letter Es
:: 'Ҭ'  0x4ac Cyrillic Capital Letter Te
:: 'Ү'  0x4ae Cyrillic Capital Letter Straight
:: 'Ұ'  0x4b0 Cyrillic Capital Letter Straight
:: 'Ҳ'  0x4b2 Cyrillic Capital Letter Ha
:: 'Ҵ'  0x4b4 Cyrillic Capital Ligature Te
:: 'Ҷ'  0x4b6 Cyrillic Capital Letter Che
:: 'Ҹ'  0x4b8 Cyrillic Capital Letter Che
:: 'Һ'  0x4ba Cyrillic Capital Letter Shha
:: 'Ҽ'  0x4bc Cyrillic Capital Letter Abkhasian
:: 'Ҿ'  0x4be Cyrillic Capital Letter Abkhasian
:: 'Ӂ'  0x4c1 Cyrillic Capital Letter Zhe
:: 'Ӄ'  0x4c3 Cyrillic Capital Letter Ka
:: 'Ӈ'  0x4c7 Cyrillic Capital Letter En
:: 'Ӌ'  0x4cb Cyrillic Capital Letter Khakassian
:: 'Ӑ'  0x4d0 Cyrillic Capital Letter A
:: 'Ӓ'  0x4d2 Cyrillic Capital Letter A
:: 'Ӕ'  0x4d4 Cyrillic Capital Ligature A
:: 'Ӗ'  0x4d6 Cyrillic Capital Letter Ie
:: 'Ә'  0x4d8 Cyrillic Capital Letter Schwa
:: 'Ӛ'  0x4da Cyrillic Capital Letter Schwa
:: 'Ӝ'  0x4dc Cyrillic Capital Letter Zhe
:: 'Ӟ'  0x4de Cyrillic Capital Letter Ze
:: 'Ӡ'  0x4e0 Cyrillic Capital Letter Abkhasian
:: 'Ӣ'  0x4e2 Cyrillic Capital Letter I
:: 'Ӥ'  0x4e4 Cyrillic Capital Letter I
:: 'Ӧ'  0x4e6 Cyrillic Capital Letter O
:: 'Ө'  0x4e8 Cyrillic Capital Letter Barred
:: 'Ӫ'  0x4ea Cyrillic Capital Letter Barred
:: 'Ӯ'  0x4ee Cyrillic Capital Letter U
:: 'Ӱ'  0x4f0 Cyrillic Capital Letter U
:: 'Ӳ'  0x4f2 Cyrillic Capital Letter U
:: 'Ӵ'  0x4f4 Cyrillic Capital Letter Che
:: 'Ӹ'  0x4f8 Cyrillic Capital Letter Yeru
:: 'Ա'  0x531 Armenian Capital Letter Ayb
:: 'Բ'  0x532 Armenian Capital Letter Ben
:: 'Գ'  0x533 Armenian Capital Letter Gim
:: 'Դ'  0x534 Armenian Capital Letter Da
:: 'Ե'  0x535 Armenian Capital Letter Ech
:: 'Զ'  0x536 Armenian Capital Letter Za
:: 'Է'  0x537 Armenian Capital Letter Eh
:: 'Ը'  0x538 Armenian Capital Letter Et
:: 'Թ'  0x539 Armenian Capital Letter To
:: 'Ժ'  0x53a Armenian Capital Letter Zhe
:: 'Ի'  0x53b Armenian Capital Letter Ini
:: 'Լ'  0x53c Armenian Capital Letter Liwn
:: 'Խ'  0x53d Armenian Capital Letter Xeh
:: 'Ծ'  0x53e Armenian Capital Letter Ca
:: 'Կ'  0x53f Armenian Capital Letter Ken
:: 'Հ'  0x540 Armenian Capital Letter Ho
:: 'Ձ'  0x541 Armenian Capital Letter Ja
:: 'Ղ'  0x542 Armenian Capital Letter Ghad
:: 'Ճ'  0x543 Armenian Capital Letter Cheh
:: 'Մ'  0x544 Armenian Capital Letter Men
:: 'Յ'  0x545 Armenian Capital Letter Yi
:: 'Ն'  0x546 Armenian Capital Letter Now
:: 'Շ'  0x547 Armenian Capital Letter Sha
:: 'Ո'  0x548 Armenian Capital Letter Vo
:: 'Չ'  0x549 Armenian Capital Letter Cha
:: 'Պ'  0x54a Armenian Capital Letter Peh
:: 'Ջ'  0x54b Armenian Capital Letter Jheh
:: 'Ռ'  0x54c Armenian Capital Letter Ra
:: 'Ս'  0x54d Armenian Capital Letter Seh
:: 'Վ'  0x54e Armenian Capital Letter Vew
:: 'Տ'  0x54f Armenian Capital Letter Tiwn
:: 'Ր'  0x550 Armenian Capital Letter Reh
:: 'Ց'  0x551 Armenian Capital Letter Co
:: 'Ւ'  0x552 Armenian Capital Letter Yiwn
:: 'Փ'  0x553 Armenian Capital Letter Piwr
:: 'Ք'  0x554 Armenian Capital Letter Keh
:: 'Օ'  0x555 Armenian Capital Letter Oh
:: 'Ֆ'  0x556 Armenian Capital Letter Feh
:: 'Ⴀ'  0x10a0 Georgian Capital Letter An
:: 'Ⴁ'  0x10a1 Georgian Capital Letter Ban
:: 'Ⴂ'  0x10a2 Georgian Capital Letter Gan
:: 'Ⴃ'  0x10a3 Georgian Capital Letter Don
:: 'Ⴄ'  0x10a4 Georgian Capital Letter En
:: 'Ⴅ'  0x10a5 Georgian Capital Letter Vin
:: 'Ⴆ'  0x10a6 Georgian Capital Letter Zen
:: 'Ⴇ'  0x10a7 Georgian Capital Letter Tan
:: 'Ⴈ'  0x10a8 Georgian Capital Letter In
:: 'Ⴉ'  0x10a9 Georgian Capital Letter Kan
:: 'Ⴊ'  0x10aa Georgian Capital Letter Las
:: 'Ⴋ'  0x10ab Georgian Capital Letter Man
:: 'Ⴌ'  0x10ac Georgian Capital Letter Nar
:: 'Ⴍ'  0x10ad Georgian Capital Letter On
:: 'Ⴎ'  0x10ae Georgian Capital Letter Par
:: 'Ⴏ'  0x10af Georgian Capital Letter Zhar
:: 'Ⴐ'  0x10b0 Georgian Capital Letter Rae
:: 'Ⴑ'  0x10b1 Georgian Capital Letter San
:: 'Ⴒ'  0x10b2 Georgian Capital Letter Tar
:: 'Ⴓ'  0x10b3 Georgian Capital Letter Un
:: 'Ⴔ'  0x10b4 Georgian Capital Letter Phar
:: 'Ⴕ'  0x10b5 Georgian Capital Letter Khar
:: 'Ⴖ'  0x10b6 Georgian Capital Letter Ghan
:: 'Ⴗ'  0x10b7 Georgian Capital Letter Qar
:: 'Ⴘ'  0x10b8 Georgian Capital Letter Shin
:: 'Ⴙ'  0x10b9 Georgian Capital Letter Chin
:: 'Ⴚ'  0x10ba Georgian Capital Letter Can
:: 'Ⴛ'  0x10bb Georgian Capital Letter Jil
:: 'Ⴜ'  0x10bc Georgian Capital Letter Cil
:: 'Ⴝ'  0x10bd Georgian Capital Letter Char
:: 'Ⴞ'  0x10be Georgian Capital Letter Xan
:: 'Ⴟ'  0x10bf Georgian Capital Letter Jhan
:: 'Ⴠ'  0x10c0 Georgian Capital Letter Hae
:: 'Ⴡ'  0x10c1 Georgian Capital Letter He
:: 'Ⴢ'  0x10c2 Georgian Capital Letter Hie
:: 'Ⴣ'  0x10c3 Georgian Capital Letter We
:: 'Ⴤ'  0x10c4 Georgian Capital Letter Har
:: 'Ⴥ'  0x10c5 Georgian Capital Letter Hoe
:: 'Ꭰ'  0x13a0 Cherokee Capital Letter 
:: 'Ꭱ'  0x13a1 Cherokee Capital Letter 
:: 'Ꭲ'  0x13a2 Cherokee Capital Letter 
:: 'Ꭳ'  0x13a3 Cherokee Capital Letter 
:: 'Ꭴ'  0x13a4 Cherokee Capital Letter 
:: 'Ꭵ'  0x13a5 Cherokee Capital Letter 
:: 'Ꭶ'  0x13a6 Cherokee Capital Letter 
:: 'Ꭷ'  0x13a7 Cherokee Capital Letter 
:: 'Ꭸ'  0x13a8 Cherokee Capital Letter 
:: 'Ꭹ'  0x13a9 Cherokee Capital Letter 
:: 'Ꭺ'  0x13aa Cherokee Capital Letter 
:: 'Ꭻ'  0x13ab Cherokee Capital Letter 
:: 'Ꭼ'  0x13ac Cherokee Capital Letter 
:: 'Ꭽ'  0x13ad Cherokee Capital Letter 
:: 'Ꭾ'  0x13ae Cherokee Capital Letter 
:: 'Ꭿ'  0x13af Cherokee Capital Letter 
:: 'Ꮀ'  0x13b0 Cherokee Capital Letter 
:: 'Ꮁ'  0x13b1 Cherokee Capital Letter 
:: 'Ꮂ'  0x13b2 Cherokee Capital Letter 
:: 'Ꮃ'  0x13b3 Cherokee Capital Letter 
:: 'Ꮄ'  0x13b4 Cherokee Capital Letter 
:: 'Ꮅ'  0x13b5 Cherokee Capital Letter 
:: 'Ꮆ'  0x13b6 Cherokee Capital Letter 
:: 'Ꮇ'  0x13b7 Cherokee Capital Letter 
:: 'Ꮈ'  0x13b8 Cherokee Capital Letter 
:: 'Ꮉ'  0x13b9 Cherokee Capital Letter 
:: 'Ꮊ'  0x13ba Cherokee Capital Letter 
:: 'Ꮋ'  0x13bb Cherokee Capital Letter 
:: 'Ꮌ'  0x13bc Cherokee Capital Letter 
:: 'Ꮍ'  0x13bd Cherokee Capital Letter 
:: 'Ꮎ'  0x13be Cherokee Capital Letter 
:: 'Ꮏ'  0x13bf Cherokee Capital Letter 
:: 'Ꮐ'  0x13c0 Cherokee Capital Letter 
:: 'Ꮑ'  0x13c1 Cherokee Capital Letter 
:: 'Ꮒ'  0x13c2 Cherokee Capital Letter 
:: 'Ꮓ'  0x13c3 Cherokee Capital Letter 
:: 'Ꮔ'  0x13c4 Cherokee Capital Letter 
:: 'Ꮕ'  0x13c5 Cherokee Capital Letter 
:: 'Ꮖ'  0x13c6 Cherokee Capital Letter 
:: 'Ꮗ'  0x13c7 Cherokee Capital Letter 
:: 'Ꮘ'  0x13c8 Cherokee Capital Letter 
:: 'Ꮙ'  0x13c9 Cherokee Capital Letter 
:: 'Ꮚ'  0x13ca Cherokee Capital Letter 
:: 'Ꮛ'  0x13cb Cherokee Capital Letter 
:: 'Ꮜ'  0x13cc Cherokee Capital Letter 
:: 'Ꮝ'  0x13cd Cherokee Capital Letter 
:: 'Ꮞ'  0x13ce Cherokee Capital Letter 
:: 'Ꮟ'  0x13cf Cherokee Capital Letter 
:: 'Ꮠ'  0x13d0 Cherokee Capital Letter 
:: 'Ꮡ'  0x13d1 Cherokee Capital Letter 
:: 'Ꮢ'  0x13d2 Cherokee Capital Letter 
:: 'Ꮣ'  0x13d3 Cherokee Capital Letter 
:: 'Ꮤ'  0x13d4 Cherokee Capital Letter 
:: 'Ꮥ'  0x13d5 Cherokee Capital Letter 
:: 'Ꮦ'  0x13d6 Cherokee Capital Letter 
:: 'Ꮧ'  0x13d7 Cherokee Capital Letter 
:: 'Ꮨ'  0x13d8 Cherokee Capital Letter 
:: 'Ꮩ'  0x13d9 Cherokee Capital Letter 
:: 'Ꮪ'  0x13da Cherokee Capital Letter 
:: 'Ꮫ'  0x13db Cherokee Capital Letter 
:: 'Ꮬ'  0x13dc Cherokee Capital Letter 
:: 'Ꮭ'  0x13dd Cherokee Capital Letter 
:: 'Ꮮ'  0x13de Cherokee Capital Letter 
:: 'Ꮯ'  0x13df Cherokee Capital Letter 
:: 'Ꮰ'  0x13e0 Cherokee Capital Letter 
:: 'Ꮱ'  0x13e1 Cherokee Capital Letter 
:: 'Ꮲ'  0x13e2 Cherokee Capital Letter 
:: 'Ꮳ'  0x13e3 Cherokee Capital Letter 
:: 'Ꮴ'  0x13e4 Cherokee Capital Letter 
:: 'Ꮵ'  0x13e5 Cherokee Capital Letter 
:: 'Ꮶ'  0x13e6 Cherokee Capital Letter 
:: 'Ꮷ'  0x13e7 Cherokee Capital Letter 
:: 'Ꮸ'  0x13e8 Cherokee Capital Letter 
:: 'Ꮹ'  0x13e9 Cherokee Capital Letter 
:: 'Ꮺ'  0x13ea Cherokee Capital Letter 
:: 'Ꮻ'  0x13eb Cherokee Capital Letter 
:: 'Ꮼ'  0x13ec Cherokee Capital Letter 
:: 'Ꮽ'  0x13ed Cherokee Capital Letter 
:: 'Ꮾ'  0x13ee Cherokee Capital Letter 
:: 'Ꮿ'  0x13ef Cherokee Capital Letter 
:: 'Ᏸ'  0x13f0 Cherokee Capital Letter 
:: 'Ᏹ'  0x13f1 Cherokee Capital Letter 
:: 'Ᏺ'  0x13f2 Cherokee Capital Letter 
:: 'Ᏻ'  0x13f3 Cherokee Capital Letter 
:: 'Ᏼ'  0x13f4 Cherokee Capital Letter 
:: 'Ᏽ'  0x13f5 Cherokee Capital Letter 
:: 'Ḁ'  0x1e00 Latin Capital Letter A
:: 'Ḃ'  0x1e02 Latin Capital Letter B
:: 'Ḅ'  0x1e04 Latin Capital Letter B
:: 'Ḇ'  0x1e06 Latin Capital Letter B
:: 'Ḉ'  0x1e08 Latin Capital Letter C
:: 'Ḋ'  0x1e0a Latin Capital Letter D
:: 'Ḍ'  0x1e0c Latin Capital Letter D
:: 'Ḏ'  0x1e0e Latin Capital Letter D
:: 'Ḑ'  0x1e10 Latin Capital Letter D
:: 'Ḓ'  0x1e12 Latin Capital Letter D
:: 'Ḕ'  0x1e14 Latin Capital Letter E
:: 'Ḗ'  0x1e16 Latin Capital Letter E
:: 'Ḙ'  0x1e18 Latin Capital Letter E
:: 'Ḛ'  0x1e1a Latin Capital Letter E
:: 'Ḝ'  0x1e1c Latin Capital Letter E
:: 'Ḟ'  0x1e1e Latin Capital Letter F
:: 'Ḡ'  0x1e20 Latin Capital Letter G
:: 'Ḣ'  0x1e22 Latin Capital Letter H
:: 'Ḥ'  0x1e24 Latin Capital Letter H
:: 'Ḧ'  0x1e26 Latin Capital Letter H
:: 'Ḩ'  0x1e28 Latin Capital Letter H
:: 'Ḫ'  0x1e2a Latin Capital Letter H
:: 'Ḭ'  0x1e2c Latin Capital Letter I
:: 'Ḯ'  0x1e2e Latin Capital Letter I
:: 'Ḱ'  0x1e30 Latin Capital Letter K
:: 'Ḳ'  0x1e32 Latin Capital Letter K
:: 'Ḵ'  0x1e34 Latin Capital Letter K
:: 'Ḷ'  0x1e36 Latin Capital Letter L
:: 'Ḹ'  0x1e38 Latin Capital Letter L
:: 'Ḻ'  0x1e3a Latin Capital Letter L
:: 'Ḽ'  0x1e3c Latin Capital Letter L
:: 'Ḿ'  0x1e3e Latin Capital Letter M
:: 'Ṁ'  0x1e40 Latin Capital Letter M
:: 'Ṃ'  0x1e42 Latin Capital Letter M
:: 'Ṅ'  0x1e44 Latin Capital Letter N
:: 'Ṇ'  0x1e46 Latin Capital Letter N
:: 'Ṉ'  0x1e48 Latin Capital Letter N
:: 'Ṋ'  0x1e4a Latin Capital Letter N
:: 'Ṍ'  0x1e4c Latin Capital Letter O
:: 'Ṏ'  0x1e4e Latin Capital Letter O
:: 'Ṑ'  0x1e50 Latin Capital Letter O
:: 'Ṓ'  0x1e52 Latin Capital Letter O
:: 'Ṕ'  0x1e54 Latin Capital Letter P
:: 'Ṗ'  0x1e56 Latin Capital Letter P
:: 'Ṙ'  0x1e58 Latin Capital Letter R
:: 'Ṛ'  0x1e5a Latin Capital Letter R
:: 'Ṝ'  0x1e5c Latin Capital Letter R
:: 'Ṟ'  0x1e5e Latin Capital Letter R
:: 'Ṡ'  0x1e60 Latin Capital Letter S
:: 'Ṣ'  0x1e62 Latin Capital Letter S
:: 'Ṥ'  0x1e64 Latin Capital Letter S
:: 'Ṧ'  0x1e66 Latin Capital Letter S
:: 'Ṩ'  0x1e68 Latin Capital Letter S
:: 'Ṫ'  0x1e6a Latin Capital Letter T
:: 'Ṭ'  0x1e6c Latin Capital Letter T
:: 'Ṯ'  0x1e6e Latin Capital Letter T
:: 'Ṱ'  0x1e70 Latin Capital Letter T
:: 'Ṳ'  0x1e72 Latin Capital Letter U
:: 'Ṵ'  0x1e74 Latin Capital Letter U
:: 'Ṷ'  0x1e76 Latin Capital Letter U
:: 'Ṹ'  0x1e78 Latin Capital Letter U
:: 'Ṻ'  0x1e7a Latin Capital Letter U
:: 'Ṽ'  0x1e7c Latin Capital Letter V
:: 'Ṿ'  0x1e7e Latin Capital Letter V
:: 'Ẁ'  0x1e80 Latin Capital Letter W
:: 'Ẃ'  0x1e82 Latin Capital Letter W
:: 'Ẅ'  0x1e84 Latin Capital Letter W
:: 'Ẇ'  0x1e86 Latin Capital Letter W
:: 'Ẉ'  0x1e88 Latin Capital Letter W
:: 'Ẋ'  0x1e8a Latin Capital Letter X
:: 'Ẍ'  0x1e8c Latin Capital Letter X
:: 'Ẏ'  0x1e8e Latin Capital Letter Y
:: 'Ẑ'  0x1e90 Latin Capital Letter Z
:: 'Ẓ'  0x1e92 Latin Capital Letter Z
:: 'Ẕ'  0x1e94 Latin Capital Letter Z
:: 'Ạ'  0x1ea0 Latin Capital Letter A
:: 'Ả'  0x1ea2 Latin Capital Letter A
:: 'Ấ'  0x1ea4 Latin Capital Letter A
:: 'Ầ'  0x1ea6 Latin Capital Letter A
:: 'Ẩ'  0x1ea8 Latin Capital Letter A
:: 'Ẫ'  0x1eaa Latin Capital Letter A
:: 'Ậ'  0x1eac Latin Capital Letter A
:: 'Ắ'  0x1eae Latin Capital Letter A
:: 'Ằ'  0x1eb0 Latin Capital Letter A
:: 'Ẳ'  0x1eb2 Latin Capital Letter A
:: 'Ẵ'  0x1eb4 Latin Capital Letter A
:: 'Ặ'  0x1eb6 Latin Capital Letter A
:: 'Ẹ'  0x1eb8 Latin Capital Letter E
:: 'Ẻ'  0x1eba Latin Capital Letter E
:: 'Ẽ'  0x1ebc Latin Capital Letter E
:: 'Ế'  0x1ebe Latin Capital Letter E
:: 'Ề'  0x1ec0 Latin Capital Letter E
:: 'Ể'  0x1ec2 Latin Capital Letter E
:: 'Ễ'  0x1ec4 Latin Capital Letter E
:: 'Ệ'  0x1ec6 Latin Capital Letter E
:: 'Ỉ'  0x1ec8 Latin Capital Letter I
:: 'Ị'  0x1eca Latin Capital Letter I
:: 'Ọ'  0x1ecc Latin Capital Letter O
:: 'Ỏ'  0x1ece Latin Capital Letter O
:: 'Ố'  0x1ed0 Latin Capital Letter O
:: 'Ồ'  0x1ed2 Latin Capital Letter O
:: 'Ổ'  0x1ed4 Latin Capital Letter O
:: 'Ỗ'  0x1ed6 Latin Capital Letter O
:: 'Ộ'  0x1ed8 Latin Capital Letter O
:: 'Ớ'  0x1eda Latin Capital Letter O
:: 'Ờ'  0x1edc Latin Capital Letter O
:: 'Ở'  0x1ede Latin Capital Letter O
:: 'Ỡ'  0x1ee0 Latin Capital Letter O
:: 'Ợ'  0x1ee2 Latin Capital Letter O
:: 'Ụ'  0x1ee4 Latin Capital Letter U
:: 'Ủ'  0x1ee6 Latin Capital Letter U
:: 'Ứ'  0x1ee8 Latin Capital Letter U
:: 'Ừ'  0x1eea Latin Capital Letter U
:: 'Ử'  0x1eec Latin Capital Letter U
:: 'Ữ'  0x1eee Latin Capital Letter U
:: 'Ự'  0x1ef0 Latin Capital Letter U
:: 'Ỳ'  0x1ef2 Latin Capital Letter Y
:: 'Ỵ'  0x1ef4 Latin Capital Letter Y
:: 'Ỷ'  0x1ef6 Latin Capital Letter Y
:: 'Ỹ'  0x1ef8 Latin Capital Letter Y
:: 'Ἀ'  0x1f08 Greek Capital Letter Alpha
:: 'Ἁ'  0x1f09 Greek Capital Letter Alpha
:: 'Ἂ'  0x1f0a Greek Capital Letter Alpha
:: 'Ἃ'  0x1f0b Greek Capital Letter Alpha
:: 'Ἄ'  0x1f0c Greek Capital Letter Alpha
:: 'Ἅ'  0x1f0d Greek Capital Letter Alpha
:: 'Ἆ'  0x1f0e Greek Capital Letter Alpha
:: 'Ἇ'  0x1f0f Greek Capital Letter Alpha
:: 'Ἐ'  0x1f18 Greek Capital Letter Epsilon
:: 'Ἑ'  0x1f19 Greek Capital Letter Epsilon
:: 'Ἒ'  0x1f1a Greek Capital Letter Epsilon
:: 'Ἓ'  0x1f1b Greek Capital Letter Epsilon
:: 'Ἔ'  0x1f1c Greek Capital Letter Epsilon
:: 'Ἕ'  0x1f1d Greek Capital Letter Epsilon
:: 'Ἠ'  0x1f28 Greek Capital Letter Eta
:: 'Ἠ'  0x1f20 Greek Capital Letter Eta
:: 'Ἡ'  0x1f29 Greek Capital Letter Eta
:: 'Ἢ'  0x1f2a Greek Capital Letter Eta
:: 'Ἣ'  0x1f2b Greek Capital Letter Eta
:: 'Ἤ'  0x1f2c Greek Capital Letter Eta
:: 'Ἥ'  0x1f2d Greek Capital Letter Eta
:: 'Ἦ'  0x1f2e Greek Capital Letter Eta
:: 'Ἧ'  0x1f2f Greek Capital Letter Eta
:: 'Ἰ'  0x1f38 Greek Capital Letter Iota
:: 'Ἱ'  0x1f39 Greek Capital Letter Iota
:: 'Ἲ'  0x1f3a Greek Capital Letter Iota
:: 'Ἳ'  0x1f3b Greek Capital Letter Iota
:: 'Ἴ'  0x1f3c Greek Capital Letter Iota
:: 'Ἵ'  0x1f3d Greek Capital Letter Iota
:: 'Ἶ'  0x1f3e Greek Capital Letter Iota
:: 'Ἷ'  0x1f3f Greek Capital Letter Iota
:: 'Ὀ'  0x1f48 Greek Capital Letter Omicron
:: 'Ὁ'  0x1f49 Greek Capital Letter Omicron
:: 'Ὂ'  0x1f4a Greek Capital Letter Omicron
:: 'Ὃ'  0x1f4b Greek Capital Letter Omicron
:: 'Ὄ'  0x1f4c Greek Capital Letter Omicron
:: 'Ὅ'  0x1f4d Greek Capital Letter Omicron
:: 'Ὑ'  0x1f59 Greek Capital Letter Upsilon
:: 'Ὓ'  0x1f5b Greek Capital Letter Upsilon
:: 'Ὕ'  0x1f5d Greek Capital Letter Upsilon
:: 'Ὗ'  0x1f5f Greek Capital Letter Upsilon
:: 'Ὠ'  0x1f68 Greek Capital Letter Omega
:: 'Ὡ'  0x1f69 Greek Capital Letter Omega
:: 'Ὢ'  0x1f6a Greek Capital Letter Omega
:: 'Ὣ'  0x1f6b Greek Capital Letter Omega
:: 'Ὤ'  0x1f6c Greek Capital Letter Omega
:: 'Ὥ'  0x1f6d Greek Capital Letter Omega
:: 'Ὦ'  0x1f6e Greek Capital Letter Omega
:: 'Ὧ'  0x1f6f Greek Capital Letter Omega
:: 'Ᾰ'  0x1fb8 Greek Capital Letter Alpha
:: 'Ᾱ'  0x1fb9 Greek Capital Letter Alpha
:: 'Ὰ'  0x1fba Greek Capital Letter Alpha
:: 'Ῐ'  0x1fd8 Greek Capital Letter Iota
:: 'Ῑ'  0x1fd9 Greek Capital Letter Iota
:: 'Ῠ'  0x1fe8 Greek Capital Letter Upsilon
:: 'Ῡ'  0x1fe9 Greek Capital Letter Upsilon
:: 'Ａ'  0xff21 Fullwidth Latin Capital Letter
:: 'Ｂ'  0xff22 Fullwidth Latin Capital Letter
:: 'Ｃ'  0xff23 Fullwidth Latin Capital Letter
:: 'Ｄ'  0xff24 Fullwidth Latin Capital Letter
:: 'Ｅ'  0xff25 Fullwidth Latin Capital Letter
:: 'Ｆ'  0xff26 Fullwidth Latin Capital Letter
:: 'Ｇ'  0xff27 Fullwidth Latin Capital Letter
:: 'Ｈ'  0xff28 Fullwidth Latin Capital Letter
:: 'Ｉ'  0xff29 Fullwidth Latin Capital Letter
:: 'Ｊ'  0xff2a Fullwidth Latin Capital Letter
:: 'Ｋ'  0xff2b Fullwidth Latin Capital Letter
:: 'Ｌ'  0xff2c Fullwidth Latin Capital Letter
:: 'Ｍ'  0xff2d Fullwidth Latin Capital Letter
:: 'Ｎ'  0xff2e Fullwidth Latin Capital Letter
:: 'Ｏ'  0xff2f Fullwidth Latin Capital Letter
:: 'Ｐ'  0xff30 Fullwidth Latin Capital Letter
:: 'Ｑ'  0xff31 Fullwidth Latin Capital Letter
:: 'Ｒ'  0xff32 Fullwidth Latin Capital Letter
:: 'Ｓ'  0xff33 Fullwidth Latin Capital Letter
:: 'Ｔ'  0xff34 Fullwidth Latin Capital Letter
:: 'Ｕ'  0xff35 Fullwidth Latin Capital Letter
:: 'Ｖ'  0xff36 Fullwidth Latin Capital Letter
:: 'Ｗ'  0xff37 Fullwidth Latin Capital Letter
:: 'Ｘ'  0xff38 Fullwidth Latin Capital Letter
:: 'Ｙ'  0xff39 Fullwidth Latin Capital Letter
:: 'Ｚ'  0xff3a Fullwidth Latin Capital Letter
:: '𐐀'  0x10400 Deseret Capital Letter Long
:: '𐐁'  0x10401 Deseret Capital Letter Long
:: '𐐂'  0x10402 Deseret Capital Letter Long
:: '𐐃'  0x10403 Deseret Capital Letter Long
:: '𐐄'  0x10404 Deseret Capital Letter Long
:: '𐐅'  0x10405 Deseret Capital Letter Long
:: '𐐆'  0x10406 Deseret Capital Letter Short
:: '𐐇'  0x10407 Deseret Capital Letter Short
:: '𐐈'  0x10408 Deseret Capital Letter Short
:: '𐐉'  0x10409 Deseret Capital Letter Short
:: '𐐊'  0x1040a Deseret Capital Letter Short
:: '𐐋'  0x1040b Deseret Capital Letter Short
:: '𐐌'  0x1040c Deseret Capital Letter Ay
:: '𐐍'  0x1040d Deseret Capital Letter Ow
:: '𐐎'  0x1040e Deseret Capital Letter Wu
:: '𐐏'  0x1040f Deseret Capital Letter Yee
:: '𐐐'  0x10410 Deseret Capital Letter H
:: '𐐑'  0x10411 Deseret Capital Letter Pee
:: '𐐒'  0x10412 Deseret Capital Letter Bee
:: '𐐓'  0x10413 Deseret Capital Letter Tee
:: '𐐔'  0x10414 Deseret Capital Letter Dee
:: '𐐕'  0x10415 Deseret Capital Letter Chee
:: '𐐖'  0x10416 Deseret Capital Letter Jee
:: '𐐗'  0x10417 Deseret Capital Letter Kay
:: '𐐘'  0x10418 Deseret Capital Letter Gay
:: '𐐙'  0x10419 Deseret Capital Letter Ef
:: '𐐚'  0x1041a Deseret Capital Letter Vee
:: '𐐛'  0x1041b Deseret Capital Letter Eth
:: '𐐜'  0x1041c Deseret Capital Letter Thee
:: '𐐝'  0x1041d Deseret Capital Letter Es
:: '𐐞'  0x1041e Deseret Capital Letter Zee
:: '𐐟'  0x1041f Deseret Capital Letter Esh
:: '𐐠'  0x10420 Deseret Capital Letter Zhee
:: '𐐡'  0x10421 Deseret Capital Letter Er
:: '𐐢'  0x10422 Deseret Capital Letter El
:: '𐐣'  0x10423 Deseret Capital Letter Em
:: '𐐤'  0x10424 Deseret Capital Letter En
:: '𐐥'  0x10425 Deseret Capital Letter Eng
:: '𐐦'  0x10426 Deseret Capital Letter Oi
:: '𐐧'  0x10427 Deseret Capital Letter Ew
