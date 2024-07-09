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
