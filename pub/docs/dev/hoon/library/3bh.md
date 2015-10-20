section 3bH, names etc
======================

### `++clan`

    ++  clan                                                ::  ship to rank
      |=  who=ship  ^-  rank
      =+  wid=(met 3 who)
      ?:  (lte wid 1)   %czar
      ?:  =(2 wid)      %king
      ?:  (lte wid 4)   %duke
      ?:  (lte wid 8)   %earl
      ?>  (lte wid 16)  %pawn
    ::

Ship class

    ~zod/main=> (clan ~zod)
    %czar
    ~zod/main=> (clan ~tyr)
    %czar
    ~zod/main=> (clan ~doznec)
    %king
    ~zod/main=> (clan ~tasfyn-partyv)
    %duke

### `++glam`

    ++  glam
      |=  zar=@pD  ^-  tape
      %+  snag  zar
      ^-  (list tape)
      :~  "Tianming"  "Pepin the Short"  "Haile Selassie"  "Alfred the Great"
          "Tamerlane"  "Pericles"  "Talleyrand"  "Yongle"  "Seleucus"
          "Uther Pendragon"  "Louis XVI"  "Ahmad Shāh Durrānī"  "Constantine"
          "Wilhelm I"  "Akbar"  "Louis XIV"  "Nobunaga"  "Alexander VI"
          "Philippe II"  "Julius II"  "David"  "Niall Noígíallach"  "Kublai Khan"
          "Öz Beg Khan"  "Ozymandias"  "Ögedei Khan"  "Jiang Jieshi"  "Darius"
          "Shivaji"  "Qianlong"  "Bolesław I Chrobry"  "Tigranes"  "Han Wudi"
          "Charles X"  "Naresuan"  "Frederick II"  "Simeon"  "Kangxi"
          "Suleiman the Magnificent"  "Pedro II"  "Genghis Khan"  "Laozi"
          "Porfirio Díaz"  "Pakal"  "Wu Zetian"  "Garibaldi"  "Matthias Corvinus"
          "Leopold II"  "Leonidas"  "Sitting Bull"  "Nebuchadnezzar II"
          "Rhodes"  "Henry VIII"  "Attila"  "Catherine II"  "Chulalongkorn"
          "Uthmān"  "Augustus"  "Faustin"  "Chongde"  "Justinian"
          "Afonso de Albuquerque"  "Antoninus Pius"  "Cromwell"  "Innocent X"
          "Fidel"  "Frederick the Great"  "Canute"  "Vytautas"  "Amina"
          "Hammurabi"  "Suharto"  "Victoria"  "Hiawatha"  "Paul V"  "Shaka"
          "Lê Thánh Tông"  "Ivan Asen II"  "Tiridates"  "Nefertiti"  "Gwangmu"
          "Ferdinand & Isabella"  "Askia"  "Xuande"  "Boris Godunov"  "Gilgamesh"
          "Maximillian I"  "Mao"  "Charlemagne"  "Narai"  "Hanno"  "Charles I & V"
          "Alexander II"  "Mansa Musa"  "Zoe Porphyrogenita"  "Metternich"
          "Robert the Bruce"  "Pachacutec"  "Jefferson"  "Solomon"  "Nicholas I"
          "Barbarossa"  "FDR"  "Pius X"  "Gwanggaeto"  "Abbas I"  "Julius Caesar"
          "Lee Kuan Yew"  "Ranavalona I"  "Go-Daigo"  "Zenobia"  "Henry V"
          "Bảo Đại"  "Casimir III"  "Cyrus"  "Charles the Wise"  "Sandrokottos"
          "Agamemnon"  "Clement VII"  "Suppiluliuma"  "Deng Xiaoping"
          "Victor Emmanuel"  "Ajatasatru"  "Jan Sobieski"  "Huangdi"  "Xuantong"
          "Narmer"  "Cosimo de' Medici"  "Möngke Khan"  "Stephen Dušan"  "Henri IV"
          "Mehmed Fatih"  "Conn Cétchathach"  "Francisco Franco"  "Leo X"
          "Kammu"  "Krishnadevaraya"  "Elizabeth I"  "Norton I"  "Washington"
          "Meiji"  "Umar"  "TR"  "Peter the Great"  "Agustin I"  "Ashoka"
          "William the Conqueror"  "Kongolo Mwamba"  "Song Taizu"
          "Ivan the Terrible"  "Yao"  "Vercingetorix"  "Geronimo"  "Rurik"
          "Urban VIII"  "Alexios Komnenos"  "Maria I"  "Tamar"  "Bismarck"
          "Arthur"  "Jimmu"  "Gustavus Adolphus"  "Suiko"  "Basil I"  "Montezuma"
          "Santa Anna"  "Xerxes"  "Beyazıt Yıldırım"  "Samudragupta"  "James I"
          "George III"  "Kamehameha"  "Francesco Sforza"  "Trajan"
          "Rajendra Chola"  "Hideyoshi"  "Cleopatra"  "Alexander"
          "Ashurbanipal"  "Paul III"  "Vespasian"  "Tecumseh"  "Narasimhavarman"
          "Suryavarman II"  "Bokassa I"  "Charles Canning"  "Theodosius"
          "Francis II"  "Zhou Wen"  "William Jardine"  "Ahmad al-Mansur"
          "Lajos Nagy"  "Theodora"  "Mussolini"  "Samuil"  "Osman Gazi"
          "Kim Il-sung"  "Maria Theresa"  "Lenin"  "Tokugawa"  "Marcus Aurelius"
          "Nzinga Mbande"  "Edward III"  "Joseph II"  "Pulakesi II"  "Priam"
          "Qin Shi Huang"  "Shah Jahan"  "Sejong"  "Sui Wendi"  "Otto I"
          "Napoleon III"  "Prester John"  "Dido"  "Joao I"  "Gregory I"
          "Gajah Mada"  "Abd-ar Rahmān III"  "Taizong"  "Franz Josef I"
          "Nicholas II"  "Gandhi"  "Chandragupta II"  "Peter III"
          "Oba Ewuare"  "Louis IX"  "Napoleon"  "Selim Yavuz"  "Shun"
          "Hayam Wuruk"  "Jagiełło"  "Nicaule"  "Sargon"  "Saladin"  "Charles II"
          "Brian Boru"  "Da Yu"  "Antiochus III"  "Charles I"
          "Jan Pieterszoon Coen"  "Hongwu"  "Mithridates"  "Hadrian"  "Ptolemy"
          "Benito Juarez"  "Sun Yat-sen"  "Raja Raja Chola"  "Bolivar"  "Pius VII"
          "Shapur II"  "Taksin"  "Ram Khamhaeng"  "Hatshepsut"  "Alī"  "Matilda"
          "Ataturk"
      ==
    ::

Retrieve carrier name.

    ~zod/main=> (glam ~zod)
    "Tianming"
    ~zod/main=> (glam ~fyr)
    "Bolivar"

### `++glon`

    ++  glon
      |=  lag=lang
      ^-  (unit tape)
      ?+  lag  ~
        %aa  [~ "Afar"]
        %ab  [~ "Abkhazian"]
        %ae  [~ "Avestan"]
        %af  [~ "Afrikaans"]
        %ak  [~ "Akan"]
        %am  [~ "Amharic"]
        %an  [~ "Aragonese"]
        %ar  [~ "Arabic"]
        %as  [~ "Assamese"]
        %av  [~ "Avaric"]
        %ay  [~ "Aymara"]
        %az  [~ "Azerbaijani"]
        %ba  [~ "Bashkir"]
        %be  [~ "Belarusian"]
        %bg  [~ "Bulgarian"]
        %bh  [~ "Bihari"]
        %bi  [~ "Bislama"]
        %bm  [~ "Bambara"]
        %bn  [~ "Bengali"]
        %bo  [~ "Tibetan"]
        %br  [~ "Breton"]
        %bs  [~ "Bosnian"]
        %ca  [~ "Catalan"]
        %ce  [~ "Chechen"]
        %ch  [~ "Chamorro"]
        %co  [~ "Corsican"]
        %cr  [~ "Cree"]
        %cs  [~ "Czech"]
        %cu  [~ "Slavonic"]
        %cv  [~ "Chuvash"]
        %cy  [~ "Welsh"]
        %da  [~ "Danish"]
        %de  [~ "German"]
        %dv  [~ "Maldivian"]
        %dz  [~ "Dzongkha"]
        %ee  [~ "Ewe"]
        %el  [~ "Greek"]
        %en  [~ "English"]
        %eo  [~ "Esperanto"]
        %es  [~ "Spanish"]
        %et  [~ "Estonian"]
        %eu  [~ "Basque"]
        %fa  [~ "Persian"]
        %ff  [~ "Fulah"]
        %fi  [~ "Finnish"]
        %fj  [~ "Fijian"]
        %fo  [~ "Faroese"]
        %fr  [~ "French"]
        %fy  [~ "Frisian"]
        %ga  [~ "Irish Gaelic"]
        %gd  [~ "Scottish Gaelic"]
        %gl  [~ "Galician"]
        %gn  [~ "Guarani"]
        %gu  [~ "Gujarati"]
        %gv  [~ "Manx"]
        %ha  [~ "Hausa"]
        %he  [~ "Hebrew"]
        %hi  [~ "Hindi"]
        %ho  [~ "Hiri Motu"]
        %hr  [~ "Croatian"]
        %ht  [~ "Haitian Creole"]
        %hu  [~ "Hungarian"]
        %hy  [~ "Armenian"]
        %hz  [~ "Herero"]
        %ia  [~ "Interlingua"]
        %id  [~ "Indonesian"]
        %ie  [~ "Occidental"]
        %ig  [~ "Igbo"]
        %ii  [~ "Nuosu"]
        %ik  [~ "Inupiaq"]
        %io  [~ "Ido"]
        %is  [~ "Icelandic"]
        %it  [~ "Italian"]
        %iu  [~ "Inuktitut"]
        %ja  [~ "Japanese"]
        %jv  [~ "Javanese"]
        %ka  [~ "Georgian"]
        %kg  [~ "Kongo"]
        %ki  [~ "Kikuyu"]
        %kj  [~ "Kwanyama"]
        %kk  [~ "Kazakh"]
        %kl  [~ "Kalaallisut"]
        %km  [~ "Central Khmer"]
        %kn  [~ "Kannada"]
        %ko  [~ "Korean"]
        %kr  [~ "Kanuri"]
        %ks  [~ "Kashmiri"]
        %ku  [~ "Kurdish"]
        %kv  [~ "Komi"]
        %kw  [~ "Cornish"]
        %ky  [~ "Kyrgyz"]
        %la  [~ "Latin"]
        %lb  [~ "Luxembourgish"]
        %lg  [~ "Ganda"]
        %li  [~ "Limburgish"]
        %ln  [~ "Lingala"]
        %lo  [~ "Lao"]
        %lt  [~ "Lithuanian"]
        %lu  [~ "Luba-Katanga"]
        %lv  [~ "Latvian"]
        %mg  [~ "Malagasy"]
        %mh  [~ "Marshallese"]
        %mi  [~ "Maori"]
        %mk  [~ "Macedonian"]
        %ml  [~ "Malayalam"]
        %mn  [~ "Mongolian"]
        %mr  [~ "Marathi"]
        %ms  [~ "Malay"]
        %mt  [~ "Maltese"]
        %my  [~ "Burmese"]
        %na  [~ "Nauru"]
        %nb  [~ "Norwegian Bokmål"]
        %nd  [~ "North Ndebele"]
        %ne  [~ "Nepali"]
        %ng  [~ "Ndonga"]
        %nl  [~ "Dutch"]
        %nn  [~ "Norwegian Nynorsk"]
        %no  [~ "Norwegian"]
        %nr  [~ "South Ndebele"]
        %nv  [~ "Navajo"]
        %ny  [~ "Chichewa"]
        %oc  [~ "Occitan"]
        %oj  [~ "Ojibwa"]
        %om  [~ "Oromo"]
        %or  [~ "Oriya"]
        %os  [~ "Ossetian"]
        %pa  [~ "Punjabi"]
        %pi  [~ "Pali"]
        %pl  [~ "Polish"]
        %ps  [~ "Pashto"]
        %pt  [~ "Portuguese"]
        %qu  [~ "Quechua"]
        %rm  [~ "Romansh"]
        %rn  [~ "Rundi"]
        %ro  [~ "Romanian"]
        %ru  [~ "Russian"]
        %rw  [~ "Kinyarwanda"]
        %sa  [~ "Sanskrit"]
        %sc  [~ "Sardinian"]
        %sd  [~ "Sindhi"]
        %se  [~ "Northern Sami"]
        %sg  [~ "Sango"]
        %si  [~ "Sinhala"]
        %sk  [~ "Slovak"]
        %sl  [~ "Slovenian"]
        %sm  [~ "Samoan"]
        %sn  [~ "Shona"]
        %so  [~ "Somali"]
        %sq  [~ "Albanian"]
        %sr  [~ "Serbian"]
        %ss  [~ "Swati"]
        %st  [~ "Sotho"]
        %su  [~ "Sundanese"]
        %sv  [~ "Swedish"]
        %sw  [~ "Swahili"]
        %ta  [~ "Tamil"]
        %te  [~ "Telugu"]
        %tg  [~ "Tajik"]
        %th  [~ "Thai"]
        %ti  [~ "Tigrinya"]
        %tk  [~ "Turkmen"]
        %tl  [~ "Tagalog"]
        %tn  [~ "Tswana"]
        %to  [~ "Tonga"]
        %tr  [~ "Turkish"]
        %ts  [~ "Tsonga"]
        %tt  [~ "Tatar"]
        %tw  [~ "Twi"]
        %ty  [~ "Tahitian"]
        %ug  [~ "Uighur"]
        %uk  [~ "Ukrainian"]
        %ur  [~ "Urdu"]
        %uz  [~ "Uzbek"]
        %ve  [~ "Venda"]
        %vi  [~ "Vietnamese"]
        %vo  [~ "Volapük"]
        %wa  [~ "Walloon"]
        %wo  [~ "Wolof"]
        %xh  [~ "Xhosa"]
        %yi  [~ "Yiddish"]
        %yo  [~ "Yoruba"]
        %za  [~ "Zhuang"]
        %zh  [~ "Chinese"]
        %zu  [~ "Zulu"]
      ==
    ::

ISO language code

    ~zod/main=> (glon %cs)
    [~ "Czech"]
    ~zod/main=> (glon %en)
    [~ "English"]
    ~zod/main=> (glon %mz)
    ~

### `++gnom`

    ++  gnom                                                ::  ship display name
      |=  [[our=@p now=@da] him=@p]  ^-  @t
      =+  yow=(scot %p him)
      =+  pax=[(scot %p our) %name (scot %da now) yow ~]
      =+  woy=((hard ,@t) .^(%a pax))
      ?:  =(%$ woy)  yow
      (rap 3 yow ' ' woy ~)
    ::

Fetch display name from %ames

    ~zod/main=> (gnom [->-< -<-] ~zod)
    '~zod |Tianming|'
    ~zod/main=> (gnom [->-< -<-] ~doznec)
    '~doznec ~doznec'
    ~zod/main=> (gnom [->-< -<-] ~tug)
    '~tug |Go-Daigo|'

### `++gnow`

    ++  gnow
      |=  [who=@p gos=gcos]  ^-  @t
      ?-    -.gos
          %czar                 (rap 3 '|' (rap 3 (glam who)) '|' ~)
          %king                 (rap 3 '_' p.gos '_' ~)
          %earl                 (rap 3 ':' p.gos ':' ~)
          %pawn                 ?~(p.gos %$ (rap 3 '.' u.p.gos '.' ~))
          %duke
        ?:  ?=(%anon -.p.gos)  %$
        %+  rap  3
        ^-  (list ,@)
        ?-    -.p.gos
            %punk  ~['"' q.p.gos '"']
            ?(%lord %lady)
          =+  ^=  nad
              =+  nam=`name`s.p.p.gos
              %+  rap  3
              :~  p.nam
                  ?~(q.nam 0 (cat 3 ' ' u.q.nam))
                  ?~(r.nam 0 (rap 3 ' (' u.r.nam ')' ~))
                  ' '
                  s.nam
              ==
          ?:(=(%lord -.p.gos) ~['[' nad ']'] ~['(' nad ')'])
        ==
      ==
    ::

XX Document

### `++hunt`

    ++  hunt                                                ::  first of unit dates
      |=  [one=(unit ,@da) two=(unit ,@da)]
      ^-  (unit ,@da)
      ?~  one  two
      ?~  two  one
      ?:((lth u.one u.two) one two)
    ::

XX Document

### `++mojo`

    ++  mojo                                                ::  compiling load
      |=  [pax=path src=*]
      ^-  (each twig (list tank))
      ?.  ?=(@ src)
        [%| ~[[leaf/"musk: malformed: {<pax>}"]]]
      =+  ^=  mud
          %-  mule  |.
          ((full vest) [1 1] (trip src))
      ?:  ?=(| -.mud)  mud
      ?~  q.p.mud
        :~  %|
            leaf/"musk: syntax error: {<pax>}"
            leaf/"musk: line {<p.p.p.mud>}, column {<q.p.p.mud>}"
        ==
      [%& p.u.q.p.mud]
    ::

XX Document

### `++mole`

    ++  mole                                                ::  new to old sky
      |=  ska=$+(* (unit (unit)))
      |=  a=*
      ^-  (unit)
      =+  b=(ska a)
      ?~  b  ~
      ?~  u.b  ~
      [~ u.u.b]
    ::

XX Document

### `++much`

    ++  much                                                ::  constructing load
      |=  [pax=path src=*]
      ^-  gank
       =+  moj=(mojo pax src)
      ?:  ?=(| -.moj)  moj
      (mule |.((slap !>(+>.$) `twig`p.moj)))
    ::

XX Document

### `++musk`

    ++  musk                                                ::  compiling apply
      |=  [pax=path src=* sam=vase]
      ^-  gank
      =+  mud=(much pax src)
      ?:  ?=(| -.mud)  mud
      (mule |.((slam p.mud sam)))
    ::

XX Document

### `++saxo`

    ++  saxo                                                ::  autocanon
      |=  who=ship
      ^-  (list ship)
      ?:  (lth who 256)  [who ~]
      [who $(who (sein who))]
    ::

Compute list of ancestors

    ~zod/main=> (saxo ~pittyp-pittyp)
    ~[~pittyp-pittyp ~dalnel ~del]
    ~zod/main=> (saxo ~tasfyn-partyv)
    ~[~tasfyn-partyv ~doznec ~zod]
    ~zod/main=> (saxo ~ractul-fodsug-sibryg-modsyl--difrun-mirfun-filrec-patmet)
    ~[~ractul-fodsug-sibryg-modsyl--difrun-mirfun-filrec-patmet ~zod]

### `++sein`

    ++  sein                                                ::  autoboss
      |=  who=ship  ^-  ship
      =+  mir=(clan who)
      ?-  mir
        %czar  who
        %king  (end 3 1 who)
        %duke  (end 4 1 who)
        %earl  (end 5 1 who)
        %pawn  `@p`0
      ==

Compute direct senior.

    ~zod/main=> (sein ~tasfyn-partyv)
    ~doznec
    ~zod/main=> (sein ~doznec)
    ~zod
    ~zod/main=> (sein ~zod)
    ~zod
    ~zod/main=> (sein ~pittyp-pittyp)
    ~dalnel
    ~zod/main=> (sein ~dalnel)
    ~del
    ~zod/main=> (sein ~ractul-fodsug-sibryg-modsyl--difrun-mirfun-filrec-patmet)
    ~zod

Compute Phonemic base.

    ~zod/main=> (saxo ~rabdec-monfer)
    ~[~rabdec-monfer ~dalnel ~del]
    ~zod/main=> `@rd`~rabdec-monfer
    0x5fd25
    [%rlyd 0x5.fd25]
    0b1.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000
    ~zod/main=> `@p`0x5.fd25
    ~rabdec-monfer

    For `@rd` and `@p` see the [odors](../reference/odors) reference
