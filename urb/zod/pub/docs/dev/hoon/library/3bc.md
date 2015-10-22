section 3bC, Gregorian UTC
==========================

### `++dawn`

Weekday of Jan 1

    ++  dawn                                                ::  weekday of jan 1
      |=  yer=@ud
      =+  yet=(sub yer 1)
      %-  mod  :_  7
      :(add 1 (mul 5 (mod yet 4)) (mul 4 (mod yet 100)) (mul 6 (mod yet 400)))
    ::

Computes which day of the week January 1st falls on for a year `yer`,
producing an atom. Weeks are zero-indexed beginning on Sunday.

`yer` is an unsigned decimal, [`@ud`]().

    ~zod/try=> (dawn 2.015)
    4
    ~zod/try=> (dawn 1)
    1
    ~zod/try=> (dawn 0)
    ! subtract-underflow
    ! exit

------------------------------------------------------------------------

### `++daws`

Weekday of date

    ++  daws                                                ::  weekday of date
      |=  yed=date
      %-  mod  :_  7
      (add (dawn y.yed) (sub (yawn [y.yed m.yed d.t.yed]) (yawn y.yed 1 1)))
    ::

Produces the day of the week of a given date `yed` as an atom. Weeks are
zero-indexed beginning on Sunday.

`yed` is a [`date`]().

    ~zod/try=> (daws [[a=%.y y=2.014] m=6 t=[d=6 h=21 m=9 s=15 f=~[0xa16]]])
    5
    ~zod/try=> (daws (yore -<-))
    2

(second example always returns the current date).

------------------------------------------------------------------------

### `++deal`

Add leap seconds

    ++  deal                                                ::  to leap sec time
      |=  yer=@da
      =+  n=0
      =+  yud=(yore yer)
      |-  ^-  date
      ?:  (gte yer (add (snag n lef:yu) ~s1))
        (yore (year yud(s.t (add n s.t.yud))))
      ?:  &((gte yer (snag n lef:yu)) (lth yer (add (snag n lef:yu) ~s1)))
        yud(s.t (add +(n) s.t.yud))
      ?:  =(+(n) (lent lef:yu))
        (yore (year yud(s.t (add +(n) s.t.yud))))
      $(n +(n))
    ::

Produces a [`date`]() with the 25 leap seconds added.

`yer` is an absolute date, [`@da`]().

    ~zod/try=> (yore (bex 127))
    [[a=%.y y=226] m=12 t=[d=5 h=15 m=30 s=8 f=~]]
    ~zod/try=> (deal `@da`(bex 127))
    [[a=%.y y=226] m=12 t=[d=5 h=15 m=30 s=33 f=~]]
    ~zod/try=> (yore (bex 126))
    [[a=%.n y=146.138.512.088] m=6 t=[d=19 h=7 m=45 s=4 f=~]]

------------------------------------------------------------------------

### `++lead`

Subtract leap seconds

    ++  lead                                                ::  from leap sec time
      |=  ley=date
      =+  ler=(year ley)
      =+  n=0
      |-  ^-  @da
      =+  led=(sub ler (mul n ~s1))
      ?:  (gte ler (add (snag n les:yu) ~s1))
        led
      ?:  &((gte ler (snag n les:yu)) (lth ler (add (snag n les:yu) ~s1)))
        ?:  =(s.t.ley 60)
          (sub led ~s1)
        led
      ?:  =(+(n) (lent les:yu))
        (sub led ~s1)
      $(n +(n))
    ::

Produces an absolute date ([`@ud`]()) with the 25 leap seconds
subtracted.

`ley` is a [`date`]().

    ~zod/try=> (yore `@da`(bex 127))
    [[a=%.y y=226] m=12 t=[d=5 h=15 m=30 s=8 f=~]]
    ~zod/try=> (lead (yore `@da`(bex 127)))
    ~226.12.5..15.29.43
    ~zod/try=> (lead (yore `@da`(bex 126)))
    ~146138512088-.6.19..07.44.39

------------------------------------------------------------------------

### `++dust`

Print UTC format

    ++  dust                                                ::  print UTC format
      |=  yed=date
      ^-  tape
      =+  wey=(daws yed)
      ;:  weld
          `tape`(snag wey (turn wik:yu |=(a=tape (scag 3 a))))
          ", "  ~(rud at d.t.yed)  " "
          `tape`(snag (dec m.yed) (turn mon:yu |=(a=tape (scag 3 a))))
          " "  (scag 1 ~(rud at y.yed))  (slag 2 ~(rud at y.yed))  " "
          ~(rud at h.t.yed)  ":"  ~(rud at m.t.yed)  ":"  ~(rud at s.t.yed)
          " "  "+0000"
      ==
    ::

Produces a [tape]() of the date in UTC format.

`yed` is a [`date`]().

    ~zod/try=> (dust (yore ->-))
    "Tue, 21 Oct 2014 21:35:12 +0000"
    ~zod/try=> (dust [[a=%.y y=2.002] m=10 t=[d=11 h=12 m=20 s=55 f=~]])
    "Fri, 11 Oct 2002 12:20:55 +0000"

------------------------------------------------------------------------

### `++stud`

Parse UTC format

    ++  stud                                                ::  parse UTC format
      |=  cud=tape
      ^-  (unit date)
      =-  ?~  tud  ~ 
          `[[%.y &3.u.tud] &2.u.tud &1.u.tud &4.u.tud &5.u.tud &6.u.tud ~]
      ^=  tud
      %+  rust  cud
      ;~  plug
        ;~(pfix (stun [5 5] next) dim:ag)
      ::
        %+  cook
          |=  a=tape
          =+  b=0
          |-  ^-  @
          ?:  =(a (snag b (turn mon:yu |=(a=tape (scag 3 a)))))
              +(b)
          $(b +(b))
        (ifix [ace ace] (star alf))
      ::
        ;~(sfix dim:ag ace)  
        ;~(sfix dim:ag col)
        ;~(sfix dim:ag col)  
        dim:ag  
        (cold ~ (star next))
      ==
    ::

Accepts a [tape]() containing a date in UTC format and produces the
[unit]() of a [`date`]().

    ~zod/try=> (stud "Tue, 21 Oct 2014 21:21:55 +0000")
    [~ [[a=%.y y=2.014] m=10 t=[d=21 h=21 m=21 s=55 f=~]]]
    ~zod/try=> (stud "Wed, 11 Oct 2002 12:20:55 +0000")
    [~ [[a=%.y y=2.002] m=10 t=[d=11 h=12 m=20 s=55 f=~]]]
    ~zod/try=> (stud "Wed, 11 Oct 2002")
    ~

------------------------------------------------------------------------

### `++unt`

UGT to UTC time

    ++  unt                                                 ::  UGT to UTC time
      |=  a=@da
      (div (sub a ~1970.1.1) (bex 64))
    ::

    ~zod/try=/hom> (unt -<-)
    1.413.927.704
    ~zod/try=> (unt ~20014.1.1)
    569.413.670.400
    ~zod/try=> (unt ~2014.1.1)
    1.388.534.400

Transforms Urbit Galactic Time to UTC time, producing an atom.

`a` is an [atom]().

------------------------------------------------------------------------

### `++yu`

UTC format constants

    ++  yu                                                  ::  UTC format constants
      |%

    ~zod/try=/hom> yu
    <4.pgn 250.tmw 41.cmo 414.rvm 101.jzo 1.ypj %164>

------------------------------------------------------------------------

### `++mon`

Months

      ++  mon  ^-  (list tape)
        :~  "January"  "February"  "March"  "April"  "May"  "June"  "July"
            "August"  "September"  "October"  "November"  "December"
        ==
      ::

Produces a list of [tapes]() containing the 12 months of the year.

    ~zod/try=/hom> mon:yu
    <<
      "January"
      "February"
      "March"
      "April"
      "May"
      "June"
      "July"
      "August"
      "September"
      "October"
      "November"
      "December"
    >>
    ~zod/try=/hom> (snag 1 mon:yu)
    "February"

------------------------------------------------------------------------

### `++wik`

Weeks

      ++  wik  ^-  (list tape)
        :~  "Sunday"  "Monday"  "Tuesday"  "Wednesday"  "Thursday"
            "Friday"  "Saturday"
        ==
      ::

Produces a list of [tapes]() containing the 7 days of the week,
beginning with Sunday.

    ~zod/try=/hom> wik:yu
    <<"Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday">>
    ~zod/try=/hom> (snag 2 wik:yu)
    "Tuesday"
    ~zod/try=/hom> (snag (daws (yore -<-)) wik:yu)
    "Tuesday"

------------------------------------------------------------------------

### `++les`

Leap second dates

      ++  les  ^-  (list ,@da)
        :~  ~2015.7.1 ~2012.7.1  ~2009.1.1  ~2006.1.1  ~1999.1.1  ~1997.7.1
            ~1996.1.1  ~1994.7.1  ~1993.7.1  ~1992.7.1  ~1991.1.1  ~1990.1.1
            ~1988.1.1  ~1985.7.1  ~1983.7.1  ~1982.7.1  ~1981.7.1  ~1980.1.1
            ~1979.1.1  ~1978.1.1  ~1977.1.1  ~1976.1.1  ~1975.1.1  ~1974.1.1
            ~1973.1.1 ~1972.7.1
        ==

Produces a list of the (absolute) dates ([`@da`]) of the 25 leap seconds

    ~zod/try=/hom> les:yu
    ~[
      ~2015.7.1
      ~2012.7.1
      ~2009.1.1
      ~2006.1.1
      ~1999.1.1
      ~1997.7.1
      ~1996.1.1
      ~1994.7.1
      ~1993.7.1
      ~1992.7.1
      ~1991.1.1
      ~1990.1.1
      ~1988.1.1
      ~1985.7.1
      ~1983.7.1
      ~1982.7.1
      ~1981.7.1
      ~1980.1.1
      ~1979.1.1
      ~1978.1.1
      ~1977.1.1
      ~1976.1.1
      ~1975.1.1
      ~1974.1.1
      ~1973.1.1
      ~1972.7.1
    ]
    ~zod/try=/hom> (snag 2 les:yu)
    ~2006.1.1

------------------------------------------------------------------------

### `++lef`

Back-shifted leap second dates

      ++  lef  ^-  (list ,@da)
        :~  ~2015.6.30..23.59.59   ~2012.6.30..23.59.59
            ~2008.12.31..23.59.58  ~2005.12.31..23.59.57
            ~1998.12.31..23.59.56  ~1997.6.30..23.59.55
            ~1995.12.31..23.59.54  ~1994.6.30..23.59.53
            ~1993.6.30..23.59.52   ~1992.6.30..23.59.51
            ~1990.12.31..23.59.50  ~1989.12.31..23.59.49
            ~1987.12.31..23.59.48  ~1985.6.30..23.59.47
            ~1983.6.30..23.59.46   ~1982.6.30..23.59.45
            ~1981.6.30..23.59.44   ~1979.12.31..23.59.43
            ~1978.12.31..23.59.42  ~1977.12.31..23.59.41
            ~1976.12.31..23.59.40  ~1975.12.31..23.59.39
            ~1974.12.31..23.59.38  ~1973.12.31..23.59.37
            ~1972.12.31..23.59.36  ~1972.6.30..23.59.35
        ==
    ::

Produces a list of absolute dates ([`@da`]()) that represent the Urbit
Galactc Time equivalents of the UTC leap second dates in [`++les`](/doc/hoon/library/3bc#++les).

    ~zod/try=/hom> lef:yu
    ~[
      ~2015.6.30..23.59.59
      ~2012.6.30..23.59.59
      ~2008.12.31..23.59.58
      ~2005.12.31..23.59.57
      ~1998.12.31..23.59.56
      ~1997.6.30..23.59.55
      ~1995.12.31..23.59.54
      ~1994.6.30..23.59.53
      ~1993.6.30..23.59.52
      ~1992.6.30..23.59.51
      ~1990.12.31..23.59.50
      ~1989.12.31..23.59.49
      ~1987.12.31..23.59.48
      ~1985.6.30..23.59.47
      ~1983.6.30..23.59.46
      ~1982.6.30..23.59.45
      ~1981.6.30..23.59.44
      ~1979.12.31..23.59.43
      ~1978.12.31..23.59.42
      ~1977.12.31..23.59.41
      ~1976.12.31..23.59.40
      ~1975.12.31..23.59.39
      ~1974.12.31..23.59.38
      ~1973.12.31..23.59.37
      ~1972.12.31..23.59.36
      ~1972.6.30..23.59.35
    ]
    ~zod/try=/hom> (snag 2 lef:yu)
    ~2005.12.31..23.59.57
